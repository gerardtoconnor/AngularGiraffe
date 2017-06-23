module Giraffe.HttpRouter

open System.Threading.Tasks
open Giraffe.Task
open Giraffe.HttpHandlers
open FSharp.Core.Printf
open System.Collections.Generic
open HttpRouter.RouterParsers

type HttpContext() = 
    class end

type Continuation = HttpContext -> Task<HttpContext>

//result of any handler
type HttpHandler = HttpContext -> Task<HttpContext option>

let inline (>=>) (a:HttpHandler) (b:HttpHandler) : HttpHandler =
    fun ctx ->
        task {
            let! ctxo = a ctx
            match ctxo with
            | Some ctx -> return! b ctx
            | None -> return None
        }


let modmatch (ca: char []) =
    let result = Array.zeroCreate<int>(ca.Length)
    let rec modtest m i j= 
        let rec charMod m i j =
            let rec dupeTest mr m i j =
                if j < i then // should be i - 1
                    if mr = result.[j] then //duplicate found
                        //printfn "found duplicate j.[%i]:%i for char i.[%i]:%c on mod %i" j result.[j] i ca.[i] m
                        for z in 0 .. result.Length - 1 do
                            result.[z] <- Unchecked.defaultof<int>
                        modtest (m+1) 0 0  //failed, next mod 
                    else
                        //printfn "no dupe at j.[%i]:%i so onto next j" j result.[j] 
                        
                        dupeTest mr m i (j+1)
                else
                    result.[i] <- mr // add to results
                    //printfn "no duplicates found for char i.[%i]:%c on mod %i" i ca.[i] m
                    charMod m (i+1) 0 //no duplicates so process next char
            
            //start of char mod
            if i < ca.Length then
                let mr = int(ca.[i]) % m
                printfn "char code for %c is %i" ca.[i] mr 
                dupeTest mr m i 0
            else
                //printfn "results are %A" result
                m, Array.min result, Array.max result 
        charMod m 0 0
    modtest 2 0 0




//this is a model for further performant router that uses struct nodes
type CrawlerState =
| FullScan = 0uy
| MidMatching = 1uy
| ChildNodeMatchScan = 2uy
| FinalMatchedCloseout = 3uy
| EndPathCompleteMatch = 4uy
| EndPathEndMatcingCompleteMatch = 5uy

type INodeType =
| UnInit = 0uy
| EmptyNode = 1uy
| HandlerFn = 2uy
| SubRouteFn = 3uy
| ApplyMatchFn = 4uy
| MatchCompleteFn = 5uy
// performance Node Trie

type Instr =
| Hop = 0uy
| Retry = 1uy
| Terminal = 2uy     

[<Struct>]
type AryNode =
    val Char : byte
    val Hop : int16
    val Instr : Instr
    new (char,hop,instr) = { Char=char ; Hop=hop ; Instr=instr }

let runPath (nodes:AryNode []) (fns:(bool*(unit->string)) []) (path:string) (ctx:HttpContext) =
    let rec go p n acc =
        match (byte path.[p]) = nodes.[n].Char with
        | true ->
            match nodes.[n].Instr with 
            | Instr.Terminal ->
                match fns.[int nodes.[n].Hop] with
                | true,  fn -> go (p + 1) (n + 1) ((fn ())::acc)
                | false, fn -> Some((fn ())::acc)
            | _ -> go (p + 1) (int nodes.[n].Hop) acc        
        | false ->
            match nodes.[n].Instr with
            | Instr.Retry -> go (p + 1) (n + 1) acc
            | _ -> None
    go 0 0 []

type PathChunk =
| Token of string
| Parse of (string -> int -> int -> obj option)

type PathType =
| Path of string * HttpHandler
| SubRoute of string * HttpHandler
| Match of PathChunk list * (obj -> HttpHandler)

let route (path:string) (fn:HttpHandler) = Path (path,fn)

let subRoute (path:string) (fn:HttpHandler) = SubRoute (path,fn)

let routef (fmt:StringFormat<'U,'T>) (fn:'T -> HttpHandler) =
    let path = fmt.Value
    let last = path.Length - 1

    let rec go i acc =
        let n = path.IndexOf('%',i)     // get index of next '%'
        if n = -1 || n = last then
            // non-parse case & end
            Token( path.Substring(i,n - i) ) :: acc
        else
            let fmtc = path.[n + 1]
            if fmtc = '%' then 
                go (n + 2) (Token( path.Substring(i,n - i) ) :: acc)
            else 
                match formatStringMap.TryGet fmtc with
                | false, prs ->
                    failwith <| sprintf "Invalid parse char in path %s, pos: %i, char: %c" path n fmtc
                | true , prs ->
                    go (n + 2) (Parse(prs)::acc)
    
    let fnCast = fun (o:obj) -> (o :?> 'T) |> fn 
    Match(go 0 [], fnCast) 

/// Domain Types
////////////////

type IRoute =
    abstract member Path : PathChunk List
    abstract member Fn : HttpHandler

let handler : HttpHandler = Some >> Task.FromResult 

type PathExpr =
| Route of string
| Routef of StringFormat<_,_>
| SubRoute of string


type PathType2 =
| Path of string * HttpHandler
| Match of MatchChunk list * (obj -> HttpHandler)
| PathRoute of string * HttpHandler * RouteNode
| MatchRoute of MatchChunk list * (obj -> HttpHandler) * RouteNode
with static member (>=>) (pe:PathExpr) ()

and PathNode() =
    
    member __.BindPath (path:string,h:HttpHandler) =
        ()

    member __.BindPath (path:string,hn:HttpHandler*RouteNode) =
        ()

    member __.BindParse<'U,'T> (path:StringFormat<'U,'T>,h:('T -> HttpHandler)) =
        ()

    member __.BindParse<'U,'T> (path:StringFormat<'U,'T>,h:('T -> HttpHandler)*RouteNode) =
        ()

and RouteNode(pnl : PathNode list) =
    let mutable routes = []
    member __.Routes
        with get() = routes
        and set v = routes <- v
    static member (>=>) (h:HttpHandler) (n:RouteNode) =
        (h,n)

and routeBase() =
    
    let aryNodes = Array.zeroCreate<AryNode>(0)
    let fnNodes = Array.zeroCreate<HttpHandler>(0)
    // traverse the tree and map the functions to arrays
    member __.ProcessTree (h:HttpHandler,n:RouteNode) =
        // processing of entire route tree here
        ()
    static member (>=>) (b:RouteBase) (hr:HttpHandler * RouteNode) =
        b.ProcessTree hr
    static member (>=>) (h:HttpHandler) (b:RouteBase) =
        fun (ctx : HttpContext) ->
            runPath aryNodes fnNodes ctx.Request.Path.Value ctx

let inline route (path:string) (tail:'T) = 
    let pn = PathNode()
    pn.BindPath (path, tail)

let (>=|) (h:HttpHandler) (ls:PathNode list) =
    (h,ls)

// let webapi = 
//     routeBase()  // where array builder starts, it needs to recieve other nodes some how
//         >=> // this needs to be overriden to deliver node to rbase ??
//         RouteNode [ // this needs to get a collection of paths that can later be parsed incl child nodes
//             PathNode "/about" >=> // having immeadiatly after may not work as fn/case needs httphandler to pack (as well as child nodes!?)
//                 authenticationHandler >=> // looks like normal binding but how can node be handed back!?
//                     RouteNode [ 
//                         PathNode "/auth" >=> text "you are authenticated"
//                     ]
// ]

let webapp = 
    GET >=| [
        route "/about" >=> text "about"
        route "/auth"  >=> 
            choose [
                AuthHandler >=| [
                    path "/authorised user" >=> text "/authorised user"
                    path "/authorised manger" >=> text "/authorised manger"
                ]
                UnAuthHandler >=> text "You are not authorised"
            ]
        route "/other" >=> text "other"
    ]


//((cts |> fn arg1) |> fn arg2)  
//hndl

// [<Struct>]
// type State = {
//     mutable succ : Continuation
//     mutable fail : Continuation
//     ctx : HttpContext
//     }

// type State2 = 
//     struct
//         val mutable succ : Continuation []
//         val mutable succPos : int
//         val mutable fail : Continuation []
//         val mutable failPos : int
//         val ctx : HttpContext
//         member x.Succ
//             with get() = 
//                 match x.succPos with 
//                 | -1 -> x.succ.[0] x.ctx 
//                 | _ ->
//                     x.succPos <- x.succPos - 1
//                     x.succ.[x.succPos] x.ctx

//         member x.Fail
//             with get() = 
//                 match x.failPos with 
//                 | -1 -> x.fail.[0] x.ctx 
//                 | _ ->
//                     x.succPos <- x.succPos - 1
//                     x.succ.[x.succPos] x.ctx
                
//         new(ictx) = { ctx = ictx ; succ = Unchecked.defaultof<Continuation> ; fail = Unchecked.defaultof<Continuation> }
//     end

// let state = {
//     succ=Unchecked.defaultof<Continuation>;
//     fail=Unchecked.defaultof<Continuation>;
//     ctx=Unchecked.defaultof<HttpContext>
//     }

// let State2 = State2(Unchecked.defaultof<HttpContext>)

// let (>=>) (a:Continuation) (b:Continuation) = 
//     fun (s:State2) -> 
//         let s2 = State2(ctx)
//         s2.succ <- s.succ
//         s.succ <- b


