module Giraffe.HttpRouter

open System.Threading.Tasks
open Giraffe.Task
open Giraffe.HttpHandlers
open FSharp.Core.Printf
open System.Collections.Generic
open Giraffe.HttpRouter.RouterParsers

type HttpContext() = 
    class end

//type Continuation = HttpContext -> Task<HttpContext>

//result of any handler
type HttpHandler = HttpContext -> Task<HttpContext option>

let compose (a:HttpHandler) (b:HttpHandler) : HttpHandler =
    fun ctx ->
        task {
            let! ctxo = a ctx
            match ctxo with
            | Some ctx -> return! b ctx
            | None -> return None
        }

let (>=>) = compose 
let handler : HttpHandler = Some >> Task.FromResult 

type Parser = string -> int -> int -> struct(bool*obj)

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
////////////////////////

type Instr =
| Next = 0uy        // if match, move to next
| Hop = 1uy         // if match, hop to node at HOP val ?? needed?
| Retry = 2uy       // when matching multiple routes, if matched, jump to HOP, else cont to next node
| FnContinue = 3uy  // a partial match/subroute that allows matching to cont (move next) while fns pulled
| FnFinish = 4uy    // ending function that requires no further matching, get fn and go
| FnFinishOrRetry = 5uy  // where at last, FnFinish, otherwise Retry eg '/' , '/about/ -> '/' has both functionality
| FnContOrFinish = 6uy   // where at last, FnFinish, otherwise FnCont eg '/a%s' , '/a%s/b -> 'a' has both functions, need to test partial first

type HandleFn =
| HandleFn      of HttpHandler      // plain handler 
| ParseStart    of int * Parser     // argCount * parser
| ParseMid      of Parser           // parser
| ParseComplete of (System.Type []) * int * (obj -> HttpHandler) // types * argCount * fn 
| ParseApplyEnd of (System.Type []) * int * Parser * (obj -> HttpHandler) // types * argCount * parser * fn
| ParseMulti    of HandleFn list

[<Struct>]
type AryNode =
    val Char  : byte
    val Hop   : int16
    val Instr : Instr
    new (char,hop,instr) = { Char = char ; Hop = hop ; Instr = instr }

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

/// Domain Types
////////////////
type PathChunk =
| Token of string
| Parse of Parser

type PathExpr =
| Route of string
| Routef of (PathChunk list) * (obj -> HttpHandler)

type HandlerMap =
| Handle of string * HttpHandler
| Handlef of (PathChunk list) * (obj -> HttpHandler)

type PathNode(pe : PathExpr) =
    member val ChildRoute = [] with get,set
    member val HandleChain = None  with get,set 
    
    member x.GetBinding () =
        match pe , x.HandleChain with
        | Routef (pcl,fn) , Some hc -> Handlef(pcl,(fun (o:obj) -> fn o >=> hc ))
        | Routef (pcl,fn) , None    -> Handlef(pcl,fn)
        | Route path      , Some hc -> Handle(path,hc)
        | Route path      , None    -> failwith "no handlers were provided for path:" + path 
    // overloads
    static member (=>) (pn:PathNode,h:HttpHandler) = 
        match pn.HandleChain with
        | Some ph -> pn.HandleChain <- Some(ph >=> h)
        | None    -> pn.HandleChain <- Some h
        pn
    static member (=>) (pn:PathNode,rn:PathNode list) = 
        pn.ChildRoute <- rn
        pn

type AryBuilder() =
    let ary = ResizeArray<AryNode>()
    let aryPos = 0
    let fns = ResizeArray<HandleFn>()
    let fnsPos = 0


let router (paths: PathNode list) =
    
    let ary = ResizeArray<AryNode>()
    let fns = ResizeArray<HandleFn>()
    

    let placeholder (c:char) = AryNode(c,0,Instr.Next)
    // let aryNodes = Array.zeroCreate<AryNode>(0)
    // let fnNodes = Array.zeroCreate<HttpHandler>(0)
    // traverse the tree and map the functions to arrays
    let rec brancher i cl state =


    let rec tokenize i cl state =
        match cl with
        | Token tk ->
            for ti in 0 .. tk.Length - 1 do
                AryNode(byte(tk.Chars ti),i + 1, Instr.Next) // <<<<<<<<<<< not checked
        | Parse pr -> 
    
    let rec branchesNeeded i ls acc =
        let processStr str =
            if str.Length > 0 
            then ary.Add (placeholder str.[0])
                   branchesNeeded (i + 1) t (i + 1)
            else failwith (sprintf "Invalid empty route format token %A" pcl)        
        
        match ls with
        | [] -> acc
        | h :: t ->
            match h.GetBinding() with
            | Handlef (pcl,_) ->
                match pcl with
                | (Token str) :: _ -> if str.Length > 0 
                                      then ary.Add (placeholder str.[0])
                                            branchesNeeded (i + 1) t (i + 1)
                                      else failwith (sprintf "Invalid empty route format token %A" pcl)
                | _ -> failwith (sprintf "Invalid route format %A" pcl) 
            | Handle  (str,_) -> if str.Length > 0
                                 then ary.Add placeholder 
                                        branchesNeeded (i + 1) t (i + 1)
                                 else failwith (sprintf "Invalid empty route format token %A" 

    //for each set of branches set up a retry array
    let rec brancher i ls state =
        match ls with
        | [] -> ()
        | h :: t ->
            match h.GetBinding() with
            | Handlef (pcl,fn) ->
                match pcl.head with
                | Token str -> if str.Length > 0 then str.[0] else ()
                | Parse prs -> () //todo: need figure out handling
            | Handle  (str,fn) -> if str.Length
            go t 
    let rec go pls state =
        // add branch placeholders

    go paths []

    fun ctx ->
        runPath aryNodes fnNodes ctx.Request.Path.Value ctx        

// handler functions
let inline route (path:string) = PathNode(Route path)

type Bindy() =
    member x.EatMe<'U,'T> (sf:StringFormat<'U,'T>) (fn : 'T -> HttpHandler) (v2:obj) = v2 :?> 'T |> fn
let bindy = Bindy()

let inline routef (fmt:StringFormat<'U,'T>) (fn:'T -> HttpHandler) =
    let path = fmt.Value
    let last = path.Length - 1

    let rec go i acc =
        let n = path.IndexOf('%',i)     // get index of next '%'
        if n = -1 || n = last then
            // non-parse case & end
            let tl = Token( path.Substring(i,n - i) ) :: acc
            PathNode(Routef(tl,bindy.EatMe<'U,'T> fmt fn))       
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
    go 0 []

let text (v:string) = fun (ctx:HttpContext) -> ctx |> Some |> Task.FromResult 
let pn = route "/about"
let pn2 = pn => text "about"

let webapp = routeBase [
                route "/about" => text "about" => text "again"
                route "/auth"  => [
                    route "/cats" => text "cats"
                    routef "/dog%is-sds" (fun v -> text v)                    
                        ]
                    // choose [
                    //     AuthHandler >=| [
                    //         path "/authorised user" >=> text "/authorised user"
                    //         path "/authorised manger" >=> text "/authorised manger"
                    //     ]
                    //     UnAuthHandler >=> text "You are not authorised"
                    // ]
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


