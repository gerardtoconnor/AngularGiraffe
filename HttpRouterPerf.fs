module Giraffe.HttpRouter

open NonStructuralComparison
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

let typeMap = function
    | 'b' -> typeof<bool>   // bool
    | 'c' -> typeof<char>   // char
    | 'i' -> typeof<int>    // int
    | 'd' -> typeof<int64>  // int64
    | 'f' -> typeof<float>  // float
    | _   -> typeof<string> // string

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
| FnEnd = 4uy
| FnEndOrNext = 5uy       // end function at last character (Handler/MatchComplete)
| FnFinish = 6uy    // parse mid to end path function represents (MatchToEnd)
| FnFinishOrNext = 7uy    // parse midToEnd and match, try match next before parse finish /%
| FnFinishOrRetry = 8uy  // where at last, FnFinish, otherwise Retry eg '/' , '/about/ -> '/' has both functionality
| FnContOrFinish = 9uy   // where at last, FnFinish, otherwise FnCont eg '/a%s' , '/a%s/b -> 'a' has both functions, need to test partial first
| NOP = 100uy 

//FnContOrFinish requires hack due to two functions (i=Cont / i+1=Fin ?)

(*
    Route permutaions
    /               //../ node is (End|Next)
    /test           //..t node is (Next(Hop)|Cont|Finish|||End)
    /test%s
    /test%s/tail
    /testy
    /about
    /telephone
    /tent/%i/next   // ../ node is (Contx2) >> parseTry State (/|s)
    /tent/%s/next
    /tent/%spost    
    List of nodes needed
    /,f#,FinNxt|t,h#6,rty|a,h#,nxt|b,_,nxt|o,_,nxt|u,_,nxt|t,f#,Fin|e,_,n| ...
    ... |t,_,nxt|/,f#,Cont|/,h#,rty|s,_,n|p|o|s|t,f#,Fin|n|e|x|t,f#,fin|
    ... t,f#,ContFin|

    ..t,f#,EndNext|y,f#,EndRetry|
*)

type HandleFn =
| HandleFn      of HttpHandler      // plain handler 
| ParseStart    of int * Parser     // argCount * parser
| ParseMid      of int * Parser     // argIndex * parser
| ParseComplete of (System.Type []) * (obj -> HttpHandler) // types * fn 
| ParseApplyEnd of (System.Type []) * Parser * (obj -> HttpHandler) // types * parser * fn
| ParseMulti    of HandleFn list

// tree compression node
type CNode(c:char) =
    member val Char = c with get
    member val Edges = Dictionary<char,CNode>() with get
    member val EndFn = None : HttpHandler option with get,set
    member val FnList = [] with get,set  

type FnFlag =
| End = 0uy
| Retry = 1uy

[<Struct>]
type FnNode =
    val OnFail   : FnFlag
    val Handle : HandleFn
    new (flag:FnFlag, handle:HandleFn) = { OnFail = flag ; Handle = handle} 

[<Struct>]
type AryNode =
    val Char  : byte
    val Hop   : uint16
    val Instr : Instr
    new (char,hop,instr) = { Char = char ; Hop = hop ; Instr = instr }
    new (char:char,hop:int,instr) = { Char = byte char ; Hop = uint16 hop ; Instr = instr }

let runPath (path:string) (nodes:AryNode []) (fns:Dictionary<int,string>) =
    let rec go p n rt =
        match (byte path.[p]) = nodes.[n].Char with
        | true ->
            match n = (int nodes.[n].Hop) with
            | true -> Some(fns.[n])
            | false -> go (p + 1) (int nodes.[n].Hop) nodes.[n].Retry          
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
    member pn.AddHandler (h:HttpHandler) =
        match pn.HandleChain with
        | Some ph -> pn.HandleChain <- Some(ph >=> h)
        | None    -> pn.HandleChain <- Some h
        pn
    member pn.AddChildPaths (rnl:PathNode list) =
        pn.ChildRoute <- rnl
        pn
    // overloads
    static member (=>) (pn:PathNode,h:HttpHandler) =  pn.AddHandler  h

    static member (=>) (pn:PathNode,rn:PathNode list) = pn.AddChildPaths rn

///Compose Extentions
type ComposeExtension = ComposeExtension with
    static member        (?<-) (ComposeExtension, (a:PathNode) , (b:HttpHandler)) = a.AddHandler b
    static member        (?<-) (ComposeExtension, (a:PathNode) , (b:PathNode list)) = a.AddChildPaths b
    static member inline (?<-) (ComposeExtension, a , b) = compose a b
let inline (>=>) a b = (?<-) ComposeExtension a b

let parsef<'T when 'T : struct> (fmt:StringFormat<'U,'T>,fn:'T -> HttpHandler) =
    let path = fmt.Value
    let last = path.Length - 1
    let rec go i acc =
        let n = path.IndexOf('%',i)     // get index of next '%'
        if n = -1 || n = last then
            // non-parse case & end
            let tl = Token( path.Substring(i,n - i) ) :: acc
            Handlef(tl,(fun (o:obj) -> o :?> 'T |> fn))      
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

//

let router (paths: PathNode list) =
    
    let ary = ResizeArray<AryNode>()
    let fns = ResizeArray<HandleFn>()

    let rec go (n:CNode) = 
        let addEdges (dict:Dictionary<_,_>) =
            let edgeLast = dict.Count - 1
            let ipos = ary.Count // take snapshot of initial position to update placeholders later (next node)
            let nodes , _ = 
                dict 
                |> Seq.fold
                    (fun (nodes,ec) kvp ->
                        if ec >= edgeLast then 
                            AryNode(kvp.Key,ary.Count,Instr.Next) |> ary.Add
                            go kvp.Value // !! Run path for last node now (so on next ittr arycount updated )
                            nodes,ec //should always be final fold
                        else
                            // create placeholder retry array from edge chars, last is next into its path,
                            // these will be updated after child runs with relevant hop index numbers 
                            AryNode(kvp.Key,0,Instr.Retry) |> ary.Add
                        // paths computed back to front (to allow last cont) so put in list
                            (ec,kvp.Value) :: nodes ,(ec + 1) )
                    ([],0)
            
            // now using nodelist, with placeholders in, compute child branches & update placeholders
            // with postiions each time
            nodes 
            |> List.iter (fun acc (i,node) ->  
                            let onode = ary.[ipos + i] // get place holder
                            ary.[ipos + i] <- AryNode(onode.Char,ary.Count,onode.Instr) // update placeholder with valid hop 
                            go node )

        let rec addFunctions (fls: HandleFn list) =
            match fls with
            | [] -> ()
            | h :: t ->
                match h with
                | HandleFn h        ->  // plain handler
                    () // this overlaps with the End Fn and not possible !?!?
                | ParseStart (i,p)  -> // argCount * parser
                    
                | ParseMid (i,p)    -> // argIndex * parser
                | ParseComplete (ta,ofh)   -> // types * fn 
                | ParseApplyEnd (ta,p,ofh) -> // types * parser * fn


        match n.EndFn with
        | Some (h:HttpHandler) -> 
            match n.Edges.Count , n.FnList.IsEmpty with
            | 0  , true -> // No Edges or functions other then end  
                AryNode(n.Char,fns.Count,Instr.FnEnd) |> ary.Add // fns.Count = next index about to be added
                fns.Add (HandleFn h)
                //end
            | ec , true -> // additional Edges as well as ending
                AryNode(n.Char,fns.Count,Instr.FnEndOrNext) |> ary.Add
                fns.Add (HandleFn h) 
                addEdges n.Edges
            | 0 , false -> // function continuations ...
                
                AryNode(n.Char,fns.Count,Instr.FnEndOrNext) |> ary.Add            

                
        | None ->

    //using now compiled arrays, provide handler to process path queries
    fun ctx ->
        runPath aryNodes fnNodes ctx.Request.Path.Value ctx        

// handler functions
let inline route (path:string) = PathNode(Route path)

// type Bindy() =
//     member x.EatMe<'U,'T> (sf:StringFormat<'U,'T>) (fn : 'T -> HttpHandler) (v2:obj) = v2 :?> 'T |> fn
// let bindy = Bindy()

let inline routef (fmt:StringFormat<'U,'T>) (fn:'T -> HttpHandler) =
    let path = fmt.Value
    let last = path.Length - 1

    let rec go i acc =
        let n = path.IndexOf('%',i)     // get index of next '%'
        if n = -1 || n = last then
            // non-parse case & end
            let tl = Token( path.Substring(i,n - i) ) :: acc
            PathNode(Routef(tl,(fun (o:obj) -> o :?> 'T |> fn)))       
        else
            let fmtc = path.[n + 1]
            if fmtc = '%' then 
                go (n + 2) (Token( path.Substring(i,n - i) ) :: acc)
            else 
                match formatMap.TryGet fmtc with
                | false, prs ->
                    failwith <| sprintf "Invalid parse char in path %s, pos: %i, char: %c" path n fmtc
                | true , prs ->
                    go (n + 2) (Parse(prs)::acc)
    go 0 []

let text (v:string) = fun (ctx:HttpContext) -> ctx |> Some |> Task.FromResult 
let pn = route "/about"
let pn2 = pn >=> text "about"

let webapp = router [
                route "/about" >=> text "about" >=> text "again"
                route "/auth"  >=> [
                    route "/cats" >=> text "cats"
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

(*
    Route build process
    1  take path lists and compress list to remove overlaps (into trie structure?)
    2  once paths are into trie, (with child path lists not not proccessed, stored at end nodes) can begin crawl
    3  in each node, lay down path
    4  if Fns & child nodes, combo flag, fns added to fnAry, index back added to Hop
    5  if childnodes, lay down retry ary
*)







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


type IFlag =
| Next =        0b00000001        // if match, move to next
| Hop =         0b00000010         // if match, hop to node at HOP val ?? needed?
| Retry =       0b00000100       // when matching multiple routes, if matched, jump to HOP, else cont to next node
| FnContinue =  0b00001000  // a partial match/subroute that allows matching to cont (move next) while fns pulled
| FnFinish =    0b00010000    // ending function that requires no further matching, get fn and go
| NOP =         0b00000000

let flag = IFlag.FnFinish ||| IFlag.Retry
if flag = (IFlag.FnContinue ||| IFlag.Retry) then printf "&&& works!"
flag = (IFlag.FnContinue ||| IFlag.Retry)
flag = (IFlag.FnContinue ||| IFlag.Retry)
let err = IFlag.Next ||| IFlag.Retry
let bitval (v:IFlag) (p) = v &&& (IFlag.Next <<< p) 
bitval flag 2
