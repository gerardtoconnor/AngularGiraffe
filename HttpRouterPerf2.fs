module Giraffe.HttpRouter

open System
open OptimizedClosures 
open NonStructuralComparison
open System.Threading.Tasks
open Giraffe.Task
open Microsoft.AspNetCore.Http
open Giraffe.HttpHandlers
open FSharp.Core.Printf
open Microsoft.FSharp.Reflection
open System.Collections.Generic
open Giraffe.RouterParsers

let [<Literal>] Parsy    = '^' // this character is invalid url char so can be used internally as placeholder instruction
let [<Literal>] BParsy = 94uy    
let [<Literal>] Endy    = '|'
let [<Literal>] BEndy    = 124uy
let [<Literal>] Forky = '¬'
let [<Literal>] BForky = 172uy

let typeMap = function
    | 'b' -> typeof<bool>   // bool
    | 'c' -> typeof<char>   // char
    | 'i' -> typeof<int>    // int
    | 'd' -> typeof<int64>  // int64
    | 'f' -> typeof<float>  // float
    | _   -> typeof<string> // string

// performance Node Trie
////////////////////////


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

/// Domain Types
////////////////
type PathChunk =
| Token of string
| Parse of char

type PathExpr =
| Route of string
| Routef of int * (PathChunk list) * (obj -> HttpHandler)

type HandlerMap =
| SubRoute of string * HandlerMap list
| Handle of string * HttpHandler
| Handlef of int * (PathChunk list) * (obj -> HttpHandler)

type HandleFn =
| HandleFn            of HttpHandler      // plain handler 
| ParseStart          of int * Parser * int    // argCount * parser * onFail
| ParseMid            of int * Parser * int    // argIndex * parser * onFail
| ParseCompleteSingle of (obj -> HttpHandler) // types * fn * onFail 
| ParseApplyEndSingle of Parser * (obj -> HttpHandler) // types * parser * fn * onFail
| ParseCompleteTuple  of (obj [] -> HttpHandler)  // types * fn * onFail 
| ParseApplyEndTuple  of Parser * (obj [] -> HttpHandler) // types * parser * fn * onFail

type CHandleFn =
| CHandleFn            of HttpHandler      // plain handler 
| CParseStart          of int * Parser * CNode     // argCount * parser
| CParseMid            of int * Parser * CNode    // argIndex * parser
| CParseCompleteSingle of (obj -> HttpHandler) // types * fn 
| CParseApplyEndSingle of Parser * (obj -> HttpHandler) // types * parser * fn
| CParseCompleteTuple  of (obj [] -> HttpHandler) // types * fn 
| CParseApplyEndTuple  of Parser * (obj [] -> HttpHandler) // types * parser * fn

// tree compression node (temporary structure to compress paths to tree before laying array)
and CNode(c:char) =
    member val Char = c with get
    member val Edges = Dictionary<char,CNode>() with get
    member val EndFn = None : HttpHandler option with get,set
    member val FnList = [] with get,set
    member x.PathStep (v:char)  =
        match x.Edges.TryGetValue v with
        | true, node -> node
        | false, _ -> 
            let node = CNode(v)
            x.Edges.Add(v,node)
            node
    member x.PathTraverse (p:string) =
        let rec go i (n:CNode) =
            if i < p.Length 
            then go (i + 1) (n.PathStep p.[i])
            else n
        go 0 x
    member x.AddFn (fn:CHandleFn) =
        x.FnList <- fn :: x.FnList
        // match fn with
        // | CHandleFn f -> x.EndFn <- Some f
        // | ofn -> x.FnList <- ofn :: x.FnList
    member x.AddHandlerMap (hm:HandlerMap) =
        match hm with
        | SubRoute (path, fnl) ->
            let fnode = x.PathTraverse path
            for fn in fnl do
                fnode.AddHandlerMap fn
        | Handle (path,fn) -> 
            let fnode = x.PathTraverse path
            //CHandleFn(fn) |> fnode.AddFn 
            fnode.EndFn <- Some fn
        | Handlef (argCount,pcl,omap) ->
            //let argCount = pcl |> List.fold (fun acc v -> match v with | Parse _ -> acc + 1 | _ -> acc ) 0
            let tary = Array.zeroCreate<System.Type>(argCount)
            let getTplFn () =
                let tupleType = FSharpType.MakeTupleType tary
                FSharpValue.PreComputeTupleConstructor(tupleType)
            let rec go ls (n:CNode) (argIdx:int) =
                match ls with
                | [] -> ()
                | [Parse c] -> // completes in match
                    if argCount > 1 then
                        tary.[argIdx] <- typeMap c
                        let oamap = getTplFn ()
                        CParseApplyEndTuple(formatMap.[c],oamap >> omap) |>  n.AddFn
                    else
                        CParseApplyEndSingle(formatMap.[c],omap) |>  n.AddFn
                | [Token t] -> // completes on token
                    let cnode = n.PathTraverse t
                    if argCount > 1 then
                        let oamap = getTplFn ()
                        CParseCompleteTuple(oamap >> omap) |> cnode.AddFn
                    else
                        CParseCompleteTuple(omap) |> cnode.AddFn
                | head :: tail ->
                    match head with
                    | Token t ->
                        let cnode = n.PathTraverse t
                        go tail cnode argIdx
                    | Parse c ->
                        if argIdx = 0 then
                            tary.[0] <- typeMap c
                            let cnode = CNode(Parsy) //<<< HACK
                            CParseStart(argCount,formatMap.[c],cnode) |> n.AddFn
                            go tail cnode (argIdx + 1)
                        else
                            tary.[argIdx] <- typeMap c
                            let cnode = CNode(Parsy) //<<< HACK
                            CParseMid(argIdx,formatMap.[c],cnode) |> n.AddFn
                            go tail cnode (argIdx + 1)
            go pcl x 0

[<Struct>]
type AryNode =
    val Char  : byte
    val Hop   : uint16
    
    new (char,hop) = { Char = char ; Hop = hop }
    new (char:char,hop:int) = { Char = byte char ; Hop = uint16 hop }


/// PathNode
/////////////////////////

type PathNode(pe : PathExpr) =
    member val ChildRoute = [] with get,set
    member val HandleChain = None  with get,set 
    
    member x.GetBinding () =
        match pe , x.HandleChain, x.ChildRoute with
        | Routef (ac,pcl,fn) , Some hc , [] -> Handlef(ac,pcl,(fun (o:obj) -> fn o >=> hc ))
        | Routef (ac,pcl,fn) , None    , [] -> Handlef(ac,pcl,fn)
        | Route path         , Some hc , [] -> Handle(path,hc)
        | Route path         , None    , ls -> SubRoute( path , ls |> List.map (fun (pn:PathNode) -> pn.GetBinding() ) )
        | _,_,_    ->  failwith (sprintf "no handlers were provided for path:%A" pe)
    
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

/// Compose Extentions
/////////////////////////////////

type ComposeExtension = ComposeExtension with
    static member        (?<-) (ComposeExtension, (a:PathNode) , (b:HttpHandler)) = a.AddHandler b
    static member        (?<-) (ComposeExtension, (a:PathNode) , (b:PathNode list)) = a.AddChildPaths b
    static member inline (?<-) (ComposeExtension, a , b) = compose a b
let inline (>=>) a b = (?<-) ComposeExtension a b

/// Router handle compiler
/////////////////////////////////

type ParseState(argCount:int) = 
    member val Retry = 0 with get,set
    member val Parsers = Array.zeroCreate<Parser>(argCount) with get,set
    member val PStart = Array.zeroCreate<int>(argCount) with get,set
    member val PEnd = Array.zeroCreate<int>(argCount) with get,set
    member val MaxAttempt = 0 with get,set
    member val CurArg = 0 with get,set
    member val TotalArgs = argCount with get

let router (paths: PathNode list) =

    let nary,fary = 

        let ary = ResizeArray<AryNode>()
        let fns = ResizeArray<HandleFn>()

        let rec go (n:CNode) = 
                
            let rec fnmap (ls: CHandleFn list) = 
                
                let rec addParseNode (pn:CNode) =
                    match pn.Edges.Count, pn.FnList with
                    | 1 , [] ->
                        for kvp in pn.Edges do // knowing there is only one edge
                            AryNode(kvp.Key,ary.Count + 1) |> ary.Add
                            addParseNode kvp.Value
                    | 0 , fns -> fnmap fns 
                    | _ , _   -> failwith "invalid addParseNode scenario" 
                
                let completionFn (c:char) (fn:HandleFn) =
                    AryNode(c,fns.Count) |> ary.Add
                    fn |> fns.Add //on completion just set to 0 as goes to next depending on char 
                    
                let parseFn (pn:CNode) (setfn:int -> HandleFn) =
                    let fnpos = fns.Count
                    AryNode(Parsy,fnpos) |> ary.Add
                    setfn 0 |> fns.Add
                    addParseNode pn // adds rest of node
                    fns.[fnpos] <- setfn ary.Count

                let fullfnmap (c:char) (h: CHandleFn) =
                    match h with
                    | CParseStart (i,p,pn)  -> parseFn pn (fun fp -> ParseStart(i,p,fp))
                    | CParseMid   (i,p,pn)  -> parseFn pn (fun fp -> ParseMid(i,p,fp))
                    | CParseCompleteSingle (ofh)     -> completionFn c (ParseCompleteSingle(ofh))
                    | CParseCompleteTuple  (ofh)     -> completionFn c (ParseCompleteTuple(ofh))
                    | CParseApplyEndSingle (prs,ofh) -> completionFn c (ParseApplyEndSingle(prs,ofh))
                    | CParseApplyEndTuple  (prs,ofh) -> completionFn c (ParseApplyEndTuple(prs,ofh))
                    | _ -> failwith "plain handler in parse pipeline"
                
                match ls with
                | []     -> ()
                | [h]    -> fullfnmap Endy h
                | h :: t ->
                    fullfnmap Forky h
                    fnmap t

            if n.Edges.Count > 0 then
                //debug
                if n.Edges.Count > 1 then
                    printf "#%i edge dict {" n.Edges.Count
                    for kvp in n.Edges do
                        printf "%c," kvp.Key
                    printfn "}"

                let edgeLast = n.Edges.Count - 1
                let ipos = ary.Count // take snapshot of initial position to update placeholders later (next node)
                let nodes , _ = 
                    n.Edges 
                    |> Seq.fold
                        (fun (nodes,ec) kvp ->
                            if ec >= edgeLast then 

                                match kvp.Value.EndFn with
                                | Some fn ->
                                    if kvp.Value.Edges.Count = 0 && kvp.Value.FnList.IsEmpty then
                                        AryNode(Endy,fns.Count) |> ary.Add
                                    else
                                        AryNode(Forky,fns.Count) |> ary.Add
                                    HandleFn fn |> fns.Add 
                                | None  -> ()
                                
                                match kvp.Value.FnList with
                                | []  -> 
                                    AryNode(kvp.Key,ary.Count + 1) |> ary.Add // points to next fwd index 
                                    go kvp.Value // !! Run path for last node now (so on next ittr arycount updated )
                                    nodes,ec //should always be final fold
                                | fns ->
                                    AryNode(kvp.Key,0) |> ary.Add
                                    fnmap fns
                                    (ec,kvp.Value) :: nodes ,(ec + 1)
                                       
                            else
                                // create placeholder retry array from edge chars, last is next into its path,
                                // these will be updated after child runs with relevant hop index numbers 
                                AryNode(kvp.Key,0) |> ary.Add //Retry
                                // paths computed back to front (to allow last cont) so put in list
                                (ec,kvp.Value) :: nodes ,(ec + 1)

                        ) ([],0)
                
                // now using nodelist, with placeholders in, compute child branches & update placeholders
                // with postiions each time
                nodes 
                |> List.iter (fun (i,node) ->  
                                let onode = ary.[ipos + i] // get place holder
                                ary.[ipos + i] <- AryNode(onode.Char,uint16 ary.Count) // update placeholder with valid hop 
                                go node // run this node to populate array
                                )
            go n

        let root = CNode(Parsy)
        let rec addPaths (ls:PathNode list) (n:CNode) = 
            match ls with
            | [] -> ()
            | h :: t ->
                    h.GetBinding() |> n.AddHandlerMap
                    addPaths t n
        addPaths paths root
        
        go root

        ary.ToArray() , fns.ToArray()
    
    for n in nary do
        match char n.Char with
        | Parsy | Endy | Forky -> printfn "{%c|%i|%A}" (char n.Char) (n.Hop) (fary.[int n.Hop])
        | x -> printfn "{%c|%i}" (char n.Char) (n.Hop)
    fary |> Array.iteri (fun i v -> printfn "f%i-> %A" i v)
        
    let inline noneTask () = Task.FromResult None

                    // Parse Helper funcitons
    let _tryParse (cont:'T -> Task<HttpContext option>) (fail:unit->Task<HttpContext option>) (pr:struct(bool*'T))   =
        match pr with
        | struct(true,v) -> cont v
        | struct(false,_) -> fail ()
    
    let tryParse = FSharpFunc<_,_,_,_>.Adapt _tryParse

    let _applyParse path (state:ParseState) offset (cont:obj [] -> Task<HttpContext option>) (fail:unit->Task<HttpContext option>) =
        let results = Array.zeroCreate<obj>(state.TotalArgs)
        let rec pgo i =                
            if i >= 0 then
                let pfn = state.Parsers.[i]
                tryParse.Invoke((fun v ->
                    results.[i] <- v
                    pgo (i - 1)), 
                    fail,
                    pfn.Invoke(path,state.PStart.[i],state.PEnd.[i]))
            else
                cont results
        pgo (state.TotalArgs + offset)
    
    let applyParse = FSharpFunc<_,_,_,_,_,_>.Adapt _applyParse

    
    
    //use compiled instruciton & function arrays to process path queries
    fun (ctx:HttpContext) ->
        // use adapted functions (FSharpFunc<_,_,_>.Adapt(f)) for runtime (esp rec) funcs with no partial application & multiple args         
        let path : string = ctx.Request.Path.Value
        
        let _endProcess p n failFn =
            if p = path.Length - 1 then
                match fary.[int nary.[n].Hop] with
                | HandleFn f -> f ctx
                | ParseApplyEndSingle (prs,fn) ->
                    match prs.Invoke(path,p,path.Length - 1) with
                    | struct(true,v) -> (fn v) ctx
                    | struct(false,_)-> failFn ()
                | xfn -> failwith(sprintf "unhandled funciton match case %A" xfn)
            else failFn ()

        let endProcess = FSharpFunc<_, _, _, _>.Adapt _endProcess
            
        let rec go(p,n) =
            
            let rec parsing (p,n,retry,fp,state:ParseState) : Task<HttpContext option> =                
                
                let completionFn (pfn:HandleFn) (failFn:unit -> Task<HttpContext option>) =
                    match pfn with
                    | ParseApplyEndTuple (prs,fn) ->
                        applyParse.Invoke( path, state, -2,
                            (fun results ->
                                tryParse.Invoke((fun v ->
                                    results.[state.TotalArgs - 1] <- v
                                    (fn results) ctx        
                                    ), 
                                    failFn,
                                    prs.Invoke(path,p,path.Length - 1))
                            ),
                            failFn)
                    | ParseCompleteSingle (fn) ->
                        tryParse.Invoke(
                            (fun v -> (fn v) ctx),
                            failFn,
                            state.Parsers.[0].Invoke(path, state.PStart.[0], state.PEnd.[0]))

                    | ParseCompleteTuple (fn) ->
                        applyParse.Invoke(path, state, -1,
                            (fun results -> (fn results) ctx),
                            failFn)
                    | xfn -> failFn () 
                        //failwith(sprintf "unhandled Finish funciton match case %A" xfn)
            
                // Parse function begin
                if p < path.Length then 
                    match nary.[n].Char with
                    | BParsy ->
                        match fary.[int nary.[n].Hop] with
                        | ParseMid (i,prs,fp) ->
                            state.Parsers.[i] <- prs
                            state.PStart.[i] <- p
                            state.CurArg <- i
                            parsing(p,n + 1,retry,fp,state)                        
                        | xfn -> failwith(sprintf "unhandled parse Continue function match case %A" xfn) 
                    | BEndy  ->
                        completionFn fary.[int nary.[n].Hop] noneTask
                    | BForky ->
                        completionFn fary.[int nary.[n].Hop] 
                            (fun () -> go(p,n + 1) ) // todo: recheck logic !
                    | x ->
                        if byte path.[p] = x then
                            go(p + 1,n + 1)
                        else
                            match int nary.[n].Hop - n with
                            | 1 -> 
                                noneTask ()
                            | hop ->
                                go(p,hop)
                else noneTask ()                                

            if p < path.Length then
                // matching function begin
                match nary.[n].Char with
                | BParsy ->
                    match fary.[int nary.[n].Hop] with
                    | ParseStart (i,prs,fp) ->
                        let ps = ParseState(i)
                        ps.Parsers.[0] <- prs
                        ps.PStart.[0] <- p
                        ps.Retry <- (n + 1)
                        parsing(p,n + 1,p,fp,ps)
                    | xfn -> failwith(sprintf "unhandled Parse funciton match case %A" xfn) 
                | BEndy  ->
                    completionFn fary.[int nary.[n].Hop] noneTask
                | BForky ->
                    completionFn fary.[int nary.[n].Hop] 
                        (fun () -> parsing(retry,n + 1,retry,fp,state) )
                | x ->
                    if byte path.[p] = x then
                        if state.PEnd.[state.CurArg] = 0 then // HACK: need to impliment better logic here to get parse end
                            state.PEnd.[state.CurArg] <- p - 1
                        parsing(p + 1,n + 1,retry,fp,state)
                    else
                        match int nary.[n].Hop - n with
                        | 1 -> 
                            if fp = 0 then
                                noneTask ()
                            else
                                parsing(retry,fp,retry,fp,state) /// todo:  revew logic !/!?!?!
                        | hop ->
                            parsing(p,hop,retry,fp,state)
            else noneTask ()                                    






                match nary.[n].Instr with
                | Instr.Next ->
                    if   nary.[n].Char = (byte path.[p]) 
                    then go(p+1,n+1)
                    else noneTask ()
                | Instr.Retry ->
                    if   nary.[n].Char = (byte path.[p]) 
                    then go (p+1,int nary.[n].Hop)
                    else go (p,n + 1)
                | Instr.FnEnd ->
                    endProcess.Invoke(p, n, noneTask)

                | Instr.FnFinish ->
                    // verify differnce between finish and End as duplicated
                    match fary.[int nary.[n].Hop] with
                    | HandleFn f -> f ctx
                    | ParseApplyEndSingle (prs,fn) ->
                        match prs.Invoke(path, p, (path.Length - 1)) with
                        | struct(true,v) -> (v |> fn) ctx
                        | struct(false,_)-> noneTask ()
                    | xfn -> failwith(sprintf "unhandled Finish funciton match case %A" xfn)
                | Instr.FnEndOrNext ->
                    endProcess.Invoke(p,n,(fun () -> go (p+1,n+1))) // <<<<< p / p + 1 ??

                | Instr.FnContinue ->
                    match fary.[int nary.[n].Hop] with
                    | ParseStart (i,prs) ->
                        let ps = ParseState(i)
                        ps.Parsers.[0] <- prs
                        ps.PStart.[0] <- p
                        ps.Retry <- (n + 1)
                        parsing (p) (n + 1) ps
                    | xfn -> failwith(sprintf "unhandled Continue funciton match case %A" xfn)
                | Instr.FnEndOrCont ->
                    endProcess.Invoke(p, n, (fun () ->
                        // n = end node
                        // n + 1 = parse node with '^' (cx)
                        // n + 2 = matching pattern
                        match fary.[int nary.[n + 1].Hop] with
                        | ParseStart (i,prs) -> // HACK Parse Node current has '^' with next node being start of match, tidy later
                            let ps = ParseState(i)
                            ps.Retry <- n + 2
                            ps.Parsers.[0] <- prs
                            ps.PStart.[0] <- p
                            parsing (p + 1) (n + 2) ps
                        | xfn -> failwith(sprintf "unhandled init go Continue function match case %A" xfn)                                    
                    ))
                | _ -> noneTask ()
            else noneTask () 
        go (0,0)    

/// handler functions (only 2 needed thus far, )
/////////////////////

let inline route (path:string) = PathNode(Route path)

// type Bindy() =
//     member x.EatMe<'U,'T> (sf:StringFormat<'U,'T>) (fn : 'T -> HttpHandler) (v2:obj) = v2 :?> 'T |> fn
// let bindy = Bindy()
let routef (fmt:StringFormat<'U,'T>) (fn:'T -> HttpHandler) =
    let path = fmt.Value
    let last = path.Length - 1

    let rec go i acc argCount =
        if i >= last then
            PathNode(Routef(argCount,List.rev acc,(fun (o:obj) -> o :?> 'T |> fn)))
        else
            let n = path.IndexOf('%',i)     // get index of next '%'
            if n = -1 || n = last then
                // non-parse case & end
                let tl = Token( path.Substring(i,last - i + 1) ) :: acc
                PathNode(Routef(argCount,List.rev tl,(fun (o:obj) -> o :?> 'T |> fn))) 
            else
                let fmtc = path.[n + 1]
                if fmtc = '%' then 
                    go (n + 2) (Token( path.Substring(i,n - i ) ) :: acc) argCount
                else 
                    match formatMap.ContainsKey fmtc with
                    | false ->
                        failwith <| sprintf "Invalid parse char in path %s, pos: %i, char: %c" path n fmtc
                    | true  ->
                        let tl = Parse(fmtc) :: Token(path.Substring(i,n - i) )::acc
                        go (n + 2) tl (argCount + 1)
    go 0 [] 0

/// Testing
////////////////////////////////
// let text (v:string) = fun (ctx:HttpContext) -> ctx |> Some |> Task.FromResult 
// let pn = route "/about"
// let pn2 = pn >=> text "about"

// let webapp = router [
//                 route "/about" >=> text "about" >=> text "again"
//                 route "/auth"  >=> [
//                     route "/cats" >=> text "cats"
//                     routef "/dog%is-sds" (fun v -> text v)                    
//                         ]
//                     // choose [
//                     //     AuthHandler >=| [
//                     //         path "/authorised user" >=> text "/authorised user"
//                     //         path "/authorised manger" >=> text "/authorised manger"
//                     //     ]
//                     //     UnAuthHandler >=> text "You are not authorised"
//                     // ]
//                 route "/other" >=> text "other"
//     ]

// (*
//     Route build process
//     1  take path lists and compress list to remove overlaps (into trie structure?)
//     2  once paths are into trie, (with child path lists not not proccessed, stored at end nodes) can begin crawl
//     3  in each node, lay down path
//     4  if Fns & child nodes, combo flag, fns added to fnAry, index back added to Hop
//     5  if childnodes, lay down retry ary
// *)







// //((cts |> fn arg1) |> fn arg2)  
// //hndl

// // [<Struct>]
// // type State = {
// //     mutable succ : Continuation
// //     mutable fail : Continuation
// //     ctx : HttpContext
// //     }

// // type State2 = 
// //     struct
// //         val mutable succ : Continuation []
// //         val mutable succPos : int
// //         val mutable fail : Continuation []
// //         val mutable failPos : int
// //         val ctx : HttpContext
// //         member x.Succ
// //             with get() = 
// //                 match x.succPos with 
// //                 | -1 -> x.succ.[0] x.ctx 
// //                 | _ ->
// //                     x.succPos <- x.succPos - 1
// //                     x.succ.[x.succPos] x.ctx

// //         member x.Fail
// //             with get() = 
// //                 match x.failPos with 
// //                 | -1 -> x.fail.[0] x.ctx 
// //                 | _ ->
// //                     x.succPos <- x.succPos - 1
// //                     x.succ.[x.succPos] x.ctx
                
// //         new(ictx) = { ctx = ictx ; succ = Unchecked.defaultof<Continuation> ; fail = Unchecked.defaultof<Continuation> }
// //     end

// // let state = {
// //     succ=Unchecked.defaultof<Continuation>;
// //     fail=Unchecked.defaultof<Continuation>;
// //     ctx=Unchecked.defaultof<HttpContext>
// //     }

// // let State2 = State2(Unchecked.defaultof<HttpContext>)

// // let (>=>) (a:Continuation) (b:Continuation) = 
// //     fun (s:State2) -> 
// //         let s2 = State2(ctx)
// //         s2.succ <- s.succ
// //         s.succ <- b


// type IFlag =
// | Next =        0b00000001        // if match, move to next
// | Hop =         0b00000010         // if match, hop to node at HOP val ?? needed?
// | Retry =       0b00000100       // when matching multiple routes, if matched, jump to HOP, else cont to next node
// | FnContinue =  0b00001000  // a partial match/subroute that allows matching to cont (move next) while fns pulled
// | FnFinish =    0b00010000    // ending function that requires no further matching, get fn and go
// | NOP =         0b00000000

// let flag = IFlag.FnFinish ||| IFlag.Retry
// if flag = (IFlag.FnContinue ||| IFlag.Retry) then printf "&&& works!"
// flag = (IFlag.FnContinue ||| IFlag.Retry)
// flag = (IFlag.FnContinue ||| IFlag.Retry)
// let err = IFlag.Next ||| IFlag.Retry
// let bitval (v:IFlag) (p) = v &&& (IFlag.Next <<< p) 
// bitval flag 2
