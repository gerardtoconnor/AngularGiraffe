module Giraffe.HttpRouter

open System
open NonStructuralComparison
open System.Threading.Tasks
open Giraffe.Task
open Microsoft.AspNetCore.Http
open Giraffe.HttpHandlers
open FSharp.Core.Printf
open Microsoft.FSharp.Reflection
open System.Collections.Generic
open Giraffe.RouterParsers

// type HttpContext() = 
//     class end

// //type Continuation = HttpContext -> Task<HttpContext>

// //result of any handler
// type HttpHandler = HttpContext -> Task<HttpContext option>

// let compose (a:HttpHandler) (b:HttpHandler) : HttpHandler =
//     fun ctx ->
//         task {
//             let! ctxo = a ctx
//             match ctxo with
//             | Some ctx -> return! b ctx
//             | None -> return None
//         }

// let (>=>) = compose 
// let handler : HttpHandler = Some >> Task.FromResult 

let typeMap = function
    | 'b' -> typeof<bool>   // bool
    | 'c' -> typeof<char>   // char
    | 'i' -> typeof<int>    // int
    | 'd' -> typeof<int64>  // int64
    | 'f' -> typeof<float>  // float
    | _   -> typeof<string> // string

// performance Node Trie
////////////////////////

type Instr =
| Next = 0uy        // if match, move to next
| Hop = 1uy         // if match, hop to node at HOP val ?? needed?
| Retry = 2uy       // when matching multiple routes, if matched, jump to HOP, else cont to next node
| FnContinue = 3uy  // a partial match/subroute that allows matching to cont (move next) while fns pulled
| FnEnd = 4uy
| FnEndOrNext = 5uy       // end function at last character (Handler/MatchComplete)
| FnEndOrCont = 6uy
| FnFinish = 7uy    // parse mid to end path function represents (MatchToEnd)
| FnFinishOrNext = 8uy    // parse midToEnd and match, try match next before parse finish /%
| FnFinishOrRetry = 9uy  // where at last, FnFinish, otherwise Retry eg '/' , '/about/ -> '/' has both functionality
| FnContOrFinish = 10uy   // where at last, FnFinish, otherwise FnCont eg '/a%s' , '/a%s/b -> 'a' has both functions, need to test partial first
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

/// Domain Types
////////////////
type PathChunk =
| Token of string
| Parse of char

type PathExpr =
| Route of string
| Routef of int * (PathChunk list) * (obj -> HttpHandler)

type HandlerMap =
| Handle of string * HttpHandler
| Handlef of int * (PathChunk list) * (obj -> HttpHandler)

type HandleFn =
| HandleFn            of HttpHandler      // plain handler 
| ParseStart          of int * Parser     // argCount * parser
| ParseMid            of int * Parser     // argIndex * parser
| ParseCompleteSingle of (obj -> HttpHandler) // types * fn 
| ParseApplyEndSingle of Parser * (obj -> HttpHandler) // types * parser * fn
| ParseCompleteTuple  of (obj [] -> HttpHandler) // types * fn 
| ParseApplyEndTuple  of Parser * (obj [] -> HttpHandler) // types * parser * fn

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
        match fn with
        | CHandleFn f -> x.EndFn <- Some f
        | ofn -> x.FnList <- ofn :: x.FnList
    member x.AddHandlerMap (hm:HandlerMap) =
        match hm with
        | Handle (path,fn) -> 
            let fnode = x.PathTraverse path
            fnode.EndFn <- Some fn
            fnode
        | Handlef (argCount,pcl,omap) ->
            //let argCount = pcl |> List.fold (fun acc v -> match v with | Parse _ -> acc + 1 | _ -> acc ) 0
            let tary = Array.zeroCreate<System.Type>(argCount)
            let getTplFn () =
                let tupleType = FSharpType.MakeTupleType tary
                FSharpValue.PreComputeTupleConstructor(tupleType)
            let rec go ls (n:CNode) (argIdx:int) =
                match pcl with
                | [] -> n
                | [Parse c] -> // completes in match
                    if argCount > 1 then
                        tary.[argIdx] <- typeMap c
                        let oamap = getTplFn ()
                        CParseApplyEndTuple(formatMap.[c],oamap >> omap) |>  n.AddFn
                    else
                        CParseApplyEndSingle(formatMap.[c],omap) |>  n.AddFn
                    n
                | [Token t] -> // completes on token
                    let cnode = x.PathTraverse t
                    if argCount > 1 then
                        let oamap = getTplFn ()
                        CParseCompleteTuple(oamap >> omap) |> n.AddFn
                    else
                        CParseCompleteTuple(omap) |> n.AddFn
                    n
                | head :: tail ->
                    match head with
                    | Token t ->
                        let cnode = n.PathTraverse t
                        go tail cnode argIdx
                    | Parse c ->
                        if argIdx = 0 then
                            tary.[0] <- typeMap c
                            let cnode = CNode('_')
                            CParseStart(argCount,formatMap.[c],cnode) |> n.AddFn
                            go tail cnode (argIdx + 1)
                        else
                            tary.[argIdx] <- typeMap c
                            let cnode = CNode('_')
                            CParseMid(argIdx,formatMap.[c],cnode) |> n.AddFn
                            go tail cnode (argIdx + 1)
            go pcl x 0

[<Struct>]
type AryNode =
    val Char  : byte
    val Hop   : uint16
    val Instr : Instr
    new (char,hop,instr) = { Char = char ; Hop = hop ; Instr = instr }
    new (char:char,hop:int,instr) = { Char = byte char ; Hop = uint16 hop ; Instr = instr }

/// PathNode
/////////////////////////

type PathNode(pe : PathExpr) =
    member val ChildRoute = [] with get,set
    member val HandleChain = None  with get,set 
    
    member x.GetBinding () =
        match pe , x.HandleChain with
        | Routef (ac,pcl,fn) , Some hc -> Handlef(ac,pcl,(fun (o:obj) -> fn o >=> hc ))
        | Routef (ac,pcl,fn) , None    -> Handlef(ac,pcl,fn)
        | Route path      , Some hc -> Handle(path,hc)
        | Route path      , None    -> failwith ("no handlers were provided for path:" + path)
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
            
            let rec mapFuncs (fnl:CHandleFn list) =
                let inline completionFn (t:CHandleFn list) (fn:unit -> HandleFn) =
                    if t.IsEmpty then
                        AryNode('_',fns.Count, Instr.FnFinish ) |> ary.Add
                        fn () |> fns.Add
                    else
                        AryNode('_',fns.Count, Instr.FnFinishOrNext ) |> ary.Add
                        fn () |> fns.Add
                        mapFuncs t

                match fnl with
                | [] -> ()
                | h :: t ->
                    match h with
                    | CHandleFn h        ->  // plain handler
                        () // this overlaps with the End Fn and not possible !?!?
                    | CParseStart (i,p,pn)  -> // argCount * parser
                        AryNode('_',fns.Count, Instr.FnContinue ) |> ary.Add
                        ParseStart(i,p) |> fns.Add
                        go pn
                        mapFuncs t                  
                    | CParseMid (i,p,pn)    -> // argIndex * parser
                        AryNode('_',fns.Count, Instr.FnContinue ) |> ary.Add
                        ParseMid(i,p) |> fns.Add
                        go pn
                        mapFuncs t
                    | CParseCompleteSingle (ofh)   -> // types * fn
                        completionFn t (fun () -> ParseCompleteSingle(ofh))
                        // if t.IsEmpty then
                        //     AryNode('_',fns.Count, Instr.FnFinish ) |> ary.Add
                        //     ParseCompleteSingle(ofh) |> fns.Add
                        // else
                        //     AryNode('_',fns.Count, Instr.FnFinishOrNext ) |> ary.Add
                        //     ParseCompleteSingle(ofh) |> fns.Add
                        //     mapFuncs t
                    | CParseCompleteTuple (ofh)   -> // types * fn
                        completionFn t (fun () -> ParseCompleteTuple(ofh))
                        // if t.IsEmpty then
                        //     AryNode('_',fns.Count, Instr.FnFinish ) |> ary.Add
                        //     ParseCompleteTuple(ofh) |> fns.Add
                        // else
                        //     AryNode('_',fns.Count, Instr.FnFinishOrNext ) |> ary.Add
                        //     ParseCompleteTuple(ofh) |> fns.Add
                        //     mapFuncs t
                    | CParseApplyEndSingle (prs,ofh) -> // types * parser * fn
                        completionFn t (fun () -> ParseApplyEndSingle(prs,ofh))
                        // if t.IsEmpty then
                        //     AryNode('_',fns.Count, Instr.FnFinish ) |> ary.Add
                        //     ParseApplyEndSingle(p,ofh) |> fns.Add
                        // else
                        //     AryNode('_',fns.Count, Instr.FnFinishOrNext ) |> ary.Add
                        //     ParseApplyEndSingle(p,ofh) |> fns.Add
                        //     mapFuncs t
                    | CParseApplyEndTuple (prs,ofh) -> // types * parser * fn
                        completionFn t (fun () -> ParseApplyEndTuple(prs,ofh))
                        // if t.IsEmpty then
                        //     AryNode('_',fns.Count, Instr.FnFinish ) |> ary.Add
                        //     ParseApplyEndTuple(p,ofh) |> fns.Add
                        // else
                        //     AryNode('_',fns.Count, Instr.FnFinishOrNext ) |> ary.Add
                        //     ParseApplyEndTuple(p,ofh) |> fns.Add
                        //     mapFuncs t
            
            let addEdges (dict:Dictionary<_,_>) (fnl:CHandleFn list) =
                if dict.Count = 0 then
                    mapFuncs fnl
                else
                    let edgeLast = dict.Count - 1
                    let ipos = ary.Count // take snapshot of initial position to update placeholders later (next node)
                    let nodes , _ = 
                        dict 
                        |> Seq.fold
                            (fun (nodes,ec) kvp ->
                                if ec >= edgeLast then 
                                    if fnl.IsEmpty then 
                                        AryNode(kvp.Key,ary.Count,Instr.Next) |> ary.Add
                                        go kvp.Value // !! Run path for last node now (so on next ittr arycount updated )
                                        nodes,ec //should always be final fold
                                    else
                                        AryNode(kvp.Key,0,Instr.Retry) |> ary.Add

                                        mapFuncs fnl // add functions to end                                
                                        
                                        (ec,kvp.Value) :: nodes ,(ec + 1)
                                else
                                    // create placeholder retry array from edge chars, last is next into its path,
                                    // these will be updated after child runs with relevant hop index numbers 
                                    AryNode(kvp.Key,0,Instr.Retry) |> ary.Add
                                // paths computed back to front (to allow last cont) so put in list
                                    (ec,kvp.Value) :: nodes ,(ec + 1) 
                            ) ([],0)
                    
                    // now using nodelist, with placeholders in, compute child branches & update placeholders
                    // with postiions each time
                    nodes 
                    |> List.iter (fun (i,node) ->  
                                    let onode = ary.[ipos + i] // get place holder
                                    ary.[ipos + i] <- AryNode(onode.Char,uint16 ary.Count,onode.Instr) // update placeholder with valid hop 
                                    go node // run this node to populate array
                                    )

            match n.EndFn with
            | Some (h:HttpHandler) -> 
                match n.Edges.Count , n.FnList.IsEmpty with
                | 0  , true -> // No Edges or functions other then end  
                    AryNode(n.Char,fns.Count,Instr.FnEnd) |> ary.Add // fns.Count = next index about to be added
                    fns.Add (HandleFn h)
                    //end
                | _ , _ -> // additional Edges as well as ending
                    AryNode(n.Char,fns.Count,Instr.FnEndOrNext) |> ary.Add
                    fns.Add (HandleFn h)
                    addEdges n.Edges n.FnList    
            | None ->
                addEdges n.Edges n.FnList

        let root = CNode('/')
        let rec addPaths (ls:PathNode list) (n:CNode) = 
            match ls with
            | [] -> ()
            | h :: t ->
                    printfn "processing head path %A" h
                    let cnode = h.GetBinding() |> n.AddHandlerMap
                    printfn "finished handler add for %A" h
                    if not h.ChildRoute.IsEmpty then
                        printfn "Child routes found so processing for %A" h
                        addPaths h.ChildRoute cnode
                    printfn "finished processing head so rec going in tail for %A" h
                    addPaths t n
        addPaths paths root
        
        go root

        ary.ToArray() , fns.ToArray()


        
    let inline noneTask () = Task.FromResult None

    //use compiled instruciton & function arrays to process path queries
    fun (ctx:HttpContext) ->
        let path : string = ctx.Request.Path.Value
        
        let endProcess p n failFn =
            if p = path.Length - 1 then
                match fary.[int nary.[n].Hop] with
                | HandleFn f -> f ctx
                | ParseApplyEndSingle (prs,fn) ->
                    match prs path p (path.Length - 1) with
                    | struct(true,v) -> (fn v) ctx
                    | struct(false,_)-> failFn ()
                | xfn -> failwith(sprintf "unhandled funciton match case %A" xfn)
            else failFn ()
            
        let rec go p n =
            
            let rec parsing p n (state:ParseState) : Task<HttpContext option> =

                // Parse Helper funcitons
                let tryParse (cont:'T -> Task<HttpContext option>) (fail:unit->Task<HttpContext option>) (pr:struct(bool*'T))   =
                    match pr with
                    | struct(true,v) -> cont v
                    | struct(false,_) -> fail ()
                
                let applyParse j (cont:obj [] -> Task<HttpContext option>) (fail:unit->Task<HttpContext option>) =
                    let results = Array.zeroCreate<obj>(state.TotalArgs)
                    let rec pgo i =                
                        if i >= 0 then
                            let pfn = state.Parsers.[i]
                            pfn path state.PStart.[i] state.PEnd.[i]
                            |> tryParse (fun v ->
                                results.[i] <- v
                                pgo (i - 1)
                            ) fail
                        else
                            cont results
                    pgo j
                
                // Parse function begin
                if p < path.Length then 

                    match nary.[n].Instr with
                    | Instr.Next ->
                        if   nary.[n].Char = (byte path.[p]) 
                        then 
                            if state.PEnd.[state.CurArg] = 0 then // HACK: need to impliment better logic here to get parse end
                                state.PEnd.[state.CurArg] <- p - 1
                            parsing (p+1) (n+1) state
                        else parsing (state.PStart.[state.CurArg]) (state.Retry) state
                     | Instr.FnContinue ->
                        match fary.[int nary.[n].Hop] with
                        | ParseMid (i,prs) ->
                            state.Parsers.[i] <- prs
                            state.PStart.[i] <- p
                            state.CurArg <- i
                            parsing p (n + 1) state                        
                        | xfn -> failwith(sprintf "unhandled Continue funciton match case %A" xfn)
                    | Instr.FnFinish ->
                        match fary.[int nary.[n].Hop] with
                        | ParseApplyEndTuple (prs,fn) ->
                            applyParse (state.TotalArgs - 2) (fun results ->
                                prs path p (path.Length - 1) 
                                |> tryParse (fun v ->
                                    results.[state.TotalArgs - 1] <- v
                                    (fn results) ctx        
                                    ) noneTask
                                ) noneTask
                        | ParseCompleteSingle (fn) ->
                            state.Parsers.[0] path state.PStart.[0] state.PEnd.[0]
                            |> tryParse (fun v -> (fn v) ctx) noneTask
                        | ParseCompleteTuple (fn) ->
                            applyParse (state.TotalArgs - 1) (fun results -> (fn results) ctx) noneTask
                        | xfn -> failwith(sprintf "unhandled Finish funciton match case %A" xfn)
                    | Instr.FnFinishOrNext ->
                        match fary.[int nary.[n].Hop] with
                        | ParseApplyEndTuple (prs,fn) ->
                            applyParse (state.TotalArgs - 2) 
                                (fun results ->
                                    prs path p (path.Length - 1) 
                                    |> tryParse (fun v ->
                                                results.[state.TotalArgs - 1] <- v
                                                (fn results) ctx) 
                                            (fun () -> go state.PStart.[0] (n+1))
                                        )
                                (fun () -> go state.PStart.[0] (n+1))
                        | ParseCompleteSingle (fn) ->
                            state.Parsers.[0] path state.PStart.[0] state.PEnd.[0]
                            |> tryParse (fun v -> (fn v) ctx ) 
                                        (fun () -> go state.PStart.[0] (n+1))
                        | ParseCompleteTuple (fn) ->
                            applyParse (state.TotalArgs - 1) 
                                (fun results -> (fn results) ctx)
                                (fun () -> go state.PStart.[0] (n+1))
                        | xfn -> failwith(sprintf "unhandled Finish funciton match case %A" xfn)
                    | _ -> noneTask ()
                else noneTask ()

            // matching function begin
            match nary.[n].Instr with
            | Instr.Next ->
                if   nary.[n].Char = (byte path.[p]) 
                then go (p+1) (n+1)
                else noneTask ()
            | Instr.Retry ->
                if   nary.[n].Char = (byte path.[p]) 
                then go (p+1) (int nary.[n].Hop)
                else go (p) (n + 1)
            | Instr.FnEnd ->
                endProcess p n noneTask
                // if p = path.Length - 1 then
                //     match fary.[int nary.[n].Hop] with
                //     | HandleFn f -> f ctx
                //     | ParseApplyEndSingle (prs,fn) ->
                //         match prs path p (path.Length - 1) with
                //         | struct(true,v) -> (v |> fn) ctx
                //         | struct(false,_)-> NoneTask ()
                //     | xfn -> failwith(sprintf "unhandled funciton match case %A" xfn)
                // else NoneTask ()
            | Instr.FnFinish ->
                // verify differnce between finish and End as duplicated
                match fary.[int nary.[n].Hop] with
                | HandleFn f -> f ctx
                | ParseApplyEndSingle (prs,fn) ->
                    match prs path p (path.Length - 1) with
                    | struct(true,v) -> (v |> fn) ctx
                    | struct(false,_)-> noneTask ()
                | xfn -> failwith(sprintf "unhandled Finish funciton match case %A" xfn)
            | Instr.FnEndOrNext ->
                endProcess p n (fun () -> go (p+1) (n+1)) // <<<<< p / p + 1 ??
                // match fary.[int nary.[n].Hop] with
                // | HandleFn f -> f ctx
                // | ParseApplyEndSingle (prs,fn) ->
                //     match prs path p (path.Length - 1) with
                //     | struct(true,v) -> (v |> fn) ctx
                //     | struct(false,_)-> Task.FromResult None
                // | xfn -> failwith(sprintf "unhandled funciton match case %A" xfn)
            | Instr.FnContinue ->
                match fary.[int nary.[n].Hop] with
                | ParseStart (i,prs) ->
                    let ps = ParseState(i)
                    ps.Parsers.[0] <- prs
                    ps.PStart.[0] <- p
                    parsing p n ps
                | xfn -> failwith(sprintf "unhandled Continue funciton match case %A" xfn)
            | Instr.FnEndOrCont ->
                endProcess p n (fun () ->
                    // n = end node
                    // n + 1 = parse node with '_'
                    // n + 2 = matching pattern
                    match fary.[int nary.[n + 1].Hop] with
                    | ParseStart (i,prs) -> // HACK Parse Node current has '_' with next node being start of match, tidy later
                        let ps = ParseState(i)
                        ps.Retry <- n + 2
                        ps.Parsers.[0] <- prs
                        ps.PStart.[0] <- p
                        parsing (p + 1) (n + 2) ps
                    | xfn -> failwith(sprintf "unhandled Continue funciton match case %A" xfn)                                    
                )
            | _ -> noneTask ()
               
        go 0 0    

/// handler functions (only 2 needed thus far, )
/////////////////////

let inline route (path:string) = PathNode(Route path)

// type Bindy() =
//     member x.EatMe<'U,'T> (sf:StringFormat<'U,'T>) (fn : 'T -> HttpHandler) (v2:obj) = v2 :?> 'T |> fn
// let bindy = Bindy()
let inline routef (fmt:StringFormat<'U,'T>) (fn:'T -> HttpHandler) =
    let path = fmt.Value
    let last = path.Length - 1

    let rec go i acc argCount =
        let n = path.IndexOf('%',i)     // get index of next '%'
        if n = -1 || n = last then
            // non-parse case & end
            if n <> i && last <> i then
                let tl = Token( path.Substring(i,last - i + 1) ) :: acc
                PathNode(Routef(argCount,List.rev tl,(fun (o:obj) -> o :?> 'T |> fn)))
            else
                PathNode(Routef(argCount,List.rev acc,(fun (o:obj) -> o :?> 'T |> fn))) 
        else
            let fmtc = path.[n + 1]
            if fmtc = '%' then 
                go (n + 2) (Token( path.Substring(i,n - i ) ) :: acc) argCount
            else 
                match formatMap.ContainsKey fmtc with
                | false ->
                    failwith <| sprintf "Invalid parse char in path %s, pos: %i, char: %c" path n fmtc
                | true  ->
                    let tl = Parse(fmtc) :: (if n + 1 = last then acc else Token(path.Substring(i,n - i) )::acc)
                    if n + 2 > last then
                        PathNode(Routef(argCount,List.rev tl,(fun (o:obj) -> o :?> 'T |> fn)))
                    else
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
