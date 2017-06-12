module Test.HttpRouterToken

open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Primitives
open FSharp.Core.Printf
open System.Collections.Generic
open Microsoft.FSharp.Reflection
open Giraffe.AsyncTask
open Giraffe.HttpHandlers
open HttpRouter.RouterParsers

// implimenation of (router) Trie Node
// assumptions: memory and compile time not relevant, all about execution speed, initially testing with Dictionary edges

////////////////////////////////////////////////////
// Node Trie using node mapping functions
////////////////////////////////////////////////////

/// Tail Clip: clip end of 'str' string staring from int pos -> end
let inline (-|) (str:string) (from:int) = str.Substring(from,str.Length - from)

let commonPath (str1:string) (str2:string) =
    let rec go i =
        if i < str1.Length && i < str2.Length then
            if str1.[i] = str2.[i] 
            then go (i + 1)
            else i                               
        else i
    go 0

type PathMatch =
| SubMatch of int
| PathInToken
| TokenInPath
| ZeroToken
| ZeroMatch
| FullMatch

let getPathMatch (path:string) (token:string) =
    if token.Length = 0 then ZeroToken
    else
        let cp = commonPath path token
        let tokenMatch = cp = token.Length
        let pathMatch = cp = path.Length
        if cp = 0 then ZeroMatch
        elif tokenMatch && pathMatch then FullMatch
        elif tokenMatch then TokenInPath
        elif pathMatch  then PathInToken
        else SubMatch cp

type Node(token:string) = 
    
    let mutable midFns = []
    let mutable endFns = []
    
    let addMidFn (mfn:MidCont) = midFns <- mfn :: midFns |> List.sortBy (fun f -> f.Precedence)
    let addEndFn (efn:EndCont) = endFns <- efn :: endFns |> List.sortBy (fun f -> f.Precedence) 
    
    let addRouteFn fn (node:Node) =
        match fn with
        | Empty -> ()
        | Mid mfn -> addMidFn mfn
        | End efn -> addEndFn efn
    //do addRouteFn routeFn
    //let mutable hasEdges = false //quick field to check if node has edges
    let edges = Dictionary<char,Node>()
    
    let splitNode (node:Node) (pos:int) =
        // need to split existing node out
        let sedges = node.Edges //get ref to pass to split node
        let baseToken = node.Token.Substring(0,pos) //new start base token
        let childToken = (node.Token -| pos)
        let snode = Node(childToken)
        node.Edges.Add(childToken.[0],snode)
        //node.Add childToken Empty // create split node
        node.Token <- baseToken
        snode.Edges <- sedges
        node.Edges <- Dictionary<_,_>() //wipe edges from node
        //copy over existing functions
        snode.MidFns <- node.MidFns
        snode.EndFns <- node.EndFns
        //clear functions from existing node 
        node.MidFns <- List.empty
        node.EndFns <- List.empty

    // let addEdge (token:string) routeFn =
    //     match edges.TryGetValue token.[0] with
    //     | true, node -> 
    //         match routeFn with
    //         | Empty -> ()
    //         | Mid mfn -> node.AddMidFn mfn
    //         | End efn -> node.AddEndFn efn
    //         node
    //     | false, _ -> 
    //         let node = Node(token)
    //         match routeFn with
    //         | Empty -> ()
    //         | Mid mfn -> node.AddMidFn mfn
    //         | End efn -> node.AddEndFn efn
    //         edges.Add(token.[0],node)
    //         if not hasEdges then hasEdges <- true //quick field to check if node has edges
    //         node

    member val Edges = edges with get,set        
    member val Token = token with get,set
    
    member __.MidFns
        with get() = midFns 
        and set v = midFns <- v
    member __.AddMidFn = addMidFn

    member __.EndFns 
        with get()  = endFns 
        and set v = endFns <- v 
    member __.AddEndFn = addEndFn
               
    member x.EdgeCount 
        with get () = edges.Count
    member x.GetEdgeKeys = edges.Keys
    member x.TryGetValue v = edges.TryGetValue v

    static member rec AddPath (node:Node) (path:string) (rc:ContType) =
        match getPathMatch path node.Token with
        | ZeroToken ->
            // if node empty/root
            node.Token <- path
            addRouteFn rc node
        | ZeroMatch ->
            failwith "path passed to node with non-matching start in error"
        | FullMatch -> addRouteFn rc node
        | PathInToken ->
            splitNode node (path.Length)
            addRouteFn rc node
        | TokenInPath ->
            //path extends beyond this node
            let rem = path -| (node.Token.Length)
            match node.TryGetValue rem.[0] with
            | true, cnode ->
                AddPath cnode rem rc // recursive path scan
            | fales, _    ->
                let nnode = Node(rem)
                addRouteFn rc nnode
        | SubMatch (i) ->
            splitNode node (i)
            let rem = path -| i
            let nnode = Node(rem)
            node.Edges.Add(rem.[0],nnode)
            addRouteFn rc nnode
                        
// Route Continuation Functions    
and MidCont =
| SubRouteMap of HttpHandler
| ApplyMatch of (char * (char []) * (Node)) // (parser , nextChar , contNode) list
| ApplyMatchAndComplete of ( char * int * (obj -> HttpHandler)) // (lastParser, No# Parsers, Cont Fn)
    member x.Precedence
        with get () =
            match x with
            | SubRouteMap _ -> 1
            | ApplyMatch (c,_,_) -> (int c)
            | ApplyMatchAndComplete (c,_,_) -> 256 + (int c) 
and EndCont = 
| HandlerMap of HttpHandler
| MatchComplete of ( (int) * (obj -> HttpHandler) ) // ( No# Parsers, Cont Fn) 
    member x.Precedence
        with get () =
            match x with
            | HandlerMap _ -> 1
            | MatchComplete _ -> 2 
and ContType =
| Empty
| Mid of MidCont
| End of EndCont   


////////////////////////////////////////////////////
// Helper Functions
////////////////////////////////////////////////////

// Bindy is a hack to encapsulate type inferance application in node trie of multiple types, partially applied functions fail
type Bindy() =
    member x.EatMe<'U,'T> (sf:StringFormat<'U,'T>) (fn : 'T -> HttpHandler) (v2:obj) = v2 :?> 'T |> fn

let inline bindMe (sf:StringFormat<'U,'T>) (fn : 'T -> HttpHandler) = 
    let b = Bindy()
    b.EatMe<'U,'T> sf fn

// temporary compose out handler to allow composition out of route functions, same as wraping in () or using <|
let inline (==>) (a:HttpHandler -> Node -> Node) (b:HttpHandler) = a b

let addCharArray (c:char) (ary:char []) =
    if ary |> Array.exists (fun v -> v = c) then
        ary
    else 
        let nAry = Array.zeroCreate<_>(ary.Length + 1)
        Array.blit ary 0 nAry 0 ary.Length
        nAry.[ary.Length] <- c
        nAry

// helper to get child node of same match format (slow for now, needs optimisation)
let getPostMatchNode fmt (nxt:char) (ils:MidCont list) = 
    let rec go (ls:MidCont list) (acc:MidCont list) (no:Node option) =
        match ls with
        | [] -> 
            match no with 
            | None -> 
                let n = Node(Empty)
                n ,(ApplyMatch(fmt,[|nxt|],n)) :: acc |> List.sortBy (fun fn -> fn.Precedence)
            | Some n -> n, acc |> List.sortBy (fun fn -> fn.Precedence)
        | hfn :: tfns ->
            match hfn with
            | ApplyMatch (f,ncl,n) ->
                if f = fmt then
                    let nncl = addCharArray nxt ncl
                    go tfns (ApplyMatch(f,nncl,n)::acc) (Some(n))
                else go tfns (hfn::acc) no
            | _ -> go tfns (hfn::acc) no
    go ils [] None
////////////////////////////////////////////////////
// Routing Node Map Functions used to build trie
////////////////////////////////////////////////////

// Simple route that iterates down nodes and if function found, execute as normal
let routeT (path:string) (fn:HttpHandler) (root:Node) = 
    Node.AddPath root path (fn |> HandlerMap |> End)

let subRouteT (path:string) (fn:HttpHandler) (root:Node) =
    Node.AddPath root path (fn |> SubRouteMap |> Mid)  

// parsing route that iterates down nodes, parses, and then continues down further notes if needed
let routeTf (path : StringFormat<_,'T>) (fn:'T -> HttpHandler) (root:Node)=
    let last = path.Value.Length - 1

    let rec go i ts pcount (node:Node) =
        let pl = path.Value.IndexOf('%',i)
        if pl < 0 then
            //Match Complete
            addRoutContToPath path.Value i 0 (MatchComplete( pcount , bindMe path fn ) |> End) node              
        else
            if pl + 1 <= last then
                let fmtChar = path.Value.[pl + 1]
                // overrided %% -> % case
                if fmtChar = '%' then
                    //keep token start, skip 
                    go (i + 2) ts pcount node
                // formater with valid key
                else if formatStringMap.ContainsKey fmtChar then

                    if pl + 1 = last then // if finishes in a parse
                        if node.MidFns |> List.exists (function | ApplyMatchAndComplete(c,_,_) -> fmtChar = c | _ -> false )
                        then sprintf "duplicate paths detected '%s', Trie Build skipping..." path.Value |> failwith
                        else 
                            node.AddMidFn <| ApplyMatchAndComplete( fmtChar , pcount + 1 , bindMe path fn )
                            node.Token <- path.Value.Substring(ts,pl - ts + 1)
                        node
                    else
                        //otherwise add mid pattern parse apply
                        
                        let cnode,midFns = getPostMatchNode fmtChar path.Value.[i+2] node.MidFns                                                    
                        node.MidFns <- midFns //update adjusted functions
                        node.Token <- path.Value.Substring(ts,pl - ts + 1)
                        go (i + 2) (pl + 2) (pcount + 1) cnode
                // badly formated format string that has unknown char after %
                else
                    failwith (sprintf "Routef parsing error, invalid format char identifier '%c' , should be: b | c | s | i | d | f" fmtChar)
                    go (pl + 1) ts pcount node
            else
                //normal string match path/chain
                node.Token <- path.Value.Substring(ts,last - ts + 1) //<<<<< check
                go (pl + 1) (pl+1) pcount node

    go 0 0 0 root 

// choose root will apply its root node to all route mapping functions to generate Trie at compile time, function produced will take routeState (path) and execute appropriate function

// process path fn that returns httpHandler
let private processPath (rs:RouteState) (root:Node) : HttpHandler =
    fun (succ:Continuation) (fail:Continuation) (ctx:HttpContext) -> 
    
    let path = rs.path
    let ipos = rs.pos
    let last = path.Length - 1
    
    let rec checkMatchSubPath pos (node:Node) = // this funciton is only used by parser paths
        //this function doesn't test array bounds as all callers do so before
        match node.TryGetValue path.[pos] with
        | true, n -> 
            if pos = last then //if this pattern match shares node chain as substring of another
                if n.EndFns.IsEmpty
                then pos, None
                else pos, Some n
            else checkMatchSubPath (pos + 1) n
        | false,_ -> //failed node match on pos represents start of a match
            if pos = last then
                if node.EndFns.IsEmpty
                then pos, None
                else pos, Some node
            else
                if node.MidFns.IsEmpty
                then pos, None
                else pos, Some node
    
    /// (next match chars,pos,match completion node) -> (parse end,pos skip completed node,skip completed node) option
    let rec getNodeCompletion (cs:char []) pos (node:Node) =
        match path.IndexOfAny(cs,pos) with // jump to next char ending (possible instr optimize vs node +1 crawl) 
        | -1 -> None
        | x1 -> //x1 represents position of match close char but rest of chain must be confirmed 
            match checkMatchSubPath x1 node with
            | x2,Some cn -> Some(x1 - 1,x2,cn)                 // from where char found to end of node chain complete
            | x2,None   ->  getNodeCompletion cs (x1 + 1) node // char foundpart of match, not completion string

    let createResult (args:obj list) (argCount:int) (fn:obj -> HttpHandler) =
        let input =  
            match argCount with
            | 0 -> Unchecked.defaultof<obj> //HACK: need routeF to throw error on zero args
            | 1 -> args.Head // HACK: look into performant way to safely extract
            | _ ->
                let values = Array.zeroCreate<obj>(argCount)
                let valuesTypes = Array.zeroCreate<System.Type>(argCount)
                let rec revmap ls i = // List.rev |> List.toArray not used to minimise list traversal
                    if i < 0 then ()
                    else
                        match ls with
                        | [] -> ()
                        | h :: t -> 
                            values.[i] <- h
                            valuesTypes.[i] <- h.GetType()
                            revmap t (i - 1)
                revmap args (argCount - 1)
                
                let tupleType = FSharpType.MakeTupleType valuesTypes
                FSharpValue.MakeTuple(values, tupleType)
        fn input succ fail ctx

    let saveRouteState pos = 
        rs.pos <- pos
        ctx.Items.[routerKey] <- rs 

    let rec processEnd (fns:EndCont list) pos args =
        match fns with
        | [] -> fail ctx
        | h :: t ->
            match h with                    
            | HandlerMap fn -> fn succ fail ctx // run function with all parameters
            | MatchComplete (i,fn) -> createResult args i fn 

    let rec processMid (fns:MidCont list) pos args =
        
        let applyMatchAndComplete pos acc ( f,i,fn ) tail =
            match formatStringMap.[f] path pos last with
            | Some o -> createResult (o :: acc) i fn
            | None -> processMid tail pos acc // ??????????????????
        
        let rec applyMatch (f:char,ca:char[],n) pos acc tail : Task<HttpContext> =
            match getNodeCompletion ca pos n with
            | Some (fpos,npos,cnode) ->
                match formatStringMap.[f] path pos fpos with
                | Some o -> 
                    if npos = last then //if have reached end of path through nodes, run HandlerFn
                        processEnd cnode.EndFns npos (o::acc)
                    else
                        processMid cnode.MidFns npos (o::acc)
                | None -> processMid tail pos acc // keep trying    
            | None -> processMid tail pos acc // subsequent match could not complete so fail
        
        match fns with
        | [] -> fail ctx
        | h :: t ->
            match h with
            | ApplyMatch x -> applyMatch x pos args t
            | ApplyMatchAndComplete x -> applyMatchAndComplete pos args x t
            | SubRouteMap (fn) ->
                saveRouteState pos
                fn succ fail ctx

    let rec crawl pos (node:Node) =
        match node.TryGetValue path.[pos] with
        | true, n ->
            if pos = last then //if have reached end of path through nodes, run HandlerFn
                processEnd n.EndFns pos []
            else                //need to continue down chain till get to end of path
                crawl (pos + 1) n
        | false , _ ->
            // no further nodes, either a static url didnt match or there is a pattern match required            
            processMid node.MidFns pos []

    crawl ipos root

let routeTrie (fns:(Node->Node) list) : HttpHandler =
    let root = Node(Empty)
    // precompile the route functions into node trie
    let rec go ls =
        match ls with
        | [] -> ()
        | h :: t ->
            h root |> ignore
            go t
    go fns

    fun succ fail ctx ->
        //get path progress (if any so far)
        let routeState =
            match ctx.Items.TryGetValue routerKey with
            | true, (v:obj) -> v :?> RouteState  
            | false,_-> RouteState(ctx.Request.Path.Value)
        processPath routeState root succ fail ctx