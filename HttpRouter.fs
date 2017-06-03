module Test.HttpRouter

open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Primitives
open FSharp.Core.Printf
open System.Collections.Generic
open Microsoft.FSharp.Reflection
open Giraffe.AsyncTask
open Giraffe.HttpHandlers


//between inclusive
let inline intIn x l u = (x - l) * (u - x) >= 0

let inline int64In x l u = (x - l) * (u - x) >= 0L

let inline floatIn x l u = (x - l) * (u - x) >= 0.

let routerKey = "router_pos"

type RouteState(path:string) =
    member val path = path with get
    member val pos = 0 with get , set

/// Private Range Parsers that quickly try parse over matched range (all fpos checked before running in preceeding functions)

let private stringParse (path:string) ipos fpos = path.Substring(ipos,fpos - ipos + 1) |> box |> Some

let private  charParse (path:string) ipos fpos = path.[ipos] |> box |> Some // this is not ideal method (but uncommonly used)

let private boolParse (path:string) ipos fpos =
    if intIn (fpos - ipos) 4 5 then 
        match path.[ipos] with
        | 't' | 'T' -> true  |> box |> Some // todo: Laxy matching, i'll complete later
        | 'f' | 'F' -> false |> box |> Some
        | _ -> None
    else None

let private intParse (path:string) ipos fpos =

    let mutable result = 0
    let mutable negNumber = false
    let rec go pos =
        let charDiff = int path.[pos] - int '0'
        if intIn charDiff 0 9 then
            result <- (result * 10) + charDiff
            if pos = fpos then
                if negNumber then - result else result 
                |> box |> Some 
            else go (pos + 1)       // continue iter
        else None
    //Start Parse taking into account sign operator
    match path.[ipos] with
    | '-' -> negNumber <- true ; go (ipos + 1)
    | '+' -> go (ipos + 1)
    | _ -> go (ipos)
    
let private int64Parse (path:string) ipos fpos =

    let mutable result = 0L
    let mutable negNumber = false
    let rec go pos =
        let charDiff = int64 path.[pos] - int64 '0'
        if int64In charDiff 0L 9L then
            result <- (result * 10L) + charDiff
            if pos = fpos then
                if negNumber then - result else result 
                |> box |> Some 
            else go (pos + 1)       // continue iter
        else None
    //Start Parse taking into account sign operator
    match path.[ipos] with
    | '-' -> negNumber <- true ; go (ipos + 1)
    | '+' -> go (ipos + 1)
    | _ -> go (ipos)

let decPower = [|1.;10.;100.;1000.;10000.;100000.;1000000.;10000000.;100000000.;100000000.|] |> Array.map (fun d -> 1. / d) // precompute inverse once at compile time
    
let floatParse (path:string) ipos fpos =
    let mutable result = 0.
    let mutable decPlaces = 0
    let mutable negNumber = false
    
    let rec go pos =
        if path.[pos] = '.' then
            decPlaces <- 1
            if pos < fpos then go (pos + 1) else None
        else
            let charDiff = float path.[pos] - float '0'
            if floatIn charDiff 0. 9. then
                if decPlaces = 0 then 
                    result <- (result * 10.) + charDiff
                else
                    result <- result + ( charDiff * decPower.[decPlaces]) // char is divided using multiplication of pre-computed divisors
                    decPlaces <- decPlaces + 1
                if pos = fpos || decPlaces > 9 then
                    if negNumber then - result else result 
                    |> box |> Some 
                else go (pos + 1)   // continue iter
            else None   // Invalid Character in path

    //Start Parse taking into account sign operator
    match path.[ipos] with
    | '-' -> negNumber <- true ; go (ipos + 1)
    | '+' -> go (ipos + 1)
    | _ -> go (ipos)

let formatStringMap =
    dict [
    // Char    Range Parser
    // ---------------  -------------------------------------------
        'b', (boolParse  )  // bool
        'c', (charParse  )  // char
        's', (stringParse)  // string
        'i', (intParse   )  // int
        'd', (int64Parse )  // int64
        'f', (floatParse )  // float
    ]

// implimenation of (router) Trie Node
// assumptions: memory and compile time not relevant, all about execution speed, initially testing with Dictionary edges

type Node(iRouteFn:RouteCont) = 
    let mutable hasEdges = false //quick field to check if node has edges
    let edges = Dictionary<char,Node>()
    member val RouteFn = iRouteFn with get,set 
    member x.Add v routeFn =
        match edges.TryGetValue v with
        | true, node -> 
                //   ( current ),( new )
            match routeFn with
            | EmptyMap -> ()
            | y -> node.RouteFn <- y
            // | MultiMatch ls -> routeFn :: ls |> List.sortBy (fun v -> v.Precedent) |> MultiMatch
            // | x -> [routeFn ; x ] |> List.sortBy (fun v -> v.Precedent) |> MultiMatch
            //| _, y -> y // TEMPORARY MATCH SCHEME
            node
        | false, _ -> 
            let node = Node(routeFn)
            edges.Add(v,node)
            if not hasEdges then hasEdges <- true //quick field to check if node has edges
            node
            
    member x.EdgeCount 
        with get () = edges.Count
    
    member x.GetEdgeKeys = edges.Keys
    member x.TryGetValue v = edges.TryGetValue v
    //member val Edges = edges with get

    member x.Search v =
        match edges.TryGetValue v with
        | true,node-> Some node
        | false,_-> None

// Route Continuation Functions    
and RouteCont =
| EmptyMap
| HandlerMap of HttpHandler
| SubRouteMap of HttpHandler
| MultiMatch of RouteCont list
| ApplyMatch of ( char * char * (Node) ) // (parser , nextChar , contNode) 
| MatchComplete of ( (int) * (obj -> HttpHandler) ) // ( No# Parsers, Cont) 
| ApplyMatchAndComplete of ( char * int * (obj -> HttpHandler) ) // (lastParser, No# Parsers, Cont)
    member x.Precedent 
        with get () =
            match x with
            | MultiMatch _ -> 0 
            | SubRouteMap _ -> 1
            | HandlerMap _ -> 2
            | ApplyMatch _ -> 3
            | ApplyMatchAndComplete _ -> 4
            | MatchComplete _ -> 5
            | EmptyMap -> 6
////////////////////////////////////////////////////
// Node Trie using node mapping functions
////////////////////////////////////////////////////

// Bindy is a hack to encapsulate type inferance application in node trie of multiple types, partially applied functions fail
type Bindy() =
    member x.EatMe<'U,'T> (sf:StringFormat<'U,'T>) (fn : 'T -> HttpHandler) (v2:obj) = v2 :?> 'T |> fn

let inline bindMe (sf:StringFormat<'U,'T>) (fn : 'T -> HttpHandler) = 
    let b = Bindy()
    b.EatMe<'U,'T> sf fn 

let inline (==>) (a:HttpHandler -> Node -> Node) (b:HttpHandler) = a b

let private addRoutContToPath (path:string) (rc:RouteCont)  (root:Node) =     
    let last = path.Length - 1 
    let rec go i (node:Node) =
        if i = last then
            node.Add path.[i] rc
        else
            let nextNode = node.Add path.[i] EmptyMap
            go (i + 1) nextNode
    go 0 root

// Simple route that iterates down nodes and if function found, execute as normal
let tRoute (path:string) (fn:HttpHandler) (root:Node) = 
    addRoutContToPath path (HandlerMap fn) root

let tSubRoute (path:string) (fn:HttpHandler) (root:Node) =
    addRoutContToPath path (SubRouteMap fn) root

// parsing route that iterates down nodes, parses, and then continues down further notes if needed
let inline tRoutef (path : StringFormat<_,'T>) (fn:'T -> HttpHandler) (root:Node)=
    let last = path.Value.Length - 1
    let rec go i (pcount) (node:Node)  =
        if i = last then
            // have reached the end of the string match so add fn handler, and no continuation node            
            node.RouteFn <- (MatchComplete( pcount , bindMe path fn ))
            node
        else
            if path.Value.[i] = '%' && i + 1 <= last then
                let fmtChar = path.Value.[i + 1]
                // overrided %% -> % case
                if fmtChar = '%' then
                    node.Add '%' EmptyMap
                    |> go (i + 2) pcount
                // formater with valid key
                else if formatStringMap.ContainsKey fmtChar then

                    if i + 1 = last then // if at the end of the parse
                        node.RouteFn <- ApplyMatchAndComplete( fmtChar , pcount + 1 , bindMe path fn )
                        node
                    else
                        let newNodeBranch =
                            match node.RouteFn with
                            | ApplyMatch (_,_,n) -> n 
                            | x -> Node(EmptyMap)
                        node.RouteFn <- ApplyMatch( fmtChar ,path.Value.[i+2], newNodeBranch ) // need to insert Parser on this current node before match string part
                        go (i + 2) (pcount + 1) newNodeBranch 
                // badly formated format string that has unknown char after %
                else
                    failwith (sprintf "Routef parsing error, invalid format char identifier '%c' , should be: b | c | s | i | d | f" fmtChar)
                    node.Add path.Value.[i] EmptyMap
                    |> go (i + 1) pcount
            else
                node.Add path.Value.[i] EmptyMap
                |> go (i + 1) pcount
    go 0 0 root 

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
                match n.RouteFn with
                | MatchComplete _ -> pos, Some n
                | _ -> pos, None
            else checkMatchSubPath (pos + 1) n
        | false,_ -> //failed node match on pos represents start of a match
            if pos = last then
                match node.RouteFn with
                | MatchComplete _ -> pos, Some node
                | _ -> pos, None
            else
                match node.RouteFn with
                | ApplyMatch _ 
                | ApplyMatchAndComplete _ -> (pos), Some node
                | _ -> (pos), None

    let rec getNodeCompletion (c:char) pos (node:Node) =
        match path.IndexOf(c,pos) with
        | -1 -> None
        | x1 -> 
            match checkMatchSubPath x1 node with
            | x2,Some cn -> Some(x1 - 1,x2,cn)                 // from where char found to end of node chain complete
            | x2,None   ->  getNodeCompletion c x2 node // char foundpart of match, not completion string

    let createResult (args:obj list) (argCount:int) (fn:obj -> HttpHandler) : Task<HttpContext> =
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

    let matchFinalNodeFn (fn:RouteCont) pos acc : Task<HttpContext> =
        match fn with
        | EmptyMap -> fail ctx // the chain didnt end with a handler (in error) so fail
        | SubRouteMap _ -> fail ctx // a subroute cannot be a final pos route Cont
        | HandlerMap fn ->
            fn succ fail ctx // run function with all parameters
        | ApplyMatch _ -> fail ctx // the chain didnt end with a handler (in error) so fail
        | MatchComplete (i,fn) ->  
            createResult acc i fn // a chain with parses, ending in string has ended and can now be applied 
        | ApplyMatchAndComplete ( f,i,fn ) -> // a chain with parsers (optional) ends with pattern match also
            match formatStringMap.[f] path pos last with
            | Some o -> 
                createResult (o :: acc) i fn
            | None -> fail ctx

    let rec applyMatch (f:char,c:char,n) pos node acc : Task<HttpContext> =
        match getNodeCompletion c pos node with
        | Some (fpos,npos,cnode) ->
            match formatStringMap.[f] path pos fpos with
            | Some o -> 
                match cnode.RouteFn with
                | ApplyMatch (f2,c2,n2) -> applyMatch (f2,c2,n2) npos cnode (o :: acc)
                | x -> matchFinalNodeFn x npos (o :: acc)
            | None -> fail ctx
            
        | None -> fail ctx // subsequent match could not complete so fail

    let rec crawl pos (node:Node) =
        match node.TryGetValue path.[pos] with
        | true, n ->
            if pos = last then //if have reached end of path through nodes, run HandlerFn
                matchFinalNodeFn n.RouteFn pos []
            else                //need to continue down chain till get to end of path
                crawl (pos + 1) n
        | false , _ ->
            // no further nodes, either a static url didnt match or there is a pattern match required
            match node.RouteFn with
            | ApplyMatch (f,c,n) -> applyMatch (f,c,n) pos n []
            | ApplyMatchAndComplete _ -> matchFinalNodeFn node.RouteFn pos [] //apply on match in the base crawler means single match
            | SubRouteMap (fn) ->
                saveRouteState pos
                fn succ fail ctx
            | _ -> fail ctx // only a partial match would cause no next node so anything else is err
    
    let preCrawl pos (node:Node) =
        if path.Length = 0 then
            match node.TryGetValue '/' with
            | true, n -> matchFinalNodeFn node.RouteFn pos []
            | false , _ -> fail ctx
        else
            crawl pos node

    preCrawl ipos root        

let routeTrie (fns:(Node->Node) list) : HttpHandler =
    let root = Node(EmptyMap)
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