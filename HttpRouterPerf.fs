module Giraffe.HttpRouter

open Giraffe.HttpHandlers
open FSharp.Core.Printf
open System.Collections.Generic
open HashDepot


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

let inline intIn x l u = (x - l) * (u - x) >= 0

//#time
let path = "/test/cats/dogs" //6 -> 9
let sting = "cats"
let start = 6

let mutable result = 0

for i in 1 .. 100000000 do
    if System.String.CompareOrdinal(path,start,sting,0,sting.Length) = 0  then
        result <- result + 1
printfn "result is %i" result

let rec go i =  
    let rec word j k =
        if k < sting.Length  then //&& j < path.Length
            if path.[j] = sting.[k] then
                //printfn "pos %i matching %c" k sting.[k] 
                word (j+1) (k+1)
            else
                //printfn "failed at pos %i matching %c" k sting.[k]
                false
        else
            //printfn "matching complete at pos %i" k
            true
    if i > 0 then
        if word start 0 then
            result <- result + 1
        go (i - 1)
    else
        printfn "result is %i" result                

go 100000000


let testAry = [|'i';'t';'b';'q';|]

let node = TNode(testAry,INodeType.EmptyNode)
node.Edges.[0] <- Unchecked.defaultof<TNode>
node.Token <- "imToken"

let result = modmatch testAry

let paths = [|
    "/"
    "/test"
    "/about"
|]

let work =
    paths
    |> Array.map 
        (fun path -> path.ToCharArray() |> List.ofArray)

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

/// Domain Types
////////////////

type PathExpr =
| Route of string
| Routef of StringFormat<_,_>
| SubRoute of string

type MatchChunk =
| Token of string
| Parse of char

type PathType =
| Path of string
| Match of MatchChunk list * (obj -> HttpHandler)
| PathRoute of string * HttpHandler * RouteNode
| MatchRoute of MatchChunk list * (obj -> HttpHandler) * RouteNode

and PathNode(pathType:PathType) =
    let pathType = pathType
    member __.BindFn (h:HttpHandler) =
        ()

and RouteNode(preFn:HttpHandler) =
    let preFn = preFn
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
            
let webapi = 
    routeBase >=> 
        RouteNode [
            PathNode "/about" >=> 
                authenticationHandler >=> 
                    RouteNode [ 
                        PathNode "/auth" >=> text "you are authenticated"
                    ]
]