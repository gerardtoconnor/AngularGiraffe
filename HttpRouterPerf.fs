module Giraffe.HttpRouter

open Giraffe.HttpHandlers
open FSharp.Core.Printf
open System.Collections.Generic


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
| FullScan = 0y
| MidMatching = 1y
| ChildNodeMatchScan = 2y
| FinalMatchedCloseout = 3y
| EndPathCompleteMatch = 4y
| EndPathEndMatcingCompleteMatch = 5y

type INodeType =
| UnInit = 0y
| EmptyNode = 1y
| HandlerFn = 2y
| SubRouteFn = 3y
| ApplyMatchFn = 4y
| MatchCompleteFn = 5y
// performance Node Trie

type MidMatcher =
    struct
        val Fmt : char
        val NextChar : char
        val NextNode : TNode
    end
and EndMatcher =
    struct
        val Fmt : char
        val ArgCount : int
        val MapRouteFn : (obj -> HttpHandler)
   end
and EndCompleter =
    struct
        val ArgCount : int
        val MapRouteFn : (obj -> HttpHandler)
    end
and TNode =
        val UBound : int
        val LBound : int
        val Mod : int
        val Token : string       
        val Edges : TNode []
        val NodeType : INodeType
        val HandlerFn : HttpHandler
        val EndCompleterFn : EndCompleter
        val MidMatchFns : MidMatcher []
        val EndMatchFns : EndMatcher []
        
        new(ca:char [],nt) = 
                let m,l,h = modmatch ca
                {
                    LBound = l
                    UBound = h
                    Mod = m
                    Token = ""
                    NodeType = nt
                    Edges = Array.zeroCreate<TNode>(h - l)
                    HandlerFn = Unchecked.defaultof<HttpHandler>
                    EndCompleterFn = Unchecked.defaultof<EndCompleter>
                    MidMatchFns = Array.zeroCreate<MidMatcher>(0)
                    EndMatchFns = Array.zeroCreate<EndMatcher>(0)
                }
        // new(noteType:INodeType,vnl:(char * TNode) list) =
        //     let cmin,cmax = 
        //         vnl |> List.fold (fun (n,x) (c,d) -> min n c,max x c) (System.Char.MaxValue,System.Char.MinValue)
        //         |> fun (n,x) -> int n,int x
        //     let edgeArray = Array.zeroCreate<TNode>(cmax - cmin  + 1)
        //     for (c,n) in vnl do
        //         edgeArray.[(int c) - cmin] <- n
        //     {
        //         LBound = cmin
        //         UBound = cmax
        //         INodeType = INodeType.EmptyNode
        //         Edges = edgeArray
        //         HandlerFn = Unchecked.defaultof<HttpHandler>
        //         MidMatchFns = Array.zeroCreate<MidMatcher>(0)
        //         EndMatchFns = Array.zeroCreate<EndMatcher>(0)
        //     }
    //end

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