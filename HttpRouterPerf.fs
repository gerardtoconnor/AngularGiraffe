module Giraffe.HttpRouter

open Giraffe.HttpHandlers
open FSharp.Core.Printf
open System.Collections.Generic

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

// type MidMatcher =
//     struct
//         val Fmt : char
//         val NextChar : char
//         val NextNode : TNode
//     end
// and EndMatcher =
//     struct
//         val Fmt : char
//         val ArgCount : int
//         val MapRouteFn : (obj -> HttpHandler)
//    end
// and EndCompleter =
//     struct
//         val ArgCount : int
//         val MapRouteFn : (obj -> HttpHandler)
//     end
type TNode =

        //charet upper & lower inclusive bounds of array
        val UBound : int
        val LBound : int
        val Completion : string
        //sparse array ref
        val Edges : TNode []
        val NodeType : INodeType
        val HandlerFn : HttpHandler
        val MidMatchFns : MidMatcher []
        val EndMatchFn : EndMatcher
        
        //,edgeArray:TNode[],hdlrFn:obj,midFns:MidMatcher [],endFns:EndMatcher []
        new(cmin:int,cmax:int,nt:INodeType,edgeArray:TNode[],hdlrFn:HttpHandler,midFns:MidMatcher [],endFn:EndMatcher) = 
            {
                LBound = cmin
                UBound = cmax
                Completion = ""
                NodeType = nt
                Edges = edgeArray
                HandlerFn = hdlrFn
                MidMatchFns = midFns
                EndMatchFn = endFn
            }

let inline intIn x l u = (x - l) * (u - x) >= 0
let nextNode (n: TNode) (v:char) =
    let vi = (int v)
    if intIn vi n.LBound n.UBound then
        let result = n.Edges.[vi - n.LBound]
        //let tnull = Unchecked.defaultof<TNode>
        if result.NodeType = INodeType.UnInit  then
            None
        else
            Some result
    else None

let setValue (vnl:(char * TNode) list )=
    let cmin,cmax = 
        vnl |> List.fold (fun (n,x) (c,d) -> min n c,max x c) (System.Char.MaxValue,System.Char.MinValue)
        |> fun (n,x) -> int n,int x
    let newNode = {
        LBound = cmin 
        UBound= cmax
        INodeType = INodeType.EmptyNode
        Edges = Array.zeroCreate<TNode>(UBound - LBound + 1)
         }
    if intIn vi root.LBound root.Ubound then
        root.Edges.[vi] <- child

