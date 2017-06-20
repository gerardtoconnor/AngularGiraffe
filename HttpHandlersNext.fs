module Giraffe.HttpHandlers

open System
open System.Text
open System.Threading.Tasks
open System.Collections.Generic
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Primitives
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Mvc.Razor
open Microsoft.AspNetCore.Mvc.ViewFeatures
open FSharp.Core.Printf
open DotLiquid
open Giraffe.Common
open Giraffe.FormatExpressions
open Giraffe.HttpContextExtensions
open Giraffe.RazorEngine
open Giraffe.HtmlEngine
open Giraffe.ValueTask

type Continuation = HttpContext -> Task<HttpContext>
type Continuation2 = delegate of HttpContext -> Task<HttpContext>

//result of any handler
type HttpHandler = (Continuation2 * HttpContext ) -> Task<HttpContext>

type HttpHandler2 = delegate of (Continuation2 * HttpContext) -> Task<HttpContext>

type Handler =
    member x.Http (ctx:HttpContext) = v

let (>=>) (a: HttpHandler) (b: HttpHandler) : HttpHandler =
    fun (next,ctx) ->
        let del1 = HttpHandler2(a).Invoke
        let del2 = HttpHandler2(b)
        del2(del1 ctx)
    

/// Combines two HttpHandler functions into one.
let compose (a : HttpHandler) (b : HttpHandler) =
    fun (next : Continuation) (ctx:HttpContext) ->
        let childCont = b next // pass next fn forward down pipe
        let parentCont = a childCont
        parentCont ctx

let (>=>) = compose

type ErrorHandler = exn -> ILogger -> HttpHandler

let rec choose (handlers : HttpHandler list) : HttpHandler =
    fun (next: Continuation) (ctx : HttpContext) ->
            task {
                match handlers with
                | [] -> return ctx
                | handler :: tail ->
                    let! rtn = handler next ctx // << need to not bind if computation completed
                    choose tail next ctx //if a branch fails, go to next handler in list
            }

let httpVerb (verb : string) : HttpHandler =
    fun next ctx ->
        if ctx.Request.Method.Equals verb
        then next ctx
        else Task.FromResult ctx



open System
let ispeak dts =
    let newdts = DateTime.FromOADate dts
    match newdts.DayOfWeek, newdts.Hour with
    | DayOfWeek.Saturday, _ | DayOfWeek.Sunday, _ -> false
    | _, h when h >= 8 && h < 20 -> true
    | _ -> false

let inline isbetween r std edd = r >= std && r < edd+1.

let aggrF (data:float[]) (data2:float[]) std edd pob sac =
    let newd =
        Array.zip data data2 
        |> Array.filter (fun (date, _) -> 
            let dateInRange = isbetween date std edd
            match pob with
            | "Peak" -> ispeak date && dateInRange
            | "Offpeak" -> not(ispeak date) && dateInRange
            | _ -> dateInRange)
    match sac with 
    | 0 -> newd |> Array.averageBy (fun (_, value) -> value)
    | 2 -> newd |> Array.sumBy (fun (_, value) -> 1.0)
    | _ -> newd |> Array.sumBy (fun (_, value) -> value)


/// my solution
let inline isbetween2 r std edd = (r - std) * (edd - r + 1.) >= 0.

let aggrF2 (data:float[]) (data2:float[]) std edd pob sac =
    let filter =
        match pob with
        | "Peak"    -> fun date -> ispeak date && isbetween2 date std edd
        | "Offpeak" -> fun date -> not(ispeak date) && isbetween2 date std edd
        | _         -> fun date -> isbetween2 date std edd
    let fold (fn:'T->float->'T) (istate:'T) =
        if data.Length <> data2.Length then failwith <| sprintf "Error Data arrays are different %i vs %i" data.Length data2.Length 
        let rec go i (state:'T) (periodFound:bool) =
            if i < data.Length then
                if filter data.[i] 
                then go (i + 1) (fn state data2.[i]) true
                else 
                    if periodFound 
                    then state
                    else go (i + 1) state false
            else state
        go 0 istate false
    match sac with 
    | 0 -> fold (fun (s,i) v -> (s + v, i + 1) ) (0.,0) |> fun (s,i) -> if i <> 0 then s / float i else 0.
    | 2 -> fold (fun acc _ -> acc + 1.) 0.
    | _ -> fold (+) 0.

let aggrF3 (data:float[]) (data2:float[]) std edd pob sac =
    let isValidTime = match pob with
                        | "Peak" -> (fun x -> ispeak x)
                        | "Offpeak" -> (fun x -> not(ispeak x))
                        | _ -> (fun _ -> true)

    let endDate = edd + 1.0

    let rec aggr i sum count =
        if i >= (Array.length data) || data.[i] >= endDate then
            match sac with 
            | 0 -> sum / float(count)
            | 2 -> float(count)
            | _ -> float(sum)
        else if data.[i] >= std && isValidTime data.[i] then
            aggr (i + 1) (sum + data2.[i]) (count + 1)
        else
            aggr (i + 1) sum count

    aggr 0 0.0 0



#time 
let d1 = Array.init 1000000 float
let d2 = Array.init 1000000 float

aggrF d1 d2 0.0 1000000.0 "Test" 0

aggrF2 d1 d2 0.0 1000000.0 "Test" 0

aggrF3 d1 d2 0.0 1000000.0 "Test" 0