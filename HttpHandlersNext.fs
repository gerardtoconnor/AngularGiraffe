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

//result of any handler
type HttpHandler = Continuation -> HttpContext -> Task<HttpContext>

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
                    let! rtn = handler next ctx
                    choose tail next ctx //if a branch fails, go to next handler in list
            }

let httpVerb (verb : string) : HttpHandler =
    fun next ctx ->
        if ctx.Request.Method.Equals verb
        then next ctx
        else Task.FromResult ctx 