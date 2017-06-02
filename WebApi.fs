module WebApi

open System
open System.Collections.Generic
open System.IO
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Giraffe.HttpHandlers
open Test.HttpRouter
open Giraffe.Middleware
open Giraffe.Common
open Giraffe.AsyncTask


let AuthTestHandler : HttpHandler =
    fun succ fail ctx -> task {
        let! (prin:Security.Claims.ClaimsPrincipal) = ctx.Authentication.AuthenticateAsync("Test")
        if prin.Identity.Name = "Gerry" then
            return! succ ctx
        else
            return! fail ctx
    }

// let dist (path:string) (ctx:HttpContext) : HttpHandlerResult =
//     let file = readFileAsString 
//     let ext = path.LastIndexOf(".")
//     match file with
type WeatherForecast = {
    dateFormatted: string;
    temperatureC: int;
    temperatureF: int;
    summary: string;
}
let weatherForecasts () =
    let bands = [
        (-10,0,"Freezing");
        (0,10,"Cold");
        (10,20,"Nice");
        (20,100,"Scorching")
        ]
    let rec go temp dayOff ls =
        match ls with
        | [] -> None
        | (l,u,d) :: t ->
            if l <= temp && temp <= u then
                let fc = {    
                    dateFormatted= System.DateTime.Today.AddDays(float dayOff).ToString()
                    temperatureC = temp
                    temperatureF = temp + 40
                    summary= d
                }
                Some fc
            else
                go temp dayOff t
    
    let rnd = Random()
    //do! Task.Delay(1000)
    let noForcasts = 8
    let fcs = Array.zeroCreate(noForcasts)
    for i in 0 .. noForcasts - 1 do
        match go (rnd.Next(-10,30)) i bands with
        | Some fc -> fcs.[i] <- fc
        | None -> ()
    fcs

let webApi : HttpHandler =
     GET >=>
            routeTrie [
                tRoute "/" <| text "Hello world, from Giraffe!"
                tRoute "/test" <| text "Giraffe testing working"
                tRoute "/auth" ( AuthTestHandler >=> text "your Authorised" )
                tRoute "/SampleData/WeatherForecasts" <| json (weatherForecasts ())
                tRoutef "/value/%s/cats/%s/end" (fun (v1,v2) -> sprintf "we recieved in [%s] and [%s]" v1 v2 |> text)
                //tRoutef "/value/%s" (fun (v) -> text (v:?> string))        
            ]   

// let webApi : HttpHandler =
//     printfn "##webapi entered"
//     choose [
//         GET >=>
//             choose [
//                 route "/" >=> text "Hello world, from Giraffe!"
//                 route "/test" >=> text "Giraffe testing working"
//                 route "/auth" >=> AuthTestHandler >=> text "your Authorised"
//                 route "/SampleData/WeatherForecasts" >=> json (weatherForecasts ())
//                 routef "/value/%s" printfn        
//             ]
//         ]

let testApi : HttpHandler =
    choose [
        GET >=>
            choose [
                route "/" >=> text "Hello world, from Giraffe!"
                route "/test" >=> text "Giraffe test working"
                subRoute "/auth" >=> 
                            choose [
                                route "/dashboard" >=> text "Auth Dashboard"
                                route "/inbox" >=> text "Auth Inbox"
                                subRoute "/manager" >=>
                                    route "/payroll" >=> text "Manager Payroll"
                                    route "/timesheets" >=> text "Manager Timesheets"
                            ]
                route "/data" >=> text "json (weatherForecasts ())"
                routef "/value/%s" text         
            ]
        ]
    
