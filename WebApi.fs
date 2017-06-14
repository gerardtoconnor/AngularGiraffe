module WebApi

open System
open System.Collections.Generic
open System.IO
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Giraffe.HttpHandlers
open Test.HttpRouterToken
open Giraffe.Middleware
open Giraffe.Common
open Giraffe.ValueTask
//open Giraffe.AsyncTask


let AuthTestHandler : HttpHandler =
    fun succ fail ctx -> task {
        let! (prin:Security.Claims.ClaimsPrincipal) = ctx.Authentication.AuthenticateAsync("Test")
        if prin.Identity.Name = "Gerry" then
            return! succ ctx
        else
            return! fail ctx
    }

// let dist (path:sring) (ctx:HttpContext) : HttpHandlerResult =
//     let file = readFileAsSring 
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
                routeTf "/name%sssn%i" (fun (n,s) -> sprintf "your name is [%s] with social security number [%i]" n s |> text)                 
                routeT "/" <| text "Hello world, from Giraffe!"
                routeT "/test" <| text "Giraffe testing working"
                subRouteT "/deep" ==> 
                    routeTrie [
                        routeT "/hasktag" ==> text "hashtag deep"
                        routeT "/solo" ==> text "han Solo"
                        routeTf "/map/%s" text
                        routeTf "/rev/%s/end" (sprintf "string path hit:%s" >> text)
                        routeTf "/rev/%i/end" (fun v1 -> sprintf "integer path hit:%i" v1 |>  text )
                        routeTf "/rev/%s/end/%s" (fun (v1,v2) -> sprintf "double whammy baby, [%s] as well as [%s]" v1 v2 |> text)                      
                ]
                routeT "/auth" ==> choose [
                                        AuthTestHandler >=> text "your Authorised" 
                                        setStatusCode 404 >=> text "Not Found"
                                    ]                                  
                routeT "/SampleData/WeatherForecasts" <| json (weatherForecasts ())
                routeTf "/value/%s/cats/%s/end" (fun (v1,v2) -> sprintf "we recieved in [%s] and [%s]" v1 v2 |> text)
                //routef "/value/%s" (fun (v) -> text (v:?> sring))        
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

let chooseApi : HttpHandler =
    choose [
        route "/" >=> text "Hello world, from Giraffe!"
        route "/test" >=> text "Giraffe test working"
        route "/about" >=> text "Giraffe about page!"
        route "/wheretofindus" >=> text "our location page"
        route "/ourstory" >=> text "our story page"
        route "/products" >=> text "product page"
        route "/delivery" >=> text "delivery page"
        routef "/data/%s/weather" (fun v -> sprintf "json (weatherForecasts (%s))" v |> text)
        routef "/value/%s" text 
        subRoute "/auth" >=> choose [
            route "/dashboard" >=> text "Auth Dashboard"
            route "/inbox" >=> text "Auth Inbox"
            route "/helpdesk" >=> text "Auth Helpdesk"
            routef "/parse%slong%istrings%sand%sIntegers" (fun (a,b,c,d) -> sprintf "%s | %i | %s | %s" a b c d |> text)
            routef "token/%s" (fun v -> text ("following token recieved:" + v))                                    
            subRoute "/manager" >=> choose [
                route "/payroll" >=> text "Manager Payroll"
                route "/timesheets" >=> text "Manager Timesheets"
                route "/teamview" >=> text "Manager Teamview"
                routef "/team%ssales%f" (fun (t,s) -> sprintf "team %s had sales of %f" t s |> text)
                routef "/accesscode/%i" (fun i -> sprintf "manager access close is %i" i |> text)
                subRoute "/executive" >=> choose [
                    route "/finance" >=> text "executive finance"
                    route "/operations" >=> text "executive operations"
                    route "/mis" >=> text "executive mis"
                    routef "/area/%s" (sprintf "executive area %s" >> text)
                    routef "/area/%s/district/%s/costcode%i" (fun (a,d,c) -> sprintf "executive area %s district %s costcode %s"  a d c |> text)
                 ]
            ]
        ]
    ]
    
let trieApi : HttpHandler =
    routeTrie [
        routeT "/" ==> text "Hello world, from Giraffe!"
        routeT "/test" ==> text "Giraffe test working"
        routeT "/about" ==> text "Giraffe about page!"
        routeT "/wheretofindus" ==> text "our location page"
        routeT "/ourstory" ==> text "our story page"
        routeT "/products" ==> text "product page"
        routeT "/delivery" ==> text "delivery page"
        routeTf "/data/%s/weather" (fun v -> sprintf "json (weatherForecasts (%s))" v |> text)
        routeTf "/value/%s" text 
        subRouteT "/auth" ==> routeTrie [
            routeT "/dashboard" ==> text "Auth Dashboard"
            routeT "/inbox" ==> text "Auth Inbox"
            routeT "/helpdesk" ==> text "Auth Helpdesk"
            routeTf "/parse%slong%istrings%sand%sIntegers" (fun (a,b,c,d) -> sprintf "%s | %i | %s | %s" a b c d |> text)
            routeTf "token/%s" (fun v -> text ("following token recieved:" + v))                                    
            subRouteT "/manager" ==> routeTrie [
                routeT "/payroll" ==> text "Manager Payroll"
                routeT "/timesheets" ==> text "Manager Timesheets"
                routeT "/teamview" ==> text "Manager Teamview"
                routeTf "/team%ssales%f" (fun (t,s) -> sprintf "team %s had sales of %f" t s |> text)
                routeTf "/accesscode/%i" (fun i -> sprintf "manager access close is %i" i |> text)
                subRouteT "/executive" ==> routeTrie [
                    routeT "/finance" ==> text "executive finance"
                    routeT "/operations" ==> text "executive operations"
                    routeT "/mis" ==> text "executive mis"
                    routeTf "/area/%s" (sprintf "executive area %s" >> text)
                    routeTf "/area/%s/district/%s/costcode%i" (fun (a,d,c) -> sprintf "executive area %s district %s costcode %i"  a d c |> text)
                 ]
            ]
        ]
    ]