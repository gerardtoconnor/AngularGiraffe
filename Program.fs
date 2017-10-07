module AngularGiraffe

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection;
open Microsoft.Extensions.Logging
open Microsoft.Extensions.FileProviders
open Microsoft.AspNetCore.SpaServices.Webpack
open Giraffe.HttpHandlers
//open Test.HttpRouterToken
open Giraffe.Middleware
open Giraffe.Common
open Giraffe.Tasks
open Giraffe.TokenRouter
//open Giraffe.AsyncTask

// let webApp : HttpHandler = 
//     router [
//         subRoute "/api" => WebApi.webApi
//         route "/plainroute" => text "plain route hit"
//         routef "/dist/%s" <| fun path next ctx -> next ctx
//         route "/__webpack_hmr" <| fun next ctx -> next ctx
//         route "/" => PreRender.renderIndex
//         routef "/multiRoute/%s" text 
//         routef "/v/%s" (fun s -> PreRender.renderRoute s)
//         //setStatusCode 404 >=> text "Not Found xxx" 
//     ]


// ---------------------------------
// Error handler
// ---------------------------------



let errorHandler (ex : Exception) (logger : ILogger) : HttpHandler =
    logger.LogError(EventId(0), ex, "An unhandled exception has occurred while executing the request.")
    (clearResponse >=> setStatusCode 500 >=> text ex.Message)

// ---------------------------------
// Config and Main
// ---------------------------------

let configureApp (app : IApplicationBuilder) = 
    // app.UseStaticFiles( 
    //     //     StaticFileOptions(
    //     //         FileProvider = 
    //     //             new PhysicalFileProvider(
    //     //                     Path.Combine(Directory.GetCurrentDirectory(), @"dist")),
    //     //         RequestPath = PathString("/dist")
    //     // )
    //     ) |> ignore
    app.UseGiraffeErrorHandler errorHandler
    app.UseGiraffe WebApi.webApi
    app.UseWebpackDevMiddleware(WebpackDevMiddlewareOptions(HotModuleReplacement = true))

let configureServices (services : IServiceCollection) =
    services.AddNodeServices()
    services.AddResponseCaching() |> ignore
    // let sp  = services.BuildServiceProvider()
    // let env = sp.GetService<IHostingEnvironment>()
    // let viewsFolderPath = Path.Combine(env.ContentRootPath, "Views")
    // services.AddRazorEngine viewsFolderPath |> ignore

let configureLogging (loggerFactory : ILoggerFactory) =
    loggerFactory.AddConsole(LogLevel.Trace).AddDebug() |> ignore

[<EntryPoint>]
let main argv =                                
    WebHostBuilder()
        .UseKestrel()
        .UseContentRoot(Directory.GetCurrentDirectory())
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(Action<IServiceCollection> configureServices)
        //.ConfigureLogging(Action<ILoggerFactory> configureLogging)
        .Build()
        .Run()
    0