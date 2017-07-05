module PreRender

open System.IO
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.NodeServices
open Microsoft.AspNetCore.SpaServices.Prerendering
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Http.Extensions
open Giraffe.Task
//open Giraffe.AsyncTask
open Giraffe.HttpHandlers
open Giraffe.Common
open WebApi

let private moduleExport = JavaScriptModuleExport("./ClientApp/dist/main-server")
let private writeLayoutRender (res:HttpResponse) (file:string) (tag:string) (data:string) (trender:Task<RenderToStringResult>) =
    let enum = System.IO.File.ReadLines(file).GetEnumerator()
    let write str = res.WriteAsync(str) //|> task.AwaitTask

    let rec go searching = task {
        if enum.MoveNext() then
            if searching then
                let i = enum.Current.IndexOf(tag)
                if i < 0 then
                    do! write enum.Current
                    return! go true
                else
                    do! write ( enum.Current.Substring(0,i) )
                    let! (prerenderedHtml : RenderToStringResult) = trender
                    //do! write <| sprintf "<script>var TRANSFER_CACHE = '%s'</script>" data
                    do! write prerenderedHtml.Html
                    do! write ( enum.Current.Substring(i + tag.Length,enum.Current.Length - i - tag.Length) )
                    return! go false
            else
                do! write enum.Current
                return! go false
    }
    go true
         
let private executeResultAsync dataToSupply : HttpHandler =
    fun ctx ->
        let nodeServices = ctx.RequestServices.GetRequiredService<INodeServices>()
        let hostEnv = ctx.RequestServices.GetRequiredService<IHostingEnvironment>()
        let applicationBasePath = hostEnv.ContentRootPath
        let request = ctx.Request
        let response = ctx.Response;

        task {

        let prerenderedHtml = 
            Prerenderer.RenderToString(
                            applicationBasePath,
                            nodeServices,
                            moduleExport,
                            request.GetEncodedUrl(),
                            request.Path + request.QueryString.Value,
                            dataToSupply,
                            30000 //* timeoutMilliseconds */ 
            )

        response.ContentType <- "text/html"

        //"./Views/Home/Index.html"

        do! writeLayoutRender response (applicationBasePath + "/Views/Home/Index.html") "<app></app>" dataToSupply prerenderedHtml
        //do! response.WriteAsync(prerenderedHtml.Html) |> Async.AwaitTask

        return Some ctx
    }

let renderIndex  : HttpHandler = executeResultAsync "" 

let loadmap = 
    dict [
            ("fetch-data", fun () -> WebApi.weatherForecasts () |> serializeJson)
        ]

let renderRoute (path:string) = 
    let data =
        match loadmap.TryGetValue path with
        | true,fn -> fn()
        | false,_ -> "" 
    executeResultAsync data


    // let! prerenderedHtml = 
    //     Prerenderer.RenderToString(
    //                     applicationBasePath,
    //                     nodeServices,
    //                     moduleExport,
    //                     request.GetEncodedUrl(),
    //                     request.Path + request.QueryString.Value,
    //                     dataToSupply,
    //                     30000 //* timeoutMilliseconds */ 
    //     ) |> Async.AwaitTask

    // response.ContentType <- "text/html"

    // //"./Views/Home/Index.html"

    // do! response.WriteAsync(prerenderedHtml.Html) |> Async.AwaitTask