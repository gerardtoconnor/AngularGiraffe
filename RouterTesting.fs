module RouterTesting
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open NSubstitute
open Giraffe.HttpHandlers
open Giraffe.HttpTokenRouter
open Giraffe.HttpRouteArray

type RouterTest () =
    
    let ctx = Substitute.For<HttpContext>()

    let routeArray = 
    let tokenApi : HttpHandler =
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

    let araryApi : HttpHandler =
        router [
            route "/" => text "Hello world, from Giraffe!"
            route "/test" => text "Giraffe test working"
            route "/about" => text "Giraffe about page!"
            route "/wheretofindus" => text "our location page"
            route "/ourstory" => text "our story page"
            route "/products" => text "product page"
            route "/delivery" => text "delivery page"
            routef "/data/%s/weather" (fun v -> sprintf "json (weatherForecasts (%s))" v |> text)
            routef "/value/%s" text 
            route "/auth" => [
                route "/dashboard" => text "Auth Dashboard"
                route "/inbox" => text "Auth Inbox"
                route "/helpdesk" => text "Auth Helpdesk"
                routef "/parse%slong%istrings%sand%sIntegers" (fun (a,b,c,d) -> sprintf "%s | %i | %s | %s" a b c d |> text)
                routef "token/%s" (fun v -> text ("following token recieved:" + v))                                    
                route "/manager" => [
                    route "/payroll" => text "Manager Payroll"
                    route "/timesheets" => text "Manager Timesheets"
                    route "/teamview" => text "Manager Teamview"
                    routef "/team%ssales%f" (fun (t,s) -> sprintf "team %s had sales of %f" t s |> text)
                    routef "/accesscode/%i" (fun i -> sprintf "manager access close is %i" i |> text)
                    route "/executive" => [
                        route "/finance" => text "executive finance"
                        route "/operations" => text "executive operations"
                        route "/mis" => text "executive mis"
                        routef "/area/%s" (sprintf "executive area %s" >> text)
                        routef "/area/%s/district/%s/costcode%i" (fun (a,d,c) -> sprintf "executive area %s district %s costcode %i"  a d c |> text)
                    ]
                ]
            ]
        ]

    [<Benchmark>]
    member x.RouteArray() =
        ()