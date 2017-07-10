module RouterTesting
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

type RouterTest () =
    
    [<Benchmark>]
    member x.RouteArray() =
        ()