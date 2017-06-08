module HttpRouter.RouterParsers

//between inclusive
let inline intIn x l u = (x - l) * (u - x) >= 0

let inline int64In x l u = (x - l) * (u - x) >= 0L

let inline floatIn x l u = (x - l) * (u - x) >= 0.

let routerKey = "router_pos"

type RouteState(path:string) =
    member val path = path with get
    member val pos = 0 with get , set

/// Private Range Parsers that quickly try parse over matched range (all fpos checked before running in preceeding functions)

let private stringParse (path:string) ipos fpos = path.Substring(ipos,fpos - ipos + 1) |> box |> Some

let private  charParse (path:string) ipos _ = path.[ipos] |> box |> Some // this is not ideal method (but uncommonly used)

let private boolParse (path:string) ipos fpos =
    if intIn (fpos - ipos) 4 5 then 
        match path.[ipos] with
        | 't' | 'T' -> true  |> box |> Some // todo: Laxy matching, i'll complete later
        | 'f' | 'F' -> false |> box |> Some
        | _ -> None
    else None

let private intParse (path:string) ipos fpos =

    let mutable result = 0
    let mutable negNumber = false
    let rec go pos =
        let charDiff = int path.[pos] - int '0'
        if intIn charDiff 0 9 then
            result <- (result * 10) + charDiff
            if pos = fpos then
                if negNumber then - result else result 
                |> box |> Some 
            else go (pos + 1)       // continue iter
        else None
    //Start Parse taking into account sign operator
    match path.[ipos] with
    | '-' -> negNumber <- true ; go (ipos + 1)
    | '+' -> go (ipos + 1)
    | _ -> go (ipos)
    
let private int64Parse (path:string) ipos fpos =

    let mutable result = 0L
    let mutable negNumber = false
    let rec go pos =
        let charDiff = int64 path.[pos] - int64 '0'
        if int64In charDiff 0L 9L then
            result <- (result * 10L) + charDiff
            if pos = fpos then
                if negNumber then - result else result 
                |> box |> Some 
            else go (pos + 1)       // continue iter
        else None
    //Start Parse taking into account sign operator
    match path.[ipos] with
    | '-' -> negNumber <- true ; go (ipos + 1)
    | '+' -> go (ipos + 1)
    | _ -> go (ipos)

let decPower = [|1.;10.;100.;1000.;10000.;100000.;1000000.;10000000.;100000000.;100000000.|] |> Array.map (fun d -> 1. / d) // precompute inverse once at compile time
    
let floatParse (path:string) ipos fpos =
    let mutable result = 0.
    let mutable decPlaces = 0
    let mutable negNumber = false
    
    let rec go pos =
        if path.[pos] = '.' then
            decPlaces <- 1
            if pos < fpos then go (pos + 1) else None
        else
            let charDiff = float path.[pos] - float '0'
            if floatIn charDiff 0. 9. then
                if decPlaces = 0 then 
                    result <- (result * 10.) + charDiff
                else
                    result <- result + ( charDiff * decPower.[decPlaces]) // char is divided using multiplication of pre-computed divisors
                    decPlaces <- decPlaces + 1
                if pos = fpos || decPlaces > 9 then
                    if negNumber then - result else result 
                    |> box |> Some 
                else go (pos + 1)   // continue iter
            else None   // Invalid Character in path

    //Start Parse taking into account sign operator
    match path.[ipos] with
    | '-' -> negNumber <- true ; go (ipos + 1)
    | '+' -> go (ipos + 1)
    | _ -> go (ipos)

let formatStringMap =
    dict [
    // Char    Range Parser
    // ---------------  -------------------------------------------
        'b', (boolParse  )  // bool
        'c', (charParse  )  // char
        's', (stringParse)  // string
        'i', (intParse   )  // int
        'd', (int64Parse )  // int64
        'f', (floatParse )  // float
    ]
