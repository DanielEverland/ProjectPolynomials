// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
// See the 'F# Tutorial' project for more help.

type Poly = int list

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

let rec add = function
| (x::xs, y::ys)   -> (x + y)::add(xs, ys)
| ([], ys)         -> ys
| (xs, [])         -> xs

let rec mulC (c, xs) =
 match xs with
 | x::xs   -> (c * x)::mulC(c, xs)
 | _        -> xs

let rec sub = function
 | (x::xs, y::ys)   -> (x - y)::sub(xs, ys)
 | ([], ys)         -> mulC(-1, ys)
 | (xs, [])         -> xs

let eval (x, xs) =
    let rec doEval(i, x, xs) =
        match xs with
        | x0::xs -> int(float(x0) * float(x) ** float(i)) + doEval(i + 1, x, xs)
        | _ -> 0
    doEval(0, x, xs)

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    0
