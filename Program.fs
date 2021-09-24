﻿type Poly = int list

let abs x =
    if x > 0 then x else x * -1

let rec add (xs, ys) =
    match (xs, ys) with
    | (x::xs, y::ys)   -> (x + y)::add(xs, ys)
    | ([], ys)         -> ys
    | (xs, [])         -> xs
    
let rec mulC (c, xs) =
    match xs with
        | x::xs    -> (c * x)::mulC(c, xs)
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

let isLegal xs =
 match xs with
    | [] -> true
    | _ -> List.last xs <> 0

let prune xs =
    let i = List.findIndexBack (fun x -> x <> 0) xs
    xs.[0..i]

let toString (xs) =
    let rec doToString(i, ys) =
        if List.isEmpty ys then "" else
        let x0::zs = ys
        let del = if x0 > 0 then "+" else "-"
        match (x0, i) with
        | (0, _) -> "" + doToString(i + 1, zs)
        | (_, 0) -> x0.ToString() + doToString(i + 1, zs)
        | (_, 1) -> sprintf " %s %ix" del (abs x0) + doToString(i + 1, zs)
        | _ -> sprintf " %s %ix^%i" del (abs x0) i + doToString(i + 1, zs)
    sprintf "%s" (doToString(0, xs))

let mul (polA, polB) =
    let rec doMul (i:int, pol) =
        if List.isEmpty pol then [] else
        let l = [ for n in 1 .. i -> 0 ]
        let a::polC = pol
        add( (mulC( a, (List.append l polB ) )), (doMul( (i + 1), polC) ) )
    prune (doMul(0, polA))

let pow (pol, k) =
    let rec doPow(polB, n) =
        match n with
        | 0 -> [1]
        | 1 -> polB
        | _ -> doPow(mul(polB, pol), (n - 1))
    doPow(pol, k)

let compose (polA:int list, polB:int list) =
    let rec doCompose (i:int, pol) =
        if List.isEmpty pol then [] else
        let a::polC = pol
        if i = 0 then add([a], (doCompose( (i + 1), polC) ) ) else
        add( (mulC( a, (pow(polB, i)) )), (doCompose( (i + 1), polC) ) )
    doCompose(0, polA)

let derivative (poly:int list) =
    let rec doDerivative (i, nPoly) =
        match nPoly with
        | [] -> []
        | _ -> (List.head nPoly * i)::doDerivative(i + 1, (nPoly.[1..]))
    doDerivative (1, poly.[1..])