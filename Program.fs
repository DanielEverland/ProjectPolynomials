open FsCheck;;

type Poly = int list

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

let prune (xs) =
    let result = List.tryFindIndexBack (fun x -> x <> 0) xs
    match result with
    | None -> []
    | _ -> xs.[0..result.Value]

let rec sub = function
    | (x::xs, y::ys)   -> prune((x - y)::sub(xs, ys))
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

type Degree =   | MinusInf
                | Fin of int

let degree poly =
    match poly with
    | [] -> MinusInf
    | _ -> Fin (List.findIndexBack (fun x -> x <> 0) poly)

let addD (a, b) =
    match (a, b) with
    | (MinusInf, _) | (_, MinusInf) -> MinusInf
    | (Fin f, Fin g) -> Fin (f + g)

// Tests:
let addInv p1 p2 = isLegal(add( (prune p1), (prune p2) ));;
let _ = Check.Quick addInv;;

// 1
let addAss p1 p2 p3 = add(add(p1, p2), p3) = add(p1, add(p2, p3));;
let _ = Check.Quick addAss;;

// 2
let addCom p1 p2 = add(p1, p2) = add(p2, p1);;
let _ = Check.Quick addCom;;

// 3
Check.Quick (fun p -> (add(p, []) = p) && add([], p) = p);;

// 4
Check.Quick (fun p -> sub(p, p) = [])

// 5
Check.Quick (fun p1 p2 p3 -> mul(mul(p1, p2), p3) = mul(p1, mul(p2, p3)));;

// 6
Check.Quick (fun p1 p2 -> mul(p1, p2) = mul(p2, p1))

// 7
Check.Quick (fun p -> mul(prune(p), [1]) = prune(p) && prune(p) = mul([1], prune(p)))

// 8
Check.Quick (fun p1 p2 p3 -> mul(p1, add(p2, p3)) = add(mul(p1, p2), mul(p1, p3)))

// 9
Check.Quick (fun p1 p2 p3 -> mul(add(prune(p1), prune(p2)), prune(p3)) = add(mul(prune(p1), prune(p3)), mul(prune(p2), prune(p3)))