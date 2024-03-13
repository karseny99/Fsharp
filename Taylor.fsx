let fact = 
    let rec fact' acc = function
    | n when n = 1. -> acc
    | n -> fact' (acc*n) (n - 1.)
    fact' 1.


let abs x = 
    if x >= 0. then x
    else -x

let floatComp x y eps =
    if abs(x - y) < eps then true
    else false


// General loop
let while2 cur exitCond f x eps =
    let rec while3 f acc cur i =
        if exitCond cur (f cur x (i+1)) eps then (acc, i)
        else while3 f (acc+cur) (f cur x (i+1)) (i+1)
    while3 f 0. cur 1

let taylor_naive x = 
    let computeNextNaive u x n =
        if n >= 1 then (pown (-1.0) (n - 1)) * (pown 2.0 (2*n - 1)) * (pown x (2*n)) / ( fact (2.0*float(n)) )
        else 0.

    while2 (computeNextNaive -1 x 1) floatComp computeNextNaive x 1e-7


let taylor x = 
    let computeNext u x n = 
        if n > 1 then (-1.)*(u * 4. * x * x) / (2. * float(n)) / (2. * float(n) - 1.)
        else (2. * x * x) / 2.

    while2 (computeNext -1 x 1) floatComp computeNext x 1e-7


// Task with built-in function
let sinInPowerOfTwo x = pown (sin x) 2

let a = 0.0
let b = 1.0
let n = 10

let main n =
    printfn "   x    Built-in      Naive iter     Smart iter"
    for i=0 to n do
        let x = a+(float i)/(float n)*(b-a)
        let (u, v) = taylor_naive x
        let (i, o) = taylor x
        printfn "%5.2f  %10.6f  %10.6f %d   %10.6f %d" x (sinInPowerOfTwo x) u v i o
// make sure to improve this table to include the required number of iterations
// for each of the methods

main n
