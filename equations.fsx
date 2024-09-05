// Определите функции для решение алгебраических уравнений

let eps = 0.00001 

let rec dichotomy f a b =
    let c = (a+b)/2.
    if (f c) < 0.00001 then c
    else if (f c)*(f a) < 0 then dichotomy f a c
    else dichotomy f c b

let rec iterations phi x0 =
    if abs(x0 - phi(x0)) < eps then
        phi(x0)
    else
        iterations phi (phi(x0))

let newthon f f' x0 = 
    iterations (fun x->x - f(x)/f'(x)) x0

// Решите 3 уравнения (начиная со своего номера варианта) с использованием 3-х методов
let f1 = log(x) - x + 1.8
let f2 = x*tan(x) - 1./3.
let f3 = tan(x/2.) - 1./tan(x/2.) + x

let f1' = 1./x -1.
let f2' = tan(x) + x /(cos(x)**2.)
let f3' = 0.5*(1./(cos(x/2.)**2.)+1./(sin(x/2.)**2.)) + 1.

let phi1 = x - f1(x) / f1'(x)
let phi2 = x - f2(x) / f2'(x)
let phi3 = x - f3(x) / f3'(x)

let main = 
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f1 0. 1.) (iterations phi1 0.) (newthon f1 f1' 1.)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f2 0. 1.) (iterations phi2 0.) (newthon f2 f2' 1.)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f3 0. 1.) (iterations phi3 0.) (newthon f3 f3' 1.)

 
