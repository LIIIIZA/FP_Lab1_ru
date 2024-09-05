// Print a table of a given function f, computed by taylor series

// function to compute
let f = 3.0 ** x

let a = 0.0
let b = 1.0
let n = 10

let fact = 
    let rec fact' acc = function
      | 1. -> acc
      | i -> fact' (acc*i) (i-1.0)
    fact' 1.

let while_naive f x =
    let rec exp acc i arg = 
      if arg < 0.00001 then acc
      else exp (acc+arg) (i+1.) (f x (i+1.))
    exp 0. 0. x 

let while_naive_terms f x =
    let rec exp acc i arg = 
      if arg < 0.00001 then i
      else exp (acc+arg) (i+1.) (f x (i+1.))
    exp 0. 0. x 

let while_smart f x =
    let rec exp acc i arg = 
      if arg < 0.00001 then acc
      else exp (acc+arg) (i+1.) (arg * (f x (i+1.)))
    exp 0. 0. x 

let while_smart_terms f x =
    let rec exp acc i arg = 
      if arg < 0.00001 then i
      else exp (acc+arg) (i+1.) (arg * (f x (i+1.)))
    exp 0. 0. x 
 
let naive x i = (log(3.0))**(float i) * (x**(float i)) / fact i
let not_dumb x i = log(3.0) * x / (float i)

// Define a function to compute f using naive taylor series method
let taylor_naive x = while_naive naive x
let taylor_naive_terms x = while_naive_terms naive x


// Define a function to do the same in a more efficient way
let taylor x = while_smart not_dumb x
let taylor_terms x = while_smart_terms not_dumb x

let main =
   for i=0 to n do
     let x = a+(float i)/(float n)*(b-a)
     printfn "%5.2f  %10.6f  %10.6f   %10.6f %5.2f %5.2f" x (f x) (taylor_naive x) (taylor x) (taylor_naive_terms x) (taylor_terms x)
// make sure to improve this table to include the required number of iterations
// for each of the methods

main

