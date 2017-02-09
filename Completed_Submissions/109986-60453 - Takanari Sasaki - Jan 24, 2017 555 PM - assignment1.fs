(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Takanari Sasaki, Id Number: 260639497 *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code MUST compile and must NOT go into infinite
loops.  An assignment like that means you have not tested it.  You will get
ZERO FOR THE ENTIRE ASSIGMENT even if the problem is only with one
question.  If you are not able to get the code to compile and run do not
submit it.  *)

(* module hw1_sol.  Use this if you want to load the file into an interactive session.*)

(* Question 1 *) (* Do not edit this line. *)

let rec sumlist l = 
  match l with
  | [] -> 0.0
  | x :: xs -> x + sumlist(xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y) :: pairlists(xs, ys)

let w_mean weights data =

    let pairOfWeightAndDataList = pairlists(weights, data)
    let sumOfWeights = sumlist weights

    let rec sumOfProducts inputPairs = 
        match inputPairs with
        | [] -> 0.0
        | (y,z) :: xs -> (y * z) + sumOfProducts(xs)

    (sumOfProducts pairOfWeightAndDataList) / sumOfWeights
  
(* Question 2. *)  (* Do not edit this line. *)
let rec memberof pair = 
    match pair with
    | (x, []) -> false
    | (x, y :: ys) -> 
        if (x = y) then true
        else memberof (x, ys)

let rec remove(item, lst) =
    match lst with
    | [] -> []
    | x :: xs -> 
        if (x = item) then remove(item, xs)
        else x :: remove(item, xs)

(* Question 3. *)  (* Do not edit this line. *)
let findMax l = 
  let rec helper(l,m) = 
    match l with
    | [] -> m
    | x :: xs ->
        if (m > x) then helper (xs, m)
        else helper (xs, x)
    
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
let rec selsort l = 

    let max = findMax l
    let maxRemovedList = remove(max, l)

    match maxRemovedList with
    | [] -> [max]
    | _ -> max :: selsort(maxRemovedList)       // _ symbol provides the default pattern, meaning that it matches all other things like a wildcard.


(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists = 
    match twolists with
    | ([], yList) -> []
    | (x::xs, yList) ->
        if (memberof(x, yList)) then
          x :: common(xs, remove(x, yList))
        else common(xs, yList) 


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)
let rec split l =
    match l with
    | [] -> ([], [])
    | [x] -> ([x], [])      // If only one elemet is left in the list, next statement will not work.
    | x1 :: x2 :: xs ->
        let (list1, list2) = split(xs)      // hit the base case, which is ([], []) or ([x], []).
        (x1 :: list1, x2 :: list2)

let rec merge twolists =
    match twolists with
    | ([], y) -> y
    | (x, []) -> x
    | (x :: xs, y :: ys) -> 
        if (x < y) then x :: merge(xs, y :: ys)
        else y :: merge(x :: xs, ys)

let rec mergesort l = 
    match l with
    | [] -> []
    | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
    | n::ns -> 
        let (list1, list2) = split l
        merge((mergesort list1),(mergesort list2))

