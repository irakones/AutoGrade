(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Yi Feng Yan, Id Number: 260687858 *) (* Edit this line. *)

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
    | x::xs -> x + (sumlist xs)

let rec pairlists twolists =
  match twolists with
  | ([], []) -> []
  | ([], x::xs) -> failwith "Error -- lists are not of the same length"
  | (x::xs, []) -> failwith "Error -- lists are not of the same length"
  | (x::xs, y::ys) -> (x, y)::(pairlists (xs, ys))

let w_mean weights data =
    let rec productlist lists =
        match lists with
        | [] -> []
        | x::xs ->
            let (left: float, right: float) = x
            (left * right)::(productlist xs)
    let numerator = pairlists (weights, data)
    sumlist (productlist numerator) / sumlist weights
      
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
    let (key, set) = pair
    match set with
    | [] -> false
    | x::xs ->
        if key = x then true
        else memberof (key, xs)

let rec remove(item, lst) =
    match lst with
    | [] -> []
    | x::xs ->
        if item = x then remove (item, xs)
        else x::(remove (item, xs))

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =
      match l with
      | [] -> m
      | x::xs ->
          if x > m then helper (xs, x)
          else helper(xs, m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =
    match l with
    | [] -> []
    | x::xs ->
        try
            let maxima = findMax xs
            if x < maxima then maxima::(selsort (remove (maxima, x::xs)))
            else x::(selsort (remove (x, xs)))
        with
        | :? System.Exception -> [x]

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
    let (left, right) = twolists
    match left with
    | [] -> []
    | x::xs ->
        if memberof (x, right) then x::(common (remove (x, xs), right))
        else common (xs, right)

(* Question 6. *)   (* Do not edit this line. *)

let rec split l =
    let rec actualsplit k =
        match k with
        | [] -> []
        | x::xs ->
            match xs with
            | [] -> [x]
            | y::ys -> x::(actualsplit ys)
    match l with
    | [] -> ([], [])
    | x::xs -> (actualsplit l, actualsplit xs)

let rec merge twolists =
    match twolists with
    | (xs, []) -> xs
    | ([], ys) -> ys
    | (x::xs, y::ys) ->
        if x > y then y::(merge (x::xs, ys))
        else x::(merge (xs, y::ys))

let rec mergesort l = 
  match l with
  | [] -> []
  | n::[] -> [n]
  | n::ns ->
      let (left, right) = split l
      merge (mergesort left, mergesort right)
