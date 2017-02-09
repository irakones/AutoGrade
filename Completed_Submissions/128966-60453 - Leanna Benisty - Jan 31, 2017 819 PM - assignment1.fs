(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Leanna Benisty, Id Number: 260690685 *) (* Edit this line. *)

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
    | x::xs -> x + sumlist xs
let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y) :: pairlists (xs, ys)

(* multiplylist is a function that multiplies pairs of elements in a list 
and returns a single list *)
let rec multiplylist l =
  match l with
    | [] -> 0.0
    | (x, y)::xs -> (x * y) + multiplylist xs
let w_mean weights data =  
  let denominator = sumlist weights
  let numerator = multiplylist (pairlists (weights, data))
  numerator / denominator

(* Question 2. *)  (* Do not edit this line. *)
let rec memberof pair = 
  match pair with
  | (x,[]) -> false
  | (x, y::ys) -> 
    if x = y then true
    else memberof (x,ys)

let rec remove(item, lst) = 
  match (item, lst) with  
    | (x, []) -> []
    | (x, y::ys) ->
      if x = y then remove(x, ys)
      else y::remove(x, ys)

(* Question 3. *)  (* Do not edit this line. *)
let findMax l = 
  let rec helper(l,m) = 
    match l with
      | [] -> m
      | x::xs ->
        if x > m then helper (xs,x)
        else helper (xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper (xs,x)

(* Question 4. *)  (* Do not edit this line. *)
let rec selsort l = 
  match l with  
    | [] -> []
    | x::xs ->
      let max = findMax (x::xs)
      max::selsort(remove (max,x::xs))

(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists = 
  match twolists with
  | ([], y) -> []
  | (x::xs, y) ->
    if memberof(x,y) then x::common(remove(x,xs), remove(x,y))
    else common(remove(x,xs), y)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)
let rec split l = 
  match l with
  | [] -> [], []
  | [x] -> [x], []
  | x::x'::xs ->
    let (l1,l2) = split(xs)
    (x::l1, x'::l2)

let rec merge twolists = 
  match twolists with
  | ([],[]) -> []
  | (x::xs, []) ->
    x::merge(xs, [])
  | ([], y::ys) ->
    y::merge([], ys)
  | x::xs, y::ys ->
    if x > y then y::(merge(x::xs,ys))
    else x::(merge(xs, y::ys))
  
let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let (l1,l2) = split(n::ns)
    merge(mergesort(l1), mergesort(l2))