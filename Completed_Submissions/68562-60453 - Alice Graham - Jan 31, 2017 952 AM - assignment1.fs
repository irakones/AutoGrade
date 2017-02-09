﻿(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Alice Graham, Id Number: 260533964 *) (* Edit this line. *)

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
    | (x::xs, y::ys) -> (x,y)::pairlists (xs,ys)

let w_mean weights data =
  let a = pairlists (weights,data) |> List.map(fun (x,y) -> x*y) //* Uses an anonymous function that simply multiples weights and data. Converts list of pairs to list of products.
  (sumlist a)/sumlist weights
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
  match pair with
    |(y,[]) -> false
    |(y,x::xs) -> if (x=y) then true else memberof (y,xs)


let rec remove(item, lst) =
  match lst with
    | [] -> []
    | x::xs -> if (x=item) then remove(item,xs) else x::remove(item,xs)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
    | [] -> m
    | x::xs -> if (x>m) then helper(xs,x) else helper(xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =
  match l with
  | [] -> []
  | _ -> findMax(l)::selsort (remove(findMax(l),l))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
  match twolists with
  | ([],[]) -> []
  | (_, []) -> []
  | ([], _) -> []
  | (x::xs, y) -> if memberof (x,y) then x::common (xs,remove(x,y)) else common(xs,y)


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
  match l with
  | [] -> ([],[])
  | x::xs ->
    match split(xs) with
    | (a,b) -> (x::b,a) //* Switches the order of the returned pair of lists to alternatingly add to each list.

let rec merge twolists =
  match twolists with
  | ([],[]) -> []
  | ([],a) -> a
  | (b,[]) -> b
  | (x::xs,y::ys) -> if (x<y) then x::merge(xs,y::ys) else if (x=y) then x::merge(xs,ys) else y::merge(x::xs,ys) //* Adds duplicate elements only once.

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    match split(l) with
    | ([],[]) -> []
    | ([],a) -> a
    | (a,[]) -> a
    | (x,y) -> merge(mergesort(x),mergesort(y))
    

