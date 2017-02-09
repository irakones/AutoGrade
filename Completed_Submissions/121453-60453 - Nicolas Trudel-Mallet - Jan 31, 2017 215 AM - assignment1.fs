(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Nicolas Trudel-Mallet, Id Number: 260689998 *) (* Edit this line. *)

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
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::(pairlists (xs,ys))

let w_mean weights data = 
  let rec multiply pairedlist = 
    match pairedlist with
      | [] -> []
      | (x,y)::xs -> (x*y)::(multiply xs)
  (sumlist (multiply (pairlists (weights,data))))/(sumlist weights)
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
    | (_,[]) -> false
    | (item,x::xs) -> if x = item then true else memberof(item,xs)

let rec remove(item, lst) = 
  match lst with
    | [] -> []
    | x::xs -> if x = item then remove(item,xs) else x::(remove(item,xs))

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
      | [] -> m
      | x::xs -> if x > m then helper(xs,x) else helper(xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  match l with
    | [] -> []
    | x::xs ->
      let max = findMax l
      let l2 = remove(max,l)
      max::(selsort l2)

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
  match twolists with
    | ([],_) -> []
    | (x::xs,second) -> if memberof(x,second) && not(memberof(x,xs)) then x::(common(xs,second)) else common(xs,second)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  match l with
    | [] -> ([],[])
    | [x] -> ([x],[])
    | x::y::xs ->
      let first,second = split xs
      (x::first,y::second)

let rec merge twolists = 
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> x::xs
    | (x::xs, []) -> x::xs
    | (x::xs, y::ys) -> if x < y then x::(merge (xs, y::ys)) else y::(merge (x::xs, ys))

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns ->
    let first,second = split l
    merge (mergesort first, mergesort second)