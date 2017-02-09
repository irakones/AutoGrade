(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Kyle Levy, Id Number: 260604024 *) (* Edit this line. *)

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
    | x::xs -> x + sumlist(xs)

let rec pairlists twolists =
    match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y)::pairlists (xs, ys)          (* Recursively puts pairs together from each list *)

(* Helper function used to calculate the product of each x_i and w_i *)

let rec product l =
    match l with
    | [] -> []
    | (x, y)::xs -> (x * y)::product(xs)

let w_mean weights data = sumlist (product(pairlists(weights, data))) / sumlist weights     (* Uses 3 previous functions to compute weighted mean. *)

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
    match pair with
    | (a, []) -> false   (* If the list is empty, false must be returned. *)
    | (a, (x::xs)) -> if (x = a) then true else memberof(a, xs)   (* Recursively call on rest of list. *)

let rec remove(item, lst) =
    match lst with
    | [] -> lst
    | (x::xs) -> if (x = item) then remove(item, xs) else x::(remove(item, xs))

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l, m) = 
    match l with
    | [] -> m
    | (x::xs) -> if (x > m) then helper(xs, x) else helper(xs, m)
  
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs, x)

(* Question 4. *)  (* Do not edit this line. *)

(* Uses methods from questions 2 and 3. *)
  
let rec selsort l = 
  let rec remove(item, lst) =
    match lst with
    | [] -> lst
    | (x::xs) -> if (x = item) then remove(item, xs) else x::(remove(item, xs))
  let findMax l = 
    let rec helper(l,m) = 
      match l with
      | [] -> m
      | (x::xs) -> if (x > m) then helper(xs, x) else helper(xs, m) 
    match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper(xs,x) 
  
  match l with
  | [] -> []
  | (x::xs) -> findMax(l)::selsort(remove(findMax(l), l))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    let rec memberof pair =
        match pair with
        | (a, []) -> false   
        | (a, (x::xs)) -> if (x = a) then true else memberof(a, xs)   
    
    match twolists with
    | ([], []) -> []
    | ([], y::ys) -> []
    | (x::xs, []) -> []
    | (x::xs, y::ys) -> if (memberof(x, y::ys)) then x::common(xs, (y::ys)) else common(xs, y::ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =  
  match l with
  | [] -> [], []
  | [n] -> [n], []
  | x::y::xs -> let (l1,l2) = split xs 
                (x::l1, y::l2)

let rec merge twolists = 
    match twolists with
    | ([], []) -> []
    | ([], y::ys) -> y::ys
    | (x::xs, []) -> x::xs
    | (x::xs, y::ys) -> if (x < y) then x::merge(xs, y::ys) else y::merge(x::xs, ys) 

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (l1, l2) = split l         (* Continually splits lists in half. *)
             merge(mergesort l1, mergesort l2)    (* Continually merges lists of 1 element in order. *)
