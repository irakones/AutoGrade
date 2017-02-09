(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Zhesheng Gu, Id Number: 260618448 *) (* Edit this line. *)

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
    | x :: xs -> x + sumlist xs

let rec pairlists twolists =
    match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs,[]) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y)::pairlists (xs, ys)

let w_mean weights data = 
    let pairs = pairlists (weights, data)
    let prodt (a : float, b : float) = a * b
    let value = List.map prodt pairs
    sumlist(value) / sumlist(weights)

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
    match pair with
    | (_,[]) -> false;
    | (item,x::xs) ->
        if x = item then
            true
        else
            memberof (item, xs)

let rec remove(item, lst) = 
    match lst with
    | [] -> []
    | x::xs -> if x = item then remove(item, xs) else x::remove(item,xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with 
    |[] -> m
    |x::xs ->
        if x<m then helper(xs, m)
        else helper (xs, x)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with
    | [] -> []
    | x::xs -> 
        let max = findMax l
        let newl = remove (max,l)
        max::selsort newl

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> []
    | (x::xs,[]) -> []
    | (x::xs,l2) ->
        if memberof(x,l2) then x::common(xs,l2)
        else common(xs,l2)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    match l with
    | [] -> ([], [])
    | [a] -> ([a], [])
    | x::y::rst -> 
        let (a,b) = split rst
        (x::a, y::b)

let rec merge twolists = 
    match twolists with
    | ([], ys) -> ys
    | (xs, []) -> xs
    | (x::xs, y::ys) -> 
        if x <= y then x :: merge (xs, y::ys)
        else y :: merge (x::xs, ys)

let rec mergesort l = 
    match l with
    | [] -> []
    | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
    | n::ns -> 
        let (l1,l2) = split l
        merge (mergesort l1, mergesort l2)