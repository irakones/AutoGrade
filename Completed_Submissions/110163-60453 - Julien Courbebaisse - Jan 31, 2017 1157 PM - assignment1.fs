(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Alfred E. Neumann, Id Number: 17294104 *) (* Edit this line. *)

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
    |[]->0.0
    |x::s->x + sumlist s

printfn "Question 1 test: %f" (sumlist [1.0;2.0;3.0;4.0])  
let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists(xs, ys) 

let w_mean weights data =
  let list = pairlists(weights,data)
  let list2 = list |> List.map(fun (x,y) -> x*y)
  let num = sumlist list2
  let denom = sumlist weights
  num/denom
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with 
  |(x,[])->false
  |(x,y::z) -> if (x=y) then true else memberof(x,z)
let rec remove(item, lst) = 
  match lst with
    |[]->[]
    |x::z -> if item <> x then x::remove(item, z) else remove(item,z)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with 
      |[]->m
      |(x::z) -> if m<x then helper(z,x) else m
  match l with
  | [] -> failwith "Error, empty list"
  | (x::z) -> helper(z,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  let max = findMax(l)
  match l with
  |[]->[]
  |(x::z)->max::selsort(remove(max,z))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
  let rec help(fst,scd,pfst,pscd)= 
    match (fst,scd) with
    |([],[])->[]
    |([],z)->[]
    |(x::z,[])->help(z,pscd,pfst,pscd)
    |(x::c,y::z)->if (x=y) then x::help(c,z,pfst,pscd) else help(fst,z,pfst,pscd)
  let rec help2 lsts=
    match lsts with
    |(x,y)->help(x,y,x,y)
    
  help2(twolists)


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = failwith "Not implemented"

let rec merge twolists = failwith "Not implemented"

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> failwith "Not implemented"



