(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Alix Wedge, Id Number: 260354885 *) (* Edit this line. *)

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
  | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys)

let w_mean weights data =  
  let rec multiply (weights,data) = 
    match (weights,data) with
    | ([],[]) -> []
    | ([],_) -> failwith "Error -- lists are not of the same length"
    | (_,[]) -> failwith "Error -- lists are not of the same length"
    | (x::xs,y::ys) ->
      let temp = (x * y):float
      temp::(multiply (xs,ys))
  let numerator = sumlist(multiply (weights,data))
  let denominator = sumlist weights
  numerator / denominator

  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
  | (x, y::ys) -> (x = y) || memberof (x, ys)
  | (x, []) -> false

let rec remove(item, lst) = 
  match (item,lst) with
  | (x,y::ys) -> if (x = y) then remove(x,ys) else y::remove(x,ys)
  | (x,[]) -> []

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
    | [] -> m
    | x::xs -> if (x > m) then helper(xs,x) else helper(xs,m)
      
  match l with
  | [] -> failwith "Error -- empty list" 
  | (x::xs) -> helper(xs,x)


(* Question 4. *)  (* Do not edit this line. *)
let rec selsort l = 
  match l with 
  | [] -> []
  | [x] -> l
  | x::xs ->
   if (x >= (findMax xs)) then 
    let newList = remove(x,xs)
    x::selsort(newList)
   else selsort(xs@[x])


(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists = 
  match twolists with
  | ([],[]) -> []
  | (x::xs,[]) -> []
  | ([],x::xs) -> []
  | (x::xs,ys) -> 
    if memberof(x,ys) then
      let newList = remove(x,ys)
      x::common(xs,newList) 
    else common(xs,ys)


 

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
  let rec splitHelper l left right = 
    match l with
    | [] -> (left,right)
    | [x] -> (left@[x],right)
    | x::y::xs -> splitHelper xs (left@[x]) (right@[y])
  splitHelper l [] []

let rec merge twolists = 
  match twolists with
    | (xs,[])->xs
    | ([],ys)->ys
    | (x::xs, y::ys) ->
      if (x <= y) then x::merge(xs,y::ys)
      else y::merge(x::xs,ys)


let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let (l1,l2) = split l
    merge(mergesort l1, mergesort l2)
