(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Alex Mapley, Id Number: 260605161 *) (* Edit this line. *)

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

let rec sumlist l: float = 
    match l with
    |   [] -> 0.0
    |   x::xs ->
            let s = sumlist xs
            x+s 
;;


let rec pairlists twolists = 
  match twolists  with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> 
            (x,y)::pairlists(xs,ys)
;;


let w_mean weights data = 
    let rec multiplyTuples lsTuples =
      match lsTuples with
      | [] -> []
      | (x,y)::xs -> 
        (x*y) :: multiplyTuples(xs)
    sumlist (multiplyTuples(pairlists (weights, data))) / sumlist weights
;;


(* Question 2. *)  (* Do not edit this line. *)
let rec memberof pair =
  match pair with
    | (x,[]) -> false
    | (x,y::ys) ->
      if x = y then true
      else memberof (x,ys)
;;


let rec remove(item, lst) =
  match lst with
    | [] -> lst
    | x::xs -> 
      if x = item then remove(item, xs)
      else x :: remove(item,xs)
;;


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  //Recursive Helper, does the bulk of the work
  let rec helper(l,m) = 
    match l with
    | [] -> m
    | x::xs -> 
        if m < x then helper(xs,x) 
        else helper(xs,m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)
;;


(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with
    | [] -> []
    | x::xs -> findMax(l) :: selsort(remove(findMax(l), l))
;;


(* Question 5. *)  (* Do not edit this line. *)

let rec common (list1,list2) = 
  let rec helper a list =
    //remove all elements of a kind from a list
    match list with
    | [] -> false
    | x::xs ->
      if x = a then true
      else helper a xs

  match list1 with
    | [] -> []
    | x::xs ->
      let inCommon = (helper x list2)
      if inCommon = true then x :: common((remove(x, list1)), list2)
      else common(xs,list2)
;;


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
  match l with
  | [] ->([],[]) //base case
  | [x] -> ([x],[])
  | x::y::z -> 
    let (left,right) = split z //alternating split
    (x::left, y::right)
;;


let rec merge twolists = 
  match twolists with
  | ([],[]) -> [] //base case
  | ([x],[]) -> [x]
  | ([],[y]) -> [y]
  | x::xs, y::ys ->
    if (x < y) then x :: merge(xs, y::ys)
    else y :: merge(x::xs, ys)
  | (x::z::xs, []) ->
    if (x < z) then x :: merge(z::xs, [])
    else z :: merge(x::xs, [])
  | ([], y::z::ys) ->
    if (y < z) then y :: merge([], z::ys)
    else z :: merge([], y::ys)
;;


let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns ->
    let (left,right) = split l
    merge (mergesort left, mergesort right)


