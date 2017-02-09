(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Saul Zetler, Id Number: 260501080 *) (* Edit this line. *)

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

let rec sumlist l:float= 
  match l with
  | [] -> 0.0
  | x::xs -> x + sumlist xs

let rec pairlists twolists =
  match twolists with
  | ([],[]) -> []
  | ([],x::xs) -> failwith "Error -- lists are not of the same length"
  | (x::xs, []) -> failwith "Error -- lists are not of the same length"
  | (x::xs, y::ys) -> (x, y)::pairlists (xs, ys) 

let w_mean weights data = 
  let wtot = sumlist weights 
  let p = pairlists (weights, data)
  let rec listAccum pairedLists total = 
    match pairedLists with
    | [] -> 0.0
    | (w, d)::ps -> listAccum ps total+w*d
  (listAccum p 0.0)/wtot  
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
  | (_, []) -> false
  | (v, x::xs) -> 
    if v = x then true
    else memberof (v, xs)

let rec remove(item, lst) = 
  match lst with
  | [] -> []
  | x::xs -> 
    if x = item then remove(item, xs)
    else x::remove(item, xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper l m = 
    match l with
    | [] -> m
    | (x::xs) -> 
      if x > m then helper xs x
      else helper xs m
  match l with
  | [] -> failwith "Error -- empty list"
  | x::xs -> helper xs x

(* Question 4. *)  (* Do not edit this line. *)

let rec selsort l = 
  match l with 
  | [] -> []
  | x::xs -> 
    let m = findMax l
    let xs = remove (x, xs)
    if x = m then
      x::selsort xs
    else selsort (xs @ [x])

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  let outlist = []
  match twolists with
  | ([], []) -> []
  | (x::xs, []) -> []
  | ([], y::ys) -> []
  | (x::xs, y::ys) -> 
    if memberof(x, y::ys) then 
      if memberof(x, common(xs, y::ys)) then 
        common(xs, y::ys)
      else x::common(xs, y::ys)
    else common(xs, y::ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  match l with
  | [] -> ([], [])
  | [x] -> ([x], [])
  | x1::x2::xs -> 
    let xs1, xs2 = split xs 
    (x1::xs1, x2::xs2)

let rec merge twolists = 
  match twolists with
  | ([], []) -> []
  | (x::xs, []) -> x::xs
  | ([], y::ys) -> y::ys
  | (x::xs, y::ys) -> 
    if x < y then x::merge (xs, y::ys)
    else y::merge (x::xs, ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let (left, right) = split l
    let list1 = mergesort left
    let list2 = mergesort right
    merge (list1, list2)