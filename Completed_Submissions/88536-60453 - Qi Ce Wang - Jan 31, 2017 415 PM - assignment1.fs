(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Henry (Qi Ce) Wang, Id Number: 260580986 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code MUST compile and must NOT go into infinite
loops.  An assignment like that means you have not tested it.  You will get
ZERO FOR THE ENTIRE ASSIGMENT even if the problem is only with one
question.  If you are not able to get the code to compile and run do not
submit it.  *)

(*module hw1_sol  *)
(*Use this if you want to load the file into an interactive session.*)
(* Question 1 *) (* Do not edit this line. *)
let rec sumlist l = 
  match l with
  |[]-> 0.0
  |x::xs -> x + sumlist(xs)

let rec pairlists twolists =
  match twolists with
  | ([],[]) -> []
  | ([],x::xs) -> failwith "Error -- lists are not of the same length"
  | (x::xs, []) -> failwith "Error -- lists are not of the same length"
  | (x::xs, y::ys) -> (x,y)::(pairlists (xs, ys))

let w_mean weights data =
  let sumOfWeight = sumlist weights
  let pairedUpList = pairlists(weights, data)
  let rec helper listOfTuples = 
    match listOfTuples with
    |[] -> 0.0
    |(x,y)::xys -> x*y + helper(xys)
  if (sumOfWeight = 0.0) then failwith "Error -- sum of weights equals 0 and thus involves division by 0"
  else (helper pairedUpList)/sumOfWeight


(* Question 2. *)  (* Do not edit this line. *)
let rec memberof pair = 
  match pair with
  |(x, []) -> false
  |(x, y::ys) ->
    if (x = y) then true
    else memberof(x, ys)
let rec remove(item, lst) = 
  match lst with
  |[] -> []
  |x::xs ->
    if (item = x) then remove(item, xs)
    else x::remove(item, xs)

(* Question 3. *)  (* Do not edit this line. *)
let findMax l = 
  let rec helper(l,m) = 
   match l with
    |[] -> m
    |x::xs ->
    if (m > x) then helper(xs, m)
    else helper(xs, x)
  match l with
  |[] -> failwith "Error -- empty list"
  |(x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
let rec selsort l =
  match l with
  |[] -> []
  |x::xs -> 
    let m = findMax(l)
    m::selsort(remove(m,l))

(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists =
  match twolists with
  |([],[]) -> []
  |([], y::ys) -> []
  |(x::xs, []) -> []
  |((x::xs),(y::ys)) ->
    if (memberof (x, (y::ys))) then x::common(remove(x,xs), (y::ys))
    else common(xs, (y::ys))

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)
let rec split l =
  match l with
  |[] -> ([],[])
  |[x] -> ([x],[])
  |x::(y::rs) ->
    let (l1, l2) = split rs
    (x::l1, y::l2)

let rec merge twolists =
  match twolists with
  |([],[]) -> []
  |(x::xs, []) -> (x::xs)
  |([], y::ys) -> (y::ys)
  |(x::xs, y::ys) ->
    if(x<y) then x::(merge(xs,y::ys))
    else y::(merge(ys, x::xs))

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns ->
    let (l1, l2) = split l
    merge(mergesort l1, mergesort l2)