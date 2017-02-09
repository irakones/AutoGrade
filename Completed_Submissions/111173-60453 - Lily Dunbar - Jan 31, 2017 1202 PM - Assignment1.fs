(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Lily Dunbar, Id Number: 260624831*) (* Edit this line. *)

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
    |[] -> 0.0
    |x::xs -> x+sumlist xs

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys)

let w_mean weights data =
  let paired = pairlists (weights,data)
  let top:float = List.fold(fun acc (x,y) -> acc + (x*y)) 0.0 paired
  top/sumlist(weights)

(* Question 2. *)  (* Do not edit this line. *)
let rec memberof pair =
  let (x,y) = pair 
  let n = x
  let list = y

  let test (a, b) = 
    if a=b then true else false 

  match list with 
    |[] -> false
    |x::xs -> if (test (x,n)) then true else (memberof(n,xs))
let rec remove(item, lst) = 
    match lst with
    |[] -> []
    |x::xs ->
    if (item = x) then remove(item,xs)
    else x :: remove(item, xs)

(* Question 3. *)  (* Do not edit this line. *)
let findMax l = 

  let rec helper(l,m) = 
    let list = l
    let max = m
    
    match list with
    |[] -> max
    | x::xs when x < max -> helper(xs, max)
    | x::xs when x > max -> helper(xs, x)
    | x::xs when x = max -> helper(xs, max)
    | _ -> max 

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
let rec duplicates l =
  match l with 
  |[] -> []
  |x::xs -> 
  if memberof(x,xs)
      then x :: duplicates(remove(x,xs))
  else x :: duplicates(xs)
let rec selsort l = 
  let s = duplicates(l)
  match s with 
  |[] -> []
  |x::xs -> (findMax(s))::(selsort(remove(findMax(s),s)))

(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists = 
  match twolists with
  |([],l2) -> []
  |(x::xs, l2) ->
  if memberof(x,l2)
    then x :: common((remove(x,xs),l2))
  else common(remove(x,xs),l2)  

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)
let rec split l =
  let rec helper l acc1 acc2 =
    match l with 
    |[] -> (acc1, acc2)
    |[x] -> (x::acc1, acc2)
    |x::y::xs -> helper xs (acc1@[x]) (acc2@[y])
  helper l [] [] 
let rec merge twolists = 
  let rec helper l acc =
    match l with 
    |([],[]) -> acc
    |([], x::xs) | (x::xs, []) -> helper ([], xs) (acc@[x])
    |(x::xs, y::ys) -> 
      if x < y then helper(xs, y::ys) (acc@[x])
      else helper (x::xs, ys) (acc@[y])
  helper twolists []
let rec mergesort l = 
  match l with
  |[] -> []
  |(x::[]) -> x::[] (* Without this you will go into an infinite loop. *)
  |x::xs -> 
      let (l1,l2) = split l
      merge(mergesort l1, mergesort l2)
