(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Abby Oi Kwan Leung, Id Number: 260675748 *) (* Edit this line. *)

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
(*computes the sum of a list of floats*)
(*write code here*)
  match l with
    | [] -> 0.00
    | x::xs -> x+sumlist xs

let rec pairlists twolists =
(*takes two lists of floats the same length and produces a list of matched pairs*)
(*write code here*)
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y)::pairlists (xs, ys)

let w_mean weights data = 
(*computes the weighted mean*)
(*write code here*)
  let temp = pairlists (weights, data)
  let temporary = temp |> List.map(fun(x,y) -> (x*y))
  let top = sumlist temporary
  let bottom = sumlist weights
  let weightedMean = top/bottom
  weightedMean

  
(* Question 2. *)  (* Do not edit this line. *)

(*tests whether an element is a member of a given list*)
(*write code here*)
let rec memberof pair = 
  match pair with
    | (x, []) -> false
    | (x, y::ys) -> if x = y then true else memberof(x, ys)

let rec remove(item, lst) = 
(*takes an element and a list and removes all copies of the element
from the list. If the element is not in the list the function should return the same list. Do not use
memberof to implement remove*)
(*write code here*)
  match lst with
    | [] -> []
    | x::xs -> if item = x then remove (x, xs) else x::remove(item, xs)

(* Question 3. *)  (* Do not edit this line. *)
(* finds the largest value of a list of elements of a comparion type. *)
let findMax l =
  let rec helper(l2,m) =
  (*add code here*)
    match l2 with
      | [] -> m
      | y::ys -> if y>m then helper (ys,y) else helper (ys,m)
  (*original code*)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
(* implements selection sort and removes duplicates. *)
let rec selsort l =
  (*add code here*)
   match l with
    | [] -> []
    | l ->  findMax l :: selsort (remove (findMax l, l))

(*takes a pair of lists and forms a new list containing a unique copy of each element that occurs in both lists*)
let rec common twolists =
    match twolists with
       | ([],[]) -> []
       | ([],x::xs) -> []
       | (x::xs, []) -> []
       | (x::xs, y) -> if memberof (x,y) then x :: common (remove(x, xs), remove(x, y)) else common (remove(x, xs), remove(x, y))
       
(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)
(* the given list l is split into two equal (if the length of l is odd then one of
the \halves is one item longer than the other) lists l1 and l2. These lists are sorted recursively
and then the results are merged back to give a single sorted list *)
let rec split l =
(*produces a pair of lists*)
  let rec helper l l1 l2 =
    match l with
      | [] -> (l1, l2)
      | x::[] -> (x::l1, l)
      | x::x'::xs -> helper (xs) (x::l1) (x'::l2)
  helper l [] []
let rec merge twolists = 
(*merges sorted lists*)
  match twolists with
    | ([],[]) -> []
    | (x,[]) -> x
    | ([],y) -> y
    | (x::xs,y::ys) -> if x<y then x::merge(xs, y::ys) else y::merge(x::xs, ys)
let rec mergesort l = 
(*implements the overall algorithm*)
  match l with
    | [] -> []
    | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
    | n::ns ->  let (x,y) = split (n::ns) in merge(mergesort x, mergesort y)