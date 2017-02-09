(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Srinivas Nadella, Id Number: 260531213 *) (* Edit this line. *)

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
    |x::xs -> x + sumlist xs

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists (xs,ys)

let w_mean weights data =
  let pairWeightMean = pairlists(weights,data)
  let rec helper pairWeightMean =
    match pairWeightMean with
      | [] -> []
      | (a,b)::ax -> (a*b)::helper(ax)
  sumlist(helper(pairWeightMean))/sumlist(weights)
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
  match pair with
    | (_,[]) -> false
    | (y,x::xs) -> if y = x then true else memberof(y,xs)

let rec remove(item, lst) = 
  match (item, lst) with
    | (item, []) -> [] 
    | (item, x::xs) -> if item = x then remove(item,xs) else x::remove(item,xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
      | [] -> m
      | s::ss -> if s > m then helper(ss,s) else helper(ss,m) 
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  (*largest element is chosen,put in front,then next largest is chosen etc.*)
  (*remove duplicates*)
  match l with 
    | [] -> []
    | x::xs -> findMax(x::xs)::selsort(remove(findMax(x::xs),x::xs))

(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists = 
  match twolists with
    | ([],[]) -> []
    | [],b -> []
    | a,[] -> []
    | (a::ax,b) -> if memberof(a,b) then a::common(ax,remove(a,b)) else common(ax,remove(a,b))

(* VASU REMOVE DUPLICATES SOMEHOW *)
(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

(*have to double pattern match*)
let rec split l = 
  match l with 
    | [] -> ([],[])
    | x::xs -> 
    match xs with
      | [] -> ([x],[])
      | b::bs -> 
      let (l,r) = split(bs)
      (x::l,b::r)

let rec merge twolists = 
  match twolists with
    | ([],[]) -> []
    | ([],x) -> x
    | (y,[]) -> y
    | (a,b) ->
    match a,b with
      | (a::ax,b::bx) -> if a<b then a::merge(ax,b::bx) elif a>b then b::merge(a::ax,bx) else a::merge(ax,b::bx)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
  let (x,y) = split(n::ns)
  merge(mergesort x, mergesort y)


