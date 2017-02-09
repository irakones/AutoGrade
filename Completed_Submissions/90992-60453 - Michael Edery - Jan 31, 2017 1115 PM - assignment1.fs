(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Michael Edery, Id Number: 260582667 *) (* Edit this line. *)

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
    | (x::xs, y::ys) ->
      let l = pairlists(xs, ys)
      (x, y)::l
(* DON'T FULLY GET THIS. Just used his precedent of sumprod but don't see exactly how it's operating.
  Also don't get what "twolists" is. How does the compiler know that it's two lists? *)

let w_mean weights data =  
  let pairedlist = pairlists(weights, data)
  let sumweights = sumlist weights
  let rec sumproducts list = 
    match list with
      | [] -> 0.0
      | (x,y)::xys -> (x*y)+sumproducts(xys)
  let numerator = sumproducts pairedlist
  numerator/sumweights
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
    | (x,[]) -> false
    | (x,y::ys) ->
      if x=y then true
      else memberof(x,ys)

let rec remove(item, lst) =
  match lst with
    | [] -> []
    | x::xs ->
      if x=item then remove(item,xs)
      else x::remove(item,xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(lst,m) =  (* WHY IS IT OKAY TO USE 'L' IN BOTH INNER AND OUTER FUNCTIONS? *)
    match lst with
      | [] -> m
      | x::xs ->
        if x>m then helper(xs,x)
        else helper(xs,m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
  
(* let rec selsort l =
  match l with
    | [] -> []
    | [x] -> [x]
    | x::xs -> 
      if x>findMax(xs) then x::selsort(xs)
      elif x=findMax(xs) then selsort(xs)
      else selsort(xs@[x]) *)
      
let rec selsort l = 
  match l with
    | [] -> []
    | x::xs ->
      let m = findMax(x::xs)
      m::selsort(remove(m,x::xs))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
  match twolists with
  | ([],[]) -> []
  | ([],x::xs) -> []
  | (x::xs,[]) -> []
  | (x::xs,y::ys) ->
    if memberof(x,y::ys) then
      if memberof(x,xs) then common(xs,y::ys)
      else x::common(xs,y::ys)
    else common(xs,y::ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
  match l with
    | [] -> [],[]
    | [x] -> [x],[]
    | x1::x2::xs ->
      let (l1,l2) = split(xs)    
      x1::l1,x2::l2

let rec merge twolists =
  match twolists with
    | ([],[]) -> []
    | (x::xs,[]) -> x::xs
    | ([],x::xs) -> x::xs
    | (x::xs,y::ys) ->
        if x>y then y::merge(x::xs,ys)
        else x::merge(xs,y::ys)
      
let rec mergesort l = 
  match l with
    | [] -> []
    | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
    | n::ns ->
      let (l1,l2) = split(n::ns)
      merge(mergesort(l1),mergesort(l2))

      

