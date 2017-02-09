(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Dan Bondarenko, Id Number: 260688121 *) (* Edit this line. *)

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
  | x::xs -> 
    let sum:float = sumlist xs
    x+sum;;

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> let sum = pairlists (xs,ys)
                        [(x,y)] @ sum;;           
            

let w_mean weights data = 
  let weightsum = sumlist weights
  let tuplelist = pairlists (weights,data)
  let rec summultiply tl =
    match tl with
    | [] -> 0.0
    | (a,b)::tail -> let result = summultiply tail
                     result+(a*b)
  let partsum = summultiply tuplelist
  partsum / weightsum;; 
  
 
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
  | (x,[]) -> false
  | (x,head::tail) -> 
    if (x = head) then true
    else memberof (x,tail);; 


let rec remove(item, lst) = 
  match lst with
  | [] -> []
  | x::xs -> 
    if (item = x) then remove (item,xs)
    else [x] @ remove(item,xs);;


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
    | [] -> m
    | (x::xs) -> 
      if x > m then helper (xs,x)
      else helper (xs,m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  let rec remdupl l =
    match l with
    | [] -> []
    | x::xs -> x::(remove (x, remdupl xs))
  let nodupl = remdupl l  
  match nodupl with 
  | [] -> []
  | l -> 
    let max = findMax l
    let rest = remove(max,l)
    max::(selsort rest);;

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with
  | ([],[]) -> []
  | ([],l) -> []
  | (l,[]) -> []
  | (x::xs, l) -> 
    if (memberof(x,l)) then x::(common(xs,l))
    else common(xs,l);;

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let split l =
  let rec aux l l1 l2 =
    match l with
    | [] -> (l1,l2)
    | [x] -> (x::l1,l2)
    | x::y::tail ->
      aux tail (x::l1) (y::l2)
  aux l [] [] ;;

let rec merge twolists = 
  match twolists with 
  | (x,[]) -> x
  | ([],y) -> y
  | (x::xs,y::ys) ->
    if (x<=y) then x::(merge (xs,y::ys))
    else y::(merge (x::xs,ys));;

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let (l1,l2) = split (n::ns)
    merge (mergesort l1,mergesort l2);;
