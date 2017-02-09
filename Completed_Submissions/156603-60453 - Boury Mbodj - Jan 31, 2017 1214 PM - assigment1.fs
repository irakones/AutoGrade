(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Boury Mbodj, Id Number: 260521261*) (* Edit this line. *)

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
        | hd :: tl ->  hd + sumlist (tl)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::(pairlists(xs,ys))

let rec product list =
  match list with
    | [] -> []
    | (x,y)::xs -> (x*y)::product (xs)

let w_mean weights data = 
    let denom = sumlist(weights)
    let num= sumlist(product (pairlists((weights,data))))
    num/denom
  
 (* Question 2. *)  (* Do not edit this line. *)
 
let rec memberof pair = 
  match pair with
   |(x, []) -> false
   | (x,y::ys) -> (x = y) || (memberof ( x, ys))

let rec remove(item,lst) = 
   match lst with 
    | ( h::tl)when h = item -> (remove (item,tl)) 
    | (h::tl)-> h :: (remove (item,tl)) 
    | ([])-> []

(* Question 3. *)  (* Do not edit this line. *)

let max x y = 
    if x > y then x
    else y

let findMax l = 
  let rec helper(l,m) = 
   match l with
     | [] -> m
     | x::tl ->  (helper (tl, max x m))

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

 (* Question 4. *)  (* Do not edit this line. *) 
let rec selsort l = 
  match l with
  | [] -> [] 
  | x::xs -> let max = findMax (x::xs)
             let lst= (remove (max,x::xs))
             max ::( selsort lst)

(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists = 
   let rec helper tlist=
      match tlist with
       | ([],[]) -> []
       | ([],x::xs) -> []
       | (x::xs, []) -> []
       | (x::xs, y::ys) -> if memberof (x,y::ys) then 
                              let lst = remove(x,y::ys)
                              x::helper(xs,lst)
                            else
                               helper(xs,y::ys)
   match twolists with
   | ([],[]) -> []
   | ([],x::xs) -> []
   | (x::xs, []) -> []
   | (x::xs, y::ys) ->  helper (twolists)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)
let rec split l = 
   match l with 
   |[]-> ([],[])
   |x::[]-> (x::[],[])
   |x::y::[]-> (x::[],y::[])
   |x::y::zs-> let (l1,l2)= split zs
               (x::l1,y::l2)

let rec merge twolists = 
    match twolists with
    |([],[])-> []
    |(x::[],[])-> x::[]
    |([],x::[])-> x::[]
    | (x::xs, y::ys) -> if x < y then x :: merge (xs, y::ys)
                        else y :: merge (x::xs, ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (l1, l2) = split (n::ns)
             merge (mergesort l1, mergesort l2)

