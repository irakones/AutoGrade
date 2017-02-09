(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Milosz Kulasek, Id Number: 260566781 *) (* Edit this line. *)

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
  | []-> 0.00
  | x::xs-> x + sumlist(xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) ->(x, y)::pairlists(xs,ys)

let w_mean weights data =
  let rec multpairs twolists =
    match twolists with
      | ([],[]) -> []
      | ([],x::xs) -> failwith "Error -- lists are not of the same length"
      | (x::xs, []) -> failwith "Error -- lists are not of the same length"
      | (x::xs,y::ys) -> x*y::multpairs(xs,ys)
  sumlist (multpairs (weights,data)) / sumlist weights

(* Question 2. *)  (* Do not edit this line. *)
let rec memberof pair = 
  match pair with
    |(x,[])-> false
    |(x,y::xs)-> if x=y then true else memberof(x,xs)
let rec remove(item, lst) = 
  match lst with
    |[]->[]
    |x::xs->if item=x then remove(item,xs) else x::remove(item,xs)

(* Question 3. *)  (* Do not edit this line. *)
let findMax l = 
  let rec helper(l,m) = 
    match l with
      |[]->m
      |x::xs-> if x<m then helper(xs,m) else helper(xs,x)
  match l with
    |[] -> failwith "Error -- empty list"
    |(x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
let rec selsort l = 
    match l with
        |[]->[]
        |x::xs-> let max= findMax(l)
                 max::selsort (remove(max,l))

(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists = 
    match twolists with
        | ([],[])->[]
        | ([],y) -> []
        | (x,[])->[]
        | (x::xs,y)-> if memberof(x,y)=true then x::common(xs,remove(x,y)) else common(xs,remove(x,y))

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let split l =
    let rec helper (l,n)=
        match l with
            |x0::[]-> if n=1 then [x0] else []
            |x0::x1::xs-> if n=1 then x0::helper(xs,1) else x1::helper(xs,2)
            |_->[]
    (helper(l,1),helper(l,2))
let rec merge twolists = 
    match twolists with
        |([],[])->[]
        |([],y)-> y
        |(x,[])-> x
        |(x::xs,y::ys)-> if x<y then x::merge(xs,y::ys) else y::merge(x::xs,ys)
let rec mergesort l = 
    match l with
      |[] -> []
      |(n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
      |n::ns -> let (x,y) = split l
                merge(mergesort x, mergesort y)
