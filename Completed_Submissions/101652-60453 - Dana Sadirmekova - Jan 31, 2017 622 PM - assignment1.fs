(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Dana Sadirmekova, Id Number: 260636827 *) (* Edit this line. *)

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
        let s = x+sumlist(xs)
        (x+s)
        

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys)


let w_mean weights data = failwith "Not Implemented"

(*failwith "Not implemented"*)
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
  | (var1,[]) -> false
  | (var1, x::xs) -> 
    if var1 = x then true 
    else memberof(var1,xs)
    


let rec remove(item, lst) = 
  match lst with
  | [] -> lst
  | x::xs -> 
    if item <> x then x::remove(item,xs)
    else remove(item, xs)



(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =
    match l with 
    | [] -> m
    | (x::xs) -> 
      if m > x then helper(xs,m)
      else helper(xs,x)
     
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  match l with
  | [] -> []
  | x::xs -> 
    let max = findMax(l)
    let xs = remove(max,l)
    if x = max then x::selsort(xs)
    else max::selsort(xs)


(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with 
  | (_,[]) -> []
  | ([],_) -> []
  | (x::xs, lst)-> 
    if (memberof(x,lst)) = true then x::common(xs,lst)
    else common(xs,lst)

  
(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let split l = 
  let rec helper l1 l2 l=
      match l with
      | [] -> l1, l2 
      | x::xs -> helper l2 (x::l1) xs
  helper [] [] l
    

let rec merge twolists = 
  match twolists with
  | ([],[]) -> []
  | ([],xs) -> xs
  | (ys,[]) -> ys
  | (x::xs, y::ys) -> 
      if x <= y then x::merge(xs,y::ys)
      else y::merge(x::xs,ys)


let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let(l1,l2) = split l
    merge(mergesort(l1),mergesort(l2))
   
   

