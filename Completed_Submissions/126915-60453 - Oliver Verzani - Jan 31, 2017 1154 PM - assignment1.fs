(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Oliver Verzani, Id Number: 260670509 *)

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

let rec sumlist (l: float list)  =
  match l with
  | [] -> float(0)
  | x::xs -> float(x) + sumlist xs

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists(xs, ys)

let w_mean weights data = failwith "Not Implemented"
                
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
  | (_, []) -> false
  | (a, x::xs) -> if a = x then true else memberof(a, xs)


let rec remove(item, lst) =
  match lst with
  | [] -> lst
  | (x::xs) -> if item = x then remove(item, xs) else x::remove(item,xs)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
    | (x::xs) -> let m = if x > m then x else m; 
                 helper(xs, m)
    | [] -> m
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  match l with
  | [] -> []
  | x::xs -> let max = findMax(l)
             let l = remove(max,l)
             max::selsort(l)

(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists = 
  match twolists with
  | [],[] -> []
  | x::xs, [] -> []
  | [], y::ys -> []
  | (x::xs, y::ys) -> if x = y then x::common(xs,ys) else
                                                          let listt = common([x],ys)
                                                          match listt with
                                                          | [] -> common(xs,y::ys)
                                                          | a::an -> a::common(xs, y::ys)
  
  
 /// if x = y then x::common(xs,ys) else common([x],ys) common(xs,ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  match l with 
  | [] -> ([],[])
  | x::xs -> ([x],xs)

let rec merge twolists =
  match twolists with
  | [],[] -> []
  | x::xs, [] -> x::xs
  | [], y::ys -> y::ys
  | x::xs, y::ys -> if x > y then y::merge(x::xs, ys) else x::merge(xs, y::ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | ([n]) -> [n] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (a, b) = split(l)
             let a1 = mergesort(a)
             let b1 = mergesort(b)
             merge(a1, b1)