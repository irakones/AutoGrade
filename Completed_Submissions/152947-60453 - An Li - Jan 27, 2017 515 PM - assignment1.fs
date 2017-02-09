(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: An Li, Id Number: 260716918 *) (* Edit this line. *)

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

(* Question 1. *) (* Do not edit this line. *)

let rec sumlist l =
  match l with
    | [] -> 0.0
    | x::xs -> x + sumlist xs

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y)::pairlists(xs, ys)

let w_mean weights data = 
  (*HELPER METHOD: This unpairs the entries in the lists of pairs and returns a new list with the products of each pair.*)
  let rec unpairAndMultiply pairlist =
    match pairlist with
      | [] -> []
      | (x:float, y:float)::xs -> (x * y)::unpairAndMultiply(xs)
  sumlist (unpairAndMultiply (pairlists(weights, data))) / (sumlist data)


(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
  match pair with
  | (v, []) -> false
  | (v, x::xs) -> if v = x then true
                  else memberof(v, xs)

let rec remove(item, lst) =
  match lst with
  | [] -> []
  | x::xs -> if x = item then remove(item, xs)
             else x::remove(item, xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =
    match l with
    | x::xs -> let m = if x > m then x else m
               helper(xs, m)
    | [] -> m
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =
  match l with
  | [] -> []
  | _ -> let max = (findMax l)
         max::(selsort (remove(max, l)))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
  match twolists with
  | ([],[]) -> []
  | (x::xs,[]) -> []
  | ([],y::ys) -> []
  | (x::xs,y::ys) -> if ((memberof(x, y::ys)) && not(memberof(x, common (xs, ys)))) then x::common (xs, ys)
                     elif ((memberof(y, x::xs)) && not(memberof(y, common (xs, ys)))) then y::common (xs, ys)
                     else common(xs, ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  match l with
  | [] -> ([], [])
  | [x] -> ([x], [])
  | x::y::xs -> let (a, b) = split xs in (x::a, y::b)

let rec merge twolists = 
  match twolists with
  | ([], []) -> []
  | (x::xs, []) -> x::xs
  | ([], y::ys) -> y::ys
  | (x::xs, y::ys) -> if x < y then x::merge (xs, y::ys) 
                      else y::merge (x::xs, ys) // if x<y, append x first. Otherwise, append y first.

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (a, b) = split (n::ns)
             let aa = mergesort a
             let bb = mergesort b
             merge(aa, bb)