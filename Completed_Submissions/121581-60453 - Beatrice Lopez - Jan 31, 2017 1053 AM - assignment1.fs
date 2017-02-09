(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Beatrice Lopez, Id Number: 260654565 *) (* Edit this line. *)

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
    | x::xs -> x + sumlist(xs)


let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists((xs,ys))

let rec multiplypairs lst = 
  match lst with 
    | [] -> []
    | (x, w)::xs -> (x*w)::multiplypairs(xs)

let w_mean weights data = 
  let pairs = pairlists (weights, data)
  let multipliedpairs = multiplypairs pairs 
  let numer = sumlist multipliedpairs 
  let denom = sumlist weights
  numer/denom 
 
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with 
    | (v, []) -> false 
    | (v, x::xs) -> if v = x then true
                    else memberof((v, xs))   

let rec remove (item, lst) = 
  match (item, lst) with 
    | (v, []) -> []
    | (v, x::xs) -> if v = x then remove(item, xs)
                    else x::remove(item, xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =
    match (l, m) with 
      | ([], v) -> v 
      | (x::xs, v) -> if v > x then helper(xs, v)
                      else helper(xs, x)    
  match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =
  match l with 
    | [] -> []
    | x::xs -> let max = findMax(l) 
               let sblst = remove(max, l)
               max::selsort(sblst)   

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
  let (l1, l2) = twolists
  match twolists with 
    | ([], []) -> []
    | ([], x::xs) -> [] 
    | (x::xs, []) -> []
    | (x::xs, y::ys) -> if memberof(x, l2) then x::common(xs, l2)
                        else common(xs, l2)      

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
  let l1 = []
  let l2 = []
  match l with 
    | [] -> ([],[])
    | [_] -> (l, [])
    | x::y::xs -> let (f, s) = split(xs) in (x::f, y::s)

let rec merge twolists =
  let (l1, l2) = twolists
  match twolists with 
    | ([], []) -> []
    | (x::xs, []) -> l1
    | ([], x::xs) -> l2  
    | (x::xs, y::ys) -> if x < y then x::merge(xs, l2) 
                        else y::merge(l1, ys)    
let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (l, r) = split l 
             merge (mergesort l, mergesort r) 
            