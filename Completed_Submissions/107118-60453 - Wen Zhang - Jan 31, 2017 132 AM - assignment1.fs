(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Wen(Emily) Zhang, Id Number: 260608103 *) (* Edit this line. *)

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
    | x :: tail -> x + sumlist tail
    | [] ->  0.0

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y)::pairlists(xs,ys)

let mul (a:float,b:float) = a * b
let w_mean weights data = (sumlist (List.map mul (pairlists (weights, data))))/(sumlist weights)


  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    | (x, []) -> false
    | (x, a::list) -> if (x = a) then true else memberof (x, list)

let rec remove(item, lst) = 
    match lst with
    | x :: tail -> if (x = item) then remove(item, tail) else x::(remove(item,tail))
    | [] -> []


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =
    match l with
    | x :: tail -> if (x > m) then helper(tail, x) else helper(tail,m)
    | [] -> m

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with
    | x :: tail -> if x < (findMax tail) then findMax tail::(selsort (remove(findMax tail, l))) else x::(selsort (remove(x,tail)))
    | [] -> []



(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    match twolists with
    | (x::tail,b::lb) -> if (memberof(x,b::lb)) then x::(common (tail, (remove(x,b::lb)))) else common (tail,b::lb)
    | ([], b::lb) -> []
    | (a::la, []) -> []
    | ([], []) -> []


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
    match l with
    | [] -> ([], [])
    | [x] -> ([x],[])
    | x :: (y :: tail) ->
    let (l,r) = split(tail) in
        (x :: l, y :: r)



let rec merge twolists = 
    match twolists with
    | (x::xs, y::ys) -> if (x < y) then x :: (merge (xs, y::ys)) else y :: (merge (x::xs, ys))
    | ([x],[]) -> [x]
    | ([],[y]) -> [y]
    | (xs,[]) -> xs
    | ([],ys) -> ys


let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
  let (l,r) = split(n::ns) in
    merge (mergesort l, mergesort r)



