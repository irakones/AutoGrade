(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Leila C.H. Fabing, Id Number: 260664609 *) (* Edit this line. *)

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
    | x::xs ->  (x:float) + sumlist(xs)

let rec pairlists (twolists: 'a list * 'b list) =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys)

let prodP (x,y) = x*y

let rec prodL l = 
    match l with 
    | [] -> []
    | x::xs -> (prodP x)::(prodL xs)

let w_mean weights data =
    let pairs = pairlists (weights, data)
    let wx = prodL pairs
    (sumlist wx) / (sumlist weights)

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof (pair: 'a * 'b list) = 
    match pair with 
    |(a,[]) -> false
    |(a, x::xs) -> 
        if a = x then true
        else memberof (a, xs)
let rec remove(item, lst) = 
    match (item, lst) with 
    |(a,[]) -> []
    |(a, x::xs) -> 
        if a = x then xs
        else x::remove(a,xs) 

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with 
    |[] -> m
    |x::xs -> 
        if x > m then helper(xs,x)
        else helper(xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)


(* Question 4. *)  (* Do not edit this line. *)
let rec selsort l =
    match l with 
    | [] -> [] 
    | x::xs -> 
        let a = findMax(l)
        let interim = remove(a,l) 
        let b = findMax(interim)
        if a = b then b::(remove(b,interim)|>selsort)
        else a::(remove(a,l)|>selsort)


(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> []
    | (x::xs,[]) -> []
    | (x::xs,y::ys) ->
        let l1 = x::xs
        let l2 = y::ys
        if memberof(x,l2) then x::common(xs,l2)
        else common(remove(x,l1),l2)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    match l with 
    |[] -> ([],[])
    |[x]-> ([x],[])
    |x::y::rest -> 
        let (L1,L2) = split(rest)
        (x::L1,y::L2)

let rec merge twolists = 
    match twolists with 
    |([],[]) -> []
    |(x::xs,[]) -> x::xs
    |([],x::xs) -> x::xs
    |(x::xs,y::ys) -> 
        if x < y then x::(merge(xs,y::ys))
        else y::(merge(x::xs,ys))

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let (a,b) = split(l)
    merge(mergesort(a),mergesort(b))



