(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Douglas Prisnie, Id Number: 260503691 *) (* Edit this line. *)

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
    | x::xs -> x + sumlist xs

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y) :: pairlists (xs, ys) 

let w_mean weights data =
  let den = sumlist weights
  let pairlist = pairlists (weights, data)
  let rec helper lst =
    match lst with
    | [] -> 0.0
    | (w, d) :: wsds -> (w*d/den) + (helper wsds)
  helper pairlist
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
  | (_, []) -> false
  | (element, x::xs) -> if (element = x) then true else memberof (element, xs)

let rec remove(item, lst) = 
  match lst with
  | [] -> []
  | x::xs -> if (item = x) then remove(item, xs) else x::remove(item, xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = // l is the list minus the first element, m is the first element 
    match l with
    | [] -> m
    | x::xs -> if (m>x) then helper(xs,m) else helper(xs,x)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  let max = findMax l
  let rec remDup lst m =
    if (memberof (m, lst)) then remDup (remove(m, lst)) m else lst
  let newList = remDup l max
  match newList with
  | [] -> [max]
  | [x] -> [x]
  | xs -> max :: selsort xs

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
  match twolists with
  | ([],_) -> []
  | (x::xs, ys) -> if (memberof (x,ys)) then (x::common (xs,ys)) else common (xs,ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
  let rec helper l l1 l2 =
    match l with
        | [] -> (l1,l2)
        | [x] -> (l1@[x],l2)
        | x::y::rem -> helper rem (l1@[x]) (l2@[y])
  helper l [] []

let rec merge twolists =
    let rec helper l1 l2 merged = 
        match (l1,l2) with 
        | ([],[]) -> merged
        | ([], x::xs) | (x::xs, []) -> helper [] xs (merged@[x])
        | (x::xs, y::ys) -> if x<y then helper xs l2 (merged@[x]) else helper l1 ys (merged@[y])
    match twolists with
    | (l1,l2) -> helper l1 l2 []

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let (l1,l2) = split l
    merge (mergesort l1, mergesort l2)