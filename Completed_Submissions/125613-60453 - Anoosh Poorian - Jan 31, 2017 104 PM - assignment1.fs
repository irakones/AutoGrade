(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Anoosh Poorian, Id Number: 260671527 *) (* Edit this line. *)

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
    | (x::xs, y::ys) -> (x,y)::(pairlists (xs,ys))

let w_mean weights data = 
    let rec dividend combinedlist =
        match combinedlist with
            | [] -> []
            | (x,y)::tail -> (x*y)::(dividend tail)
    let combinedlist = dividend (pairlists (weights, data))
    (sumlist combinedlist)/(sumlist weights)


(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
        | (z,[]) -> false
        | (z,x::xs) -> 
            if (x = z) then true
            else memberof (z,xs)

let rec remove(item, lst) = 
    match lst with 
        | [] -> [];
        | x::xs ->
            if (x = item) then remove(item,xs)
            else x::(remove(item,xs))

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
        | [] -> m
        | x::xs -> 
            if (x > m) then helper(xs,x)
            else helper(xs,m)

  match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper(xs,x)


(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with
        | [] -> []
        | x::xs -> 
            let max = (findMax l)
            max::(selsort (remove (max,l)))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    let (m,n) = twolists
    match twolists with
        | ([],[]) -> []
        | ([],y::ys) -> []
        | (x::xs,[]) -> []
        | (x::xs,y::ys) -> 
            if (memberof(x,n)) then x::(common (remove(x,m),remove(x,n)))
            else common(remove(x,m),n)


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    let l1 = [];
    let l2 = [];
    let rec helper lst1 lst2 l =
        match l with
            | [] -> (lst1,lst2)
            | x::xs -> helper (x::lst2) lst1 xs
    helper l1 l2 l

let rec merge twolists = 
    let (m,n) = twolists
    match twolists with
        | ([],[]) -> []
        | (x::xs,[]) -> m
        | ([],y::ys) -> n
        | (x::xs,y::ys) -> 
            if (x<y) then x::(merge (xs,n))
            else y::(merge (m,ys))

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let (m,n) = split l
    merge (mergesort m, mergesort n)