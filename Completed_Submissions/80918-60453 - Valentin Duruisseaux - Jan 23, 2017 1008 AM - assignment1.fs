(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Valentin Duruisseaux, Id Number: 260563104 *) (* Edit this line. *)

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
        |[] -> 0.0
        |x::xs -> x+(sumlist xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::(pairlists(xs,ys))

let w_mean weights data =  
    let multiplication (a:float,b:float) = a*b
    let denominator = sumlist weights
    let matchedlist = pairlists (weights,data)
    let multipliedlist = List.map multiplication matchedlist
    let numerator = sumlist multipliedlist
    numerator / denominator

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with 
        |(a,[]) -> false
        |(a,(x::xs)) -> if (x = a) then true else memberof(a,xs)


let rec remove(item, lst) = 
    match lst with 
        |[] -> []
        |(x::xs) -> if (x = item) then remove(item,xs) else x::(remove(item,xs))


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =
    match l with
        |[] -> m
        |x::xs ->
            if (x>m) then helper(xs,x) else helper(xs,m)
  match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper(xs,x)
    
(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with
        |[] -> []
        |(x::xs) -> 
            let max = findMax l
            max::(selsort(remove (max, l)))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    match twolists with
        | ([],lst2) -> []
        | (lst1,[]) -> []
        | (lst1,(x::xs)) -> 
            if memberof(x,lst1) then x::common(xs,remove(x,lst1)) else common(xs,lst1)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    match l with 
        |[] -> ([],[])
        |[x] -> ([x],[])
        |(x::y::xs) -> 
            let (xs1,xs2)=(split xs) 
            ((x::xs1),(y::xs2))

let rec merge twolists = 
    match twolists with 
        |(lst1,[]) -> lst1
        |([],lst2) -> lst2
        |((x1::xs1),(x2::xs2)) -> 
            if (x1<x2) then (x1::merge (xs1,(x2::xs2))) else (x2::merge ((x1::xs1),xs2))

let rec mergesort l = 
  match l with
  | [] -> []
  | [n] -> [n] (* to avoid infinite loop. *)
  | n::ns -> 
        let (lst1,lst2) = (split l)
        let lst3 = (mergesort lst1)
        let lst4 = (mergesort lst2)
        (merge (lst3,lst4)) 
