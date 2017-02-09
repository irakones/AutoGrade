(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Simon Paillard, Id Number: 260683399 *) (* Edit this line. *)

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
    let rec helper l a = 
        match l with
        | [] -> a
        | x::xs -> helper xs x + a
    helper l 0.0

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y) :: pairlists (xs,ys)

let w_mean weights data = 
    let w_data = pairlists (weights, data)
    let products = List.map (fun (x,y) -> x*y) w_data
    let numerator = sumlist products
    let denominator = sumlist weights
    numerator/denominator
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
    match pair with
    | (item, []) -> false
    | (item, x::xs) -> if (x=item) then true else memberof (item,xs)

let rec remove(item, lst) =
    match lst with
    | [] -> []
    | x::xs -> if (item = x) then remove (item, xs) else x::remove(item,xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
    let rec helper(l,m) =
        match l with
        | [] -> m
        | x::xs -> if (x>m) then helper(xs,x) else helper(xs,m)
    match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let findMin l = 
  let rec helper(l,m) = 
    match l with
    | [] -> m
    | x::xs -> if (x<m) then helper(xs,x) else helper(xs,m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

let rec selsort l =
  match l with
  | [] -> []
  | _  -> let min = findMin l
          let unsorted = remove(min,l)
          min :: selsort unsorted

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
    match twolists with
    | ([],_) | (_,[]) -> []
    | (x::xs,lst) -> if (memberof(x,lst)) then x::common(xs,remove(x,lst)) else common(xs,lst)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)
let rec split l =
    let rec helper list (o1,o2) = 
        match list with
        | [] -> (o1,o2)
        | [x] -> (x::o1,o2)
        | x::y::xs -> helper xs (x::o1,y::o2)
    helper l ([],[])

let rec merge twolists =
    match twolists with
    | ([],[]) -> []
    | ([],a) | (a,[]) -> a
    | (x::xs,y::ys) -> if (x < y) then x::merge(xs,y::ys) else y::merge(x::xs,ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | ([n]) -> [n] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (x,y) = split l
             let m1 = mergesort x
             let m2 = mergesort y
             merge(m1,m2)