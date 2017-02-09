
(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Jackson Leli Li, Id Number: 260681801 *) (* Edit this line. *)

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
    |x::xs -> x + sumlist xs

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y)::pairlists(xs, ys)

let w_mean weights data =
  let pairedlist = pairlists(weights, data)
  sumlist (List.map(fun (n,m)->n*m) pairedlist)/sumlist (weights)


  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
  match pair with
  | (n, []) -> false
  | (n, x::xs) -> if (n = x) then true else memberof (n,xs)


let rec remove(item, lst) = 
  match lst with
  | [] -> []
  | x::xs -> if (item = x) then remove (item, xs) else x::remove(item, xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =
    match l with
    | [] -> m 
    | x::xs -> if (x > m) then helper(xs, x) else helper(xs, m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs, x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =
  match l with
  | [] -> []
  | l -> 
    let max = findMax l
    max :: selsort (remove (max, l))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with
  | ([], []) -> []
  | (list, []) -> []
  | ([], list2) -> []
  | (x::xs, list2) -> if memberof (x, list2) then x :: common (xs, remove (x, list2)) else  common (remove (x, xs), list2)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  match l with
  | [] -> ([], [])
  | [x] -> ( [x], [])
  | x::x2::xs -> 
    let (half1, half2) = split (xs)
    (x::half1, x2::half2)
    
let rec merge twolists =
  match twolists with
  | ([], []) -> []
  | (l1, []) -> l1
  | ([], l2) -> l2
  | (x::xs, x2::xs2) -> if (x < x2) then x::merge (xs, x2::xs2) else x2::merge (x::xs, xs2)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let (l1, l2) = split l
    merge (mergesort(l1),mergesort(l2))


