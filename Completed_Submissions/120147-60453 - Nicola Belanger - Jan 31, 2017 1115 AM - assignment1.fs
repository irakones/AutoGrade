(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Nicola Belanger, Id Number: 260666492 *) (* Edit this line. *)

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

module hw1_sol

let rec sumlist l = 
  match l with
    | [] -> 0.0
    | x::xs -> x + (sumlist xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y)::(pairlists (xs, ys)) 

let w_mean weights data =
  let rec helper l =
      match l with
        | [] -> []
        | x::xs -> let (w, d) = x
                   (w*d)::(helper xs)
  (sumlist (helper (pairlists (weights, data))))/(sumlist weights)

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
  let (v, lst) = pair
  match lst with
    | [] -> false
    | x::xs -> if (v = x) then true
               else
                   memberof (v, xs)

let rec remove(item, lst) =
  match lst with
    | [] -> []
    | x::xs -> if (x = item) then remove(item, xs)
               else
                   x::remove(item, xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =
    match l with
      | [] -> m
      | x::xs -> if x > m then helper(xs, x)
                 else
                     helper(xs, m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)

let rec selsort l =
  match l with
    | [] -> []
    | x::xs -> let x = findMax l
               x::(selsort (remove((findMax l), l))) 
    
(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
  let rec find(item, l) =
    match l with
      | [] -> false
      | x::xs -> if (x = item) then true
                 else
                     find(item, xs)
  let (l1, l2) = twolists
  match l1 with
    | [] -> []
    | x::xs -> if find(x, l2) then x::(common ((remove(x, xs)), l2))
               else
                   common((remove(x, xs)), l2)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
  let rec helper threelists =
    let (l1, l2, l3) = threelists
    match l3 with
      | [] -> (l1, l2)
      | x::xs -> helper(l2, x::l1, xs)
  helper([], [], l)
  
let rec merge twolists =
  match twolists with
    | ([], []) -> []
    | (x::xs, []) -> x::(merge (xs, []))
    | ([], x::xs) -> x::(merge ([], xs))
    | (x::xs, y::ys) -> if x < y then x::(merge (xs, y::ys))
                        else
                            y::(merge (x::xs, ys))
  
let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (l1, l2) = split l
             merge ((mergesort l1), (mergesort l2))
