(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Evan Bruchet, Id Number: 260613457 *) (* Edit this line. *)

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
  let rec summer (l1, n) =
    match l1 with
    | [] -> n
    | x ::xs -> summer(xs, n + x)
  summer(l, 0.0)

let rec pairlists (l1,l2) =
  let rec helper (l3, l4, l5) =
    match (l3,l4) with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> helper(xs, ys, [x;y] ::l5)
  helper(l1,l2,[])

let w_mean weights data =  failwith "Not implemented"
  
(* Question 2. *)  (* Do not edit this line. *)


let rec memberof (n,l) = 
  match l with
  | x ::xs -> (n=x) || (memberof (n,xs))
  | [] -> false

let rec remove(n, l) = 
  match l with
  | [] -> []
  | x ::xs when (x=n) -> remove(n, xs)
  | x ::xs -> x ::(remove(n,xs))

(* Question 3. *)  (* Do not edit this line. *)

let findMax l =
  let rec helper(l,m) =
    match l with
      | [] -> m
      | (x::xs) ->
          if (m < x) then helper(xs,x) else helper(xs,m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)


(* Question 4. *)  (* Do not edit this line. *)

let rev l =
  let rec helper(l1,l2) = 
    match l1 with
    | [] -> l2
    | x::xs -> helper(xs, x::l2)
  helper(l,[])
  
let selsort l = 
  let rec helper (l1, l2) =
    match l1 with
    | [] -> rev (l2)
    | x ::xs when (x = findMax(l1)) -> helper(remove(x, xs), x ::l2)
    | x ::xs when (x < findMax(l1)) -> helper(x ::remove(findMax(xs), xs), findMax(l1) ::l2)
  (helper(l, []))

(* Question 5. *)  (* Do not edit this line. *)

let rec common (l1, l2) = 
  let rec helper(first, second) = 
    match first with
    | [] -> second
    | x ::xs when (memberof(x, l2)) -> helper(xs, x ::second)
    | x ::xs when (memberof(x, l2) = false) -> helper(xs, second)
  helper(l1, [])

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = failwith "Not implemented"

let rec merge twolists = failwith "Not implemented"

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> failwith "Not implemented"
