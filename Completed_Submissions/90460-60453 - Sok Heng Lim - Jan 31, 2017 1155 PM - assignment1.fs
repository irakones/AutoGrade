(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Sok Heng Lim, Id Number: 260581435 *) (* Edit this line. *)

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
        | h::t -> h + (sumlist t)

let rec pairlists twolists =
    match twolists with
        | ([],[]) -> []
        | ([],x::xs) -> failwith "Error -- lists are not of the same length"
        | (x::xs, []) -> failwith "Error -- lists are not of the same length"
        | (x::xs, y::ys) ->[(x,y)]@pairlists (xs,ys)


let w_mean weights data =  failwith "Not implemented"
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = failwith "Not implemented"


let rec remove(item, lst) = failwith "Not implemented"


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = failwith "Not implemented"

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = failwith "Not implemented"

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = failwith "Not implemented"

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


