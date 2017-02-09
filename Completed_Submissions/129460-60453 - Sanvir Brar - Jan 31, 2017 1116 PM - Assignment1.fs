(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Sanvir Brar, Id Number: 260673700 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code MUST compile and must NOT go into infinite
loops.  An assignment like that means you have not tested it.  You will get
ZERO FOR THE ENTIRE ASSIGMENT even if the problem is only with oned
question.  If you are not able to get the code to compile and run do not
submit it.  *)

(* module hw1_sol.  Use this if you want to load the file into an interactive session.*)

(* Question 1 *) (* Do not edit this line. *)

let rec sumlist l = 
    match l with
    | [] -> 0.0
    | head::tail -> head + (sumlist tail)

let rec pairlists twolists = failwith "was crashing grader"
(* @GRADER original solution 
let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> x :: (y :: (pairlists(xs,ys)))
*)
let w_mean weights data =  failwith "Not implemented"
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof (item,list) : bool when 'T : equality =
    match list with
    | hd::tl ->
        if hd = item then
            true
        else
            memberof (item, tl)
    | [] -> false


let rec remove(item, lst) =
    let rec removeInner item lst acc =
       match lst with
       | h::tl when item = h -> removeInner item tl acc
       | h::tl -> removeInner item tl (acc @ [h])
       | []    -> acc
    removeInner item lst []



(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l,m with 
    | [], m -> m
    | (l1 :: rest), m -> 
        let max1 = if l1 > m then l1 else m
        helper(rest, max1)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = failwith "Not implemented"
(* I know that I need to use findmax first in order to get the largest value, then I use
that value as the input for remove to remove all duplicates from the list. I append the max
value to a new list, in order to complete the selsort. I do this recusrively till the
sorting is completed.*)

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = failwith "Not implemented"
(* I know that I need to use memberof on both lists to make sure it is a member of both
then use remove to remove any copies of it from the two lists. This would make the common
function work.*)

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

