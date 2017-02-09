(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Anita Assaad, Id Number: 260613718 *) (* Edit this line. *)

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

let rec sumlist l = // failwith "Not implemented"
    match l with 
    | [] -> 0.0
    | x :: xs -> x + sumlist xs

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) ->  (x, y) :: pairlists (xs, ys)  // failwith "Not implemented"

let w_mean weights data = // failwith "Not implemented"
  let denominator = sumlist weights
  
  denominator

  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = // failwith "Not implemented"
  let item = fst pair;
  let lstq2 = snd pair;
  match lstq2 with
  | [] -> false 
  | x :: xs -> 
      if x = item then
        true 
      else memberof(item, xs)

let rec remove(item, lst) = // failwith "Not implemented"
  match lst with
  | [] -> []
  | x :: xs when x = item -> remove(item, xs) 
  | x :: xs -> x :: remove (item, xs)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =  failwith "Not implemented"

     

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =  failwith "Not implemented"


(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = failwith "Not implemented"

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = // failwith "Not implemented"
  let rec help l sublist1 sublist2 = //helper method
    match l with 
    | [] -> (sublist1, sublist2)
    | [x] -> (x:: sublist1, sublist2) // odd number of items
    | x::n::xs -> help xs (x::sublist1) (n::sublist2) 
    // adds x to sublist1, n, which comes after x, to sublist2
  in help l [] [] 

let rec merge twolists = //  failwith "Not implemented"
  let sub1 = fst twolists
  let sub2 = snd twolists
  match (sub1, sub2) with 
  | (x, []) -> x
  | ([], n) -> n
  | (x::xs, n::ns) -> 
    if x <= n then x :: merge(xs, sub2)
    else n::merge(sub1, ns)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns ->  let (xs, ys)  = split ns // failwith "Not implemented"
              in merge(mergesort xs, mergesort ys) 

