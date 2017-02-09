(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Dennis Liu, Id Number: 260581270 *) (* Edit this line. *)

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

(* computes the sum of a list of floats*)
let rec sumlist l =
    if (l:float list).IsEmpty then 0.0
    else
      l.Head + sumlist l.Tail

(* sumlist [1.0;2.0;3.1] *)

(* match the pairs in two lists*)
let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> if xs = [] then
                          [[x; y]]
                        else
                          [x; y]::pairlists(xs,ys)

(* pairlists ([1.0;2.2;3.5], [2.3;4.2;0.9]) *)

(* multiply pairs in two lists *)
let rec multlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x*y)::multlists(xs,ys)

let w_mean weights data =
  sumlist (multlists (weights,data)) / (sumlist weights)

(* w_mean [1.0;1.0;3.0] [1.0;1.0;2.0] *)

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
  match pair with
    | (x, []) -> false
    | (x, y::ys) -> if x = y then
                      true
                    else
                      memberof(x, ys)

(* memberof (2, [0;2]) *)

let rec remove(item, lst) =
  match (item, lst) with
    | (x, []) -> []
    | (x, y::ys) -> if x = y then
                      remove(x, ys)
                    else
                      y::remove(x, ys)

(* remove(1, [1;2;3;1]) *)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l =
  let rec helper(l,m) =
    match (l,m) with
    | (x::xs, y) ->
      if y > helper(xs, x) then
        y
      else
        helper(xs, x)
    | ([], y) ->
      y

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(*
let testList = [1;17;3;6;1;8;3;11;6;5;9]
findMax testList
*)

(* Question 4. *)  (* Do not edit this line. *)

let rec selsort l =
  match l with
  | [] -> []
  | _ -> (findMax l)::selsort(remove(findMax l, l))

(* selsort ([1..2..25]@[1..5..21]) *)

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
  match twolists with
  | ([],[]) -> []
  | ([],x::xs) -> failwith "Error -- lists are not of the same length"
  | (x::xs, []) -> failwith "Error -- lists are not of the same length"
  | (x::xs, y::ys) -> if x = y then
                        x::common(xs,ys)
                      else
                        common(xs,ys)

(* common([1.0;2.0;3.0], [1.0;2.0;3.2]) *)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
  match l with
  | [] -> ([], [])
  | [a] -> ([a], [])
  | a::b::cs -> let (A,B) = split cs
                (a::A, b::B)

let rec merge twolists =
  match twolists with
  | ([], ys) -> ys
  | (xs, []) -> xs
  | (x::xs, y::ys) -> if x < y then x::merge(xs, y::ys)
                      else y::merge(x::xs, ys)

let rec mergesort l =
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (A,B) = split l
             merge (mergesort A, mergesort B)

(* mergesort [15 .. -2 .. 1] *)
(* split [15 .. -2 .. 1] *)
