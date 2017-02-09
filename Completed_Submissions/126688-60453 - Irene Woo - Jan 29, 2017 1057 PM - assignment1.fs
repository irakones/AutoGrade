(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Irene Woo, Id Number: 260672948 *) (* Edit this line. *)

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

(*Compute the sum of a list of floats*)
let rec sumlist l = 
    match l with
        | [] -> 0.0
        | x::xs -> x + sumlist xs

(*Take two lists of floats the same length and produce a list of matched pairs*)
let rec pairlists twolists =
    match twolists with
        | ([],[]) -> []
        | ([],x::xs) -> failwith "Error -- lists are not of the same length"
        | (x::xs, []) -> failwith "Error -- lists are not of the same length"
        | (x::xs, y::ys) -> (x,y)::(pairlists(xs,ys))

(*Compute the weighted mean*)
let w_mean weights data = 
    let rec helper l1 = 
        match l1 with 
            | [] -> []
            | (x,y)::xs -> (x*y)::(helper(xs))
    let r1 = pairlists(weights,data)
    let r2 = helper r1
    let r3 = sumlist r2
    let r4 = sumlist weights
    r3/r4

(* Question 2. *)  (* Do not edit this line. *)

(*Tests whether an element is a member of a given list or not*)
let rec memberof pair = 
    match pair with
        | (_,[]) -> false
        | (x,y::ys) -> if (x=y) then true else memberof(x,ys)

(*Removes all copies of the element from the list*)
let rec remove(item, lst) = 
    match lst with
        | [] -> []
        | x::xs -> if (x=item) then remove(item,xs) else x::remove(item,xs)

(* Question 3. *)  (* Do not edit this line. *)

(*Find the largest value of a list of elements*)
let findMax l = 
  let rec helper(l,m) = 
    match l with
        | [] -> m
        | x::xs -> if (x>m) then helper(xs,x) else helper(xs,m)

  match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper(xs,x)


(* Question 4. *)  (* Do not edit this line. *)

(*Implements selection sort and removes duplicates.
The largest element is chosen and put in front and the rest of the list is recursively sorted 
after the largest element is removed from the rest of the list*)
let rec selsort l = 
    match l with
        | [] -> [] 
        | x::xs -> findMax(x::xs)::(selsort(remove(findMax(x::xs),x::xs)))

(* Question 5. *)  (* Do not edit this line. *)

(*Take a pair of lists and form a new list containing 
a unique copy of each element that occurs in both lists*)
let rec common twolists = 
    match twolists with
        | ([],[]) -> []
        | ([], x::xs) -> []
        | (x::xs, []) -> []
        | (x::xs,y::ys) -> if (memberof(x,y::ys)) then x::common(xs, remove(x,y::ys)) else common(xs,y::ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

(*Split l into two equal*)
let rec split l = 
    match l with
        | [] -> ([],[])
        | [x] -> ([x],[])
        | x::y::xs -> 
        (*Put x of odd index in first list, put x of even index in second list*)
        let (L,R) = split(xs) in
            (x::L,y::R)


(*Merges sorted lists*)
let rec merge twolists = 
    match twolists with
        | ([],[]) -> []
        | ([],x::xs) -> x::xs
        | (x::xs,[]) -> x::xs
        | (x::xs,y::ys) -> if (x<y) then x::merge(xs,y::ys) else y::merge(x::xs,ys)

(*Implements the whole algorithm*)
(*The given list is split into two equal lists l1 and l2
Sort the lists recursively
results are merged back to give a single sorted list*)
let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
  let (L,R) = split(n::ns)
  merge(mergesort(L), mergesort(R))
  (*split in two, sort each list recursively (call mergesort), merge them*)

