(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Jean-Christophe Gagnon, Id Number: 260639897 *) (* Edit this line. *)

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

let rec sumlist (l:float list):float = 
    match l with
    | head :: tail -> head + sumlist tail
    | [] -> float 0

let rec pairlists twolists =
    match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y) :: pairlists(xs,ys)

let w_mean (weights:float list) (data:float list):float = 
    let pairedlist = pairlists (weights,data)
    let rec step pl : float=
        match pl with
        | [] -> 0.0
        | x::xs -> fst(x) * snd(x) + step xs
    let numerator = step pairedlist
    let denominator = sumlist weights
    numerator / denominator
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    | (_, []) -> false
    | (a,h :: t) when a = h -> true
    | (a,h :: t) when a <> h -> memberof(a,t)


let rec remove(item, lst) =
    match lst with
    | [] -> []
    | h :: t when h = item -> remove(item, t)
    | h :: t when h <> item -> h :: remove(item, t)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
    let rec helper(l,m) =
        match l with
        | [] -> m
        | h :: t when h > m -> helper(t,h)
        | h :: t when h <= m -> helper(t,m)

    match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    let max = findMax l
    let nomaxlist = remove(max,l)
    match nomaxlist with
    | [] -> [max]
    | h -> max :: selsort nomaxlist

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    let l1 = fst(twolists)
    let l2 = snd(twolists)
    match l1 with
    | [] -> l2
    | h :: t -> h :: remove(h,common(l2,t))

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
    let rec step (h1, h2, sl) =
        match sl with
        | [] -> (h1, h2)
        | h :: t -> step (h2, h :: h1, t)
    step ([], [], l)

let rec merge twolists =
    match twolists with
    | ([],[]) -> []
    | (h1 :: t1, []) -> h1 :: merge(t1, [])
    | ([], h2 :: t2) -> h2 :: merge(t2, [])
    | (h1 :: t1, h2 :: t2) when h1 < h2 -> h1 :: merge(t1, h2 :: t2)
    | (h1 :: t1, h2 :: t2) -> h2 :: merge(h1 :: t1, t2)

let rec mergesort l = 
    match l with
    | [] -> []
    | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
    | h :: t ->
    let splitlist = split l
    let lefthalf = mergesort(fst(splitlist))
    let righthalf = mergesort(snd(splitlist))
    merge (lefthalf, righthalf)


