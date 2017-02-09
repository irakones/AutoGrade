(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Hui Wang, Id Number: 260620369 *) (* Edit this line. *)

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
    let rec helper (list:float list, sum:float) =
        match list with
        | [] -> 0.0
        | x::xs -> x + sumlist xs
    helper(l, 0.0)

let rec pairlists twolists = 
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y)::pairlists (xs, ys)

let w_mean weights data =
    let prod (a:float, b:float) = a*b
    let rec sumProd f plist =
        match plist with
        |[]->[]
        |x::xs->(f x)::(sumProd f xs)
    sumlist (sumProd prod (pairlists(weights, data))) / sumlist weights
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
    match snd(pair) with
    |[]->false
    |x::xs -> ((x=fst(pair)) || memberof (fst(pair), xs))

let rec remove(item, lst) =
    let rec helper (e, llist, rlist) =
        match rlist with
        |[] -> llist
        |x::xs ->
            if (e = x) then helper (e, llist, xs)
            else helper (e, llist@[x], xs)
    helper(item, [], lst)

(* Question 3. *)  (* Do not edit this line. *)

//DONE
let findMax l = 
  let rec helper(l,m) =
    match l with
    |[] -> m
    |x::xs ->
        if x<m then helper(xs, m)
        else helper (xs, x)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)

//DONE
let rec selsort l =
    let rec helper (sorted, unsorted) =
        if unsorted=[] then sorted
        else
            let e = findMax unsorted
            helper (sorted@[e], remove(e, unsorted))
    helper([],l)

(* Question 5. *)  (* Do not edit this line *)

//DONE
let rec common twolists =
    let rec helper (l1, l2, cml) =
        match l1 with
        |[] -> cml
        |x::xs ->
            if l2=[] then cml
            elif memberof(x,l2) then helper(xs, remove(x,l2), cml@[x])
            else helper (xs, l2, cml)
    helper (fst(twolists), snd(twolists), [])

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

//DONE
let rec split l =
    let rec helper (l1, l2, l) =
        match l with
        |[] -> (l1, l2)
        |[x] -> (l1, l2@[x])
        |x1::x2::xs -> helper (l1@[x1], l2@[x2], xs)
    helper ([],[],l)

let rec merge twolists =
    let rec helper (l1, l2, mgl) =
        match (l1, l2) with
        |([],[])->mgl
        |(x::xs,[])->helper(xs,l2, mgl@[x])
        |([], x::xs)->helper(l1, xs,mgl@[x])
        |(x::xs, y::ys) ->
            if x<y then helper (xs,l2, mgl@[x])
            elif x=y then helper (xs, ys, mgl@[x;y])
            else helper (l1, ys, mgl@[y])
    helper (fst(twolists), snd(twolists), [])

let rec mergesort l = 
  match l with
  | [] -> []
  | [n] -> [n] (* Without this you will go into an infinite loop. *)
  | n::ns -> merge (mergesort (fst(split l)), mergesort(snd(split l)))