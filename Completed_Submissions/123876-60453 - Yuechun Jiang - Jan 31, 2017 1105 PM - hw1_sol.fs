(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Yuechun Jiang, Id Number: 260678067 *) (* Edit this line. *)

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
    |x::xs-> x + sumlist xs;;
//let test_sumlist = sumlist [1.0;3.1;5.0;7.0;9.9];;

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists (xs, ys);;
//let test_pairlists = pairlists (['a';'b';'c';'d'],[1;2;3;4]);;
let w_mean weights data =  
    let w_sum = sumlist weights
    let helper (a,b) = a*b
    let pair_prod = List.map helper (pairlists(weights, data))
    let pair_prod_sum = sumlist pair_prod
    pair_prod_sum/w_sum;;
(*
let weights = [1.2;2.3;3.4;4.5];;
let reals = [1.0;2.0;3.0;4.0];
let test_w_mean = w_mean weights reals;;
*)

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    |(x,xs) -> 
        let a = x
        match xs with
        |[] -> false
        |x::xs -> 
            if(a=x) then true
            else memberof(a,xs);;
(*
let test_mem1 = memberof (1, [1;2;3]);;
let test_mem2 = memberof (1, [4;2;1;3]);;
let test_mem3 = memberof (1, []);;
let test_mem4 = memberof (1, [4;2;3]);;
*)
let rec remove(item, lst) =
    match lst with
    |[] -> []
    |x::xs -> 
        if(x<>item) then x::remove(item, xs)
        else remove(item, xs);;
(*
let test_remove1 = remove(2, [1;3;2;4]);;
let test_remove2 = remove(2, [1;2;3;2;4;2]);;
let test_remove3 = remove('a', []);;
*)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
    |[] -> m
    |x::xs -> 
        if(m>=x) then helper(xs,m)
        else helper(xs,x)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)
//let test_max = findMax [1;20;30;29;30;15;65;56];;

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    // removes one member in l that is a copy of m
    let rec helper (m,l) = 
        match l with
        |[] -> []
        |x::xs ->
            if(x=m) then xs
            else x::helper(m,xs)
    match l with
    |[] -> []
    |x::xs ->
        let max = findMax l
        max::selsort (helper(max,l));;
(*
let test_selsort = selsort ([1..2..25]@[1..5..21]);; 
let test_selsort2 = selsort ([2;3;6;6;3;8;8;1]);;
*)

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    match twolists with
    |([], x::xs) -> []
    |(x::xs, []) -> []
    |([], []) -> []
    |(x::xs, y::ys) ->
        if(memberof(x, y::ys)) then x::(remove(x,common (xs, y::ys)))
        else common (xs, y::ys);;
//let test_common = common ([3;4;5;5;7;2],[1;3;3;5;7;9;1]);;

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    match l with
    |[] -> ([], [])
    |[_] -> (l, [])
    |x::xs ->
        match xs with
        |[] -> ([], [])
        |y::ys ->
            let (a,b) = split ys
            (x::a, y::b);;
//let test:int list*int list = split [15..-2..1];;

let rec merge twolists = 
    match twolists with
    |([],[]) -> []
    |([],x::xs) -> x::xs
    |(x::xs, []) -> x::xs
    |(x::xs, y::ys) ->
        if (y<x) then y::merge(x::xs,ys)
        else x::merge(xs,y::ys)
//let test_merge = merge ([1;3;5],[2;4;5;6]);;
let rec mergesort l = 
  match l with
  |[] -> []
  |(n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  |n::ns ->
        let (a,b) = split l
        let l = mergesort a
        let r = mergesort b
        merge (l,r)
//let test_mergesort = mergesort[10;34;1;90;34;13;25];;

