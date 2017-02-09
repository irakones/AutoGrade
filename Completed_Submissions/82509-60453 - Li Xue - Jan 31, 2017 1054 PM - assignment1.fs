// module hw1

(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Li Xue, Id Number: 260568590 *) (* Edit this line. *)

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

let rec sumlist l1 = 
    match l1 with
    | [] -> 0.0
    | (x::xs) -> x+sumlist(xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists (xs, ys)

let w_mean (weights: float list) (data: float list) = 
    let listOfPairs = pairlists (weights, data)
    let listOfProducts = List.map (fun (x, w) -> x*w) listOfPairs
    let numerator = sumlist listOfProducts
    let denominator = sumlist weights
    numerator/denominator

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof (n, l) = 
    match l with
    | x::xs -> (n = x) || (memberof(n,xs)) 
    | [] -> false

let rec remove(item, lst) =
    match lst with
    | [] -> []
    | x::xs -> if x = item then remove(item, xs) else x::(remove(item, xs))

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper (subl, m) = 
    match subl with
    | [] -> m
    | x::xs -> if x < m then helper (xs, m) else helper (xs, x)
  match l with
  | [] -> failwith "List is empty; impossible to have a max"
  | x::xs -> helper (xs, x)

(* Question 4. *)  (* Do not edit this line. *)
let rec selsort l = 
    match l with
    | [] -> []
    | _ -> 
        let num = findMax(l)
        num::selsort(remove(num,l))

(* Question 5. *)  (* Do not edit this line. *)

let rec common (l1, l2) = 
    match l1 with
    | [] -> []
    | x::xs -> 
        let dup = common(remove(x,xs),l2) 
        if memberof(x,l2) then x::dup
        else dup

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    match l with
    | [] -> [], []
    | x::xs ->
        let l1, l2 = split xs
        x::l2, l1

let rec merge (l1, l2) = 
    match l1, l2 with
    | [], [] -> []
    | [], l
    | l, [] -> l
    | x1::r1, x2::r2 ->
        if x1 < x2 then
            x1::merge(r1, x2::r2)
        else
            x2::merge(x1::r1, r2)
    
let rec mergesort l = 
  match l with
  | [] -> []
  | [n] -> [n] 
  | _ -> 
    let l1, l2 = split l
    let s1 = mergesort l1
    let s2 = mergesort l2
    merge (s1,s2)

    

