module hw
(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Daniel Cimento, Id Number: 260679318 *) (* Edit this line. *)

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
    (*The outer function is not recursive, but I wanted to use an inner method for tail recursion and didn't want to change the function header*)
    let rec helper l n = 
        match l with
        | [] -> n
        | x::xs -> (helper xs (n + x))
    helper l 0.0

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y) :: (pairlists (xs, ys))

let w_mean weights data =
  let x = sumlist weights
  let y = pairlists (weights, data)
  let rec helper l v =
    match l with
      | [] -> v
      | (x, y) :: xs -> (helper xs (v + x*y))
  (helper y 0.0)/(x)


(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
  match pair with
    | (x, []) -> false
    | (x, y::ys) -> if (x = y) then true
                    else memberof (x, ys)


let rec remove(item, lst) =
  match lst with
    | [] -> []
    | x::xs ->  if (x = item) then remove(item, xs)
                else x :: remove(item, xs)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =
    match l with
      | [] -> m
      | x :: xs ->  if (x > m) then helper(xs,x)
                    else helper(xs, m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =
  match l with
    | [] -> []
    | x::xs ->  let z = findMax l
                z :: (selsort (remove(z, l)))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
  let rec helper(l1, l2, finallist) =
    match l1 with
    | [] ->
              finallist
    | x::xs ->
              let z = memberof(x, l2) && not (memberof(x, finallist))
              if z then helper(xs, l2, x::finallist) else helper(xs, l2, finallist)
  helper(fst twolists, snd twolists, []) 
(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
  match l with
  | [] -> ([], [])
  | [x] -> ([x], [])
  | x::y::z -> let (l1, l2) = split z in (x::l1, y::l2)


let rec merge twolists =
  match twolists with
  | ([], []) -> []
  | (x::xs, y::ys) -> if (x < y) then x :: merge (xs, y::ys)
                      else y :: merge (x::xs, ys)
  | (xs, []) -> xs
  | ([], ys) -> ys

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns ->
            let t = split l
            merge (mergesort (fst t), mergesort (snd t))


