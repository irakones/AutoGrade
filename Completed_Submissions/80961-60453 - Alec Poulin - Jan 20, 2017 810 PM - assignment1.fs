(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Alec Poulin, Id Number: 260583977 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code MUST compile and must NOT go into infinite
loops.  An assignment like that means you have not tested it.  You will get
ZERO FOR THE ENTIRE ASSIGMENT even if the problem is only with one
question.  If you are not able to get the code to compile and run do not
submit it.  *)

module hw1_sol(*.  Use this if you want to load the file into an interactive session.*)

(* Question 1 *) (* Do not edit this line. *)

let sumlist (l:float list) =
  let rec helper ((l:float list), sum:float) =
    match l with
      | [] -> sum
      | x::xs -> helper (xs, sum + x)
  helper(l, 0.)


let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y)::(pairlists (xs, ys))


let w_mean weights data =
  (* Takes a list of pairs and returns a list of the products of the elements
  in the pairs: [(a_0, b_0); (a_1, b_1); ...] -> [a_0 * b_0; a_1 * b_1; ...] *)
  let rec mult l =
    match l with
      | [] -> []
      | (a, b)::ps -> (a * b)::(mult ps)
  
  sumlist (mult (pairlists (weights, data)))/sumlist weights





(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
  match pair with
    | (a, []) -> false
    | (a, x::xs) -> if a = x then true else memberof (a, xs)


let rec remove (item, lst) =
  match lst with
    | [] -> []
    | x::xs -> if x = item then remove (item, xs) else x::remove (item, xs)





(* Question 3. *)  (* Do not edit this line. *)

let findMax l =
  let max (m, n) = if m > n then m else n
  let rec helper (l, m) =
    match l with
      | [] -> m
      | x::xs -> helper (xs, max(x, m))

  match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper (xs,x)





(* Question 4. *)  (* Do not edit this line. *)

let rec selsort l =
  if l = [] then []
  else
    let m = findMax l
    m::(selsort (remove (m, l)))





(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
  match twolists with
    | ([], l2) -> []
    | (x::xs, l2) ->
        if memberof (x, l2) then x::common (remove (x, xs), remove (x, l2))
        else common (remove (x, xs), l2)





(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

// Old version
(*let rec split l =
  match l with
  | [] -> ([], [])
  | x1::l1 ->
      match l1 with
      | [] -> ([x1], [])
      | x2::l2 ->
          let pair = split l2
          match pair with
          | (lst1, lst2) -> (x1::lst1, x2::lst2)*)

// Simpler version
let rec split l =
  match l with
  | [] -> ([], [])
  | x::[] -> ([x], [])
  | x1::x2::xs ->
      let pair = split xs
      match pair with
      | (l1, l2) -> (x1::l1, x2::l2)


let rec merge twolists =
  match twolists with
  | ([], []) -> []
  | (l1, []) -> l1
  | ([], l2) -> l2 // Actually, this line useless because of how my split function works
  | (x1::l1, x2::l2) -> if x1 < x2 then x1::(merge (l1, x2::l2)) else x2::(merge (x1::l1, l2))




let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns ->
      let pair = split l
      match pair with
      | (l1, l2) ->
          let sortedl1 = mergesort l1
          let sortedl2 = mergesort l2
          merge (sortedl1, sortedl2)



