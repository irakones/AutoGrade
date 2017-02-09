(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: James (Alex) Smith, Id Number: 260635164 *) (* Edit this line. *)

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

let rec sumlist l:float = 
  match l with 
    | [] -> 0.0
    | x::xs -> x + sumlist xs

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists (xs,ys)

let w_mean weights data = 
    let numer = sumlist (
                  pairlists(weights, data) |> 
                  List.map (fun (x, y) -> x*y)
                )
    let denom = sumlist weights
    numer/denom
  

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with 
    | (n, []) -> false
    | (n, x::xs) -> if x = n then true else memberof (n,xs)


let rec remove(item, lst) = 
  match lst with
    | [] -> []
    | x::xs -> if x = item then remove(item, xs) else x::remove(item, xs)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
      | [] -> m
      | x::xs -> if x > m then helper(xs,x) else helper(xs,m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  let max = findMax l
  let l = remove(max, l)
  match l with
  | [] -> []
  | _ -> max::selsort l


(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with
    | (x::xs, y) -> if List.contains x y then x::common(xs, y) else common (xs, y)
    | ([], y) -> []


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  match l with 
    | [] -> ([],[]) (* if the first item in l is empty, the second will also be empty *)
    | x::xs -> 
      match xs with 
        | [] -> ([x], [])
        | y::ys -> 
          let (j,k) = split ys
          (x::j, y::k)
        

let rec merge twolists = 
  match twolists with
  | (x::xs, y::ys) -> if x < y then x::merge(xs, y::ys) else y::merge(x::xs, ys)
  | (x::xs, []) -> x::merge(xs, [])
  | ([], y::ys) -> y::merge([], ys)
  | ([], []) -> []


let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n -> 
    let (x, y) = split n
    merge(mergesort x, mergesort y)
