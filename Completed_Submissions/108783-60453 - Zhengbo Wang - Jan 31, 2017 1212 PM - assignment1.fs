(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Zhengbo Wang, Id Number: 260613827 *) (* Edit this line. *)

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

let rec sumlist (l: float list) = 
  match l with
    | [] -> 0.00
    | x::xs -> x + (sumlist xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::(pairlists (xs,ys))

let w_mean (weights: float list) (data: float list) = 
  let rec multiplyMap l =
    match l with 
      | [] -> []
      | (x,y)::xs -> x*y::(multiplyMap xs)
  (sumlist (multiplyMap (pairlists (weights, data))))/(sumlist weights)
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof (pair: 'a * ('a list)) = 
  match pair with
    | (x,[]) -> false
    | (x, y::ys) -> 
      if (x=y) then true 
      else (memberof (x, ys))

let rec remove((item: 'a), (lst: 'a list)) = 
  match lst with
    | [] -> []
    | x::xs -> 
      if (item = x) then remove (item, xs)
      else x::(remove (item, xs))


(* Question 3. *)  (* Do not edit this line. *)

let findMax (l: 'a list) = 
  let rec helper(l,m) = 
    match l with 
      | [] -> m
      | x::xs ->
        if (x>m) then (helper(xs,x))
        else (helper(xs,m))
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort (l: 'a list) = 
  match l with
    | [] -> []
    | x::xs -> 
      let rec remover(m,l) =
        match l with 
          | [] -> []
          | y::ys -> 
            if (y=m) then remover(m,ys)
            else y::(remover(m,ys))
      (findMax l)::(selsort (remover((findMax l), l)))

(* Question 5. *)  (* Do not edit this line. *)

let rec common (twolists: 'a list * 'a list) = 
  match twolists with
    | ([], _) -> []
    | (x::xs, []) -> []
    | (x::xs, y::ys) -> 
      if memberof (x,y::ys) then x::common(xs,ys)
      else common(xs,ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split (l: 'a list) = 
  let rec counter (lst: 'a list) =
    match lst with
      | [] -> 0
      | x::xs -> 1 + (counter xs)
  let rec findLeft n (lst: 'a list) =
    match lst with
      | [] -> []
      | x::xs -> 
        if (n=1) then [x]
        else x::(findLeft (n-1) xs)
  let rec findRight n (lst: 'a list) =
    match lst with
      | [] -> []
      | x::xs -> 
        if (n=1) then xs
        else (findRight (n-1) xs)
  (findLeft ((counter l)/2) l, findRight ((counter l)/2) l)
  

let rec merge (twolists: 'a list * 'a list) = 
  match twolists with
    | ([],[]) -> []
    | ([],y::ys) -> y::ys
    | (x::xs,[]) -> x::xs
    | (x::xs,y::ys) -> 
      if (x<y) then x::(merge (xs,y::ys))
      else y::(merge (x::xs,ys))
let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let giveLeft pair =
      match pair with
        | (x,_) -> x
    let giveRight pair =
      match pair with
        | (_,y) -> y
    (merge(mergesort(giveLeft(split l)),mergesort(giveRight(split l))))
