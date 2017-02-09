(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Yi Nuo Li, Id Number: 260522522 *) (* Edit this line. *)

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
    | [] -> 0.0
    | head :: tail -> head + sumlist tail

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y) :: pairlists (xs, ys)

let w_mean weights data = 
    let rec newList (weights,data) = 
        match (weights,data) with 
        |([],[]) -> []
        |([],y::ys) -> failwith "No weights"
        |(x::xs,[]) -> failwith "No data"
        |(x::xs,y::ys) -> (x*y)::newList (xs,ys)

    match weights with 
    |[]->failwith "Denominator cannot be zero"
    |x::xs -> (sumlist(newList(weights,data)))/(sumlist(weights))
  
(* Question 2. *)  (* Do not edit this line. *)
let rec memberof pair=
    match pair with
    | (a, head :: tail) when head = a -> true
    | (a, head :: tail) -> memberof(a, tail)
    | (a,[]) -> false

let rec remove(item, lst) = 
    match lst with
    | h::tl when h = item -> remove (item, tl)
    | h::tl -> h :: remove (item,tl)
    | [] -> []

(* Question 3. *)  (* Do not edit this line. *)

let findMax l =
  let maxOfTwo x y = if x < y then y else x
  let rec helper(l, m) = 
     match l with 
     | h::t -> let m = maxOfTwo h m
               helper(t, m)
     | [] -> m
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs, x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  match l with
  | [] -> []
  | _ -> let m = (findMax l)
         m::(selsort(remove(m,l)))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =    
        let commonList = []   
        match twolists with
                   |([],[]) -> []
                   |(x::xs,[])-> []
                   |([],y::ys)-> []
                   |(x::xs,y::ys) -> if memberof (x,y::ys) = true then x::remove(x,common(xs,y::ys)) else common (xs, y::ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
      match l with 
        |[] -> ([],[])
        |[x] -> ([x],[])
        |x::y::xs -> let (r,s) = split xs in (x::r,y::s)

let rec merge l =
   match l with
    | ([], ys) -> ys
    | (xs, []) -> xs
    | (x::xs,y::ys) -> if x<=y then x::merge(xs, y::ys) else y::merge(x::xs, ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | [x] -> [x] 
  | xs -> let (a,b) = split xs in merge(mergesort a, mergesort b)



