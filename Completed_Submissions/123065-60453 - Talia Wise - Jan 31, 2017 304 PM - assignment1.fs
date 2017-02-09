(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Talia Wise, Id Number: 260659717 *) (* Edit this line. *)

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
    | (x::xs) -> x + (sumlist xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::( pairlists( xs, ys ))

let w_mean weights data =  
  let w = sumlist weights
  let pairs = pairlists(weights, data)
  let rec helper pairs =
    match pairs with
      |[] -> 0.0
      |((x,y)::xs) -> ((x*y)/w) + (helper(xs))
  helper(pairs)

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
  |(l,[]) -> false
  |(l,(x::xs)) ->
    if (l=x) then true
    else memberof(l,xs)

let rec remove(item, lst) = 
  match lst with
  |[] -> []
  |(x::xs) -> 
    if item=x then remove(item,xs)
    else x::remove(item,xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
      |[] -> m
      |(x::xs) -> 
        if (m>x) then helper(xs,m)
        else helper(xs,x)
    
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  match l with
  |[] -> []
  |(x::xs) -> 
    let m = (findMax l)
    m::selsort(remove(m,l))
  
(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with 
  |([],[]) -> []
  |(_,[]) -> []
  |([],_) -> []
  |(x::xs,y::ys) ->
    let lsts = selsort(x::xs),selsort(y::ys)
    match lsts with
    |(x::xs, y::ys) -> 
      if (x>y) then common(xs,y::ys)
      elif(y>x) then common(x::xs,ys)
      else x::common(xs,ys)
           
(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  match l with
  |[] -> failwith "Error -- empty list"
  |[x] -> failwith "Error -- one element"
  |[x;y] -> ([x],[y])
  |[x;y;z] -> ([x;y],[z])
  |(x::s::xs) -> 
    let (lx, ls) = split xs 
    (x::lx,s::ls)
      
let rec merge twolists = 
  match twolists with 
  | ([],x) -> x 
  | (x,[]) -> x
  | (x::xs,y::ys) -> 
    if (x<y) then x::(merge(xs,y::ys))
    else y::(merge(x::xs,ys))

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (l1,l2) = split (n::ns)
             merge (mergesort l1, mergesort l2)
  
    
