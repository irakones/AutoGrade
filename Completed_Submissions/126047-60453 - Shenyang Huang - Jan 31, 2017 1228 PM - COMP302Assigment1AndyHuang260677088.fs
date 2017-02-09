(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Andy Huang, Id Number: 260677088 *) (* Edit this line. *)

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
    | [] -> 0.0;
    | (x::xs) -> x + sumlist xs


let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> 
    (x,y)::pairlists((xs,ys)) 

let w_mean weights data = 
  let paired = pairlists (weights, data)
  let product = [for x,y in paired do yield x*y]
  sumlist(product)/sumlist(weights)



(* Question 2. *)  (* Do not edit this line. *)
let rec memberof pair = 
  match pair with
    | (x,[]) -> false
    | (x, y::ys) -> 
      if x = y then true
      else memberof (x,ys)

(*memberof (2,[2;2;3])*)
let rec remove(item, lst) =  
  match lst with
    | [] -> []
    | (x::xs) ->
      if x= item then 
        remove(item,xs)
      else 
        x::remove(item,xs)

(*remove ("andy", ["andy";"mary";"gucci"])*)


(* Question 3. *)  (* Do not edit this line. *)
let findMax l = 
  let rec helper(l,m) = 
    match l with 
      | (y::ys) -> 
        if y>m then
        helper(ys,y)
        else 
        helper(ys,m)
      | [] -> m
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)



(* Question 4. *)  (* Do not edit this line. *)
(* will use findMax from Question3 and remove()*)
let rec selsort l = 
  match l with 
    | [] -> []
    | list -> 
      let max = findMax(list)
      let noDup = remove(max,list)
      max :: selsort(noDup)
  
(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with
  | (x::xs,[]) -> []
  | ([],y::ys) -> []
  | ([],[]) -> []
  | (x::xs, y) ->
    if memberof(x,y) then
      remove(x,y)
      x::common(xs,y)
    else 
      common(xs,y)


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  match l with
    | [] -> ([],[])
    | [x] -> ([x],[])
    | x::y::rest ->
      let rest1,rest2 = split(rest)
      (x::rest1,y::rest2) 

let rec merge twolists = 
  match twolists with
    | ([],[]) -> []
    | (x,[]) -> x
    | ([],x) -> x
    | (x::xs,y::ys) ->
      if (x<y) then
        x::y::merge(xs,ys)
      else 
        y::x::merge(xs,ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let p,q = split(l)
    merge(mergesort(p),mergesort(q))

