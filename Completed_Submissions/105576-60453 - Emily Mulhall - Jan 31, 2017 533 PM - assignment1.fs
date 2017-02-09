(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Emily Mulhall, Id Number: 260626549 *) (* Edit this line. *)

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
    | x::xs -> x + sumlist xs
let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists (xs, ys)

let w_mean weights data = 
  let weightSum=sumlist weights
  let dataPairs=pairlists (weights, data)
  let rec helper (sum,pairs)=
    match pairs with
    | [] ->0.0
    | x::xs ->((fst x)*(snd x))/sum + helper (sum, xs)
  helper (weightSum, dataPairs)
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
  | (x, []) -> false
  | (x, y::ys) -> if(x=y) then true else memberof(x,ys)



let rec remove(item, lst) = 
  match lst with
  | [] -> []
  | x::xs -> if(item=x) then remove(item,xs) else x::remove(item,xs)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
    | [] -> m
    | x::xs -> if(x>m) then helper(xs,x) else helper(xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  match l with
  | [] -> []
  | x::xs -> (findMax l) :: selsort (remove(findMax l, l))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  let rec helper(i,l)=
    match l with
    |[]-> false
    |z::zs -> if(i=z) then true else helper(i,zs)
  match twolists with
  | ([],[]) -> []
  | ([],x::xs) -> []
  | (x::xs, []) -> []
  | (x::xs, y::ys) -> if(helper(x,y::ys)) then x :: common(remove(x,xs),y::ys) else common(xs,y::ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  let rec helper lst l1 l2 counter=
    match lst with
    | [] -> (l1,l2)
    | x::xs -> if counter%2=0 then helper xs (x::l1) l2 (counter+1) else  helper xs l1 (x::l2) (counter+1)
  helper l [] [] 0
let rec merge twolists = 
  match twolists with
  | ([],[]) -> []
  | ([],x::xs) -> x::xs
  | (x::xs, []) -> x::xs
  | (x::xs, y::ys) -> 
    if(x<y) then x::merge (xs,y::ys) else y::merge(x::xs,ys)
let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let splitPairs =split l
    let l1 = mergesort(fst splitPairs)
    let l2 = mergesort (snd splitPairs)
    merge(l1,l2)

