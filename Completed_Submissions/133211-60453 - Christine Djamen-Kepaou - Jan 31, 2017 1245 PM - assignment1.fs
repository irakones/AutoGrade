(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Christine Djamen, Id Number: 260681580 *) (* Edit this line. *)
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
    | n::ns -> n + sumlist ns


let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::(pairlists(xs,ys))

let w_mean weights data =  failwith "Not implemented"
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
  |(n,x::xs)->(if (n=x) then true else memberof (n,xs))
  |(n,[])-> false
  


let rec remove(item, lst) = 
  match lst with
  |[]->[]
  |x::y::xs -> (if (item=x) then remove(item,y::xs) elif (item=y) then remove(item,x::xs) else x::y::remove(item,xs)) 
  |x::xs ->  (if (item=x) then remove(item,xs) else x::remove(item,xs)) 
  |_ -> lst



(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =     
    match l with
    | [] -> m
    | (x::xs) -> (if (x>m) then helper (xs,x) else helper(xs,m))
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with
    | [] -> []
    | (x::xs)->(findMax l)::selsort (remove((findMax l),l))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
 match twolists with
  |(x::xs,y::ys)->(if (memberof(x,y::ys)) then x::common(xs,y::ys) elif (memberof(y,x::xs)) then y::common(x::xs,ys) else common(xs,ys))
  |(xs,[])-> []
  |([],ys) -> []

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)
let rec split l = 
  let rec countItems list=
    match list with
    |[]->0
    |x::xs -> 1+(countItems xs)
  let count=countItems l
  let rec helper(l1, l2, cnt)=
    match l1,cnt-1 with
    |_,0->(l1,l2)
    |x::xs,d-> (if ((cnt%2)=0) then helper (xs, x::l2, d) else helper(l1,l2,d))
  helper (l, [] ,count)

let rec merge twolists = 
  match twolists with
  |([],[])->[]
  |([],l)-> l
  |(l,[])->l
  |(x::xs,y::ys)->(if (x<y)then x::merge(xs,y::ys) else y::merge(x::xs,ys))

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> (n::[] )(* Without this you will go into an infinite loop. *)
  | n::ns -> (
    let (l1,l2)=split l 
    merge((mergesort l1),mergesort l2))


