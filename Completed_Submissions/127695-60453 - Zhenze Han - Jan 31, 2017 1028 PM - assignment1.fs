(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: zhenze han, Id Number: 260675404 *) (* Edit this line. *)

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

let rec sumlist l   = 
 
 match l with
    | [] -> 0.0
    | x::xs -> x+(sumlist xs)
sumlist [1.6;3.4];;
let rec pairlists  twolists= 
  
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) ->   (x,y)::pairlists(xs,ys) 



let rec help li  =
  match li with
  |[] -> 0.0
  |x :: xs -> let (n,p)=x in (n*p) + help xs

let w_mean weights data =  
  let pair = pairlists (weights,data)
  let de = sumlist weights
  let nu = help pair 
  match weights with
  |[] -> failwith "the given lists are empty"
  |_ -> nu / de



(* Question 2. *)  (* Do not edit this line. *)

//title youwenti
let rec memberof pair = 
  let (item,list) = pair
  match list with
  |x :: xs -> 
   if (item=x) then
    true else 
    memberof (item,xs)
  |[] -> false

 

let rec remove (n,list) = 
    match list with
    | h::tl when h = n -> tl
    | h::tl -> h :: (remove (n,tl))
    | []    -> []

(* Question 3. *)  (* Do not edit this line. *)
let findMax l = 
  let rec helper(l,m) = 
    match l with  
    |[] -> m
    |x ::xs -> if (x>m) then 
    helper(xs,x) else 
    helper(xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)


//this can find 


(* Question 4. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with  
     |[] -> m
     |x ::xs -> if (x>m) then helper(xs,x) else helper(xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)


let rec Dup list = 
  let rec helper num li = 
    match li with
    |[] -> true
    |x :: xs -> if (x=num) then false else (helper num xs)

  match list with
  |[] -> []
  |x :: xs ->
    let v = helper x xs
    if v then x :: (Dup xs) else (Dup xs)

let rec rem num lit =
  match lit with
  |[] -> []
  |x :: xs -> if num=x then (rem num xs) else x::(rem num xs)

let rec selsort l = 
  let l1 = Dup l
  match l1 with
  |[] -> []
  |_ ->
    let big = findMax l1
    big :: selsort (rem big l1)




(* Question 5. *)  (* Do not edit this line. *)



let reverse l =
    let rec reverseInner l acc =
        match l with
        | x::xs -> 
            let acc = x :: acc
            reverseInner xs acc
        | [] -> acc
    reverseInner l []


let memberof l item =
    let rec memberInner l item =
        match l with
        | x::xs -> 
            if x = item then
                true
            else 
                memberInner xs item
        | [] -> false
    memberInner l item



let isolate list =
    let rec isolateInner searchList commonlist =
        match searchList with
        | x::xs ->
            if (memberof commonlist x) then
                isolateInner xs commonlist
            else
                let commonlist = (x :: commonlist)
                isolateInner xs commonlist
        | [] -> reverse commonlist
    isolateInner list []


let common a b =
    let aUnique = isolate a
    let bUnique = isolate b
    let rec commonInner a b acc =
        match a with
        | x::xs ->
            if memberof b x then
                let acc = x :: acc
                commonInner xs b acc
            else
                commonInner xs b acc
        | [] -> reverse acc
    commonInner aUnique bUnique []
common [1;2;3;234;2;53;46;3;423;412] [3;4;5;6;1;2;234;3;5;345;345;365;634;2;31]

(* Question 6. *)   (* Do not edit this line. *)

let rec split l = 
  match l with
  |[] -> ([],[])
  |x :: []->([x],[])
  |x1 :: x2 :: xs -> 
    let (s,p) = split xs
    (x1::s,x2::p)

let rec merge twolists = 
  let (a,b) = twolists
  match (a,b) with

  |(a,[]) ->a
  |([],b) ->b
  |(x::xs,y::ys) ->
    if (x>y) then y::merge (x::xs,ys)
    else x::merge (xs,y::ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] 
  | n::ns -> 
    let (s,p) = split l
    let v = mergesort s
    let w = mergesort p
    merge (v,w)
