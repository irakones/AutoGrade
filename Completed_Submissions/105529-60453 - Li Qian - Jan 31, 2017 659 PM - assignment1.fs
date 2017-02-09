(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Carl Qian/ Li Qian, Id Number: 260617009 *) (* Edit this line. *)

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
    | [] ->0.0
    | x::xs -> x+sumlist(xs) 

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys)

let w_mean weights data =  
  let rec productsumwithweights pairlist =
    match pairlist with 
      | [] -> 0.0
      | (x,y)::xs -> x*y + productsumwithweights(xs)
  (productsumwithweights(pairlists( weights,data ))) / sumlist(weights)



(* Question 2. *)  (* Do not edit this line. *)
let rec memberof pair = 
  match pair with
    | (x,[]) -> false 
    | (x,y::ys) -> 
      if x=y then 
        true 
      else 
        memberof(x,ys) 
let rec remove(item, lst) =
  match lst with
    | [] -> []
    | (x::xs) -> 
      if x=item then
        remove(item,xs)
      else 
        x::remove(item,xs) 
    

(* Question 3. *)  (* Do not edit this line. *)
let findMax l = 
  let rec helper(l,m) = 
    match l with
      | [] -> m
      | (x::xs) ->
        if x>m then 
          helper(xs,x)
        else 
          helper(xs,m)
  match l with 
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper(l,x)

(* Question 4. *)  (* Do not edit this line. *)
let rec selsort l = 
  let rec helpersort l2 =
    match l2 with 
      | [] -> []
      | (x::xs) -> findMax(l2)::helpersort(remove(findMax(l2), l2))
  if l=[] then
    failwith "Error -- Empty list"
  else 
    helpersort(l)

(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists = 
  let rec checkcommon(item,list,result) =
    match list with 
      | [] -> result
      | (x::xs) -> 
        if x=item then 
          checkcommon(item,xs,true)
        else 
          checkcommon(item,xs,result)
  let rec checkcommonall(list1,list2,result) =
    match list1 with 
      | [] -> result
      | (x::xs) -> 
        if checkcommon(x,list2,false) then
          checkcommonall(xs,list2,x::result)
        else 
          checkcommonall(xs,list2,result)
  let rec removeduplicate(list) =
    match list with 
      | [] -> list
      | (x::xs) ->
        if checkcommon(x,xs,false) then
          removeduplicate(xs)
        else 
          x::removeduplicate(xs)
  match twolists with 
    | ([],[]) -> []
    | ([],x) -> []
    | (x, []) -> []
    | (x,y) -> removeduplicate(checkcommonall(x,y,[]))

  
(* Question 6. *)   (* Do not edit this line. *)
(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)
let rec split l = 
  let rec count l = 
    match l with  
      | [] -> 0 
      | x::xs -> 1 + count xs 
  let rec splithelper(l,counter,l1,l2) =
    match l with 
      | [] -> (l1,l2)
      | (x::xs) ->
        if counter<= count(l)/2 then
          splithelper(xs,counter+1,x::l1,l2)
        else 
          splithelper(xs,counter+1,l1,x::l2)
  splithelper(l,0,[],[])
 
let rec merge twolists = 
  match twolists with 
    | ([],[]) -> []
    | ([],x) -> x
    | (x, []) -> x
    | (x::xs, y::ys) -> 
      if x<y then 
        x::merge(xs,y::ys) 
      else
        y::merge(x::xs,ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let (l1,l2) = split l
    merge( mergesort l1, mergesort l2 )

  
    
   
    