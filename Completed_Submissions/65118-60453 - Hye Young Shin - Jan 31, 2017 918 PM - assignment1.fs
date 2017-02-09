(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Alfred E. Neumann, Id Number: 17294104 *) (* Edit this line. *)

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
    |[] -> 0.0 //if the list is empty, then return 0 since nothing was added
    |x::xs -> x + sumlist(xs)  

let rec pairlists twolists =   //one tuple of 2 lists. 
  match twolists with
    | ([],[]) -> []     // returns back a empty list since both lists are empty.
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y) :: pairlists(xs,ys)//the part i have to implement . gives back a list of tuples. 

let w_mean weights data =  
    let weightSum = sumlist weights 
    let lst = pairlists (weights,data)
    let rec sumData lst =
        match lst with
        |[] -> 0.0 
        |(x,y)::xys -> x*y + sumData(xys)
    if (weightSum = 0.0) then failwith "Error: cannot divide by 0"
    else (sumData lst)/weightSum 

(* Question 2. *)  (* Do not edit this line. *)
let rec memberof pair = 
    match pair with
    |(x,[]) -> false
    |(x,y::ys) -> 
        if(x=y) then true
        else memberof(x,ys)
let rec remove(item, lst) = 
    if memberof (item,lst) then 
        match lst with 
        |[] -> []
        |x::xs ->
            if(x=item) then remove(item,xs)
            else x::remove(item,xs) 
    else lst 


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
    |[] -> m 
    |x::xs ->
        if(m<x) then helper(xs,x)
        else helper(xs,m) 

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *) //use findmax from previous question
  
let rec selsort l = 
    match l with
    |[]->[] 
    |x::xs -> 
        let max = findMax l
        let lst = remove(max,l)
        max::selsort(lst)

(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists = 
    match twolists with
    |([],[]) -> []
    |(x::xs,[]) -> []
    |([],y::ys) -> []
    |(x::xs,y::ys) -> 
        let xs = remove(x,xs)
        if memberof(x,y::ys) then x::common(xs,ys)
        else common(xs,ys)    

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    match l with
    |[]->([],[])
    |[x]->([x],[])
    |x::(y::rest) ->
        let (l1, l2) = split(rest)  
        (x::l1, y::l2)
let rec merge twolists = 
    match twolists with
    |([],[]) -> []
    |(x::xs, []) -> x::xs
    |([], y::ys) -> y::ys
    |(x::xs,y::ys) -> 
        if (x<y) then x::merge(xs,y::ys) 
        else y::merge(ys, x::xs)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let (l1,l2) = split l 
    merge (mergesort l1, mergesort l2)


