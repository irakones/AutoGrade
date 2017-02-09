(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Veronique Demers, Id Number: 260608215 *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code MUST compile and must NOT go into infinite
loops.  An assignment like that means you have not tested it.  You will get
ZERO FOR THE ENTIRE ASSIGMENT even if the problem is only with one
question.  If you are not able to get the code to compile and run do not
submit it.  *)

(* Question 1 *) (* Do not edit this line. *)

let rec sumlist l= 
(*sums the elements of a list*)
    match l with
    |[] -> 0.0
    |x::xs-> x+sumlist xs

let rec pairlists twolists=
(*pairs two lists together*)
    match twolists with
        | ([],[])-> []
        | ([],x::xs) -> failwith "Error -- lists are not of the same length"
        | (x::xs, []) -> failwith "Error -- lists are not of the same length"
        | (x::xs, y::ys) -> (x,y)::pairlists (xs,ys)

let w_mean (weights:float list) (data:float list) =
    if sumlist weights = 0.0 then failwith "Error -- division by zero not allowed"
    else 
        let pairedlist = pairlists (weights, data) |> List.map (fun (x,y) -> x * y)
        (sumlist pairedlist)/(sumlist weights)

  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof (findx:'a, list:'a list)= 
(*outputs if an element is present in the list*)
    match (findx, list) with
        |(_,[])->false
        |(_,x::xs) -> 
            if x=findx then true
            else memberof(findx,xs)


let rec remove(item:'a, lst:'a list) = 
(*removes a specific element from a list*)
    match (item, lst) with
    |(item,[])-> []
    |(item, x::xs) -> 
        if x=item then remove(item, xs)
        else x::remove(item,xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l:'a= 
(*find the max element in a list*)
  let rec helper(l:'a list,m:'a) = 
  (*compares past head element with x*)
    match (l,m) with
    |([],m) -> m
    |(x::xs, m)-> 
        if x<=m then helper(xs,m)
        else helper(xs,x)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l:'a= 
(*sort from greatest to smallest in O(n^2) time*)
    match l with
    |[] -> []
    |x::xs ->
        if xs=[] then [x]
        elif x>=findMax(xs) then x::selsort (remove(x,xs))
        else selsort(xs@[x])


(* Question 5. *)  (* Do not edit this line. *)

let rec common (l1:'a, l2:'a) = 
(*from two lists, give a list of common elements*)
    match (l1, l2) with
    |([],[]) -> []
    |([],x::xs) -> []
    |(x::xs, []) -> []
    |(x::xs, y::ys) -> 
        if memberof (x, l2) then common (xs, y::ys)@[x]
        else common (xs, y::ys)
            
        


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
(*split a list into two equal length list if even and length/2 - length/2+1 is odd*)
    match l with
    |[]-> ([],[])
    |[x]->([x],[])
    |x::x'::xs -> 
        let (l1,l2) = split xs
        ((x::l1), (x':: l2))

let rec merge (l1,l2) = 
(*merge to lists into one and put them in order from greatest to smallest*)
    match (l1,l2) with
    |([],[])-> []
    |([],xs) -> xs
    |(xs,[]) -> xs
    |(x::xs, y::ys) -> 
        if x>y then x::merge(xs,y::ys)
        else y::merge(x::xs, ys)

let rec mergesort l = 
(*split, merge and sort a list in O(n log n) time*)
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> merge (split l)

