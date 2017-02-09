(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Xiao Ming Zheng (Sindy), Id Number: 260570143 *) (* Edit this line. *)

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

let rec sumlist l = (*failwith "Not implemented"*)
    match l with
    | [] -> 0.0
    | x::xs -> x + sumlist xs

let rec pairlists twolists =
    match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists (xs,ys)(*failwith "Not implemented"*)

let w_mean weights data =  (*failwith "Not implemented"*)
    let sumWeight = sumlist weights
    let pairs = pairlists (weights,data)
    let rec productlist l = (*find the product of the pairs*)
        match l with
        | [] -> 0.0
        | (x,y)::(xs) -> x*y + productlist (xs)
    let product = productlist pairs
    product/sumWeight


(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = (*failwith "Not implemented"*)
    match pair with
    | (_,[]) -> false
    | (x,y::ys) -> 
        if (x = y) then true
        else memberof (x,ys) 

let rec remove(item, lst) = (*failwith "Not implemented"*)
    match lst with
    | [] -> []
    | x::xs ->
        if (x = item) then remove(item,xs)
        else x::remove(item,xs)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
    let rec helper(l,m) = (*failwith "Not implemented"*)
        match l with
        | [] -> m
        | x::xs ->
            if (x>m) then helper(xs,x)
            else helper(xs,m)
    match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper(xs,x)


(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = (*failwith "Not implemented"*)
    let rec helper4(l,m) = 
        match l with
        | [] -> m
        | x::xs ->
            if (x>m) then helper4(xs,x)
            else helper4(xs,m)
    let rec remove4(m, l) = 
        match l with
        | [] -> []
        | x::xs ->
            if (x = m) then remove4(m,xs)
            else x::remove4(m,xs)
    match l with
    | [] -> []
    | x::xs -> 
        let largest = helper4(xs,x)
        let updateList = remove4(largest,l)
        largest :: selsort updateList


(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = (*failwith "Not implemented"*)
    let rec memberof5 pair = 
        match pair with
        | (_,[]) -> false
        | (x,y::ys) -> 
            if (x = y) then true
            else memberof5 (x,ys)
    let rec remove5(m, l) = 
        match l with
        | [] -> []
        | x::xs ->
            if (x = m) then remove5(m,xs)
            else x::remove5(m,xs)
    match twolists with
    | ([],_) -> []
    | (_,[]) -> []
    | (x::xs,y) ->
        if memberof5 (x,y) then
            let newList = remove5 (x,y)
            x::common (xs,newList)
        else common (xs,y)


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = (*failwith "Not implemented"*) 
    match l with
    | [] -> ([],[])
    | x::[] -> ([x],[])
    | x::(y::ys) ->
        let (x1,y1) = split ys
        (x::x1,y::y1)

let rec merge twolists = (*failwith "Not implemented"*)
    let (ll,rl) = twolists
    match (ll,rl) with
    | ([],[]) -> []
    | (x,[]) -> x
    | ([],x) -> x
    | (x::xs,y::ys) ->
        if (x<=y) then x::merge (xs,rl)
        else y::merge (ll,ys)

let rec mergesort l = 
    match l with
    | [] -> []
    | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
    | n::ns -> (*failwith "Not implemented"*)
        let (left,right) = split l
        merge(mergesort left, mergesort right)