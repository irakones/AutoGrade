(* Assignment 1 *) (* Do not edit this line. *) 
(* Student name: Sihyeon Kim, Id Number: 260680863 *) (* Edit this line. *) 
(* In the template below we have written the names of the functions that you need to define. You MUST use these names. If you introduce auxiliary functions you can name them as you like, 
except that your names should not clash with any of the names we are using. We have also shown the types that you should have. Your code MUST compile and must NOT go into infinite loops. 
An assignment like that means you have not tested it. You will get ZERO FOR THE ENTIRE ASSIGMENT even if the problem is only with one question. 
If you are not able to get the code to compile and run do not submit it. *) 
(* module hw1_sol. Use this if you want to load the file into an interactive session.*) 

(* Question 1 *) (* Do not edit this line. *) 
let rec sumlist (l: float list) =  
  match l with
    | [] -> 0.0
    | x::xs -> if (x >= 0.0) then x + (sumlist xs) else failwith "YOU NEED TO PUT POSITIVE NUMBER" 


let rec pairlists twolists = 
    match twolists with 
        | ([],[]) -> [] | ([],x::xs) -> failwith "Error -- lists are not of the same length" 
        | (x::xs, []) -> failwith "Error -- lists are not of the same length" 
        | (x::xs, y::ys) -> [(x,y)] @ pairlists (xs,ys) 

let w_mean weights data = 
    // takes a list of weights as its first argument and returns a function that takes a list of reals and computes the weighted mean.
    (* 1. take a list of weights as its first argument 
       2. returns a function that takes a list of data and computes the weighted mean
            *)
    let weightsData = pairlists ((weights:float list),(data:float list))
    let multiplied = weightsData |> List.map(fun(x,y) -> (x*y))
    let weightedMean = (sumlist multiplied) / (sumlist weights) 
    weightedMean

 (* Question 2. *) (* Do not edit this line. *) 
let rec memberof pair = 
    match pair with
    | (a, []) -> false
    | (a, x::xs) -> if a=x then true else memberof (a, xs)
memberof (3,[1;2;3])
let rec remove(item, lst) =
    match lst with
    | [] -> [] 
    | x::xs -> if (item=x) then remove(item,xs) else x::remove(item,xs)         

 
 (* Question 3. *) (* Do not edit this line. *) 
let findMax l = 
    let rec helper(l,m) =
        match l with
        | (x::xs) -> if x>m then helper(xs, x) else helper(xs, m)
        | [] -> m
    match l with 
    | [] -> failwith "Error -- empty list" 
    | (x::xs) -> helper(xs,x) 

 
(* Question 4. *) (* Do not edit this line. *) 
let rec selsort l =
    match l with
    |[] ->[]
    |l->(findMax l)::(selsort (remove ((findMax l), l)))

 (* Question 5. *) (* Do not edit this line. *) 
let rec common twolists =
    match twolists with 
    |([],[]) -> []
    |([],y) -> []
    |(x::xs, y) -> if memberof(x, y) then [x] @ (common (xs, remove (x,y))) else common (xs, y)

    
 (* Question 6. *) (* Do not edit this line. *) (* Mergesort requires that you use recursion. Using isort or some other sort defeats the whole purpose.*) 
let rec split l = 
    match l with
    | [] -> ([],[])
    | [x] -> ([x],[]) 
    | x::xs::xss -> let (list1, list2) = split xss
                    (x::list1,xs::list2)


let rec merge twolists = 
    match twolists with
    | ([],[]) -> []
    | (x,[]) -> x
    | ([],y) -> y
    | (x::xs,y::ys) ->
        if x<y then [x] @ merge (xs,y::ys)
        else [y] @ merge (x::xs,ys)

let rec mergesort l = 
    match l with 
    | [] -> [] 
    | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *) 
    | n::ns -> let (list1,list2) = split l
               merge ((mergesort list1),(mergesort list2))



       
