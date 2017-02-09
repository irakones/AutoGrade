(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Natalie Conway, Id Number: 260623099 *) 

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

let rec sumlist l : float = 
  match l with
    | [] -> 0.0
    | x::xs -> x + (sumlist (xs))

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::(pairlists (xs, ys))

let w_mean weights data =  
    let sumweigh = sumlist weights
    let retfun t = 
        let pair = pairlists (weights, t)
        let r1 = pair |> List.map (fun (x,y) -> x*y)
        let sumall = sumlist r1
        sumall/sumweigh
    retfun data



  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
        | (find, []) -> false
        | (find, x::xs) ->
            if  find = x then true
            else memberof (find, xs)


let rec remove(item, lst) = 
    match lst with
        | [] -> []
        | x::xs ->
            if x = item then (remove(item, xs))
            else x::(remove(item, xs))


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =
    match l with
        | [] -> m
        | (x::xs) ->
            if x > m then helper(xs,x)
            else
            helper(xs,m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)


(* Question 4. *)  (* Do not edit this line. *)
  
let rec myfilter out lst = 
    match lst with
        | [] -> []
        | x::xs ->
            if x = out then (myfilter out xs)
            else x::(myfilter out xs)

let rec selsort l =
    match l with
        | [] -> []
        | x::xs -> 
            let max = findMax l
            max::selsort(myfilter max l)

        

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    match twolists with 
        | ([],[]) -> []
        | ([],x::xs) -> []
        | (x::xs, []) -> []
        | (x::xs, y::ys) -> 
            if (memberof (x, y::ys)) then
                let l1 = myfilter x xs
                let l2 = myfilter  x (y::ys)
                x::(common (l1, l2))
            else
                common (xs, ys)



(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    match l with
        | [] -> ([],[])
        | [x] -> (l,[])
        | x1::x2::xs ->
            let (l1, l2) = split xs
            (x1::l1, x2::l2)

let rec merge twolists = 
    match twolists with
        | ([], l2) -> l2
        | (l1, []) -> l1
        | (x1::xs1, x2::xs2) ->
            if x1 < x2 then x1::(merge (xs1, x2::xs2))
            else x2::(merge (x1::xs1, xs2)) 

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let (l1,l2) = split l
    let lnew = mergesort l1
    let lnew2 = mergesort l2
    merge(lnew,lnew2)


