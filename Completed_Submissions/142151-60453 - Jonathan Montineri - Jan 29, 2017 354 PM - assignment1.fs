(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Jonathan Montineri, Id Number: 260738599 *) (* Edit this line. *)

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
    | (x::xs, y::ys) -> (x, y) :: pairlists (xs, ys)

let w_mean weights data = 
    let sumWeights = sumlist weights
    let paired = pairlists (weights, data)
    let weightedValues = List.map (fun (x, w) -> x * w) paired
    let total = sumlist weightedValues
    total / sumWeights
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
        |(value, []) -> false
        |(value, x::xs) -> x = value || memberof(value, xs)


let rec remove(item, lst) =  
    match lst with
        | [] -> []
        | x::xs -> if x = item then remove(item, xs)
                   else x::remove(item, xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
        |[] -> m
        |x::xs -> if x > m then helper(xs, x)
                  else helper(xs, m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with
    |[] -> []
    |xs -> let max = findMax l
           selsort (remove (max, (xs))) @ [max]
           
(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    match twolists with
        |([], list2) -> []
        |(x::xs, list2) -> if memberof(x, list2) then 
                               x::common(xs, remove(x, list2))
                           else common(xs, list2)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =

    let rec length (lst, n) =
        match lst with
        |[] -> n
        |x::xs -> length(xs, n+1)

    let rec helper (l1, l2, n) =
        if n = 0 then (l1, l2)
        else match l1 with
             |[] -> failwith ("Wrong list length")
             |x::xs -> helper(xs, l2 @ [x], n - 1)

    let halfLength = length (l, 0) / 2
    helper (l, [], halfLength)


let rec merge twolists = 
    match twolists with
    |([], []) -> []
    |([], ys) -> ys
    |(xs, []) -> xs
    |(x::xs, y::ys) -> if x < y then
                        x :: merge(xs, y::ys)
                       else
                        y :: merge(x::xs, ys)
                            

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let list1, list2 = split(n::ns)
             let twoSortedLists = (mergesort(list1), mergesort(list2))
             merge (twoSortedLists)
             



