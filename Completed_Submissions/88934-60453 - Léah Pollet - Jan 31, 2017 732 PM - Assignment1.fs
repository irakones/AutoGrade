(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Leah POllET, Id Number: 260571488 *) (* Edit this line. *)

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

//------------------------------------------
(* Question 1 *) (* Do not edit this line. *)
//------------------------------------------

(* sumlist: computes the sum of a list of floats *)
let rec sumlist l = 
    match l with
    | [] -> 0.0 //type cast it to float?? <---
    | x::xs -> x + sumlist xs

(* pairlists takes two lists of floats (same len), produces a list of matched pairs *)
let rec pairlists twolists =
  match twolists with
    | ([],[]) -> [] // /!\ Value restriction error!!! <---
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::(pairlists (xs,ys))


(* w_mean list of weights as its first argument and returns a function that takes a list of reals and computes the weighted mean.s *)
//  /!\ Remove the extra prints?
let w_mean weights data = 

    let sumOfWeights = sumlist weights
    let tempSumOfPaired = pairlists(weights,data) |> List.map (fun (x,y) -> x*y)
    let sumOfPaired = sumlist tempSumOfPaired
    sumOfPaired/sumOfWeights
    
 //------------------------------------------ 
(* Question 2. *)  (* Do not edit this line. *)
//------------------------------------------

(* memberof: tests whether an element is a member of a given list*)
//Any exception to catch??
let rec memberof pair = 
    match pair with
    | (_,[]) -> false
    | (x , y::ys) -> 
        if x = y then true
        else memberof (x,ys)
(* remove: takes an element and a list, removes all copies of the element from the list*)
let rec remove(item, lst) = 
    match lst with
    | [] -> []
    | x::xs ->
        if x = item then remove(item, xs)
        else x::remove(item, xs)

//------------------------------------------
(* Question 3. *)  (* Do not edit this line. *)
//------------------------------------------

(* findMax: finds the largest value of a list of elements of a comparion type*)
let findMax l = 
  let rec helper(l,m) = 
    match l with
    | [] -> m //the list had only 1 element, m
    | (x::xs) ->
        if x > m then helper(xs,x)
        else helper(xs,m)

  match l with
  | [] -> failwith "Error -- empty list" // /!\ Value restriction <---
  | (x::xs) -> helper(xs,x)

//------------------------------------------
(* Question 4. *)  (* Do not edit this line. *)
//------------------------------------------ 

(* selsort: implements selection sort and removes duplicates*)
let rec selsort l = 
    match l with 
    | [] -> []
    | _ -> 
        let x = findMax l 
        let l2 = remove(x,l) 
        x::selsort l2 

//------------------------------------------
(* Question 5. *)  (* Do not edit this line. *)
//------------------------------------------

(* common: takes 2 lists, forms a new list containing 1 copy of each common element *)
let rec common twolists = 
    match twolists with 
    | (_,[]) -> []
    | ([],_) -> []
    | (x::xs,ys) ->
        if memberof (x,ys) then 
            let newys = remove (x, ys)
            x::common(xs,newys)
        else common(xs,ys)
   
//------------------------------------------
(* Question 6. *)   (* Do not edit this line. *)
//------------------------------------------

(* split: that produces a pair of equal lists, (if the length of l is odd then one of the “halves” is one item longer than the other)*)
let rec split l = 
    match l with
    | [] -> ([],[])
    | x::[] -> (x::[],[])
    | (x::y::xs) -> 
        let (a,b) = split(xs)
        (x::a,y::b)

(* merge:  merges sorted lists*)
let rec merge twolists = 
    match twolists with
    | ([],[]) -> []
    | (_,[]) -> []
    | ([],_) -> []
    | (x::xs,y::ys) -> 
        if x<y then
            x::y::merge(xs,ys)
        else
            y::x::merge(xs,ys)

(*mergesort: implements the overall algorithm*)
let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  //| n::ns -> 
  | _ -> 
    let (left,right) = split l 
    merge(mergesort(left),mergesort(right))
      


