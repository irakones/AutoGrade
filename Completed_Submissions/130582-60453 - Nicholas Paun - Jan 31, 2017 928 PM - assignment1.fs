(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Nicholas M. Paun, Id Number: 260683588 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code MUST compile and must NOT go into infinite
loops.  An assignment like that means you have not tested it.  You will get
ZERO FOR THE ENTIRE ASSIGMENT even if the problem is only with one
question.  If you are not able to get the code to compile and run do not
submit it.  *)

(*module hw1_sol*) (*  Use this if you want to load the file into an interactive session.*)

(* Question 1 *) (* Do not edit this line. *)

let rec sumlist l = 
    let rec real l s =
        match l with
        | [] -> s
        | x::xs -> real xs (x+s)

    real l 0.0

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys)

let w_mean weights data =  
    let weighted_data = (List.map (fun (x,y) -> x * y)  (pairlists(weights,data)))
    sumlist weighted_data / sumlist weights
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    | (_,[]) -> false
    | (item,x::xs) -> 
        if x = item then
            true
        else
            memberof(item,xs)
let rec remove(item, lst) = 
    match lst with
    | [] -> []
    | x::xs -> 
        if x = item then
            remove(item,xs)
        else
            x::remove(item,xs)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
    | [] -> m
    | x::xs -> helper(xs,max x m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with
    | [] -> []
    | _ -> let maxitm = findMax l in maxitm::selsort(remove(maxitm,l))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    let rec uniqueIntersection x y acc =
        match x with
        | x::xs -> 
            let yprime = remove(x,y) in
                if y <> yprime then (* If removing x from ys had any effect *)
                    uniqueIntersection xs yprime (x::acc) (* Then x is in both xs and ys *)
                else 
                    uniqueIntersection xs yprime acc (* Otherwise, xs is not part of the intersection. *)
        | [] -> acc

    let xs, ys = twolists in uniqueIntersection xs ys []


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)
let rec split l = 
    let rec splitMod lst left right =
        match lst with
        | [] -> (left,right)
        | [x] -> (x::left,right) (*If there's only one element, send it to the left sublist *)
        | even::odd::xs -> splitMod xs (even::left) (odd::right) (* Even elements go left, odd elements go right *)
    

    splitMod l [] []

let rec merge twolists =
    match twolists with
    | (xs,[]) -> xs (* If only one list is left, we don't need to think about merging it with the other. *)
    | ([],ys) -> ys
    | (x::xs,y::ys) ->
        if x < y then x::merge(xs,y::ys) (* Put x in the merged list, leave y::ys alone *)
        else y::merge(x::xs,ys)
    
let rec mergesort l = 
    match l with

    | [] -> []
    | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
    | n::ns -> 
        let l, r = split (n::ns) in
            merge (mergesort l,mergesort r)