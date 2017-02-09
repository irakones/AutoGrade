(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Dong Jae Kim, Id Number: 260524474 *) (* Edit this line. *)

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
    | x :: xs -> x + sumlist(xs)
let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys)

let w_mean weights data = 
    sumlist(pairlists(weights,data) |> List.map (fun(x,y) -> x * y)) / sumlist (weights)

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    | (y, []) -> false
    | (y, x::xs) -> 
    if x = y then true
    else memberof (y,xs)

let rec remove(item, lst) = 
    match lst with
    | [] -> []
    | x::xs -> 
        if item = x  then remove(item, xs)
        else x :: remove (item, xs)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with    
    | [] -> m
    | (x::xs) -> 
    if x > m then helper(xs, x)
    else helper (xs, m)   
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =
    let max = findMax(l)
    let l2 = remove(max, l)
    match l2 with
    | [] -> max :: l2
    | (x::xs) -> max :: selsort(l2) 

(* Question 5. *)  (* Do not edit this line. *)

// finding common
let rec common twolists =
    match twolists with
    | ([],[]) -> []
    | (x::xs, []) -> []
    | ([],x::xs) -> []
    | (x::xs, y::ys) ->
        if memberof(x,y::ys) then x::common(xs,y::ys)
        else common(xs,y::ys)

            
            

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
    let rec helperSplit (l1, l2, l) = 
        match l with
        |[] -> (l1, l2) // if empty return empty lists
        |[x] -> (l1, l2@[x])  // else if it has 1 item remaining then append it to 2nd
        |x1::x2::xs -> helperSplit (l1@[x1], l2@[x2], xs) //append x1 to l1 and x2 to l2, then recursively
    helperSplit ([],[],l)     
                                    // call helpersplit(xs) to append x3 and x4
let rec merge twolists = 
    match twolists with // using pseudocode from comp 250 prof Langer's notes
    | ([],[]) -> []
    | (x::xs,[]) -> x::xs
    | ([], x::xs) -> x::xs
    | (x::xs, y::ys) -> 
    if(x > y) then x::merge (xs,y::ys)  //simplified the code a little bit compared to the first version
    else y::merge(x::xs,ys)             // i submitted - first version i strictly followed the pseudocode
                                        // for merge
let rec mergesort l = 
  match l with
  | [] -> []
  | [n] -> [n] (* Without this you will go into an infinite loop. *)
  | n::ns -> merge (mergesort (fst(split l)), mergesort(snd(split l))) // recursively merge things that was
                                                                        // mergesorted 

