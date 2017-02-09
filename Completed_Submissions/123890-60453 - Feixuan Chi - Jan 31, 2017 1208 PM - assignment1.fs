(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Feixuan Chi, Id Number: 260659960*) (* Edit this line. *)

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

// where to input;; list weights and reals??
// need to be tail rec??
let rec sumlist l = 
    match l with 
    | [] -> 0.00
    | x::xs -> x + sumlist(xs) 
    // do a recursion to sum it up      

let rec pairlists twolists =
    match twolists with
        | ([],[]) -> []
        | ([],x::xs) -> failwith "Error -- lists are not of the same length"
        | (x::xs, []) -> failwith "Error -- lists are not of the same length"
        | (x::xs, y::ys) -> 
            (x,y) :: pairlists(xs,ys)
            // recursively adding a new pair to list 
let w_mean weights data = 
    let list1 = pairlists(weights,data)
    let list2 = list1 |> List.map (fun (x,y) ->(x*y)) 
    sumlist(list2)/(sumlist weights)
//let w_mean2 weights data = 
//    sumlist(pairlists(weights,data) |> List.map (fun (x,y) ->(x*y)) )/(sumlist weights)

// use list.map 
// change list of paired set into multi then send it to sumlist to sum it up 
// is data the list of real number? double check---------------------------------------------
// double check the green underline under w_mean---------------------------------------------

(* Question 2. *)  (* Do not edit this line. *)
let rec memberof pair = 
    match pair with
    | (x, []) -> false 
    | (x, y::ys) -> 
        if x = y then true else memberof(x,ys)   
        

let rec remove(item, lst) = //failwith "Not implemented"
    match lst with 
    | [] -> []
    | x::xs -> 
        if (x <> item) then x::remove(item,xs)
        else  remove(item,xs)
       
(* Question 3. *)  (* Do not edit this line. *)
let findMax l = 
    let maxHelper x y = if x < y then y else x 
    
    let rec helper(l,m) = //failwith "Not implemented"
        match l with 
        | h::t -> helper(t,maxHelper m h) 
        | [] -> m

    match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper(xs,x)  

(* Question 4. *)  (* Do not edit this line. *)
let rec selsort l = //failwith "Not implemented"
    match l with
    | [] -> []
    | l ->  let max = findMax l 
            let sortedRest = remove(max,l)
            max :: selsort sortedRest
            
(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists =          
    match twolists with
    | ([],[]) -> []
    | (x::xs,[]) -> [] 
    | ([],y::ys) -> [] 
    | (x::xs,listy) ->
        if memberof(x,listy)  
            then x::common(xs,remove(x,listy))
        else 
            common(xs,listy)


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

// can we add another rec function inside the rec??????
let rec split list =
    let rec sp l l1 l2 =
        match l with
            | [] -> (l1,l2)
            | [x] -> (x::l1,l2)
            | x::y::tail ->
                sp tail (x::l1) (y::l2)
    in sp list [] []

let rec merge twolists = //failwith "Not implemented"
    match twolists with
    | ([],[]) -> []
    | (x,[]) -> x
    | ([],x) -> x
    | (x::xs,y::ys) ->
        if x < y
            then x::y::merge(xs,ys)
        else y::x::merge(xs,ys)
// This value is not a function and cannot be applied???? solved
// input should be ([],[]) not just [],[]


let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> //failwith "Not implemented"
    // first call split 
    // then sort 
    // last merge
    let l1,l2 = split(l) 
    merge(mergesort(l1),mergesort(l2))

