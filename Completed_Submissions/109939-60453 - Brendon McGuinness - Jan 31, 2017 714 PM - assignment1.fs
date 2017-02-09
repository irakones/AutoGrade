(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Brendon McGuinness, Id Number: 260629330 *) (* Edit this line. *)

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
    | (x::xs, y::ys) -> (x,y)::pairlists (xs, ys) 


let w_mean weights data = 
    let reals = pairlists(weights,data) |> List.map (fun (a,b) -> a*b)
    sumlist(reals)/sumlist(weights)

  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
   | (_,[]) -> false
   | (item, x::xs) -> if x = item then true
                         else memberof (item, xs)
   

let rec remove(item, lst) = 
    match lst with
    | [] -> []
    | x::xs when x = item -> remove(item, xs)
    | x::xs -> x::remove(item, xs) 


(* Question 3. *)  (* Do not edit this line. *)

//helper function for findMax
let helpMax x y = if x > y then x else y

let findMax l : 'a when 'a : comparison = 
  let rec helper(l,m) = 
    match l with
    | [] -> m
    | x::xs -> let m = helpMax x m
               helper(xs, m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with
    | [] -> []
    | x::xs -> findMax(l)::selsort(remove(findMax(l),l))

(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists = 
    let rec commonHelp(a,b) = 
        match a with
         | [] -> []
         | x::xs -> 
             if memberof(x,b) then x::commonHelp(xs,b)
             else commonHelp(xs,b)
    commonHelp(twolists)



(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let split l = 
    let rec helperSplit lst l1 l2 =
       match lst with
       | [] -> l1, l2
       | x::xs -> helperSplit xs l2 (x::l1)
    helperSplit l [] []

let rec merge twolists = 
    match twolists with
    | ([],[]) -> []
    | ([],l) -> l
    | (l,[]) -> l
    | (x::xs, y::ys) when x<y -> x::merge(xs,(y::ys))
    | (lst, y::ys) -> y::merge(lst,ys)


let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (l1,l2) = split(n::ns)
             merge(mergesort l1, mergesort l2)


