(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Xiang Chen Zhu, Id Number: 260602676 *) (* Edit this line. *)

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
  | x :: xs -> x + sumlist xs
  
let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists(xs, ys)

let w_mean weights data =
  let lt = pairlists(weights, data)
  let lttwo= lt |> List.map(fun (x,y) -> x * y)
  let numerator = sumlist lttwo
  let denominator = sumlist weights 
  numerator/denominator
(* Question 2. *)  (* Do not edit this line. *)
let rec memberof pair =
  match pair with
  |(x,[]) -> false
  |(x, y::ys) -> if (x = y) then true else memberof(x, ys)
  
let rec remove(item, lst) = 
  match lst with
  |[] -> []
  |x :: xs -> if (x<>item) then x::remove(item,xs) else remove(item,xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
    |[] -> m
    |(x::xs) -> if m<x then helper(xs,x) else helper(xs,m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)
(* Question 4. *)  (* Do not edit this line. *)
let rec selsort l =
  match l with
  |[] -> []
  | x::xs -> 
    let maximum = findMax(l)
    maximum :: selsort(remove(maximum,l))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with
  | ([],[]) -> []
  | (xs,[]) -> []
  | ([],ys) -> []
  | (x::xs, ys) -> if (memberof(x,ys)) then x::(common(remove(x,x::xs),ys)) else common(remove(x,x::xs),ys)


//let test2= common ([1;3;3;4;5;7;2],[1;3;5;7;9;1;2]);;
(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  match l with
  |[] -> ([],[])
  |[x] -> ([x],[])
  |x::y::zs -> let (a,b) = split zs in (x::a,y::b)
let rec merge twolists = 
  match twolists with
  |(xs,[])-> xs
  |([],ys)-> ys
  |(x::xs,y::ys)-> if (x<y) then x::merge(xs,y::ys) else y::merge(x::xs,ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (xs,ys) = split (n::ns) in merge(mergesort(xs),mergesort(ys))

//mergesort [15 .. -2 .. 1];;

