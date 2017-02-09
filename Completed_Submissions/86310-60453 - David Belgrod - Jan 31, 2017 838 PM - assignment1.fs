(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: David Belgrod, Id Number: 260555077 *) (* Edit this line. *)

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


(*add is a method*)
let add (x:float) y = x + y

let rec sumlist l = 
  match l with
    | x :: y :: tail -> add x y + sumlist tail
    | _ -> 0.

(*failwith "Not implemented"*)

(*take out later*)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y) :: pairlists (xs, ys);
    (*failwith "Not implemented"*)
  
let w_mean weights data = 
    (*pairlists (weights,data) |> multiply2 *)
    let num = pairlists (weights,data) |> List.map (fun (x,y) -> x * y) |> sumlist 
    let denom = sumlist weights
    num / denom 


(*failwith "Not implemented"*)
  
(* Question 2. *)  (* Do not edit this line. *)
let rec memberof pair = 
  match pair with
  | (x, y::ys) when x = y -> true;
  | (x, []) -> false
  | (x, y::ys) when x <> y -> memberof (x,ys); 
  | (_, y::ys) -> false;
(*failwith "Not implemented"*)

let rec remove(item, lst) =
  match item,lst with
  | x, y::ys when x = y ->  remove(x,ys); 
  | x, [] -> [];
  | x, y::ys when x <> y -> y :: remove (x, ys);
  | _, y::ys -> y::ys 
(*failwith "Not implemented"*)


(* Question 3. *)  (* Do not edit this line. *)
let findMax l = 
  let rec helper(l,m) = 
    match l with
    | x :: xs when m < x -> helper(xs, x);
    | x :: xs when m >= x -> helper(xs, m);
    | _ -> m 
  (*failwith "Not implemented"*)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)

let rec selsort l = 

    match l with
    | x :: xs -> findMax(x::xs) :: selsort (remove(findMax (x::xs) , x::xs)) ; 
    | _ -> []
 
let q4 = [1..2..25]@[1..5..21]
(*failwith "Not implemented"*)

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with
  | x :: xs, y :: ys when memberof(x, y::ys)-> x :: common (xs, y::ys)
  | x :: xs, y :: ys when not(memberof(x, y::ys)) -> common (xs, y::ys)
  | [], [] -> []
  | _, y :: ys -> []
  | x :: xs, [] -> []

let q5 = ([3;4;5;7;2;9],[1;3;5;7;9;1])

(*failwith "Not implemented"*)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let q6 = [15 .. -2 .. 1]
let rec split l  = 
    match l with
    | x::y::xs -> let xx,xy = split xs in x::xx, y::xy;
    | [] -> [],[]
    | [x] -> [x],[]
(*failwith "Not implemented"*)

let q6split = split q6;
let rec merge twolists = 
  match twolists with
  | x::xs, y::ys when x>y -> y::merge(x::xs,ys)
  | x::xs, y::ys when y>x -> x::merge(xs, y::ys)
  | x::xs, y::ys when x=y -> x::y::merge(xs, ys)
  | [],[] -> []
  | x::xs, _ -> x :: merge([],xs)
  | _, y::ys -> y :: merge([], ys)

(*failwith "Not implemented"*)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let L,R = split(n::ns) 
             merge(mergesort(L),mergesort(R));
  (*failwith "Not implemented"*)



