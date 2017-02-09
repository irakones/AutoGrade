module assignment1
(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Sebastian Restrepo, Id Number: 260554053 *) (* Edit this line. *)

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

//sumlist [3.0; 4.5; 9.9]

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],y::ys) -> failwith "Error -- lists are not of the same length"
    | (x::xs,[]) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y) :: pairlists (xs,ys)

//pairlists([1; 2; 3],[2; 3; 4]);;

let rec multlist l =
    match l with
    | [] -> []
    | (x,y) :: rst -> (x*y) :: multlist(rst)

//multlist[(1.0,2.0);(2.0,4.0);(3.0,6.0)];;

let w_mean weights data = //fun x y -> n:float
    match (weights,data) with
    | ([],[]) -> failwith "Error -- there are no values to compute"
    | (x::xs,[]) -> failwith "Error -- the lists are not of the same length"
    | ([],x::xs) -> failwith "Error -- the lists are not of the same length"
    | (x::xs,y::ys) -> sumlist(multlist(pairlists(x::xs,y::ys)))/sumlist(x::xs)

//w_mean [3.0; 2.0; 8.0] [2.75; 0.75; 3.5]

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    | (x,[]) -> false
    | (x,y::ys) -> if (x=y) then true else memberof(x,ys)

//memberof("e",["t";"e";"s";"t"])

let rec remove(item, l) =
    match l with
    | [] -> l
    | x::xs -> if (item=x) then remove(item,xs) else x::remove(item,xs)

//remove(3,[1; 2; 3; 1; 2; 3])

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =
    match l with
    | [] -> m
    | x::xs -> if (x>m) then helper(xs,x) else helper(xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | x::xs -> helper(xs,x)

//findMax[1; 3; 5; 12; 2; 9]

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =
    match l with
    | [] -> []
    | x::xs -> findMax(l) :: selsort(remove(findMax(l),l))

//selsort ([1..2..25]@[1..5..21])

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
    match twolists with
    | ([],[]) -> []
    | (x::xs,[]) -> []
    | ([],y::ys) -> []
    | (x::xs,y::ys) -> if memberof(x,y::ys) then x :: common(xs,y::ys) else common(xs,y::ys)

//common([3;4;5;7;2],[1;3;5;7;9;1])

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    match l with
    |[] -> ([],[])
    |[x] -> ([x],[])
    |x1::x2::xs -> let (l1,l2) = split xs
                   (x1::l1,x2::l2)

//split [15..-2..1];;

let rec merge twolists =
    match twolists with
    | ([],[]) -> []
    | (x::xs,[]) -> x::xs
    | ([],y::ys) -> y::ys
    | (x::xs,y::ys) -> if (x<y) then x::merge(xs,y::ys) else y::merge(x::xs,ys)


let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (x, y) = split l
             merge((mergesort x),(mergesort y))

//mergesort [15..-2 ..1]

