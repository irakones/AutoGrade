module hw1
(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Adam La Morre, Id Number: 260627572 *) (* Edit this line. *)

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
//sumlist [2.0; 6.1; 12.2]

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y) :: pairlists (xs ,ys) 
//pairlists([1;2;3],[2;3;4]);;

let rec listProduct l = 
    match l with
    | [] -> []
    |(x, y) :: rst -> (x * y) :: listProduct(rst)
//listProduct [(1, 2); (2, 3); (3, 4)];;

let w_mean weights data = //fun x y -> n:float
    match (weights,data) with
    | ([],[]) -> failwith "Error -- nothing to calculate"
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    |(x::xs, y::ys) -> sumlist(listProduct(pairlists(weights,data)))/sumlist(x::xs)
//w_mean [1.0;4.0] [45.5;100.0]
                        
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    |(x,[]) -> false
    |(x,y::ys) -> if (x=y) then true else memberof(x,ys)
//memberof(2,[3;4;5;2;6])

let rec remove(item, lst) = 
    match lst with
    |[] -> lst
    |x::xs -> if (item=x) then remove(item,xs) else x::remove(item,xs) 
//remove("bob",["alice";"bob";"carl";"dave"])

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
    |[] -> m
    |x::xs -> if (x>m) then helper(xs,x) else helper(xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)
//findMax[5;8;1;2;3]

(* Question 4. *)  (* Do not edit this line. *)
let rec selsort l = 
    match l with
    |[] -> []
    |x::xs -> findMax(l) :: selsort(remove(findMax(l),l))
//selsort[1;3;2;4;7;6;5]

(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists = 
    let rec trim list = 
        match list with
        |[] -> []
        |x::xs -> if (memberof(x,xs)) then trim(xs) else x :: trim(xs)

    match twolists with
    |(x::xs,[]) -> []
    |([],y::ys) -> []
    |([],[]) -> []
    |(x::xs,y::ys) -> if (memberof(x,y::ys)) then x :: common(trim(xs),y::ys) 
                      else common(xs,y::ys)
//common ([3;4;5;7;2],[1;3;5;7;9;1;2;2])

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    match l with
    |[] -> ([],[])
    |[x] -> ([x],[])
    |x1::x2::xs -> let (l1,l2) = split xs
                   (x1::l1,x2::l2)
//split [1;2;3;4;5]

let rec merge twolists = 
    match twolists with
    |([],[]) -> []
    |(x::xs,[]) -> x::xs
    |([],y::ys) -> y::ys
    |(x::xs,y::ys) -> if (x<y) then x::merge(xs,y::ys)
                      else y::merge(x::xs,ys)
//merge ([1; 3], [2; 4])

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (x, y) = split l
             merge((mergesort x),(mergesort y))
//mergesort [100;3;2;5;6;4;99;7;22;8]
