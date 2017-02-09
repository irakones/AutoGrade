(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Zahra Khambaty, Id Number: 260577706 *) (* Edit this line. *)

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

let rec sumlist l  = 
  match l with
  |[]-> 0.0
  |x :: xs -> x + sumlist xs
let rec pairlists twolists= 
  match twolists with
  |([],[]) -> []
  |([],x::xs) -> failwith "Error -- lists are not of the same length"
  |(x::xs, []) -> failwith "Error -- lists are not of the same length"
  |(x:: xs , y :: ys) -> (x , y) :: pairlists (xs, ys) 
let rec prod l =
  match l with
  |([])->[] 
  |(a,b)::xs ->(a*b)::prod(xs)

let w_mean weights data = (sumlist (prod (pairlists (weights , data))))/sumlist weights
 
  
(* Question 2. *)  (* Do not edit this line. *)
let rec memberof args = failwith "crashed"
(* @GRADER 
let rec memberof pair list = 
  match list with
  |[]-> false
  |x :: xs -> if pair = x then true else memberof pair xs
*)
let rec remove(item,lst) =
  match lst with
  |[]->[]
  |x::xs->if item = x 
              then remove(item,xs)
          else 
              x::remove(item,xs)

(* Question 3. *)  (* Do not edit this line. *)
let findMax l =
  let rec helper(l,m) = 
    match l with
    | [] -> m
    | x::xs -> helper(xs,if (x > m) then x else m )
  match l with
  |[] -> failwith "empty list"
  |x::xs -> helper (xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =
    match l with
    |[]->[]
    |x::xs-> 
        let m = findMax l      
        m :: (selsort (remove(m,l)))

(* Question 5. *)  (* Do not edit this line. *)

let common twolists = 
    let rec commonhelper twolists =
        match twolists with
        |([],y::ys)->[]
        |(x::xs,y::ys)->
                    let n = memberof x (y::ys)
                    if n = true 
                    then x :: (commonhelper(xs,(y::ys)))
                    else
                    (commonhelper(xs,y::ys))
    commonhelper twolists

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
    match l with
    |[]->([],[])
    |x::xs->
            let (list1,list2)= split xs 
            if l.Length % 2 = 0 then (x::list1,list2) 
            else
                (list1,x::list2)

let rec merge twolists = 
    match twolists with
    |([],[])->[]
    |([],y::ys)->y::ys
    |(x::xs,[])->x::xs
    |(x::xs,y::ys)->
            if  x<y then
                 x  :: merge(xs,y::ys)
            else
                 y  :: merge (x::xs,ys) 
let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
            let (list1,list2) = split (n::ns)
            merge(mergesort list1,mergesort list2)



