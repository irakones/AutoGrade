(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Nathaniel Hopp, Id Number: 260527434 *) (* Edit this line. *)

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

let rec sumlist (l:float list) =
    match l with
    | [] -> 0.0
    | x::xs -> x + sumlist xs
let rec pairlists twolists =
    match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y) :: pairlists (xs,ys)          
let rec helperQ1 wdpair = 
    match wdpair with
    | [] -> 0.0 
    | (x,y)::xs -> x*y + helperQ1 xs  
let w_mean (weights:float list) (data:float list) =
    helperQ1 (pairlists( (weights,data) )) / sumlist weights


(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    | (_,[]) -> false
    | (x,y::ys) -> if(x=y) then true
                   else memberof (x,ys)

let rec remove(item, lst) = 
    match lst with
    | [] -> []
    | x::xs -> if(x = item) then remove(item, xs)
               else x::remove(item,xs) 
                     
(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
      match l with 
      | [] -> m
      | x::xs -> if(x>m) then helper(xs,x)
                 else helper(xs,m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with
    | [] -> []
    | x::xs -> findMax (l) :: selsort(remove(findMax (l),l)) 
(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    match twolists with
    | ([],[]) -> []
    | ([],y::ys) -> []
    | (x::xs,[]) -> [] 
    | (x::xs,y::ys) -> if(memberof(x,y::ys)) then x::common (remove(x,xs),remove(x,y::ys))
                       elif(memberof(y,x::xs)) then y::common (remove(y,x::xs),remove(y,y::ys))
                       else common(xs,ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    match l with
    | [] -> ([],[])
    | [x] -> ([x],[])
    | x::y::xs -> 
        let (lft,rht) = split xs
        (x::lft,y::rht)

let rec merge twolists = 
    match twolists with
    | ([],[]) -> []
    | (l,[]) -> l
    | ([],l) -> l
    | (x::xs,y::ys) -> if(x<=y) then x::merge (xs,y::ys)
                       else y::merge (x::xs,ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
        let (lft,rht) = split (n::ns)
        merge (mergesort lft, mergesort rht)






(*TESTING
let d = [1.0;2.0;3.0;4.0;5.0]
let w = [1.0;1.0;1.0;1.0;1.0]
sumlist w
w_mean w d
memberof (1,[1;2;3])
memberof (1, [])
memberof (1,[2;3;4;1])
remove (2, [1;3;2;4;2;2;3])
let testList = [21;17;3;6;1;19;3;11;6;5;22]
findMax testList
selsort ([1..2..25]@[1..5..21])
selsort ([1;5;3;5;7;21;1;21;5;23;21;9])
selsort ([1;2;3;4])
common ([7;1;3;5;2],[1;2;5;7;1])
common ([3;4;5;7;2],[1;3;5;7;9;1])
split ([1;7;2;3;4;5;6;7;9])
merge ([1;3;5;7],[2;4;6;7])
mergesort [15 .. -2 .. 1]
split [15 .. -2 .. 1]
mergesort [1;0;2;5;3;17;18;19;21;-1]
*)
