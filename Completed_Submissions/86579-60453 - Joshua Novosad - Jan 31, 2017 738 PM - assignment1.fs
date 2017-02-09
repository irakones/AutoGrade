(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Josh Novosad, Id Number: 260555158 *) (* Edit this line. *)

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
    | [] -> 0.0 //sum of empty list is 0.0 (because float is specified, otherwise it would be 0)
    | x::xs ->
      let s = sumlist xs //sums recursively
      s+x

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y)::pairlists (xs, ys) //creates a list of tuples recursively


let w_mean weights data =  
  let rec helper tple = //helper method that multiplies the components of tuple
    match tple with
      | ([],[]) -> []
      | (_,[]) -> []
      | ([],_) -> []
      | (x::xs,y::ys) -> (x*y)::helper(xs,ys)
  sumlist(helper(weights,data))/sumlist(weights)
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
    | (y,[]) -> false
    | (y,x::xs) -> if y=x then true else memberof (y,xs)


let rec remove(item, lst) = 
  match lst with
    | [] -> [] // empty list or end of list reached
    | x::xs -> if (x.Equals(item)) then remove(x, xs) else x::(remove(item, xs)) // if item found, delete from list; use shorter list in recursion


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
      | [] -> m // means that end of the list reached, therefore 'm' is the largest value
      | x::xs -> if (x > m) then helper(xs,x) else helper(xs,m) // smaller list, if the x value is bigger than m it replaces it in the recursion
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)



(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  match l with
    | [] -> []
    | [x] -> [x]
    | x::xs -> if x > findMax xs then x::(remove(x,l) |> selsort) else (findMax l)::(selsort (remove(findMax l,l)))
    //pipes result of remove to selsort recursively, otherwise uses findMax and remove to selsort recursively

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with
    | ([],[]) -> []
    | (_,[]) -> []
    | ([],_) -> []
    | (x::xs,y) -> if memberof(x,y) then x::common(xs,remove(x,y)) else common(xs,y)
    //checks if x exists in the list y; if it does it is added to list, in all cases recursively moves through the list

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  let rec helper lst (xs,ys) =
    match lst with
      | [] -> (xs,ys) //empty list returns the base case
      | [x] -> (x::xs,ys) //if one item, add to specific list
      | x::y::tail -> helper tail (x::xs,y::ys)
  helper l ([],[])

let rec merge twolists =
  match twolists with 
    | ([],[]) -> []
    | (l1,[]) -> l1 //append the rest of the list if one side is fully sorted in the finished lists
    | ([],l2) -> l2
    | (x::xs,y::ys) -> if x<y then x::merge(xs,y::ys) else y::merge(x::xs,ys) //assuming lists are sorted, compare the smallest element in each

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let (l1,l2) = split l //splits a list into two lists
    merge (mergesort l1, mergesort l2)
