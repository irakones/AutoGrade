(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Jason Galet, Id Number: 260678030 *) (* Edit this line. *)

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
    | x::xs -> x + sumlist(xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists (xs,ys) //pair the first element in each list and recurse

let w_mean weights data =
  match weights with
    | [] -> 0.0
    | x::xs -> let sum = sumlist weights 
               let pairs = pairlists (weights, data)
               let prods = pairs |> List.map (fun (x,y) -> x*y) //calculate the product of each pair and store in a new list
               (sumlist prods/sum)               

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
    | (_ , []) -> false
    | (key, x::xs) -> if (x = key) then true else memberof (key, xs) //recurse through each element and compare with key


let rec remove(item, lst) = 
  match lst with
    | [] -> []
    | x::xs -> if (x = item) then remove (item, xs) //remove x from list
               else x::remove (item, xs) //keep x and recurse

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
      | [] -> m
      | x::xs -> if (x > m) then helper (xs,x) //x is new max
                 else helper (xs, m)
  match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  match l with
    | [] -> []
    | [x] -> [x]
    | x::xs -> if (findMax xs >= x) then (findMax xs)::selsort (remove (findMax xs, l)) //move max to front and remove all duplicates
               else x::selsort xs //x is the largest element in the list


(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
  match twolists with
    | ([],_) -> []
    | (x::xs, ys) -> if (memberof (x,ys)) then x::common (xs, ys) //add x to common list 
                     else common (xs, ys) //x is not common to both lists


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  match l with 
  | [] -> ([], [])
  | [n] -> ([n], [])
  | n1::n2::ns -> let (l1, l2) = split ns
                  (n1::l1, n2::l2) //unzip into two new lists containing alternating elements of the original

let rec merge twolists = 
  match twolists with
    | ([], []) -> []
    | (xs, []) -> xs
    | ([], ys) -> ys
    | (x::xs, y::ys) -> if (x > y) then y::merge (x::xs, ys) //store the head of second list and recurse 
                        else x::merge (xs, y::ys) //store the head of first list and recurse

let rec mergesort l = 
  match l with
  | [] -> []
  | [n] -> [n] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (l1, l2) = split l
             merge (mergesort l1, mergesort l2) //recursively split until 1 element then merge as one sorted list



