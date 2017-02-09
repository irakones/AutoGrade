(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Erin Holley, Id Number: 260504903 *) (* Edit this line. *)

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
    | [] -> 1.0
    | x::xs -> x + (sumlist xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y)::pairlists (xs, ys)

let w_mean weights data=  
  let weightedData = (pairlists (weights, data)) |> List.map (fun (x,y) -> x*y)
  (sumlist weightedData) / (sumlist data)
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
    | (y, []) -> false
    | (y, x::xs) -> if (y=x) then (true) else (memberof (y, xs))


let rec remove (item, lst) = 
  match lst with
    | [] -> []
    | x::xs -> if (x=item) then remove(item, xs) else x::remove (item, xs)


(* Question 3. *)  (* Do not edit this line. *)
let findMax l = 
  let rec helper(l,m) = 
    match (l,m) with 
      | ([], m) -> m
      | (x::xs, m) -> if (m>x) then helper(xs,m) else helper(xs,x)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
let rec selsort l = 
  match l with
    | [] -> []
    | list -> let max = findMax (list) in max::selsort (remove(max, list))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with
    | ([],[]) -> []
    | (y::ys, []) -> []
    | ([], x::xs) -> []
    | (y::ys, x::xs) -> if (memberof(y, x::xs)) then (y::common (ys,remove(y,x::xs))) else common (ys,x::xs)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
   match l with
     | [] -> ([],[])
     | x::xs -> match xs with
                 | [] -> let (xlist,ylist) = split([]) in (x::xlist, ylist)
                 | y::ys -> let (xlist,ylist) = split(ys) in (x::xlist, y::ylist)

let rec merge twolists = 
  let rec placeelement (l,m) = 
    match l with
      | [] -> [m]
      | x::xs -> if (m>x) then (m::x::xs) else (x::placeelement(xs,m)) 
  match twolists with
    | ([],[]) -> []
    | (x::xs,[]) -> match merge(xs,[]) with
                      | [] -> [x]
                      | n::ns -> placeelement(n::ns,x)
    | ([],y::ys) -> match merge(ys,[]) with
                      | [] -> [y]
                      | n::ns -> placeelement(n::ns,y)
    | (x::xs,y::ys) -> match merge(xs,ys) with
                        | [] -> if (x>y) then [x;y] else [y;x]
                        | n::ns -> placeelement(placeelement(n::ns,x), y)

let rec mergesort l = 
  match l with
    | [] -> []
    | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
    | n::ns -> let (list1,list2) = split(n::ns) in merge((mergesort(list1),mergesort(list2)))


