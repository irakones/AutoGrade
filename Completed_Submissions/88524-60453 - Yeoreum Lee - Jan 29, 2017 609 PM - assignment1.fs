(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Yeoreum Lee, Id Number: 260563893 *) (* Edit this line. *)

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
    | x::xs -> x+(sumlist xs)

//sumlist [1.0;2.0;3.0]

let rec pairlists twolists =
        match twolists with
        | ([],[]) -> []
        | ([],x::xs) -> failwith "Error -- lists are not of the same length"
        | (x::xs, []) -> failwith "Error -- lists are not of the same length"
        | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys)

//pairlists ([40;40;20],[80;80;95])

//[(40, 80); (40, 80); (20, 95)]

let w_mean weights data = 
    let lst = pairlists (weights,data)
    let rec helper(lst,acc) = 
        match lst with
        | [] -> (sumlist acc)/(sumlist weights)
        | ((x,y)::zs) -> helper(zs,acc@[(x*y)])
    helper(lst,[])

//w_mean [40.0;40.0;20.0] [80.0;80.0;95.0]

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    | (a,[]) -> false
    | (a,x::xs) -> if (a=x) then true else memberof (a,xs)

//memberof (1,[1;2;3])

let rec remove lst = 
    let rec helper(lst,acc) = 
        match lst with
        | (item,[]) -> acc
        | (item,x::xs) -> if (item=x) then helper((item,xs),acc) else helper((item,xs),acc@[x])
    helper(lst,[])

//remove (2,[1;2;3;4])

(* Question 3. *)  (* Do not edit this line. *)
let findMax l = 
  let max a b = if a<b then b else a
  let rec helper(l,m) = 
    match l with
    | x::xs -> let m = max x m in helper(xs,m)
    | [] -> m
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

//let testList = [1;17;3;6;1;8;3;11;6;5;9]
//findMax testList

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  match l with
  | [] -> []
  | x::xs -> let m = findMax l
             (m::(selsort(remove(m,l))))

//selsort ([1..2..25]@[1..5..21])

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
    let rec helper(twolists,acc) = 
        match twolists with
        | (x::xs,[]) -> acc
        | ([],y::ys) -> acc
        | ([],[]) -> acc
        | (x::xs,y::ys) -> if memberof(x,y::ys) then helper((xs,y::ys),acc@[x]) else helper((xs,y::ys),acc)
    helper(twolists,[])

//common ([3;4;5;7;2],[1;3;5;7;9;1])

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
    let rec helper(l,accl,accr) = 
        match l with
        | [] -> (accl,accr)
        | [x] -> (accl@[x],accr)
        | x::y::zs -> helper(zs,accl@[x],accr@[y])
    helper(l,[],[])

let rec merge twolists = 
    let rec helper(twolists,acc) = 
        match twolists with
        | ([],[]) -> acc
        | (x::xs,[]) -> helper((xs,[]),acc@[x])
        | ([],y::ys) -> helper(([],ys),acc@[y])
        | ([x],[y]) -> if (x<y) then (acc@[x]) else (acc@[y]@[x])
        | (x::xs,y::ys) -> if (x<y) then helper((xs,y::ys),acc@[x]) else helper((x::xs,ys),acc@[y])
    helper(twolists,[])

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (left,right) = split l
             merge((mergesort left),(mergesort right))

//mergesort [15 .. -2 .. 1]
//split [15 .. -2 .. 1]
