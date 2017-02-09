(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Duong Tan Khoa Gael, Id Number: 260691624 *) 

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code MUST compile and must NOT go into infinite
loops.  An assignment like that means you have not tested it.  You will get
ZERO FOR THE ENTIRE ASSIGMENT even if the problem is only with one
question.  If you are not able to get the code to compile and run do not
submit it.  *)


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
    | (x::xs, y::ys) -> (x,y) :: pairlists (xs,ys)


let w_mean weights data = sumlist(pairlists(weights,data) |> List.map(fun(x,y) -> x*y)) / sumlist(weights)


(* Question 2. *)  (* Do not edit this line. *)
let rec memberof pair = 
    match pair with
    | (x,[]) -> false
    | (x, y::ys) -> if (x = y) then true else memberof (x, ys)

//memberof(10,[1;2;3])

let rec remove(item, lst) = 
    match lst with
    | [] -> []
    | x :: xs -> if(x <> item) then x :: remove(item, xs) else remove(item, xs)


(* Question 3. *)  (* Do not edit this line. *)
let findMax l = 
    let rec helper(l,m) =
        match l with
        | [] -> m
        | x :: xs -> if(x > m) then helper(xs, x) else helper(xs, m)
    match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper(xs,x)

//findMax [12;33;166;123];;
(* Question 4. *)  (* Do not edit this line. *)
let rec selsort l = 
    match l with
    | [] -> []
    | (x :: xs) -> findMax l :: selsort(remove(findMax l, l))


//let testlist = [17;3;6;1;1;8;3;11;5;9;88]
//selsort testlist

(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists = 
    match twolists with
    |(_,[]) -> []
    |([],y::ys) -> []
    |(x::xs,y::ys) -> if(memberof(x,y::ys)) then x::common(xs,y::ys) else common(xs,y::ys)

//common ([3;4;5;7;2],[1;3;5;7;9;1]);;   

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    let rec helper(l,l1,l2) =
        match l with 
        | [] -> (l1,l2)
        | [x] -> (x :: l1, l2)
        | a :: b :: cs -> helper(cs, a::l1, b::l2)
    helper(l,[],[])



// let testlist = [17;3;6;1;1;8;3;11;5;9;88]
// split testlist

let rec merge twolists = 
    match twolists with
    | ([],[]) -> []
    | (x::xs,[]) -> x::xs
    | ([], x::xs) -> x::xs
    | (x::xs, y::ys) -> if(x > y) then x::merge (xs,y::ys) else y::merge(x::xs,ys)

// let a1 = [9;7;6;4]
// let a2 = [12;4;3;1]
// merge (a1,a2)
let rec mergesort l = 
    let (x,y) = split l;
    match l with
    | [] -> []
    | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
    | n::ns ->  merge (mergesort x,mergesort y)

//let testlist = [17;3;6;1;1;8;3;11;5;9;88]
//mergesort testlist