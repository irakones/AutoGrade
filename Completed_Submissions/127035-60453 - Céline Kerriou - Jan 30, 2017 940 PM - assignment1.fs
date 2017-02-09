(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Celine Kerriou, Id Number: 260669639 *)

(* module hw1_sol.  Use this if you want to load the file into an interactive session.*)

(* Question 1 *) (* Do not edit this line. *)

let rec sumlist l =
  match l with
  | [] -> 0.0
  | x::xs -> x + sumlist xs

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::(pairlists (xs,ys))

let rec multpairs (x,y) = x*y

let rec mult l =
  match l with
    | x::xs -> (multpairs x)::(mult xs)
    | [] -> []

let w_mean weights data =
(* input list of weights output function that takes list of reals and computes weighted mean *)
  let pairs = pairlists (weights, data)
  let multlist = mult pairs
  (sumlist multlist) / float(weights.Length)


(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
(* test whether element is member of given list *)
  match pair with
    | (x, []) -> false
    | (x,y::ys) ->
      let result = (x=y)
      match result with
        | true -> true
        | false -> memberof (x, ys)

let rec remove(item, lst) =
  match lst with
    | [] -> []
    | x::xs ->
      let comp = (x=item)
      match comp with
        | true -> remove(item, xs)
        | false -> x::(remove(item,xs))


(* Question 3. *)  (* Do not edit this line. *)

let findMax l =
  let rec helper(l,m) =
    (*m is our current maximum and we go through the list*)
    match l with
      | [] -> m
      | x::xs ->
        let comp = (x<m)
        match comp with
          | true -> helper(xs, m)
          | false -> helper(xs, x)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)


(* Question 4. *)  (* Do not edit this line. *)

let rec selsort l =
  match l with
    | [] -> []
    | xs ->
      let m = findMax xs
      let nlist = (remove(m, xs))
      m::(selsort nlist)

(* Question 5. *)  (* Do not edit this line. *)
(* I created a commonsorted function in order to not sort at every recursion *)
let rec commonsorted twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> []
    | (x::xs, []) -> []
    | (x::xs, y::ys) ->
      let equal = (x=y)
      match equal with
        | true -> x::(commonsorted (xs, ys))
        | false ->
          let comp = (x<y)
          match comp with
            | true -> commonsorted(x::xs, ys)
            | false -> commonsorted(xs, y::ys)

let rec common twolists =
  match twolists with
    | ([],[]) -> []
    | ([], xs) -> []
    | (xs, []) -> []
    | (xs, ys) ->
      let l1 = (selsort xs)
      let l2 = (selsort ys)
      commonsorted (l1,l2)


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)
let isEven n = ((n % 2) = 0)

let rec split l =
  match l with
    | [] -> ([],[])
    | xs ->
      let len = l.Length/2
      let rec splithelp len twolists =
        let bool = (len>0)
        match bool with
          | false -> twolists
          | true ->
            match twolists with
            | ([],[]) -> ([],[])
            | (x::xs, []) -> twolists
            | ([],x::xs) -> splithelp (len-1) ([x],xs)
            | (xs, y::ys) -> splithelp (len-1) ((List.append xs [y]), ys)
      splithelp len ([],l)

let rec merge twolists =
  match twolists with
    | ([],[]) -> []
    | ([], xs) -> xs
    | (xs, []) -> xs
    | (x::xs, y::ys) ->
        let eq = (x=y)
        match eq with
          | true -> merge (x::xs,ys)
          | false ->
            let comp = (x>y)
            match comp with
              | true -> y::(merge (x::xs, ys))
              | false -> x::(merge (xs, y::ys))

let rec mergesort l =
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns ->
      let listpair = (split l)
      match listpair with
        | ([],[]) -> []
        | (xs, []) -> xs
        | ([], ys) -> ys
        | (xs, ys) ->
          let l1 = (mergesort xs)
          let l2 = (mergesort ys)
          merge (l2,l1)
