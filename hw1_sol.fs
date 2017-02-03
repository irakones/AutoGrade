module hw1Sol

(* Question 1 illustrating lists *)
let rec sumlist l =
  match l with
  | [] -> 0.0 
  | (x::xs) -> x + sumlist(xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y) :: (pairlists (xs,ys))

let w_mean weights data =
  let denom = sumlist weights
  let pairs = pairlists (weights, data)
  (sumlist (List.map (fun (x,y) -> x * y) pairs))/denom
  
(* w_mean [1.0;1.5;2.5;0.5;1.5] [10.3;11.7;2.0;5.0;6.5] *)

(* Question 2. *)

let rec memberof pair =
  match pair with
  | (n,[]) -> false
  | (n,(x::xs)) -> if (x = n) then true else memberof(n,xs)

let rec remove(item, lst) = 
  match lst with
  | [] -> []
  | (x::xs) -> if (x = item) then remove(item, xs) else x::(remove(item, xs))

(* Question 3. *)

let findMax l =
  let rec helper(l,m) =
    match l with
      | [] -> m
      | (x::xs) ->
          if (m < x) then helper(xs,x) else helper(xs,m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)
  
let rec selsort l =
  match l with
  | [] -> []
  | _ -> let m = (findMax l)
         m::(selsort(remove(m,l)))

(* Question 5. *)

let rec common twolists =
  match twolists with
    | (l,[]) -> []
    | ([],l) -> []
    | ((x::xs),l) ->
        if memberof(x,l)
        then
          x::(common(xs,remove(x,l)))
        else
          common(xs,l)

(* Question 6. Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
  match l with
  | [] -> ([],[])
  | x :: [] -> (x::[],[])
  | (x::y::l) ->
  let (odds,evens) = split(l)
  in
      (x::odds,y::evens)


let rec merge twolists =
  match twolists with
  | ([],R) -> R
  | (L,[]) -> L
  | (x::xs, y::ys) ->
    if (x < y)
    then
      x::merge(xs, y::ys)
    else
      y::merge(x::xs,ys)

let rec mergesort l =
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns ->
    let (M,N) = split(l) in
      let M2 = mergesort(M)
      let N2 = mergesort(N)
      in
      merge(M2,N2)

