(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Chaohe Shi, Id Number: 260579632 *) (* Edit this line. *)

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
    | x :: xs -> x + sumlist xs

let rec pairlists twolists =
  match twolists with
    | ([], []) -> []
    | ([], x :: xs) -> failwith "Error -- lists are not of the same length"
    | (x :: xs, []) -> failwith "Error -- lists are not of the same length"
    | (x :: xs, y :: ys) -> (x, y) :: pairlists(xs, ys)

let w_mean weights data =
  let rec sumpairedlist pairedlist =
    match pairedlist with
      | [] -> 0.0
      | (x, y) :: xs -> x * y + sumpairedlist xs
  sumpairedlist(pairlists(weights, data)) / sumlist weights

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
  match pair with
    | (x, []) -> false
    | (x, y :: ys) -> if x = y then true else memberof(x, ys)

let rec remove(item, lst) =
  match lst with
    | [] -> []
    | x :: xs -> if item = x then remove(item, xs) else x :: remove(item, xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l =
  let rec helper(l, m) =
    match l with
      | [] -> m
      | x :: xs -> if x > m then helper(xs, x) else helper(xs, m)
  match l with
    | [] -> failwith "Error -- empty list"
    | (x :: xs) -> helper(xs, x)

(* Question 4. *)  (* Do not edit this line. *)

let rec selsort l =
  match l with
    | [] -> []
    | l -> findMax l :: selsort(remove(findMax l, l))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
  match twolists with
    | ([], []) -> []
    | (x, []) -> []
    | ([], y) -> []
    | (x :: xs, y) -> if memberof(x, y) then x :: common(xs, y) else common(xs, y)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion. Using isort or 
some other sort defeats the whole purpose. *)

let rec split l =
  (*Let fast pointer move twice as fast as slow pointer. Then when fast pointer 
  reaches the end of the list, the slow pointer will be pointing at the middle *)
  let rec firsthalf x y =
    match x, y with
      | [], _ -> []
      | xs, ([] | [_]) -> []
      | x :: xs, y1 :: y2 :: ys -> x :: firsthalf xs ys
  let rec secondhalf x y =
    match x, y with
      | [], _ -> []
      | xs, ([] | [_]) -> xs
      | x :: xs, y1 :: y2 :: ys -> secondhalf xs ys
  firsthalf l l, secondhalf l l

let rec merge twolists =
  match twolists with
    | ([], []) -> []
    | (x, []) -> x
    | ([], y) -> y
    | (x :: xs, y :: ys) -> if x < y then x :: merge(xs, y :: ys) else y :: merge(x :: xs, ys)

let rec mergesort l =
  match l with
    | [] -> []
    | [n] -> [n] (* Without this you will go into an infinite loop. *)
    | n :: ns -> 
      let (l1, l2) = split l
      merge(mergesort l1, mergesort l2)

(* Test. *)

(* 

**************************************************
functions' types (eyeball check)
**************************************************

val sumlist : l:float list -> float
val pairlists : 'a list * 'b list -> ('a * 'b) list
val w_mean : weights:float list -> data:float list -> float
val memberof : 'a * 'a list -> bool when 'a : equality
val remove : item:'a * lst:'a list -> 'a list when 'a : equality
val findMax : l:'a list -> 'a when 'a : comparison
val selsort : l:'a list -> 'a list when 'a : comparison
val common : 'a list * 'a list -> 'a list when 'a : equality
val split : l:'a list -> 'a list * 'a list
val merge : 'a list * 'a list -> 'a list when 'a : comparison
val mergesort : l:'a list -> 'a list when 'a : comparison

**************************************************
functions' test cases (copy all to terminal, hit enter)
**************************************************

sumlist [1.0; 2.5] = 3.5,
sumlist [1.0] = 1.0,
sumlist ([]:float list) = 0.0,
pairlists ([1.0; 2.0], [3.0; 4.0]) = [(1.0, 3.0); (2.0, 4.0)],
pairlists (['a'], ['b']) = [('a', 'b')],
pairlists (([]:float list), ([]:float list)) = [],
abs(w_mean [1.0; 2.0; 3.0] [4.0; 5.0; 6.0] - 5.333333333) < 0.000000001,
abs(w_mean [1.0; 1.0; 1.0] [2.0; 3.0; 4.0] - 3.0) < 0.000000001,
abs(w_mean [1.0] [2.0] - 2.0) < 0.000000001,
memberof (1, [1; 2; 3]) = true,
memberof (1, []) = false,
memberof (1, [2; 3; 4]) = false,
remove (2, [1; 3; 2; 4; 2]) = [1; 3; 4],
remove ("bob", ["alice"; "bob"; "cat"]) = ["alice"; "cat"],
remove ('a', ['a']) = remove ('b', []),
findMax [2.0; 1.0] = 2.0,
findMax ['a'] = 'a',
findMax [-10; 9; 3; 9] = 9,
selsort ([1 .. 2 .. 25] @ [1 .. 5 .. 21]) = [25; 23; 21; 19; 17; 16; 15; 13; 11; 9; 7; 6; 5; 3; 1],
selsort ['a'] = ['a'],
selsort ([]:int list) = [],
common ([3;4;5;7;2],[1;3;5;7;9;1]) = [3; 5; 7] || common ([3;4;5;7;2],[1;3;5;7;9;1]) = [7; 5; 3],
common (['b'],['a']) = [],
common ([2],([]:int list)) = common ([],([]:int list)),
split [15 .. -2 .. 1] = ([15; 11; 7; 3], [13; 9; 5; 1]) || split [15 .. -2 .. 1] = ([15; 13; 11; 9], [7; 5; 3; 1]),
mergesort [15 .. -2 .. 1] = [1; 3; 5; 7; 9; 11; 13; 15],
mergesort ['a'] = ['a'],
mergesort ([]:int list) = [];;

 *)
