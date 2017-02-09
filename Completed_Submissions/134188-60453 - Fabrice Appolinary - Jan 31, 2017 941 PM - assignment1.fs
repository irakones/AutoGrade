(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Fabrice Appolinary, Id Number: 260698996 *) (* Edit this line. *)

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
    |[] -> 0.0
    | x::xs -> 
             let sum = sumlist xs 
             x + sum
             ;;

let rec pairlists twolists = 
    match twolists with 
      | ([],[]) -> []
      | ([],x::xs) -> failwith "Error--lists are not the same length"
      | (x::xs, []) -> failwith "Error -- lists are not the same length"
      | (x::xs , y::ys) -> 
             let result = pairlists (xs,ys)
             (x,y)::result

let w_mean weights data = 
    let paired = pairlists (weights, data)
    let rec helper pair = 
        match pair with 
            |[] -> []
            | (x,y)::xs -> 
                let result = helper xs
                ((x:float) * (y :float))::result
                
    let resultList = helper paired
    let sum1 = sumlist resultList
    let sum2 = sumlist data

    float(sum1/sum2)

(* Question 2. *)  (* Do not edit this line. *)

let  memberof li =
      let myTuple : ele : int * mylist : int list = li 
      let rec helper1 listParr  = 
        match listParr with
                |[] -> false
                |x::xs -> 
                   if x = fst myTuple then true
                   else helper1 xs
      let lst = snd myTuple

      helper1 lst;;

let remove li = 
     //to check first if the element is in the list 
     let myTuple : ele : 'a * mylist : 'a list = li
     let rec rem n lst = 
         match lst with
             | []  -> []
             | h::tl when h = n -> rem n tl
             | h::tl -> h :: (rem n tl)
     rem (fst myTuple) (snd myTuple)
               

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper2(li,m) = 
     match li with 
       |[] -> m
       | x::xs -> if x < m then helper2(xs, m)
                  else helper2(xs,x)
  match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper2(xs,x)  
(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
     
     match l with 
       |[] -> []
       |x::xs -> 
             let largestElement = findMax l
             let newList = remove (largestElement, l)
             largestElement :: selsort newList

(* Question 5. *)  (* Do not edit this line. *)

let rec common  pair  = 
      let myTuple : mylist1 : 'a list * mylist2 : 'a list = pair

      let  memberoff li =
         let myTuplee : ele : 'a * mylist : 'a list = li 
         let rec helper1 listParr  = 
             match listParr with
                |[] -> false
                |x::xs -> 
                   if x = fst myTuplee then true else helper1 xs
         let lst = snd myTuplee
         helper1 lst

      let removee li = 
     //to check first if the element is in the list 
         let myTupleee : ele : 'a * mylist : 'a list = li
         let rec rem n lst = 
             match lst with
               | []  -> []
               | h::tl when h = n -> rem n tl
               | h::tl -> h :: (rem n tl)
         rem (fst myTupleee) (snd myTupleee)
                              
      match myTuple with 
          |([_],[_]) -> []
          |([],[]) -> []
          |([],_) -> []
          |(_,[]) -> []
          |(x :: xs,y::ys) -> 
                 if memberoff( x ,(snd myTuple)) then 
                        let newArray = removee(x,(snd myTuple)) 
                        x::common(xs,newArray)
                 else 
                      common(xs, snd myTuple)     


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
      match l with 
         |[] -> ([],[])
         |_ ->   let n = l.Length
                 (l.[0..n/2 - 1], l.[n/2..n - 1])

let rec merge twolists = 
    let myTupleee : ele : 'a list * mylist : 'a list = twolists

    let rec reverseList li = 
        match li with 
          |[] -> []
          |x::xs -> (reverseList xs)@[x]
    
    let rec aux l1  l2 result : 'a list = 
        match l1, l2 with 
          |[],[] -> result
          |[],h :: t | h :: t , [] -> aux [] t (h :: result)
          | h1 :: t1 , h2 :: t2 -> 
               if h1 < h2 then aux t1 l2 (h1 :: result)
               else     aux l1 t2 (h2 :: result)

    
    reverseList (aux (fst myTupleee) (snd myTupleee) [])


let rec mergesort l = 
  match l with
  | [] -> []
  | [n] -> [n](* Without this you will go into an infinite loop. *)
  | n::ns -> 
           let (first, second) = split l
           merge (mergesort first , mergesort second) 



let l = mergesort [1;2;3;1;3;4;5;6;7;8;1]