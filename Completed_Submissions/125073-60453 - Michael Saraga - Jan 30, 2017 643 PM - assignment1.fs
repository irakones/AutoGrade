(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Michael Saraga, Id Number: 260648783 *) (* Edit this line. *)

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
    | []    -> 0.0                    // Base case: 0(id for +) on empty list                  
    | x::xs -> (x + (sumlist xs))     // add x to sum of cdr(l) elements

let rec pairlists twolists =
  match twolists with
    | ([],[])        -> []                              // Base case, fails with value restriction error if ([],[]) is untyped
    | ([],x::xs)     -> failwith "Error -- lists are not of the same length"
    | (x::xs, [])    -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::(pairlists (xs,ys))      // cons pair(x,y) with rec call on pairlists
      
let w_mean weights data =        
  pairlists (weights,data)                  // send paired data to pipe
  |> List.map (fun (x,y) -> x*y)            // multiply pairs, return list
  |> sumlist                                // sum list elements
  |> (fun x -> (x/(sumlist weights)))       // divide sum by sum of the weights

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =         
  match pair with                                   
    | (item,[])    -> false                       // no element is in the empty list
    | (item,x::xs) -> if (item = x) then true     // return if found,
                      else (memberof (item,xs))   // else search list cdr recursively

let rec remove(item, lst) =    
  match lst with
    | []    -> []                                    // base case, if the list is empty there's nothing to remove
    | x::xs -> if (item = x) then remove(item,xs)    // append elements not to be removed, else recursively go through other elems
               else (x::remove(item,xs))

(* Question 3. *)  (* Do not edit this line. *)

let findMax l =  
  let rec helper(l,m) =                              // tail recursive findMax
    match l with
      | []    -> m                                   // base case, return max_elem passed through rec calls
      | y::ys -> if (y > m) then helper(ys,y)        // check if current elem > m, pass greatest to next call
                 else helper(ys,m)

  match l with                                       // helper gets first list elem on first call
  | []    -> failwith "Error -- empty list"
  | x::xs -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =                                    // selsort implementation that removes duplicates
  match l with
    | []    -> []
    | x::xs -> let max = (findMax l)                   // find max in l
               if (x = max) then                       // append max, but not current elem if they are the same
                 (max)::(selsort (remove(max,xs)))
               else 
                 (max)::(selsort (x::remove(max,xs)))  // call selsort on cons x and the rest

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =   
  let multi_common =                           // find a list of all elements in l1 also in l2 (possible duplicates)
    match twolists with
      | ([],[])    -> []                       // base cases
      | ([],l2)    -> []                       // if one of the two lists are empty, intersect is also empty
      | (l1,[])    -> []
      | (x::xs,l2) -> if (memberof (x,l2)) then // include current elem in multi_common if its in l2 
                        (x::(common (xs,l2)))
                      else 
                        (common (xs,l2))
  match multi_common with
    | [] -> []                                // base cases
    | [x] -> [x]
    | (x::xs) -> (x::remove(x,xs))            // remove said duplicates to get list intersection

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =                               
  match l with 
    | []         -> ([],[])                  // base cases
    | [x]        -> ([x],[])                
    | x::(y::ys) -> let (lx,ly) = (split ys) // pattern match on lists in tuple while poping rec calls on stack
                    ((x::lx),(y::ly))        // return (cons(x,list of even elems),cons(y,list of odd elems))

let rec merge twolists =                     // twolists==(lx,ly) ~~~> assume lx,ly are sorted listed (increasing order)
  match twolists with
    | ([],[])       -> []                    // base cases : merging a lst with [] is just lst
    | (x::xs,[])    -> x::xs
    | ([],y::ys)    -> y::ys
    | (x::xs,y::ys) -> if (x < y) then       // choose largest elem from front of list, append to merge(sublists)
                         (x::(merge (xs,y::ys)))
                       else
                         (y::(merge (x::xs,ys)))

let rec mergesort l =
  match l with
  | []      -> []                            // base case
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns   -> let (lft,rht) =               // pattern match on split result to get lists lx,ly with property (|lx|-|ly|)<=1
                 (split (n::ns))             // GOOD OLD MERGESORT :D THANK YOU!
               (merge ((mergesort lft),(mergesort rht)))

