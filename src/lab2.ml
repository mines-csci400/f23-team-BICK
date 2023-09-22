open Util (* see util.ml *)

(******************)
(** Starter Code **)
(******************)

(*** Implementing higher-order functions ***)

let rec map (f : 'a->'b) (l : 'a list) : 'b list =
  match l with
  | [] -> []
  | head::tail -> (f head) :: (map f tail)
  
  

let rec filter (f : 'a->bool) (l : 'a list) : 'a list =
  match l with 
  | [] -> []
  | head::tail ->  
     if f head then
       head :: filter f tail
     else
       filter f tail
    

let rec fold_left (f: 'y ->'x->'y) (y:'y) (l:'x list) : 'y =
  match l with
  |[] -> y
  | head::tail -> fold_left f (f y head) tail

let rec fold_right (f : 'x->'y->'y) (y:'y) (l:'x list) : 'y =
  match l with
  | [] -> y
  | head::tail -> f head (fold_right f y tail)


(*** Using higher-order functions ***)


(* Concatenate two lists. *)
let append (l1 : 'a list) (l2 : 'a list) : 'a list =
  fold_left (fun acc x -> acc @ [x]) l1 l2

(* rev_append l1 l2 reverses l1 and concatenates it with l2 *)
let rev_append (l1 : 'a list) (l2 : 'a list) : 'a list =
  fold_left (fun acc x -> x :: acc) l2 l1

(* Concatenate a list of lists. *)
let rec flatten (l : 'a list list) : 'a list =
  (* TODO, ISA replace [] *)
  match l with
  | [] -> []
  | head::tail -> head @ flatten tail


(* Insertion Sort *)

(* Insert elt into sorted list l in sorted order *)
let rec insert (cmp : 'a->'a->bool) (elt :'a) (l:'a list) : 'a list =
  match l with 
  | [] -> [elt]
  | head::tail ->  
     if cmp elt head then
       elt :: l
     else
       head :: (insert cmp elt tail)

let insertionsort (cmp : 'a -> 'a -> bool) (l : 'a list) : 'a list =
  fold_left (fun insert x -> insert @ [x]) [] l
  
(* Selection Sort *)

(* Select the initial element from l based on cmp.  Return a tuple of
   the initial element and the list with the initial element
   removed. *)
let select (cmp : 'a->'a->bool) (l:'a list) : 'a * 'a list =
  match l with
  | [] -> invalid_arg "select"
  | a::d ->
     (* TODO, replce (a,d) *)
     (a,d)

let rec selectionsort (cmp : 'a->'a->bool) (l:'a list) : 'a list =
  (* TODO, replace l *)
  l


(* Quicksort *)

(* Partion list l around elt.  Return a tuple consisting of all
   elements before elt and all elements after elt. *)
let pivot (cmp : 'a->'a->bool) (elt :'a) (l:'a list) : 'a list * 'a list =
  let create_lists (before, after) x =
    if cmp x elt then (x::before, after)
    else (before, x::after)
  in
  fold_left create_lists ([],[]) l

(* The simple implementation of quicksort recurses on the two sublists
   and appends the sorted results. *)
let rec quicksort_simple (cmp : 'a->'a->bool) (l : 'a list) : 'a list =
  (* TODO, replace l *)
  l

(***********)
(** Tests **)
(***********)

(* See description in testing.ml *)

let list_cmp cmp l1 l2 =
  (List.sort cmp l1) = (List.sort cmp l2)

let int_list_cmp l1 l2 =
  list_cmp (-) l1 l2


let map_tests =
  ("map", (fun (f,l)->map f l), (=), (=),
   Some((fun (f,l) -> str_int_list l),
        str_int_list),
   [
     (Some("simple list"), ((fun x -> 1+x), [1;2;3;4;5]), Ok [2;3;4;5;6]);
     (Some("subtraction test"), ((fun x -> x-1), [1;2;3;4;5]), Ok [0;1;2;3;4]);
     (Some("mult test"), ((fun x -> x*2), [1;2;3;4;5]), Ok [2;4;6;8;10]); 
     (Some("small test"), ((fun x -> x*3), [7; 8]), Ok [21;24]); 
     (Some("empty test"), ((fun x -> x-1), []), Ok []);
     (Some("last test"), ((fun x -> x/3), [3;6;9;12]), Ok [1;2;3;4]);


  ])

let filter_tests =
  ("filter", (fun (f,l)->filter f l), (=), (=),
   Some((fun (f,l) -> str_int_list l),
        str_int_list),
   [
     (Some("simple list"), ((fun x -> (x mod 2)=0), [1;2;3;4;5]), Ok [2;4]);
       (* TODO: Add more tests *)
  ])

let fold_left_tests =
  ("fold_left", (fun (f,y,l)->fold_left f y l), (=), (=),
   Some((fun (f,y,l) -> str_pair string_of_int str_int_list (y,l)),
        string_of_int),
   [
     (Some("+"), ((+), 0, [1;2;3]), Ok 6);
     (Some("-"), ((-), 0, [1;2;3]), Ok (-6));
     (Some("-"), ((-), 0, []), Ok (0));
     (Some("+"), ((+), 3, [0]), Ok (3));
     (Some("/"), ((/), 6, [1;2;3]), Ok (1));
  ])

let fold_right_tests =
  ("fold_right", (fun (f,y,l)->fold_right f y l), (=), (=),
   Some((fun (f,y,l) -> str_pair string_of_int str_int_list (y,l)),
        string_of_int),
   [
     (Some("+"), ((+), 0, [1;2;3]), Ok 6);
     (Some("-"), ((-), 0, [1;2;3]), Ok 2);
     (Some("+"), ((+), 0, []), Ok (0));
     (Some("-"), ((-), 3, [0]), Ok (-3));
     (Some("/"), ((/), 1, [3;2;1]), Ok (1));
  ])


let append_tests =
  ("append", (fun (l1,l2)->append l1 l2), (=), (=),
   Some((fun x -> str_pair str_int_list  str_int_list x),
        str_int_list),
   [
     (Some("simple list"), ([1;2],[3;4]), Ok [1;2;3;4]);
     (Some("empty list"), ([],[]), Ok []);
     (Some("one empty list"), ([1;2],[]), Ok [1;2]);
     (Some("equal lists"), ([1;2],[1;2]), Ok [1;2;1;2]);
     (Some("other empty list"), ([],[1;2]), Ok [1;2;]);
  ])

let rev_append_tests =
  ("rev_append", (fun (l1,l2)->rev_append l1 l2), (=), (=),
   Some((fun x -> str_pair str_int_list  str_int_list x),
        str_int_list),
   [
     (Some("simple list"), ([1;2],[3;4]), Ok [2;1;3;4]);
     (Some("empty list"), ([],[]), Ok []);
     (Some("one empty list"), ([1;2],[]), Ok [2;1]);
     (Some("equal lists"), ([1;2],[1;2]), Ok [2;1;1;2]);
     (Some("other empty list"), ([],[1;2]), Ok [1;2;]);
  ])

let flatten_tests =
  ("flatten", (fun l -> flatten l), (=), (=),
   Some((fun l -> "[" ^ str_x_list (str_int_list) l ";" ^ "]" ),
        str_int_list),
   [
     (Some("simple list"), [[1;2];[3;4]], Ok [1;2;3;4]);
     (Some("simple list 2"), [[3;4]; [1;2]], Ok [3;4;1;2]);
     (Some("empty list"), [[]; []], Ok []);
     (Some("one empty list"), [[]; [1;2]], Ok [1;2]);
     (Some("cloned list"), [[1;2]; [1;2]], Ok [1;2;1;2]);
   ]
  )

let sort_test_cases = [
    (Some("simple list"), ((<), [1;3;4;2;5]), Ok [1;2;3;4;5]);
    (Some("longer <"), ((<), [-1;1;2;-7]), Ok ([-7;-1; 1; 2]));
    (Some("longer >"), ((>), [2;1;-1;9]), Ok ([9;2; 1; -1]));
    (Some("empty list"), ((>), []), Ok ([]));
    (Some("equal value"), ((>), [3;3;3]), Ok ([3;3;3]));
    (Some("one positive"), ((<), [-4;8;-3;-1]), Ok ([-4;-3;-1;8]));
    (Some("one negative"), ((>),  [9;7;-6;5]), Ok ([9;7;5;-6]));
  ]

let insert_tests =
  ("insert", (fun (cmp,elt,l)->insert cmp elt l), (=), (=),
   Some(((fun (cmp,elt,l) -> str_pair string_of_int str_int_list (elt,l)),
         (fun y -> str_int_list y)
     )),
   [
     (Some("simple <"), ((<), 0, [-1;1;2]), Ok ([-1; 0; 1; 2]));
     (Some("simple >"), ((>), 0, [2;1;-1]), Ok ([2; 1; 0; -1]));
     (Some("empty list"), ((>), 1, []), Ok ([1]));
     (Some("equal value"), ((>), 3, [3;3;3]), Ok ([3;3;3;3]));
     (Some("all negative"), ((<), 8, [-4;-3;-1]), Ok ([-4;-3;-1;8]));
     (Some("all positive"), ((>), -6, [9;7;5]), Ok ([9;7;5;-6]));
   ])

let insertionsort_tests =
  ("insertionsort", (fun (cmp,l) -> insertionsort cmp l), (=), (=),
   Some((fun (cmp,l) -> str_int_list l),
        str_int_list),
   sort_test_cases)


let select_test_eq (s1,l1) (s2,l2) =
  (s1 = s2) && (int_list_cmp l1 l2)

let select_tests =
  ("select", (fun (cmp,l)->select cmp l), select_test_eq, (=),
   Some(((fun (cmp,l) -> str_int_list l),
         (fun (s,l) -> str_pair string_of_int str_int_list (s,l))
     )),
   [
     (Some("simple <"), ((<), [1;-1;2]), Ok (-1,[2;1]));
     (Some("simple >"), ((>), [1;-1;2]), Ok (2,[1;-1]));
     (* TODO: Add more tests *)
   ])


let selectionsort_tests =
  ("selectionsort", (fun (cmp,l) -> selectionsort cmp l), (=), (=),
   Some((fun (cmp,l) -> str_int_list l),
        str_int_list),
   sort_test_cases)


let pivot_test_eq (a1,b1) (a2,b2) =
  (int_list_cmp a1 a2) && (int_list_cmp b1 b2)

let pivot_tests =
  ("pivot", (fun (cmp,elt,l)->pivot cmp elt l), pivot_test_eq, (=),
   Some(((fun (cmp,elt,l) -> str_pair string_of_int str_int_list (elt,l)),
         (fun y -> str_pair str_int_list  str_int_list y)
     )),
   [
     (Some("simple <"), ((<), 0, [-1;1;0;-2; 2]), Ok ([-2; -1],[2; 0; 1]));
     (Some("simple >"), ((>), 0, [-1;1;0;-2; 2]), Ok ([2; 1], [-2; 0; -1]));
     (Some("empty >"), ((>), 0, []), Ok ([], []));
     (Some("all >"), ((<), 0, [1;1;0;2; 2]), Ok ([], [2; 0; 1; 2; 1]));
     (Some("element not found"), ((>), 0, [-1;1;3;-2; 2]), Ok ([2; 3; 1],[-2; -1]));
  ])

let quicksort_simple_tests =
  ("quicksort_simple", (fun (cmp,l) -> quicksort_simple cmp l), (=), (=),
   Some((fun (cmp,l) -> str_int_list l),
        str_int_list),
   sort_test_cases)
