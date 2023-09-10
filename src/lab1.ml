open Util (* see util.ml *)

(******************)
(** Starter Code **)
(******************)

exception IndexError

(* Return the i'th element of a list *)
let rec nth (i : int)  (l: 'a list) : 'a =
  match l with
    [] -> raise IndexError
  | a::d -> 
    if i = 0 then a else nth (i - 1) d


(* Append two lists *)
let rec append (l1 : 'a list) (l2: 'a list) : 'a list =
  match l1 with
  | [] -> l2
  | head::tail -> head :: append tail l2


(* Reverse a list *)
let rec reverse (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | head::tail -> append (reverse tail) [head]

(* Length of a list *)
let rec length (l : 'a list) : int  =
  match l with
  |  [] -> 0
  |  _::tail -> 1 + length tail


(* Return the part of list l beginning at index 0 and ending at index
   iend *)
let rec list_prefix (iend : int) (l : 'a list) : 'a list =
  if iend < 0 then raise IndexError
  else if iend = 0 then []
  else
    match l with
      [] -> if iend = 0 then []
            else raise IndexError
    | a::d ->
       a :: list_prefix (iend-1) d

(* Return the part of list l beginning at istart and running through
   the end of the list *) 
let rec list_suffix (istart : int) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | fir::whole -> 
    if istart <= 0 then l else list_suffix (istart - 1) whole


(* Merge sorted lists l1 and l2 based on cmp.  The result is a sorted
   list containing all elements from both l1 and l2. *)
let rec merge (cmp : 'a->'a->bool) (l1 : 'a list) (l2 : 'a list) : 'a list =
  match (l1, l2) with 
  | ([],_) -> l2
  | (_,[]) -> l1
  | (x::xl1, y::yl2) -> 
    if cmp x y then
      x :: merge cmp xl1 l2 
    else
      y :: merge cmp l1 yl2


(* Sort list l via mergesort

   cmp is a function that compares two elements of list l.  When cmp
   returns true, its first argument comes first in the sorted list.
   When cmp returns false, its second argument comes first in the
   sorted list.  *)

let rec mergesort (cmp : 'a->'a->bool) (l:'a list) : 'a list =
  match l with
  | [] -> l
  | [_] -> l  
  | _ ->
      let mid = length l / 2 in
      let left = list_prefix mid l in
      let right = list_suffix mid l in
      let sorted_left = mergesort cmp left in
      let sorted_right = mergesort cmp right in
      merge cmp sorted_left sorted_right


(***********)
(** Tests **)
(***********)

(* See description in testing.ml *)

let nth_tests =
  ("Nth", (fun (i,l)->nth i l), (=), (=),
   Some((fun x -> str_pair string_of_int str_int_list x),
        string_of_int),
   [
     (Some("simple list"), (0, [1;2;3;4;5]), Ok 1);
     (Some("error"), (-1, [1;2;3;4;5]), Error IndexError);
       (* TODO: Add more tests *)
  ])

let append_tests =
  ("append", (fun (l1,l2)->append l1 l2), (=), (=),
   Some((fun x -> str_pair str_int_list  str_int_list x),
        str_int_list),
   [
     (Some("simple list"), ([1;2],[3;4]), Ok [1;2;3;4]);
     (Some("Different Sized Lists"), ([2;3;5;6],[7;8;9]), Ok [2;3;5;6;7;8;9]);
     (Some("Repeating Nums"), ([1;2;3],[1;2;3]), Ok [1;2;3;1;2;3]);
     (Some("Empty Lists"), ([],[]), Ok []);
  ])

let reverse_tests =
  ("reverse", reverse, (=), (=), Some(str_int_list,str_int_list),
   [
     (Some("simple list"), [1;2;3;4;5], Ok[5;4;3;2;1]);
       (* TODO: Add more tests *)
  ])

let length_tests =
  ("length", length, (=), (=), Some(str_int_list,string_of_int),
   [
     (Some("simple list"), [1;2;3;4;5], Ok 5);
       (* TODO: Add more tests *)
  ])

let list_prefix_tests =
  ("list_prefix", (fun (iend,l) -> list_prefix iend l), (=), (=),
   Some((fun x -> str_pair string_of_int  str_int_list x),
        str_int_list),
   [
     (Some("simple list"), (2,[1;2;3;4;5]), Ok [1;2]);
     (None, (0,[1;2;3;4;5]), Ok []);
     (None, (4,[1;2;3;4;5]), Ok [1;2;3;4]);
     (None, (5,[1;2;3;4;5]), Ok [1;2;3;4;5]);
     (None, (-1,[1;2;3;4;5]), Error IndexError);
     (None, (6,[1;2;3;4;5]), Error IndexError);
     (None, (10,[1;2;3;4;5]), Error IndexError);
  ])

let list_suffix_tests =
  ("list_suffix", (fun (istart,l) -> list_suffix istart l), (=), (=),
   Some((fun x -> str_pair string_of_int  str_int_list x),
        str_int_list),
   [
     (Some("simple list"), (2,[1;2;3;4;5]), Ok [3;4;5]);
       (* TODO: Add more tests *)
  ])

let merge_tests =
  ("merge", (fun (cmp,l1,l2) -> merge cmp l1 l2), (=), (=),
   Some((fun (cmp,l1,l2) -> str_pair str_int_list str_int_list (l1, l2)),
        str_int_list),
   [
     (Some("simple list"), ((<),[1;3],[2;4;5]), Ok [1;2;3;4;5]);
       (* TODO: Add more tests *)
  ])


let mergesort_tests =
  ("mergesort", (fun (cmp,l) -> mergesort cmp l), (=), (=),
   Some((fun (cmp,l) -> str_int_list l),
        str_int_list),
   [
     (Some("simple list"), ((<),[1;3;4;2;5]), Ok [1;2;3;4;5]);
     (* TODO: Add more tests *)
   ])
