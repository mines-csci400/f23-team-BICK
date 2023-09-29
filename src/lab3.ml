open Util (* see util.ml *)

(******************)
(** Starter Code **)
(******************)

(*** Binary Search Trees ***)

(* Binary Tree Data Type *)
(* 'v is the type of values in the tree *)
type 'v binary_tree =
  | Empty
  | Node of 'v binary_tree * 'v * 'v binary_tree


(* Result of comparing two values *)
(* if a < b, then cmp a b -> Lesser *)
(* if a = b, then cmp a b -> Equal *)
(* if a > b, then cmp a b -> Greater *)
type cmp_result =
  | Lesser | Equal | Greater

(* compare two data elements of type 'v *)
type 'v cmp_fun = 'v -> 'v -> cmp_result

(* Higher Order Functions on Binary Trees *)

(* Apply function f to every data element of tree t and collect the
   results in a list following an inorder traversal of the tree *)
let map_inorder (f : 'v -> 'a) (t : 'v binary_tree) : 'a list =
  (* TODO, replace [] *)
  []

(* Apply function f to every data element of tree t and collect the
   results in a list following a reverse order traversal of the tree *)
let map_revorder (f : 'v -> 'a) (t : 'v binary_tree) : 'a list =
  (* TODO, replace [] *)
  []


(* Binary Search Trees *)

(* Test if t is a binary search tree.
 *
 * That is, (recursively) are all elements to the left less the the
 * current element and all elements to the right greater than the
 * current element *)

let is_bst (cmp : 'v cmp_fun) (t : 'v binary_tree)  : bool =
  false


(* Return the maximum element of a binary search tree. *)
let rec bst_max (t : 'v binary_tree) : 'v option =
  match t with
  | Empty -> None
  | Node (_, value, Empty) -> Some value
  | Node (_, _, right) -> bst_max right


(* Return the minimum element of a binary search tree. *)
let rec bst_min t : 'v option =
match t with
  | Empty -> None
  | Node (Empty, value, _) -> Some value
  | Node (left, _, _) -> bst_min left

(* Insert element x into binary search tree t.
 *
 * Do not reinsert (duplicate) existing elements *)
let rec bst_insert (cmp : 'v cmp_fun) (t : 'v binary_tree) (x : 'v) : 'v binary_tree =
  (* TODO, replace t *)
  t

(* Search for element x in binary search tree t.
 *
 * Return true if the tree contains x and false if it does not. *)
let rec bst_search (cmp : 'v cmp_fun) (t : 'v binary_tree) (x : 'v) : bool =
  (* TODO, replace false *)
  false

(* Remove the minimum element of binary search tree t.
 *
 * Return a tuple containing the new binary tree and an option of the
 * removed element.  *)
let rec bst_remove_min (t : 'v binary_tree) : 'v binary_tree * 'v option =
  (* TODO, replace (t, None) ISA*)
  match t with
  | Empty -> (Empty, None)
  | Node (l, x, r) -> 
    match l with
    | Empty -> (r, Some x)
    | Node (_, _, _) -> 
      let (z, y) = bst_remove_min l in
        (Node (z, x, r), y)

let remove_x (t : 'v binary_tree) : 'v binary_tree * bool=
 match t with
 | Node(l,x,r) -> 
    match l with
      | Empty -> 
        match r with
        | Empty -> (Empty, true)
        | Node(_, _, _) -> (r, true)
      | Node(_, _, _) -> 
        match r with 
        | Empty -> (l, true)
        | Node(_, _, _) -> 
          let (w, z) = bst_remove_min r in (Node (l, x, w), true)


(* Remove the element x from binary search tree t
 *
 * Return a tuple containing the new binary tree and a bool that is
 * true if the element was found and false if the element was not
 * found *)
let rec bst_remove (cmp : 'v cmp_fun) (t : 'v binary_tree) (x : 'v)
        : 'v binary_tree * bool =
  match t with
  | Empty -> (Empty, false)
  | Node (l, y, r) ->
    match cmp x y with
      | Lesser -> let (newL, found) = bst_remove cmp l x in (Node(newL, y, r), found)
      | Greater -> let (newR, found) = bst_remove cmp r x in (Node(l, y, newR), found)
      | Equal -> remove_x t
      



(***********)
(** Tests **)
(***********)

(* See description in testing.ml *)

(* Convert a string tree to string *)
let rec str_int_binary_tree t =
  match t with
  | Empty -> "Empty"
  | Node(l,v,r) ->
     Printf.sprintf "Node(%s,%d,%s)"
       (str_int_binary_tree l)
       v
       (str_int_binary_tree r)

(* Convert tree to string *)
let rec str_str_binary_tree t =
  match t with
  | Empty -> "Empty"
  | Node(l,v,r) ->
     Printf.sprintf "Node(%s,\"%s\",%s)"
       (str_str_binary_tree l)
       v
       (str_str_binary_tree r)

let identity x = x

let map_printer =
  Some((fun (f,t) -> str_int_binary_tree t),
       str_int_list)

let tree_arg_printer_int =
  (fun (t,x) -> str_pair str_int_binary_tree string_of_int (t,x))

let tree_arg_printer_str =
  (fun (t,x) -> str_pair str_str_binary_tree identity (t,x))

let int_cmp : int->int->cmp_result =
  fun a b ->
  if a < b then Lesser
  else if a > b then Greater
  else Equal

let str_cmp : string->string->cmp_result =
  fun a b ->
  let c = compare a b in
  if c < 0 then Lesser
  else if c > 0 then Greater
  else Equal

let l0 = Node(Empty,0,Empty)
let l1 = Node(Empty,1,Empty)
let l2 = Node(Empty,2,Empty)
let l3 = Node(Empty,3,Empty)
let l4 = Node(Empty,4,Empty)
let l5 = Node(Empty,5,Empty)

let la = Node(Empty,"a",Empty)
let lb = Node(Empty,"b",Empty)
let lc = Node(Empty,"c",Empty)
let ld = Node(Empty,"d",Empty)
let le = Node(Empty,"e",Empty)
let lf = Node(Empty,"f",Empty)


let map_inorder_tests =
  ("map_inorder",
   (fun (f,t) -> map_inorder f t),
   (=), (=),
   map_printer,
   [
     (Some("simple tree"),
      (identity,
       Node(l1, 2, l3)),
      Ok([1;2;3]));
     (* TODO *)
  ])


let map_revorder_tests =
  ("map_revorder",
   (fun (f,t) -> map_revorder f t),
   (=), (=),
   map_printer,
   [
     (Some("simple tree"),
      (identity,
       Node(l1, 2, l3)),
      Ok([3;2;1]));
     (* TODO *)
  ])



let is_bst_int = is_bst int_cmp
let is_bst_tests_int =
  ("is_bst str",
   is_bst_int,
   (=), (=),
   Some(str_int_binary_tree,
        str_bool),
   [
     (Some("simple tree"),
      Node(Empty,
           1,
           Node(l2,
                3,
                Empty)),
      Ok(true));
     (* TODO *)
  ])

let is_bst_str = is_bst str_cmp
let is_bst_tests_str =
  ("is_bst str",
   is_bst_str,
   (=), (=),
   Some(str_str_binary_tree,
        str_bool),
   [
     (Some("simple tree"),
      Node(Empty,
           "a",
           Node(lb,
                "c",
                Empty)),
      Ok(true));
     (* TODO *)
  ])


let bst_max_tests =
  ("bst_max",
   bst_max,
   ((=) : 'v option -> 'v option -> bool),
   (=),
   Some(str_int_binary_tree,
        str_int_option),
   [
     (Some("simple tree"),
      Node(Empty,
           1,
           Node(l2, 3, Empty)),
      Ok(Some(3)));
     (* TODO *)
  ])


let bst_min_tests =
  ("bst_min",
   bst_min,
   ((=) : 'v option -> 'v option -> bool),
   (=),
   Some(str_int_binary_tree,
        str_int_option),
   [
     (Some("simple tree"),
      Node(Empty,
           1,
           Node(l2, 3, Empty)),
      Ok(Some(1)));
     (* TODO *)
  ])


let bst_insert_tests_int =
  ("bst_insert int",
   (fun (t,x) ->  bst_insert int_cmp t x),
   (=), (=),
   Some(tree_arg_printer_int, str_int_binary_tree),
   [
     (Some("simple tree"),
      (Node(l1, 2, l3),
       0),
      Ok (Node(Node(l0,1,Empty),
               2,
               l3)));
       (* TODO *)
  ])

let bst_insert_tests_str =
  ("bst_insert str",
   (fun (t,x) ->  bst_insert str_cmp t x),
   (=), (=),
   Some(tree_arg_printer_str, str_str_binary_tree),
   [
     (Some("simple tree"),
      (Node(lb, "c", ld),
       "a"),
      Ok (Node(Node(la,"b",Empty),
               "c",
               ld)));
     (* TODO *)
   ])


let bst_search_tests_int =
  ("bst_search",
   (fun (t,x) ->  bst_search int_cmp t x),
   (=), (=),
   Some(tree_arg_printer_int, str_bool),
   [
     (Some("simple tree"),
      (Node(l1, 2, l3),
       2),
      Ok true);
     (* TODO *)
   ]
  )


let bst_search_tests_str =
  ("bst_search",
   (fun (t,x) ->  bst_search str_cmp t x),
   (=), (=),
   Some(tree_arg_printer_str, str_bool),
   [
     (Some("simple tree"),
      (Node(la, "b", lc),
       "a"),
      Ok true);
     (* TODO *)
   ])

let bst_remove_min_tests =
  ("bst_remove_min",
   bst_remove_min,
   (=), (=),
   Some(str_int_binary_tree,
        (fun (t,x) -> str_pair str_int_binary_tree str_int_option (t,x))),
   [
     (Some("simple tree"),
      Node(l1, 2, l3),
      Ok (Node(Empty,2,l3),
          Some 1));
     (Some("empty tree"),
      Empty,
      Ok (Empty, None));
   ])


let bst_remove_tests_int =
  ("bst_remove",
   (fun(t,x) -> bst_remove int_cmp t x),
   (=), (=),
   Some(tree_arg_printer_int,
        (fun (t,f) -> str_pair str_int_binary_tree str_bool (t,f))),
   (* None, *)
   [
     (Some("simple tree"),
      (Node(l1, 2, l3), 1),
      Ok ((Node (Empty,2,l3)), true));
     (Some("simple tree, element not found"),
      (Node(l1, 2, l3), 5),
      Ok ((Node (l1,2,l3)), false));
     (Some("empty tree, element not found"),
      (Empty, 5),
      Ok (Empty, false));
     (Some("empty subtree, element  found"),
      (Node(Empty, 5, Empty), 5),
      Ok (Empty, true));
   ])


let bst_remove_tests_str =
  ("bst_remove",
   (fun(t,x) -> bst_remove str_cmp t x),
   (=), (=),
   Some(tree_arg_printer_str,
        (fun (t,f) -> str_pair str_str_binary_tree str_bool (t,f))),
   [
     (Some("simple tree"),
      (Node(la, "b", lc), "a"),
      Ok ((Node (Empty,"b",lc)), true));
     (* TODO *)
   ])
