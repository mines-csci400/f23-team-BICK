open Util (* see util.ml *)

(******************)
(** Starter Code **)
(******************)

(* Red-Black Tree Data Type *)
(* type data = int *)
type color = Red | Black
type 'v rbtree =
  | Empty
  | Rnode of 'v rbtree * 'v * 'v rbtree (* Red node *)
  | Bnode of 'v rbtree * 'v * 'v rbtree (* Black node *)

(* Return the color of an rbtree node *)
let rbt_color t =
  match t with
    Rnode(_,_,_) -> Red
  | Bnode(_,_,_) | Empty -> Black

(* Result of comparing two values *)
(* if a < b, then cmp a b -> Lesser *)
(* if a = b, then cmp a b -> Equal *)
(* if a > b, then cmp a b -> Greater *)
type cmp_result =
  | Lesser | Equal | Greater

(* compare two data elements of type 'v *)
type 'v cmp_fun = 'v -> 'v -> cmp_result

(* Test if t satisfies the red-black tree invariants.
 *
 *  1. Does every red node have only black children?
 *  2. Does every path from root to leaf have the same number of black nodes?
 *)
let rbt_is_invariant (t : 'v rbtree) : bool =
  (* TODO, remove false *)
  false

(* Test if red-black tree t is sorted. *)
let rec rbt_is_sorted (cmp : 'v cmp_fun) (t : 'v rbtree) : bool =
  let rec is_bst prev = function
    | Empty -> true
    | Rnode (left, value, right) | Bnode (left, value, right) ->
        let left_sorted = is_bst prev left in
        let prev_check = 
          match prev with
          | Some prev_val -> cmp prev_val value = Lesser
          | None -> true
        in
        let right_sorted = is_bst (Some value) right in
        left_sorted && prev_check && right_sorted
  in
  is_bst None t

(* Search for element x in red-black tree t.
 *
 * Return true if the tree contains x and false if it does not. *)
let rec rbt_search (cmp : 'v cmp_fun) (t : 'v rbtree) (x:'v) : bool =
  let rec inorder_traversal = function
    | Empty -> false
    | Rnode (left, value, right) | Bnode (left, value, right) ->
        let left_found = inorder_traversal left in
        if cmp x value = Lesser then true
        else if left_found then true
        else inorder_traversal right
  in
  inorder_traversal t

(* Balance constructor for a red-black tree *)
let rbt_balance (c:color) (l : 'v rbtree) (v : 'v ) (r : 'v rbtree) : 'v rbtree =
  match (c,l,v,r) with
  | (Black, Rnode(Rnode(a, x, b), y, c), z, d)
  | (Black, Rnode(a, x, Rnode(b, y, c)), z, d)
  | (Black, a, x, Rnode(Rnode(b, y, c), z, d))
  | (Black, a, x, Rnode(b, y, Rnode(c, z, d))) ->
    Rnode(Bnode(a, x, b), y, Bnode(c, z, d))
  | _ -> Bnode(l, v, r)
  

(* Insert element x into a red-black tree
 *
 * Do not reinsert (duplicate) existing elements *)
let rbt_insert (cmp : 'v cmp_fun) (t : 'v rbtree) (x:'v) : 'v rbtree =
  (* TODO, remove t *)
  t

(***********)
(** Tests **)
(***********)

(* See description in testing.ml *)

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

(* Convert tree to string *)
let rec str_x_rbtree f t =
  let h s l d r =
    Printf.sprintf "%s(%s,%s,%s)"
      s (str_x_rbtree f l) (f d) (str_x_rbtree f r)
  in
  match t with
  | Empty -> "Empty"
  | Rnode(l,d,r) ->
     h "Rnode" l d r
  | Bnode(l,d,r) ->
     h "Bnode" l d r

let str_int_rbtree t = str_x_rbtree string_of_int t
let str_str_rbtree t = str_x_rbtree str_str t

let tree_arg_printer f =
  (fun (t,x) -> str_pair (str_x_rbtree f) f (t,x))

let int_tree_arg_printer = tree_arg_printer string_of_int
let str_tree_arg_printer = tree_arg_printer str_str

exception InvalidRBTreeError

(* To check that test case inputs are valid, sorted red-black trees *)
let check_rbtree (cmp : 'v cmp_fun) (t: 'v rbtree) =
  if (rbt_is_invariant t) && (rbt_is_sorted cmp t) then t
  else raise InvalidRBTreeError

(* To check that test case expected outputs are valid, sorted red-black trees *)
let eq_rbtree cmp_fun t1 t_expected =
  t1 = (check_rbtree cmp_fun t_expected)

(* Leaf node constants to make test inputs more readable *)
let r0 =  Rnode(Empty, 0,Empty)
let r1 =  Rnode(Empty, 1,Empty)
let r2 =  Rnode(Empty, 2,Empty)
let r3 =  Rnode(Empty, 3,Empty)
let r4 =  Rnode(Empty, 4,Empty)
let r5 =  Rnode(Empty, 5,Empty)
let r6 =  Rnode(Empty, 6,Empty)
let r7 =  Rnode(Empty, 7,Empty)
let r8 =  Rnode(Empty, 8,Empty)
let r9 =  Rnode(Empty, 9,Empty)
let r10 = Rnode(Empty,10,Empty)
let r11 = Rnode(Empty,11,Empty)
let r12 = Rnode(Empty,12,Empty)
let r13 = Rnode(Empty,13,Empty)
let r14 = Rnode(Empty,14,Empty)
let r15 = Rnode(Empty,15,Empty)
let r16 = Rnode(Empty,16,Empty)
let r17 = Rnode(Empty,17,Empty)
let r18 = Rnode(Empty,18,Empty)
let r19 = Rnode(Empty,19,Empty)
let r20 = Rnode(Empty,20,Empty)
let r21 = Rnode(Empty,21,Empty)
let r22 = Rnode(Empty,22,Empty)
let r23 = Rnode(Empty,23,Empty)
let r24 = Rnode(Empty,24,Empty)
let r25 = Rnode(Empty,25,Empty)
let r26 = Rnode(Empty,26,Empty)
let r27 = Rnode(Empty,27,Empty)
let r28 = Rnode(Empty,28,Empty)
let r29 = Rnode(Empty,29,Empty)
let r30 = Rnode(Empty,30,Empty)
let r31 = Rnode(Empty,31,Empty)
let r32 = Rnode(Empty,32,Empty)
let r33 = Rnode(Empty,33,Empty)
let r34 = Rnode(Empty,34,Empty)

let b0 =  Bnode(Empty, 0,Empty)
let b1 =  Bnode(Empty, 1,Empty)
let b2 =  Bnode(Empty, 2,Empty)
let b3 =  Bnode(Empty, 3,Empty)
let b4 =  Bnode(Empty, 4,Empty)
let b5 =  Bnode(Empty, 5,Empty)
let b6 =  Bnode(Empty, 6,Empty)
let b7 =  Bnode(Empty, 7,Empty)
let b8 =  Bnode(Empty, 8,Empty)
let b9 =  Bnode(Empty, 9,Empty)
let b10 = Bnode(Empty,10,Empty)
let b11 = Bnode(Empty,11,Empty)
let b12 = Bnode(Empty,12,Empty)
let b13 = Bnode(Empty,13,Empty)
let b14 = Bnode(Empty,14,Empty)
let b15 = Bnode(Empty,15,Empty)
let b16 = Bnode(Empty,16,Empty)
let b17 = Bnode(Empty,17,Empty)
let b18 = Bnode(Empty,18,Empty)
let b19 = Bnode(Empty,19,Empty)
let b20 = Bnode(Empty,20,Empty)

let ra =  Rnode(Empty,"a",Empty)
let rb =  Rnode(Empty,"b",Empty)
let rc =  Rnode(Empty,"c",Empty)
let rd =  Rnode(Empty,"d",Empty)
let re =  Rnode(Empty,"e",Empty)
let rf =  Rnode(Empty,"f",Empty)
let rg =  Rnode(Empty,"g",Empty)
let rh =  Rnode(Empty,"h",Empty)
let ri =  Rnode(Empty,"i",Empty)
let rj =  Rnode(Empty,"j",Empty)
let rk =  Rnode(Empty,"k",Empty)

let ba =  Bnode(Empty,"a",Empty)
let bb =  Bnode(Empty,"b",Empty)
let bc =  Bnode(Empty,"c",Empty)
let bd =  Bnode(Empty,"d",Empty)
let be =  Bnode(Empty,"e",Empty)
let bf =  Bnode(Empty,"f",Empty)
let bg =  Bnode(Empty,"g",Empty)
let bh =  Bnode(Empty,"h",Empty)
let bi =  Bnode(Empty,"i",Empty)
let bj =  Bnode(Empty,"j",Empty)
let bk =  Bnode(Empty,"k",Empty)

let rbt_is_invariant_int_tests =
  ("rbt_is_invariant_int",
   rbt_is_invariant,
   (=), (=),
   Some(str_int_rbtree,
        str_bool),
   [
     (Some("simple tree"),
      Bnode(r1, 2, r3),
      Ok(true));
     (* TODO *)
   ])

let rbt_is_invariant_str_tests =
  ("rbt_is_invariant_str",
   rbt_is_invariant,
   (=), (=),
   Some(str_str_rbtree,
        str_bool),
   [
     (Some("simple tree"),
      Bnode(ra, "b", rc),
      Ok(true));
     (* TODO *)
   ])

let rbt_is_sorted_int_tests =
  ("rbt_is_sorted_int",
   (fun t -> rbt_is_sorted int_cmp t),
   (=), (=),
   Some(str_int_rbtree,
        str_bool),
   [
     (Some("simple tree"),
      Bnode(r1, 2, r3),
      Ok(true));
     (Some("empty tree"),
      Bnode(Empty, 0, Empty),
      Ok(true));
     (Some("simple true"),
      Bnode(r1, 8, r10),
      Ok(true));
     (Some("simple false"),
      Bnode(r1, 10, r8),
      Ok(false));
     (Some("complex true"),
      Bnode(Bnode(r1, 3, r5), 11, Bnode(r12, 13, Bnode(r14, 15, r16))),
      Ok(true));
     (Some("complex false"),
      Bnode(Bnode(r1, 3, r5), 11, Bnode(Bnode(r14, 15, r16), 13, r12)),
      Ok(false));
   ])


let rbt_is_sorted_str_tests =
  ("rbt_is_sorted_str",
   (fun t -> rbt_is_sorted str_cmp t),
   (=), (=),
   Some(str_str_rbtree,
        str_bool),
   [
     (Some("simple tree"),
      Bnode(ra, "b", rc),
      Ok(true));
     (Some("empty tree"),
      Bnode(Empty, "a", Empty),
      Ok(true));
     (Some("simple true"),
      Bnode(rc, "e", rk),
      Ok(true));
     (Some("simple false"),
      Bnode(rc, "k", re),
      Ok(false));
     (Some("complex true"),
      Bnode(Bnode(ra, "b", rc), "d", Bnode(re, "f", Bnode(rg, "h", ri))),
      Ok(true));
     (Some("complex false"),
      Bnode(Bnode(ra, "b", rc), "d", Bnode(Bnode(rg, "h", ri), "f", re)),
      Ok(false));
   ])

let rbt_search_int_tests =
  ("rbt_search_int",
   (fun (t,x) -> rbt_search int_cmp (check_rbtree int_cmp t) x),
   (=), (=),
   Some(int_tree_arg_printer, str_bool),
   [
     (Some("simple tree"),
      (Bnode(r1, 2, r3), 2),
      Ok(true));
     (Some("empty tree"),
      (Bnode(Empty, 0, Empty), 2),
      Ok(false));
     (Some("simple true"),
      (Bnode(r3, 7, r8), 8),
      Ok(true));
     (Some("simple false"),
      (Bnode(r3, 7, r8), 6),
      Ok(false));
     (Some("complex true"),
      (Bnode(Bnode(r1, 2, r3), 4, Bnode(Bnode(r5, 6, r7), 8, r9)), 6),
      Ok(true));
     (Some("complex false"),
      (Bnode(Bnode(r1, 2, r3), 4, Bnode(Bnode(r5, 6, r7), 8, r9)), 10),
      Ok(false));
   ])

let rbt_search_str_tests =
  ("rbt_search_str",
   (fun (t,x) -> rbt_search str_cmp (check_rbtree str_cmp t) x),
   (=), (=),
   Some(str_tree_arg_printer, str_bool),
   [
     (Some("simple tree"),
      (Bnode(ra, "b", rc), "b"),
      Ok(true));
     (Some("empty tree"),
      (Bnode(Empty, "a", Empty), "b"),
      Ok(false));
     (Some("simple true"),
      (Bnode(rc, "d", rh), "d"),
      Ok(true));
     (Some("simple false"),
      (Bnode(rc, "d", rh), "i"),
      Ok(false));
     (Some("complex true"),
      (Bnode(Bnode(ra, "b", rc), "d", Bnode(Bnode(re, "f", rg), "h", ri)), "e"),
      Ok(true));
     (Some("complex false"),
      (Bnode(Bnode(ra, "b", rc), "d", Bnode(Bnode(re, "f", rg), "h", ri)), "j"),
      Ok(false));
   ])

let rbt_balance_tester t =
  (* Note: rbt_balance does not always return a balanced tree!  We
     only enforce balance when we reach the black grandparent in a
     red-red invariant violation. *)
  match t with
    Empty -> raise InvalidRBTreeError (* we don't ever balance empty trees *)
  | Rnode(l,v,r) | Bnode(l,v,r)
    -> rbt_balance (rbt_color t) l v r

let rbt_balance_int_tests =
  ("rbt_balance_int",
   rbt_balance_tester,
   (=), (=),
   Some(str_int_rbtree, str_int_rbtree),
   [
     (Some("Case A"),
      Bnode(Rnode(r1,2,Empty),
            3,
            Empty),
      Ok(Rnode(b1,2,b3)));
     (Some("Case B"),
      Rnode(Bnode(b1,5,Empty),
            1,
            Empty),
      Ok(Bnode(Bnode(Bnode(Empty,1,Empty),5,Empty),1,Empty)));
      (Some("Case C"),
      Bnode(Bnode(Empty,5,Empty),
            1,
            b2),
      Ok(Bnode(Bnode(Empty,5,Empty),1,Bnode(Empty,2,Empty))));
      (Some("Case D"),
      Rnode(Rnode(r30,23,Empty),
            61,
            b2),
      Ok(Bnode(Rnode(Rnode(Empty,30,Empty),23,Empty),61,Bnode(Empty,2,Empty))));
      (Some("Case E"),
      Rnode(Bnode(Empty,1,Empty),
            3,
            r1),
      Ok(Bnode(Bnode(Empty,1,Empty),3,Rnode(Empty,1,Empty))));
      (Some("Case F"),
      Bnode(Bnode(b2,0,b4),
            3,
            b1),
      Ok(Bnode(Bnode(Bnode(Empty,2,Empty),0,Bnode(Empty,4,Empty)),3,Bnode(Empty,1,Empty))));
   ])

let rbt_balance_str_tests =
  ("rbt_balance_str",
   rbt_balance_tester,
   (=), (=),
   Some(str_str_rbtree, str_str_rbtree),
   [
     (Some("Case A"),
      Bnode(Rnode(ra,"b",Empty),
            "c",
            Empty),
      Ok(Rnode(ba,"b",bc))); 
      (Some("Case B"),
      Rnode(Bnode(ba,"c",bd),
            "e",
            Empty),
      Ok(Bnode(Bnode(Bnode(Empty,"a",Empty),"c",Bnode(Empty,"d",Empty)),"e",Empty))); 
      (Some("Case C"),
      Bnode(Bnode(Empty,"z",Empty),
            "x",
            bd),
      Ok(Bnode(Bnode(Empty,"z",Empty),"x",Bnode(Empty,"d",Empty)))); 
      (Some("Case D"),
      Rnode(Rnode(rc,"a",rb),
            "d",
            Empty),
      Ok(Bnode(Rnode(Rnode(Empty,"c",Empty),"a",Rnode(Empty,"b",Empty)),"d",Empty))); 
      (Some("Case E"),
      Rnode(Rnode(Empty,"a",Empty),
            "b",
            Empty),
      Ok(Bnode(Rnode(Empty,"a",Empty),"b",Empty)));
      (Some("Case F"),
      Bnode(Rnode(ba,"a",bb),
            "b",
            bc),
      Ok(Bnode(Rnode(Bnode(Empty,"a",Empty),"a",Bnode(Empty,"b",Empty)),"b",Bnode(Empty,"c",Empty))));
   ])

let rbt_insert_tester f =
  fun (t,x) ->
  check_rbtree f
    (rbt_insert f
       (check_rbtree f t)
       x)

let int_rbt_insert_tester = rbt_insert_tester int_cmp
let int_rbt_insert_tests_eq = eq_rbtree int_cmp
let rbt_insert_int_tests =
  ("rbt_insert_int",
   int_rbt_insert_tester,
   int_rbt_insert_tests_eq,  (=),
   Some(int_tree_arg_printer,
        str_int_rbtree),
   [
     (Some("simple tree"),
      (Bnode(r1, 2, Empty), 3),
      Ok(Bnode(r1, 2, r3)));
     (* TODO *)
   ])

let str_rbt_insert_tester = rbt_insert_tester str_cmp
let str_rbt_insert_tests_eq = eq_rbtree str_cmp
let rbt_insert_str_tests =
  ("rbt_insert_str",
   str_rbt_insert_tester,
   str_rbt_insert_tests_eq,  (=),
   Some(str_tree_arg_printer,
        str_str_rbtree),
   [
     (Some("simple tree"),
      (Bnode(ra, "b", Empty), "c"),
      Ok(Bnode(ra, "b", rc)));
     (* TODO *)
   ])
