
open List;;

type 'a tree =  Node of 'a * 'a tree list;;

let rec fold_tree f (Node (x, l)) =
  f x (map (fold_tree f) l);;

let is_regualar (Node(v, li) as tr) =
  let cnt = length li + 1 in
  let rec add n acc = if n < 1 then acc else add (n - 1) ((Node(0, [])) :: acc) in
  let create_empty = add (cnt - 2) [tr] in
  let super_tr = Node(0, create_empty) in
  let regualar t li =
    match li with
    | [] -> true
    | _ :: _ -> fold_left (&&) (length li + 1 = cnt) li
  in
  fold_tree (regualar) super_tr
;;

let t1 = Node(1, [Node(2, [])]);;

is_regualar t1;;


