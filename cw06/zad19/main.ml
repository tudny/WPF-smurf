
 type 'a tree = Node of 'a  * 'a tree list;;


let rosnaca t = 
  let rec aux (Node (v, lst)) = 
    List.fold_left
    (fun (maks_acc, lst_acc, maks_ja, lst_ja) (maks_spod_syna, lst_spod_syna, maks_z_synem, lst_z_synem) -> 
      match lst_z_synem with
      | [] -> invalid_arg "Miał być z synem!"
      | syn :: rest_syna ->
        let (ojcowa_dl, ojcowa_lst) = 
          if v < syn then 
            (maks_spod_syna + 1, v :: lst_z_synem)
          else
            (1, [v])
        in
          let (n_maks, n_lst) = max (maks_acc, lst_acc) (maks_spod_syna, lst_spod_syna) in
          let (n_maks, n_lst) = max (n_maks, n_lst) (ojcowa_dl, ojcowa_lst) in
          let (n_ojcowa, n_ojcowa_lst) = max (ojcowa_dl, ojcowa_lst) (maks_ja, lst_ja) in
          (n_maks, n_lst, n_ojcowa, n_ojcowa_lst)
    )
    (0, [], 1, [v])
    (List.map (aux) lst) 
  in let (_, score, _, _) = aux t in score;;


let t = Node (5, [Node(4, [Node(5, [Node(6, [])])]); Node(6, []); Node(7, [])]);;
rosnaca t;;

let t = Node (5, [Node(4, [Node(5, [Node(6, [])])]); Node(6, [Node(10, [Node(20, [Node(70, [Node(10, [])])])])]); Node(7, [])]);;
rosnaca t;;



















 (* let roznaca t = 
  let rec aux (Node (v, lst)) = 
    List.fold_left 
    (fun (dl_maks, lst_maks) (dl_maks_od_syna, lst_maks_od_syna, dl_syn, lst_syn) -> 
      match lst_syn with
      | [] -> invalid_arg "Tak nie można, pobite gary!"
      | syn :: syn_rest -> 
        let (ojcowa_dl, ojcowa_lst) =
          if v < syn then (dl_syn + 1, v :: lst_syn)
          else (1, [v])
        in 
        let (n_dl_maks, n_lst_maks) =
          if ojcowa_dl > dl_maks then (ojcowa_dl, ojcowa_lst)
          else (dl_maks, lst_maks)
        in 
        if dl_maks_od_syna > n_dl_maks then (dl_maks, lst_maks)
        else (n_dl_maks, n_lst_maks)
    )
    (0, [])
    lst
  in aux t;;




let t = Node (5, [Node(4, [Node(5, [Node(6, [])])]); Node(6, []); Node(7, [])])
roznaca t;; *)

