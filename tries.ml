type trie = {
  mutable final : bool;
  children : (My_base.letter, trie) Hashtbl.t;
}

let create_node () = {
  final = false;
  children = Hashtbl.create 10;
}
let rec inside t word = 
  match word with 
  | [] -> t.final = true
  | x::xs -> begin
    match Hashtbl.find_opt t.children x with 
      | Some child -> inside child xs
      | None -> false
  end
  ;;

let rec insert t word =
  match word with 
  | [] -> t.final <- true
  | x::xs -> 
    let child = begin
      match Hashtbl.find_opt t.children x with 
      | Some t' -> t'
      | None -> let n = create_node () in
        Hashtbl.add t.children x n;
        n
      end
    in 
    insert child xs
  ;;

let words t = 
  let rec aux prefix tr = 
    let current = if tr.final then [List.rev prefix] else [] in
    let rest =
      Hashtbl.fold (fun a child acc ->
        (aux (a :: prefix) child) @ acc
      ) tr.children []
    in
    current @ rest
  in
  aux [] t
;;

let fold f t acc = 
  let rec aux prefix t acc = 
    let acc' = if t.final then f (List.rev prefix) acc else acc
    in 
    Hashtbl.fold (fun a child acc_inner ->
      aux (a :: prefix) child acc_inner
    ) t.children acc'
  in
  aux [] t acc
;;
let iter f t = 
  let rec aux prefix t = 
    if t.final then f (List.rev prefix);
    Hashtbl.iter (fun a child  ->
      aux (a :: prefix) child 
    ) t.children
  in
  aux [] t
;;
