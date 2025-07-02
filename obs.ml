type observation_table = 
  {nb_let : int;
  prefix : Tries.trie;
  suffix : Tries.trie;
  f : (My_base.word*My_base.word, bool) Hashtbl.t;
  row_number : (My_base.word, int) Hashtbl.t;
  unique_rows : (bool list, int) Hashtbl.t;
  mutable next_row_id : int;}

let add_row t w l = 
  match Hashtbl.find_opt t.unique_rows l with 
  | None -> Hashtbl.replace t.unique_rows l t.next_row_id;
      Hashtbl.replace t.row_number w t.next_row_id ;
      t.next_row_id <- t.next_row_id +1
  |Some i -> Hashtbl.replace t.row_number w i 
;;

exception Compute_row
let compute_row t w = 
  let rec aux l =
    match l with 
    | [] -> []
    | e::es -> begin match Hashtbl.find_opt t.f (w,e) with 
      |Some x -> x :: (aux es) 
      | None -> raise Compute_row
    end
  in
  let e_words = Tries.words t.suffix in
  aux e_words
;;

let update_rows t w = 
  let row = compute_row t w in 
  add_row t w row
;;

let create_table nb_letters = 
  let prefix = Tries.create_node() in 
  Tries.insert prefix [];
  let suffix = Tries.create_node() in 
  Tries.insert suffix [];
  {nb_let = nb_letters;
  prefix = prefix;
  suffix  = suffix;
  f = Hashtbl.create 10;
  row_number = Hashtbl.create 10;
  unique_rows = Hashtbl.create 10;
  next_row_id = 0;}

let nb_rows t = 
  Hashtbl.length t.unique_rows
;;
let nb_letters t = 
  t.nb_let
;;

exception Pas_row_nb 
let get_row_number t w = 
  match Hashtbl.find_opt t.row_number w with 
    | Some n -> n
    | None -> raise Pas_row_nb
;;
exception Pas_compute 
let compute_f t w e = 
  match Hashtbl.find_opt t.f (w,e) with 
    | Some b -> b
    | None -> raise Pas_compute
;;


let iter_s t g =
  let words = Tries.words t.prefix in 
  List.iter g words
;;
let iter_sa t g =
  let words = Tries.words t.prefix in 
  let new_words = ref [] in 
  for i = 0 to (nb_letters t) - 1 do 
    new_words := (List.map (fun x -> x@[i]) words) @ !new_words
  done;
  List.iter g !new_words
;;


let print_word w =
  let chars = List.map (fun x -> String.make 1 (char_of_int (x+ int_of_char 'a'))) w in
  String.concat "" chars
;;
let print_observation_table (table : observation_table) =
  
  (* Récupérer tous les suffixes *)
  let suffixes = Tries.words table.suffix in

  (* En-tête *)
  Printf.printf "%-10s |" "Prefix";
  List.iter (fun e ->
    Printf.printf " %-5s" (print_word e)
  ) suffixes;
  print_newline ();

  (* Ligne de séparation *)
  let total_columns = List.length suffixes in
  Printf.printf "%s\n" (String.make (11 + 6 * total_columns) '-');

  (* Affichage des lignes S ∪ SΣ *)
  let aux w = let row_id =
    match Hashtbl.find_opt table.row_number w with
    | Some id -> string_of_int id
    | None -> "?"
  in
  Printf.printf "%-10s |" (print_word w);
  List.iter (fun e ->
    let value =
      match Hashtbl.find_opt table.f (w, e) with
      | Some true -> "1"
      | Some false -> "0"
      | None -> "?"
    in
    Printf.printf " %-5s" value
  ) suffixes;
  Printf.printf "   (row %s)\n" row_id in
  iter_s table aux;

  (* Ligne de séparation *)
  let total_columns = List.length suffixes in
  Printf.printf "%s\n" (String.make (11 + 6 * total_columns) '-');

  iter_sa table aux;
;;


exception Different of My_base.word
let separate_rows t u u' =
  let mot_dif e = 
    if compute_f t u e <> compute_f t u' e then raise (Different e)
  in 
  if get_row_number t u = get_row_number t u' 
    then None 
  else begin
    try Tries.iter mot_dif t.suffix;
    None 
  with 
  |Different e -> Some e
end 

let add_to_s table w f = 
  if not (Tries.inside table.prefix w) then (
  Tries.insert table.prefix w;
  let aux x = 
    let wx = w@x in 
    let f_wx = (f wx) in
    Hashtbl.add table.f (w,x) f_wx;
    for i = 0 to nb_letters table -1 do 
      let w' = w@[i] in
      let wax = w'@x in 
      let f_wax = (f wax) in
      Hashtbl.replace table.f (w',x) f_wax;
    done;
  in 
  Tries.iter aux table.suffix;
  let l = compute_row table w in 
  add_row table w l;
  for i=0 to table.nb_let -1 do 
    let w' = w@[i] in
    let l' = compute_row table w' in 
    add_row table w' l';
  done;)
;;

let recreate_rows t = 
  t.next_row_id<- 0;
  Hashtbl.clear t.unique_rows;
  Hashtbl.clear t.row_number;

  iter_s t (update_rows t);
  iter_sa t (update_rows t);;

let add_to_e table w f = 
  if not (Tries.inside table.suffix w) then (
  Tries.insert table.suffix w;
  let aux x=
    let xw = x@w in 
        Hashtbl.replace table.f (x,w) (f xw);
        for i = 0 to nb_letters table -1 do 
          let x' = x@[i] in
          let xaw = x'@w in 
          let f_xaw = (f xaw) in
          Hashtbl.replace table.f (x',w) f_xaw;
        done;
  in 
  Tries.iter aux table.prefix;
  recreate_rows table)
;;

let initial_table (teach: Teacher.teacher) = 
  let table = create_table teach.nb_letters_t in 
  let f_0 = (teach.member []) in
  Hashtbl.add table.f ([],[]) f_0 ;
  update_rows table [];
  for i=0 to table.nb_let - 1 do 
    Hashtbl.add table.f ([i], []) (teach.member [i]);
    update_rows table [i];
  done;
  table
;;
