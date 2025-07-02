let construct_auto t = 
  let nb_etats = Obs.nb_rows t in 
  let nb_letters = Obs.nb_letters t in
  let acc = Array.make nb_etats false in 
  let delta = Array.make_matrix nb_etats nb_letters (-1) in
  let test_fin w =
    let row_number = Obs.get_row_number t w in
    acc.(row_number) <- (Obs.compute_f t w [])
  in
  Obs.iter_s t test_fin;
  let constru w =
    let ligne = Obs.get_row_number t w in 
    for j = 0 to nb_letters -1 do 
      let q' = Obs.get_row_number t (w@[j]) in 
      delta.(ligne).(j) <- q';
    done;
  in
  Obs.iter_s t constru ;

  {My_base.q0 = Obs.get_row_number t [];
  nb_letters = nb_letters;
  nb_states = nb_etats;
  accepting = acc;
  delta = delta}
;;

exception Notclosed of My_base.word
exception Notconsistent of My_base.word

let check_closed t =
  let test = ref false in 
  let aux l_w u = 
    if Obs.get_row_number t u = l_w then 
      test := true;
  in
  let aux2 w = 
    test := false;
    Obs.iter_s t (aux (Obs.get_row_number t w));
    if not !test then 
      (raise  (Notclosed w) )
      
  in 
  Obs.iter_sa t aux2 
;;

let check_consistent t =
  let m = Obs.nb_letters t in 
  let aux u v = 
    if Obs.get_row_number t v = Obs.get_row_number t u then 
      for i = 0 to m-1 do 
        let w1 =  (u@[i]) in 
        let w2 =  (v@[i]) in 
        match Obs.separate_rows t w1 w2 with 
        | None -> ()
        | Some w -> 
        raise (Notconsistent (i::w))
      done;
  in 
  Obs.iter_s t (fun u -> (Obs.iter_s t (aux u)))
;;

let all_prefix w = 
  let list_pref = ref [w] in 
  let rec aux u = 
    match u with 
    | []-> ()
    | _::xs -> list_pref := (List.rev xs):: !list_pref; aux xs
  in 
  aux (List.rev w);
  !list_pref
;;

let rec make_closed_and_consistent table (teacher:Teacher.teacher) =
  try 
    check_closed table;
    check_consistent table ;
    ()
  with 
    | Notclosed w ->  Obs.add_to_s table w teacher.member; 
      make_closed_and_consistent table teacher
    | Notconsistent w -> Obs.add_to_e table w teacher.member ;
      make_closed_and_consistent table teacher
;;
let l_star (teacher:Teacher.teacher) = 
  let t = Obs.initial_table teacher in 
  let rec aux t =
    make_closed_and_consistent t teacher;
    let auto = construct_auto t in 
    My_base.print_dfa auto;
    match (teacher.counter_example auto) with 
    | None -> auto (*L'automate reconnait le langage mystère*)
    | Some w ->
        (*Ajout du contre-exemple à S et de tous ses préfixes*)
        List.iter (fun x-> Obs.add_to_s t x teacher.member) (all_prefix w);
        aux t
  in 
  aux t 
;;