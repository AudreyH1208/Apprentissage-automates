type letter = int
type word = letter list

type dfa =
{q0 : int;
nb_letters : int;
nb_states : int;
accepting : bool array;
delta : int array array}

let rec aff_mot w = 
  match w with 
  | []-> print_newline()
  | x::xs -> Printf.printf "%d" x; aff_mot xs;;
  let print_dfa (a : dfa) =
    Printf.printf "Automate DFA\n";
    Printf.printf "État initial : q%d\n" a.q0;
    Printf.printf "Nombre d'états : %d\n" a.nb_states;
    Printf.printf "Nombre de lettres : %d\n" a.nb_letters;
    Printf.printf "États acceptants : {";
    Array.iteri (fun i acc ->
      if acc then Printf.printf "q%d " i
    ) a.accepting;
    Printf.printf "}\n";
    Printf.printf "Transitions :\n";
    for q = 0 to a.nb_states - 1 do
      for l = 0 to a.nb_letters - 1 do
        Printf.printf "  δ(q%d, %d) = q%d\n" q l a.delta.(q).(l)
      done;
    done
  ;;


let to_letter i =
  char_of_int (i + int_of_char 'a')

  
let graphviz a ?etats:(etats=[||]) ?lettres:(lettres=[||]) filename =
  let open Printf in
  let out = open_out filename in
  fprintf out "digraph a {\nrankdir = LR;\n";
  (* noms des états *)
  let nom_etat =
    if etats = [||] then string_of_int
    else (fun i -> etats.(i)) in
  (* noms des lettres *)
  let nom_lettre =
    if lettres = [||] then (fun i -> String.make 1 (to_letter i))
    else (fun i -> lettres.(i)) in
  (* etats *)
  for q = 0 to a.nb_states - 1 do
    let shape = if a.accepting.(q) then "doublecircle" else "circle" in
    fprintf out "node [shape = %s, label = %s] %i;\n" shape (nom_etat q) q
  done;
  (* etat initial *)
  fprintf out "node [shape = point]; I\n";
  fprintf out "I -> %i;\n" a.q0;
  (* transitions *)
  let labels = Array.make_matrix a.nb_states a.nb_states [] in
  for q = 0 to a.nb_states - 1 do
    for x = a.nb_letters - 1 downto 0 do
      let q' = a.delta.(q).(x) in
      if q' <> -1 then
        labels.(q).(q') <- nom_lettre x :: labels.(q).(q')
    done
  done;
  for q = 0 to a.nb_states - 1 do
    for q' = 0 to a.nb_states - 1 do
      let s = String.concat "," labels.(q).(q') in
      if s <> "" then
        fprintf out "%i -> %i [ label = \"%s\" ];\n" q q' s
    done
  done;
  fprintf out "}\n";
  close_out out

let genere_pdf input_file output_file =
  let command_string =
    Printf.sprintf "dot -Tpdf %s -o %s" input_file output_file in
  ignore (Sys.command command_string)


let rec delta_star a q w = 
  match w with 
  |[] -> q
  | x::xs -> delta_star a (a.delta.(q).(x)) xs
;;

let create_dfa nb_letters states is_accepting= 
  let delt = Array.make_matrix states nb_letters (-1) in 
  {q0 =0;
  nb_letters = nb_letters;
  nb_states = states;
  accepting = is_accepting;
  delta = delt}
;;

let add_transition automata q a q' = 
  automata.delta.(q).(a)<- q';;

let random_auto n q = 
    (*n : Nombre de lettres dans l'alphabet, q : nombre d'états dans l'automate*)
    let accepting_states = Array.make q false in 
    let nb_accepting = Random.int (q-1) in 
    for i =0 to nb_accepting do 
        accepting_states.(q-1-i)<- true;
    done;
    if Random.bool () then accepting_states.(0)<- true;
    let auto = create_dfa n q accepting_states in 
    for i =0 to q-1 do
        for j =0 to n-1 do 
            let q' = Random.int q in 
            add_transition auto i j q';
        done;
    done;
    auto;;

let dfa_1_to_dfa (a:Expr.dfa) = 
  {q0 = 0; 
  nb_states = Array.length a.delta_d ;
  nb_letters = Array.length a.delta_d.(0) ;
  accepting = a.accepting_d;
  delta = a.delta_d
  }

