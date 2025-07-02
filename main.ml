(* main.ml *)
open My_modules;;

(* let auto = {My_base.q0 =0;
  nb_letters = 2;
  nb_states = 2;
  accepting = [|true;false|];
  delta = [|[|1;0|];[|1;0|]|]} *)

(* let example_1 : My_base.dfa = {q0 =0;
  nb_letters = 2;
  nb_states = 4;
  accepting = [|false;true;false;true|];
  delta = [|[|1;1|];[|2;0|];[|3;1|]; [|0;2|]|]} *)

let ex = My_base.create_dfa 2 4 [|false;true;false;true|];;
My_base.add_transition ex 0 0 1;;
My_base.add_transition ex 0 1 1;;
My_base.add_transition ex 1 0 2;;
My_base.add_transition ex 1 1 0;;
My_base.add_transition ex 2 0 3;;
My_base.add_transition ex 2 1 1;;
My_base.add_transition ex 3 0 0;;
My_base.add_transition ex 3 1 2;;

(* let example_2 : My_base.dfa = {
    q0 = 0;
    nb_letters = 2;
    nb_states = 3;
    accepting = [| true; false; false |];
    delta = [|
      [| 0; 1 |];  (* état 0 : 0 → 0, 1 → 1 *)
      [| 1; 2 |];  (* état 1 : 0 → 1, 1 → 2 *)
      [| 2; 0 |];  (* état 2 : 0 → 2, 1 → 0 *)
    |];
  } *)

Random.self_init ();;


(*let example_3 = My_base.random_auto 2 4;;*)
My_base.graphviz ex "ini.viz";;
My_base.genere_pdf "ini.viz" "ini.pdf";; 
(* 
let example_expr = My_base.dfa_1_to_dfa (Expr.ex_to_auto "(ab* )|( bb*a)");;
(* "(ab* )|( bb*a)" "(b|aa)(b*ab*a)*"*)
My_base.graphviz example_expr "example_expr.viz";;
My_base.genere_pdf "example_expr.viz" "example_expr.pdf";;
let teach_e = Teacher.create_teacher example_expr *)

(* let teach_0 = Teacher.create_teacher auto *)
let teach_1 = Teacher.create_teacher ex
(* let teach_2 = Teacher.create_teacher example_2 *)
(* let teach_3 = Teacher.create_teacher example_3 *)

(* let ini1 = Obs.initial_table teach_1 *)

(* let ini2 = Obs.initial_table teach_2 *)

(* let ini3 = Obs.initial_table teach_3;; *)

(* let temps n q =
  Random.self_init ();
  let auto = My_base.random_auto n q in
  let teach = Teacher.create_teacher auto in
  let t = Sys.time() in 
  let auto_construit = Learner.l_star teach in 
  let t' = Sys.time() in
  ignore auto_construit;
  (t'-.t);;

let aff_array t = 
  let n = Array.length t in 
  Printf.printf "[";
  for i = 0 to n-1 do 
    Printf.printf "%f ," t.(i);
  done ;
  Printf.printf "]\n";;

let aff_array_int t = 
  let n = Array.length t in 
  Printf.printf "[";
  for i = 0 to n-1 do 
    Printf.printf "%d ," t.(i);
  done ;
  Printf.printf "]\n";;

let moyenne t = 
  let s = ref 0. in 
  let n = Array.length t in 
  for i = 0 to n-1 do 
    s:= !s +. t.(i)
  done;
  !s /. (float_of_int n)

let pire t = 
  let s = ref 0. in 
  let n = Array.length t in 
  for i = 0 to n-1 do 
    if t.(i)> !s then 
      s:= t.(i) 
  done;
  !s

let trace n nb_q r =
  let etats = Array.make nb_q 0 in 
  let durees = Array.make nb_q 0. in 
  let durees' = Array.make nb_q 0. in 
  for q = 2 to nb_q do 
    etats.(q-2) <- q;
    let t = Array.make r 0. in 
    for i=0 to r-1 do 
      t.(i) <- temps n q;
    done;
    durees.(q-2) <- moyenne t;
    durees'.(q-2) <- pire t;
  done;
  Printf.printf "Pire cas \n";
  aff_array durees';
  aff_array durees;
  aff_array_int etats;; *)

let () = 
  let () = Printexc.record_backtrace true in 
  (* print_dfa auto; *)
  (* print_dfa example; *)
  (* Obs.print_observation_table ini2; *)
  (* let b = Teacher.symetric_difference (Learner.construct_auto ini2) example in  *)
  (* Obs.print_observation_table ini1; *)
  (* Printf.printf "symetric difference :\n"; *)
  (* print_dfa b; *)
  let auto_construit = Learner.l_star teach_1 in 
  (* let auto_construit = Learner.learn teach_2 in  *)
(* 
  let auto_construit = Learner.l_star teach_3 in 
*)
  My_base.graphviz auto_construit "example.viz";
  My_base.genere_pdf "example.viz" "example.pdf";
  My_base.print_dfa auto_construit; 
(* 
  let auto_construit2 = Learner.l_star teach_e in 

  My_base.graphviz auto_construit2 "example_expr_app.viz";
  My_base.genere_pdf "example_expr_app.viz" "example_expr_app.pdf";
  My_base.print_dfa auto_construit *)

;;