(* main.ml *)
open My_modules;;

(* let auto = {My_base.q0 =0;
  nb_letters = 2;
  nb_states = 2;
  accepting = [|true;false|];
  delta = [|[|1;0|];[|1;0|]|]} *)

(* let example_1 = 
   {My_base.q0 =0;
  nb_letters = 2;
  nb_states = 3;
  accepting = [|false;false;true|];
  delta = [|[|1;0|];[|2;0|];[|2;0|]|]} *)

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

(* Random.self_init ();;
let example_3 = My_base.random_auto 2 5;;
My_base.graphviz example_3 "example_random.viz";;
My_base.genere_pdf "example_random.viz" "example_random.pdf";; *)

let example_expr = My_base.dfa_1_to_dfa (Expr.ex_to_auto "((b|a)b*)");;
(* "(ab* )|( bb*a)" "(b|aa)(b*ab*a)*"*)
My_base.graphviz example_expr "example_expr.viz";;
My_base.genere_pdf "example_expr.viz" "example_expr.pdf";;
let teach_e = Teacher.create_teacher example_expr

(* let teach_0 = Teacher.create_teacher auto *)
(* let teach_1 = Teacher.create_teacher example_1 *)
(* let teach_2 = Teacher.create_teacher example_2 *)
(* let teach_3 = Teacher.create_teacher example_3 *)

(* let ini1 = Obs.initial_table teach_1 *)

(* let ini2 = Obs.initial_table teach_2 *)

(* let ini3 = Obs.initial_table teach_3;; *)

let () = 
  let () = Printexc.record_backtrace true in 
  (* print_dfa auto; *)
  (* print_dfa example; *)
  (* Obs.print_observation_table ini2; *)
  (* let b = Teacher.symetric_difference (Learner.construct_auto ini2) example in  *)
  (* Obs.print_observation_table ini1; *)
  (* Printf.printf "symetric difference :\n"; *)
  (* print_dfa b; *)
  (* let auto_construit = Learner.learn teach_0 in  *)
  (* let auto_construit = Learner.learn teach_2 in  *)
  (* let auto_construit = Learner.l_star teach_3 in 

  My_base.graphviz auto_construit "example_random2.viz";
  My_base.genere_pdf "example_random2.viz" "example_random2.pdf";
  My_base.print_dfa auto_construit; *)

  let auto_construit2 = Learner.l_star teach_e in 

  My_base.graphviz auto_construit2 "example_expr_app.viz";
  My_base.genere_pdf "example_expr_app.viz" "example_expr_app.pdf";
  My_base.print_dfa auto_construit2

;;