type teacher = {
  nb_letters_t : int;
  member : My_base.word -> bool;
  counter_example : My_base.dfa -> My_base.word option;
  }

(* Renvoie le plus petit mot d'un langage à partir d'un automate fini déterministe. *)
val shortest_word : My_base.dfa -> My_base.word option
(* Renvoie le plus petit mot appartenant à la différence symétrique de deux automates *)
val symetric_difference : My_base.dfa -> My_base.dfa -> My_base.dfa
(* Renvoie l'enseignant correcpondant à l'automate. *)
val create_teacher : My_base.dfa -> teacher