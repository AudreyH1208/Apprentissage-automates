(* Le type d'une table d'observation *)
type observation_table = 
  {nb_let : int;
  prefix : Tries.trie;
  suffix : Tries.trie;
  f : (My_base.word*My_base.word, bool) Hashtbl.t;
  row_number : (My_base.word, int) Hashtbl.t;
  unique_rows : (bool list, int) Hashtbl.t;
  mutable next_row_id : int;}


(* Renvoie le nombre de lignes *distinctes* de la table. *)
(* Les lignes sont numérotées de 0 à nb_rows - 1. *)
val nb_rows : observation_table -> int
(* Renvoie la taille de l'alphabet *)
val nb_letters : observation_table -> int
(* Renvoie le numéro de la ligne correspondant à un mot de S∪SΣ. *)
val get_row_number : observation_table -> My_base.word -> int
(* Prend en entrée un mot w∈S∪SΣ et un mot e∈E *)
(* et renvoie f(w·e). *)
val compute_f :observation_table -> My_base.word -> My_base.word -> bool
(* Applique une fonction g successivement à tous les mots de l'ensemble S. *)
val iter_s : observation_table -> (My_base.word -> unit) -> unit
(* Applique une fonction g successivement à tous les mots de l'ensemble SΣ. *)
val iter_sa : observation_table -> (My_base.word -> unit) -> unit
(* Affichage. *)
val print_word : My_base.word -> string
val print_observation_table : observation_table -> unit

(* Renvoie un mot e de l'ensemble E tel que row(u.e) ≠ row(u'.e). *)
val separate_rows : observation_table -> My_base.word -> My_base.word -> My_base.word option

(* Ajoute à S le mot w et met à jour la fonction d'appartenance et les lignes de la table d'observation. *)
val add_to_s : observation_table -> My_base.word -> (My_base.word -> bool) -> unit
(* Ajoute à E le mot w et met à jour la fonction d'appartenance et les lignes de la table d'observation. *)
val add_to_e : observation_table -> My_base.word -> (My_base.word -> bool) -> unit

(* Crée une table d'observation avec S = E = {[]}, f définie grace à teacher.member. *)
val initial_table : Teacher.teacher -> observation_table
