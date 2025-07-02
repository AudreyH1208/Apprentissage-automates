type letter = int
type word = letter list

type dfa =
{q0 : int;
nb_letters : int;
nb_states : int;
accepting : bool array;
delta : int array array}

val aff_mot : word -> unit

val print_dfa : dfa -> unit 

val graphviz :dfa -> ?etats:string array -> ?lettres:string array -> string -> unit

val genere_pdf : string -> string -> unit

val delta_star : dfa -> letter -> word -> int

val create_dfa : int->int->bool array -> dfa

val add_transition : dfa -> letter -> letter -> letter -> unit
val random_auto : int->int-> dfa

val dfa_1_to_dfa : Expr.dfa -> dfa