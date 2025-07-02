
val construct_auto : Obs.observation_table -> My_base.dfa

val check_closed : Obs.observation_table -> unit
val check_consistent : Obs.observation_table -> unit

val make_closed_and_consistent : Obs.observation_table ->Teacher.teacher -> unit

val l_star : Teacher.teacher -> My_base.dfa
