
type state = int
type dfa =
  {delta_d : state array array;
  accepting_d : bool array}

val ex_to_auto : string-> dfa