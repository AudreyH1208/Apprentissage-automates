type teacher = {
nb_letters_t : int;
member : My_base.word -> bool;
counter_example : My_base.dfa -> My_base.word option;
}


exception Trouve of My_base.word
let shortest_word (a:My_base.dfa) = 
    let file = Queue.create () in 
    Queue.push (a.q0, []) file;
    let vu = Array.make a.nb_states false in 
    try
      while not (Queue.is_empty file) do 
      let (q,u) = Queue.pop file in 
      if a.accepting.(q) 
        then raise (Trouve u) 
      else 
        (for i = 0 to a.nb_letters -1 do 
            let q' = a.delta.(q).(i) in
            if not vu.(q') then (
              vu.(q') <- true;
              Queue.push (q',(i::u)) file
            )
        done;
        )
      done;
    None
  with 
  | Trouve u -> 
    Some (List.rev u)
;;

let symetric_difference (a:My_base.dfa) (a':My_base.dfa) = 
  let n = a.nb_states in
  let n' = a'.nb_states in 
  let m = a.nb_letters in 
  let d = Array.make_matrix (n*n') m (-1) in 
  let ac = Array.make (n*n') false in
  for q=0 to n-1 do 
    for q' = 0 to n'-1 do 
      if (a.accepting.(q) && not a'.accepting.(q')) ||
        (a'.accepting.(q') && not a.accepting.(q)) then 
          ac.(n'*q+q')<- true;
      for i =0 to m-1 do
        d.(n'*q+q').(i) <- n'*a.delta.(q).(i) + a'.delta.(q').(i);
      done;
    done;
  done;
  {My_base.q0 = a.q0*n'+a'.q0 ;
  nb_letters = m;
  nb_states = n*n';
  accepting = ac;
  delta = d
  }
;;

let create_teacher a = 
  let member w = 
    let q = My_base.delta_star a (a.q0) w in
    a.accepting.(q)
  in
  let ce a' = 
    let b = symetric_difference a a' in 
    shortest_word b 
  in
  {nb_letters_t = a.nb_letters;
  member = member;
  counter_example = ce}
;;