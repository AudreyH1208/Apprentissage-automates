open Printf


type state = int

type nfa =
  {delta : state list array array;
  accepting : bool array}


type 'a regex =
  | Empty
  | Eps
  | Letter of 'a
  | Sum of 'a regex * 'a regex
  | Concat of 'a regex * 'a regex
  | Star of 'a regex


type stream = {
  str : string;
  mutable index : int
}

let new_stream str =
  {str = str; index = 0}

exception SyntaxError



let rec peek s =
  if s.index >= String.length s.str then None
  else if s.str.[s.index] = ' ' then (
    s.index <- s.index + 1; peek s
  ) else Some s.str.[s.index]


let error s =
  match peek s with
  | None ->
    printf "Unexpected end of input\n";
    raise SyntaxError
  | Some c ->
    printf "Unexpected token %c at position %d\n" c s.index;
    raise SyntaxError

let expect s c =
  match peek s with
  | Some c' when c = c' ->
    s.index <- s.index + 1
  | _ -> printf "Expected %c\n" c; error s

let discard s =
  if s.index = String.length s.str then error s
  else s.index <- s.index + 1

let is_letter c =
  'a' <= c && c <= 'z'

let to_int c =
  int_of_char c - int_of_char 'a'

let rec regex s =
  let t = term s in
  match peek s with
  | Some '|' ->
    discard s;
    let e = regex s in
    Sum (t, e)
  | _ -> t
  and term s =
    let f = factor s in
    match peek s with
    | None | Some ')' | Some '|' -> f
    | _ ->
      let t = term s in
      Concat (f, t)
  and factor s =
    let a = atom s in
    quantif s a
  and quantif s a =
    match peek s with
    | Some '*' -> discard s; quantif s (Star a)
    | _ -> a
  and atom s =
    match peek s with
    | Some '(' ->
      discard s;
      let e = regex s in
      expect s ')';
      e
    | Some c when is_letter c -> discard s; Letter (to_int c)
    | Some '&' -> discard s; Eps
    | Some '#' -> discard s; Empty
    | _ -> error s



let parse str =
  let s = new_stream str in
  let tree = regex s in
  match peek s with
  | None -> tree
  | Some _ -> printf "Expected input to end\n"; error s



let rec merge a b = 
  match a, b with 
  | [], u | u, [] -> u 
  | x::xs, y::ys when x=y -> x::(merge xs ys)
  | x::xs, y::ys when x<y -> x::(merge xs (y::ys))
  | x::xs, y::ys-> y::(merge (x::xs) ys)
;;

(* val is_empty : 'a regex -> bool *)
let rec is_empty r = 
  match r with 
  | Empty -> true
  | Eps | Letter _ | Star _ -> false
  | Sum (a,b) -> is_empty a && is_empty b
  | Concat (a,b) -> is_empty a || is_empty b
  
(* val contains_epsilon : 'a regex -> bool *)

let rec contains_epsilon r = 
  match r with
  | Empty -> false
  | Eps | Star _ -> true
  | Letter _ -> false
  | Sum (a,b) -> contains_epsilon a || contains_epsilon b
  | Concat (a,b) -> contains_epsilon a && contains_epsilon b
;;

(* val prefix : 'a regex -> 'a list *)

let rec prefix r = 
  match r with
  | Empty |Eps -> []
  | Star a -> prefix a
  | Letter a -> [a]
  | Sum (a,b) -> merge (prefix a) (prefix b)
  | Concat (a,b) -> if is_empty a || is_empty b then [] else 
    (if contains_epsilon a then merge (prefix a) (prefix b) else prefix a)
;;

(* val suffix : 'a regex -> 'a list *)

let rec suffix r = 
  match r with
  | Empty |Eps -> []
  | Star a -> suffix a
  | Letter a -> [a]
  | Sum (a,b) -> merge (suffix a) (suffix b)
  | Concat (a,b) -> if is_empty a || is_empty b then [] else 
    (if contains_epsilon b then merge (suffix a) (suffix b) else suffix b)
;;
(* val combine : 'a list -> 'b list -> ('a * 'b) list *)

let combine a b = 
  let rec aux v x = 
    match v with 
    | y::ys -> (x,y)::aux ys x
    | [] -> []
  in 
  let rec aux2 u v = 
    match u with 
    | [] -> []
    | x::xs -> (aux v x) @ (aux2 xs v)
  in
  aux2 a b 
  ;;

(* val factor : 'a regex -> ('a * 'a) list *)
let rec factor r = 
  match r with
  | Empty |Eps -> []
  | Star a -> let p = prefix a in 
  let s = suffix a in 
    merge (combine p s) (factor a)
  | Letter _ -> []
  | Sum (a,b) -> merge (factor a) (factor b)
  | Concat (a,b) -> if is_empty a || is_empty b then [] else 
    let p = prefix b in 
    let s = suffix a in 
    merge (combine s p) (merge (factor a) (factor b))
;;

let rec number_of_letters r = 
  match r with
  | Empty |Eps -> 0
  | Star a -> number_of_letters a
  | Letter _ -> 1
  | Sum (a,b) | Concat (a,b) -> number_of_letters a + number_of_letters b 
;;

(* val linearize : 'a regex -> ('a * int) regex *)

let linearize (r:state regex) = 
  let i = ref 0 in 
  let rec aux e = 
    match e with
    | Empty -> Empty
    |Eps -> Eps
    | Star a -> Star (aux a)
    | Letter l -> incr i; Letter (l, !i)
    | Sum (a,b) -> let a' = aux a in Sum (a', aux b)
    | Concat (a,b) ->  let a' = aux a in Concat (a', aux b)
  in 
  aux r
;;


let rec max_letter r = 
  match r with
  | Empty |Eps -> -1
  | Star a -> max_letter a
  | Letter a -> a
  | Sum (a,b) | Concat (a,b) -> max (max_letter a) (max_letter b)
;;

let print_pair (x, y) =
  Printf.printf "(%d, %d)" x y

let print_int_pair_list lst =
  print_string "[";
  let rec aux = function
    | [] -> ()
    | [last] -> print_pair last
    | head :: tail ->
        print_pair head;
        print_string "; ";
        aux tail
  in
  aux lst;
  print_endline "]"
;;


let glushkov r = 
  let e = linearize r in 
  let n = number_of_letters r in
  let m = max_letter r in 

  let suff = suffix e in 
  print_int_pair_list suff;
  let pre = prefix e in 
  print_int_pair_list pre;
  let fact = factor e in

  let t = Array.make_matrix (n+1) (m+1) [] in 

  let add_transition i x j = 
    t.(i).(x) <- j :: t.(i).(x)
  in 

  List.iter (fun (x,i)-> add_transition 0 x i) pre;

  List.iter (fun ((_,i),(y,j)) -> add_transition i y j) fact ;

  let final = Array.make (n+1) false in 
  List.iter (fun (_,i)-> final.(i)<-true) suff ;
  if contains_epsilon r then 
    final.(0)<- true ;
  {delta = t; accepting = final} 
;;


type dfa =
  {delta_d : state array array;
  accepting_d : bool array}

(* val build_set : nfa -> state list -> int -> state list *)

let build_set a s x = 
  let n = Array.length a.delta in 
  let d = Array.make n false in
  let process_state q = 
    List.iter (fun q' ->d.(q')<-true) a.delta.(q).(x)
  in
  List.iter process_state s;
  let l = ref [] in
  for i = (n-1) downto 0 do 
    if d.(i) then 
      l:= i:: (!l)
  done;
  !l
;;



let powerset a = 
  let n = Array.length a.delta in 
  let m = Array.length a.delta.(0) in 
  let sets = Hashtbl.create n in 
  
  Hashtbl.add sets [0] 0;
  let ind = ref 0 in 

  let transitions = ref [] in

  let add_set s = 
    match Hashtbl.find_opt sets s with 
    | Some _ -> (false, Hashtbl.find sets s)
    | None -> incr ind;
        Hashtbl.add sets s !ind; 
        (true, !ind)
  in
  let rec parcours s i = 
    for lettre = 0 to m-1 do 
      let s' = build_set a s lettre in 
      let (b, j) = add_set s' in
      transitions := (i, lettre,j):: !transitions;
      if b then (parcours s' j)
    done;
  in
  parcours [0] 0;

  let nb_etats = !ind +1 in 
  let delta = Array.make_matrix nb_etats m (-1) in 
  List.iter (fun (i,x,j)-> delta.(i).(x)<- j) !transitions;

  let accepting = Array.make nb_etats false in 
  let rec aux u i=
    match u with 
    | [] -> ()
    | x::xs -> if a.accepting.(x) then accepting.(i)<-true else aux xs i 
  in
  Hashtbl.iter aux sets ;
  {delta_d = delta; accepting_d = accepting}
;;

let ex_to_auto s = 
  let expr = parse s in 
  let g = glushkov expr in 
  let auto = powerset g in 
  auto;;