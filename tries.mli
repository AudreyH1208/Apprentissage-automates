(* Le type d'une trie *)
type trie = {
  mutable final : bool;
  children : (My_base.letter, trie) Hashtbl.t;
}
(* Renvoie une trie racine *)
val create_node : unit -> trie

(* Ajoute un mot à une trie *)
val insert : trie -> My_base.word -> unit

(* Test l'appartenant d'un mot à une trie *)
val inside : trie -> My_base.word -> bool

(* Renvoie la liste des mots contenus dans une trie *)
val words : trie -> My_base.word list

(* Applique une fonction f à l'accumulateur et tous les élémenet de la trie (fold classique) *)
val fold : (My_base.word -> 'acc -> 'acc)->trie-> 'acc -> 'acc

(* Applique une fonction f à tous les éléments d'une Trie *)
val iter : (My_base.word -> unit)->trie-> unit
