(* Minimal keyword tracking. *)

let last_keyword : string option ref = ref None
let last_keyword_clear () = last_keyword := None
let last_keyword_set (s : string) = last_keyword := Some s
