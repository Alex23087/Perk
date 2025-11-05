(* Minimal keyword tracking. *)

let last_keyword : string option ref = ref None
let clear () = last_keyword := None
let set (s:string) = last_keyword := Some s