open Core

let x = [1;2;3]

let a = Util.List.max_by ~key_fn:ident x

let () =
  match a with
  | Some x -> ExtLib.print x
  | _ -> ()

(* let () = *)
(*   ExtLib.print (part1 ()); *)
(*   ExtLib.print (part2 ()); *)
(* ;; *)
