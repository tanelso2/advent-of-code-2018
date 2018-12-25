open Core

let max_by (m: ('k, 'v, _) Map.t) ~key_fn:(f:('v -> int)): ('k * 'v) option =
  let elem = ref None in
  let max_val = ref Int.min_value in
  let iter_fn ~key:k ~data:v =
    let curr_val = f v in
    if curr_val > !max_val
    then
      (max_val := curr_val;
       elem := (Some (k, v)))
    else ()
  in
  let () = Map.iteri m ~f:iter_fn in
  !elem
