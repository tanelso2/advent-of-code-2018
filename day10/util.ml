open Core

module Map = struct
  let max_by (m: ('k, 'v, _) Map.t) ~key_fn:(f:('k -> 'v -> int)): ('k * 'v) option =
    let elem = ref None in
    let max_val = ref Int.min_value in
    let iter_fn ~key:k ~data:v =
      let curr_val = f k v in
      if curr_val > !max_val
      then
        (max_val := curr_val;
        elem := (Some (k, v)))
      else ()
    in
    let () = Map.iteri m ~f:iter_fn in
    !elem
end

module List = struct
  let max_by (l: 'a list) ~key_fn:(f:'a -> int): 'a option =
    let elem = ref None in
    let max_val = ref Int.min_value in
    let iter_fn x =
      let curr_val = f x in
      if curr_val > !max_val
      then
        (max_val := curr_val;
         elem := Some x)
      else ()
    in
    let () = List.iter l ~f:iter_fn in
    !elem
end
