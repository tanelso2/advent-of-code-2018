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

  let max_by_exn m ~key_fn:f =
    match max_by m ~key_fn:f with
    | Some x -> x
    | None -> failwith "max_by_exn failed"

  let min_by m ~key_fn:f = max_by m ~key_fn:(fun k v -> f k v |> Int.neg)

  let min_by_exn m ~key_fn:f =
    match min_by m ~key_fn:f with
    | Some x -> x
    | None -> failwith "min_by_exn failed"
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

  let max_by_exn l ~key_fn:f =
    match max_by l ~key_fn:f with
    | Some x -> x
    | None -> failwith "max_by_exn failed"

  let min_by l ~key_fn:f = max_by l ~key_fn:(fun a -> f a |> Int.neg)

  let min_by_exn l ~key_fn:f =
    match min_by l ~key_fn:f with
    | Some x -> x
    | None -> failwith "min_by_exn failed"
end
