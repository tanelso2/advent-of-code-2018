open Core

type elem = {
    value: int;
    mutable next: elem;
    mutable prev: elem;
}

type t = elem

let empty () = 
    let rec x = {
        value = 0;
        next = x;
        prev = x;
    }
    in
    x

let insert_after elt value =
  let old_next = elt.next in
  let new_elt = { value; prev = elt; next = old_next } in
  old_next.prev <- new_elt;
  elt.next <- new_elt;
  new_elt

let remove_current (elt: elem): (elem * int) =
    let {next; prev; value} = elt in
    prev.next <- next;
    next.prev <- prev;
    (next, value)

let add (elt: elem) (num: int): elem =
    let after_point = elt.next in
    insert_after after_point num

let remove (elt: elem): (elem * int) =
    let x = ref elt in
    for _ = 1 to 7 do x := !x.prev done;
    remove_current !x