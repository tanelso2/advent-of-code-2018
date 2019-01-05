open Core

let serial_number = 7403

let hundreds_digit (z: int): int = (z / 100) % 10

let power_level (x: int) (y: int): int =
  x + 10 |>
  ( * ) y |>
  (+) serial_number |>
  ( * ) (x + 10) |>
  hundreds_digit |>
  fun n -> n - 5


let grid =
  let range = List.range ~stop:`inclusive 1 300 in
  let f m (x, y) = Map.add_exn m ~key:(x, y) ~data:(power_level x y) in
  List.cartesian_product range range |>
  List.fold_left ~init:(Map.Poly.empty) ~f:f

let get_total_power (x: int) (y: int) (size: int): int option =
  let sum = ref (Some 0) in
  for i = x to x + (size - 1) do
    for j = y to y + (size - 1) do
      sum := Option.bind !sum ~f:(fun x -> Option.map (Map.find grid (i,j)) ~f:((+) x))
    done;
  done;
  !sum

let part1 (): (int * int) =
  let range = List.range ~stop:`inclusive 1 298 in
  List.cartesian_product range range |>
  List.filter ~f:(fun (x,y) -> x + 3 <= 301 && y + 3 <= 301) |>
  Util.List.max_by_exn ~key_fn:(fun (x,y) -> get_total_power x y 3 |> Option.value ~default:Int.min_value)

let part2 (): ((int * int) * int) =
  let max_val = ref Int.min_value in
  let max_set = ref None in
  for x = 1 to 300 do
    for y = 1 to 300 do
      for size = 1 to 300 do
        if x + (size - 1) >= 300 || y + (size - 1) >= 300 then () else (
        let total_power = get_total_power x y size in
        match total_power with
        | None -> ()
        | Some power ->
          if power > !max_val then
            (max_val := power;
             max_set := Some ((x,y),size))
          else ())
      done;
    done;
  done;
  Option.value_exn !max_set


let () =
  ExtLib.print (part1 ());
  ExtLib.print (part2 ())


