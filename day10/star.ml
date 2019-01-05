open Core

type star = {
    x: int;
    y: int;
    dx: int;
    dy: int;
}

type t = star

let line_pattern = "position=<\\s*(-?\\d+),\\s*(-?\\d+)> velocity=<\\s*(-?\\d+),\\s*(-?\\d+)>"

let line_re = Re.Perl.compile_pat line_pattern

let parse_line (line: string): star =
  let matches = Re.exec line_re line |> Re.get_all in
  match matches with
  | [| _; x; y; dx; dy |] -> {
      x = Int.of_string x;
      y = Int.of_string y;
      dx = Int.of_string dx;
      dy = Int.of_string dy;
    }
  | _ -> failwith "Line did not parse. That shouldn't happen"

let move ({x; y; dx; dy}: star): star = {
  x = x + dx;
  y = y + dy;
  dx = dx;
  dy = dy;
}

let bounding_box_size (l: star list): (int * int) =
  let {x = min_x; _} = Util.List.min_by_exn l ~key_fn:(fun {x; _} -> x) in
  let {x = max_x; _} = Util.List.max_by_exn l ~key_fn:(fun {x; _} -> x) in
  let {y = min_y; _} = Util.List.min_by_exn l ~key_fn:(fun {y; _} -> y) in
  let {y = max_y; _} = Util.List.max_by_exn l ~key_fn:(fun {y; _} -> y) in
  (max_x - min_x, max_y - min_y)

let get_relative_coords (l: star list): (int * int) list =
  let {x = min_x; _} = Util.List.min_by_exn l ~key_fn:(fun {x; _} -> x) in
  let {y = min_y; _} = Util.List.min_by_exn l ~key_fn:(fun {y; _} -> y) in
  let f {x; y; _} = (x - min_x, y - min_y) in
  List.map l ~f:f

let draw_chart (l: star list): string =
  let rel_coords = get_relative_coords l in
  let (max_x, _) = Util.List.max_by_exn rel_coords ~key_fn:(fun (x, _) -> x) in
  let (_, max_y) = Util.List.max_by_exn rel_coords ~key_fn:(fun (_, y) -> y) in
  let x_range = List.range ~stop:`inclusive 0 max_x in
  let y_range = List.range ~stop:`inclusive 0 max_y in 
  let chars =
    List.map y_range ~f:(fun y -> List.map x_range ~f:(fun x -> if List.mem rel_coords (x, y) ~equal:(=) then "#" else "."))
  in
  List.map chars ~f:String.concat |>
  String.concat ~sep:"\n"