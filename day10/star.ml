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

let move (_: star): star = failwith "NOIMPL"

let draw_chart (_: star list): string = failwith "NOIMPL"
