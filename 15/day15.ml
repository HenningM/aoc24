let rec read_lines () =
  try
    let line = input_line stdin in
    line :: read_lines ()
  with End_of_file -> []
;;

let split_on_first_empty_line lst =
  let rec aux before after = function
    | [] -> (List.rev before, List.rev after)
    | "" :: rest -> (List.rev before, List.rev ("" :: rest))
    | line :: rest -> aux (line :: before) after rest
  in
  aux [] [] lst
;;

type command =
  | Left
  | Right
  | Up
  | Down
  | Other
;;

let parse_command = function
  | '<' -> Left
  | '>' -> Right
  | '^' -> Up
  | 'v' -> Down
  | _   -> Other
;;

type map_elem =
  | Wall
  | Box
  | WideBoxLeft
  | WideBoxRight
  | Robot
  | Floor
  | Other
;;

let is_movable = function
  | Wall -> false
  | Box  -> true
  | WideBoxLeft -> true
  | WideBoxRight -> true
  | Robot-> true
  | Floor-> false
  | Other-> false
;;

let pos_plus_dir (x, y) = function
  | Left -> (x - 1, y)
  | Right-> (x + 1, y)
  | Up   -> (x, y - 1)
  | Down -> (x, y + 1)
  | Other-> (x, y)
;;

let parse_map_line line =
  let parse_map_element = function
    | '#' -> Wall
    | 'O' -> Box
    | '[' -> WideBoxLeft
    | ']' -> WideBoxRight
    | '@' -> Robot
    | '.' -> Floor
    | _   -> Other
  in
  String.to_seq line |> Array.of_seq |> Array.map parse_map_element
;;

let is_in_bounds map (x, y) =
  let y_len = Array.length map
  and x_len = Array.length map.(0) in
  x >= 0 && x < x_len && y >=0 && y < y_len
;;

let rec find_robot map x y =
  if is_in_bounds map (x, y) then
    let idx = Array.find_index ((=) Robot) map.(y)
    in
    match idx with
    | Some xHit -> Some (xHit, y)
    | None -> find_robot map x (y + 1)
  else
    None
;;

let swap_pos map (x1, y1) (x2, y2) =
  let new_map = Array.map Array.copy map in
  let temp = new_map.(y1).(x1) in
  new_map.(y1).(x1) <- new_map.(y2).(x2);
  new_map.(y2).(x2) <- temp;
  new_map
;;

let rec try_move map (x, y) dir =
  let cur_elem = map.(y).(x) in
  match cur_elem with
  | WideBoxLeft when dir = Up || dir = Down -> try_move_wide_box_left map (x, y) dir
  | WideBoxRight when dir = Up || dir = Down -> try_move_wide_box_right map (x, y) dir
  | elem when is_movable elem -> try_move_regular map (x, y) dir
  | _ -> map, (x, y)

and try_move_regular map (x, y) dir =
  let next_x, next_y = pos_plus_dir (x, y) dir in
  let new_map, _ = if is_in_bounds map (next_x, next_y) then try_move map (next_x, next_y) dir else map, (x, y) in
  if new_map.(next_y).(next_x) = Floor then
    (swap_pos new_map (x, y) (next_x, next_y), (next_x, next_y))
  else
    map, (x, y)

and try_move_wide_box_left map (x, y) dir =
  let _, next_y = pos_plus_dir (x, y) dir in
  let cur_try, _ = try_move map (x, next_y) dir in
  let r_try, _ = try_move cur_try (x + 1, next_y) dir in
  if r_try.(next_y).(x) = Floor && r_try.(next_y).(x + 1) = Floor then
    let swapped = swap_pos r_try (x, y) (x, next_y) in
    (swap_pos swapped (x + 1, y) (x + 1, next_y), (x, next_y))
  else
    map, (x, y)

and try_move_wide_box_right map (x, y) dir =
  let _, next_y = pos_plus_dir (x, y) dir in
  let cur_try, _ = try_move map (x, next_y) dir in
  let r_try, _ = try_move cur_try (x - 1, next_y) dir in
  if r_try.(next_y).(x) = Floor && r_try.(next_y).(x - 1) = Floor then
    let swapped = swap_pos r_try (x, y) (x, next_y) in
    (swap_pos swapped (x - 1, y) (x - 1, next_y), (x, next_y))
  else
    map, (x, y)

let apply_commands map pos commands =
  let new_map, _ = List.fold_left (fun (m, pos) c -> try_move m pos c) (map, pos) commands in
  new_map

(*
let print_map map =
  Array.map (fun line -> Array.map map_elem_to_char line |> Array.to_seq |> String.of_seq) map |> Array.iter (fun line -> print_endline line)
;;

let map_elem_to_char = function
  | Wall -> '#'
  | Box  -> 'O'
  | WideBoxLeft -> '['
  | WideBoxRight -> ']'
  | Robot-> '@'
  | Floor-> '.'
  | Other-> '?'
;;
*)


let parse_map lst = List.map (parse_map_line) lst |> Array.of_list
;;

let widen_char = function
  | '.' -> ".."
  | '@' -> "@."
  | 'O' -> "[]"
  | _ -> "##"

let parse_map_wide lst = List.map (fun line -> String.to_seq line |> Seq.map widen_char |> Seq.fold_left String.cat "") lst |> parse_map
;;

let parse_commands lst = List.fold_left (fun ns s -> String.cat s ns) "" lst |> String.to_seq |> List.of_seq |> List.map parse_command
;;

let parse_input input parse_map_func =
  let grp1, grp2 = split_on_first_empty_line input
  in
  (parse_map_func grp1, parse_commands grp2)
;;

let calc_score map =
  let sums2d = Array.mapi (fun y l -> Array.mapi (fun x e -> if e = Box || e = WideBoxLeft then (100 * y) + x else 0) l) map in
  Array.fold_left (fun s l -> s + Array.fold_left (fun ps e -> ps + e) 0 l) 0 sums2d
;;

let solve_part_one input =
  let map, commands = parse_input input parse_map in
  let robot_pos = find_robot map 0 0 in
  match robot_pos with
  | Some (x, y) -> Printf.printf "%d\n" @@ calc_score @@ apply_commands map (x, y) commands
  | None -> print_endline "Could not find robot"
;;

let solve_part_two input =
  let map, commands = parse_input input parse_map_wide in
  let robot_pos = find_robot map 0 0 in
  match robot_pos with
  | Some (x, y) -> Printf.printf "%d\n" @@ calc_score @@ apply_commands map (x, y) commands
  | None -> print_endline "Could not find robot"
;;

let () =
  let lines = read_lines () in
  solve_part_one lines;
  solve_part_two lines
