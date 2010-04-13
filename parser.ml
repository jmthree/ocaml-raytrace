type instruction = Comment
                 | Vertex of float * float * float * float * float * float
                 | Sphere of int
                 | Camera of int
                 | Ignore of string

let line_to_instr line =
  let split = Str.split (Str.regexp "[ \t]+") in
  let fos = float_of_string in
  let ios = int_of_string in
  match split line with
  | "##" :: _ -> Comment
  | ["vv"; x; y; z; dx; dy; dz] ->
      Vertex (fos x, fos y, fos z, fos dx, fos dy, fos dz)
  | ["ss"; i] -> Sphere (ios i)
  | ["cc"; i] -> Camera (ios i)
  | _         -> Ignore line

let instr_to_string instr = match instr with
  | Comment    -> "Comment"
  | Vertex (x, y, z, dx, dy, dz) ->
    Printf.sprintf "Vertex x:%.2f y:%.2f z:%.2f dx:%.2f dy:%.2f dz:%.2f" x y z dx dy dz
  | Sphere idx -> "Sphere index: " ^ (string_of_int idx)
  | Camera idx -> "Camera index: " ^ (string_of_int idx)
  | Ignore ig  -> "Ignored: " ^ ig

let parse_lines = List.map line_to_instr

let validate_instructions instrs =
  let is_vertex = function
    | Vertex _ -> true
    | _ -> false
  in
  let vtxs, others = List.partition is_vertex instrs in
  let max_idx = List.length vtxs in
  let validate_instr = function
    | Comment | Ignore _ -> ()
    | Sphere idx when idx >= 0 && idx < max_idx -> ()
    | Camera idx when idx >= 0 && idx < max_idx -> ()
    | instr -> failwith ("Invalid instruction: " ^ (instr_to_string instr))
  in
  List.iter validate_instr others

