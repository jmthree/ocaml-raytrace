module S = Scene

let line_to_instr line =
  let split = Str.split (Str.regexp "[ \t]+") in
  let fos = float_of_string in
  let ios = int_of_string in
  match split line with
  | "##" :: _ -> S.CommentInstr
  | ["vv"; x; y; z; dx; dy; dz] ->
      S.VertexInstr (fos x, fos y, fos z, fos dx, fos dy, fos dz)
  | ["am"; r; g; b] -> S.AmbientMaterialInstr (fos r, fos g, fos b)
  | ["dm"; r; g; b] -> S.DiffuseMaterialInstr (fos r, fos g, fos b)
  | ["sm"; r; g; b; p] -> S.SpecularMaterialInstr (fos r, fos g, fos b, ios p)
  | ["ss"; i] -> S.SphereInstr (ios i)
  | ["cc"; i] -> S.CameraInstr (ios i)
  | ["pl"; idx; inten] -> S.PointLightInstr (ios idx, fos inten)
  | ["dl"; idx; inten] -> S.DirectionalLightInstr (ios idx, fos inten)
  | ["se"; d; sp; sh; r; a] ->
      S.SettingsInstr (d = "d", sp = "s", sh = "a", ios r, fos a)
  | _ -> S.IgnoreInstr line

let instr_to_string instr = match instr with
  | S.CommentInstr    -> "Comment"
  | S.VertexInstr (x, y, z, dx, dy, dz) ->
      Printf.sprintf "Vertex x:%.2f y:%.2f z:%.2f dx:%.2f dy:%.2f dz:%.2f" x y z dx dy dz
  | S.AmbientMaterialInstr (r, g, b) ->
      Printf.sprintf "AmbientMaterial r:%.1f g:%.1f b:%.1f" r g b
  | S.DiffuseMaterialInstr (r, g, b) ->
      Printf.sprintf "DiffuseMaterial r:%.1f g:%.1f b:%.1f" r g b
  | S.SphereInstr idx -> "Sphere index: " ^ (string_of_int idx)
  | S.CameraInstr idx -> "Camera index: " ^ (string_of_int idx)
  | S.IgnoreInstr ig  -> "Ignored: " ^ ig
  | _ -> "foo"

let parse_lines = List.map line_to_instr

let validate_instructions instrs =
  let is_vertex = function
    | S.VertexInstr _ -> true
    | _ -> false
  in
  let vtxs, others = List.partition is_vertex instrs in
  let max_idx = List.length vtxs in
  let validate_instr = function
    | S.CommentInstr | S.IgnoreInstr _ -> ()
    | S.SphereInstr idx when idx >= 0 && idx < max_idx -> ()
    | S.CameraInstr idx when idx >= 0 && idx < max_idx -> ()
    | S.DiffuseMaterialInstr (r, g, b) as instr ->
        let check i =
          if 0.0 <= i && i <= 1.0
          then ()
          else failwith ("Invalid instruction: " ^ (instr_to_string instr))
        in
          List.iter check [r; g; b]
    | S.AmbientMaterialInstr (r, g, b) as instr ->
        let check i =
          if 0.0 <= i && i <= 1.0
          then ()
          else failwith ("Invalid instruction: " ^ (instr_to_string instr))
        in
          List.iter check [r; g; b]
    | S.SettingsInstr _ -> ()
    | S.PointLightInstr _ -> ()
    | S.DirectionalLightInstr _ -> ()
    | S.SpecularMaterialInstr _ -> ()
    | instr -> failwith ("Invalid instruction: " ^ (instr_to_string instr))
  in
  List.iter validate_instr others

