module Vector = struct
    type vector = float * float * float

    let cross (x1, y1, z1) (x2, y2, z2) =
      (y1 *. z2 -. z1 *. y2,
       x1 *. z2 +. z1 *. x2,
       x1 *. y2 +. y1 *. x2)

    let dot (x1, y1, z1) (x2, y2, z2) =
        x1 *. x2 +. y1 *. y2 +. z1 *. z2

    let size (x, y, z) = sqrt (x ** 2.0 +. y ** 2.0 +. z ** 2.0)

    let normalize ((x, y, z) as v) =
      let len = size v in
      (x /. len, y /. len, z /. len)

    let mult factor (x, y, z) =
      (factor *. x, factor *. y, factor *. z)

    let sub (x1, y1, z1) (x2, y2, z2) = (x1 -. x2, y1 -. y2, z1 -. z2)

    let add (x1, y1, z1) (x2, y2, z2) = (x1 +. x2, y1 +. y2, z1 +. z2)

    let to_string (x, y, z) =
      Printf.sprintf "Vector x:%.4f y:%.4f z:%.4f" x y z
end

type index  = int
type color  = int * int * int
type fcolor = float * float * float

type instruction = CommentInstr
                 | VertexInstr of float * float * float * float * float * float
                 | AmbientMaterialInstr of float * float * float
                 | DiffuseMaterialInstr of float * float * float
                 | SpecularMaterialInstr of float * float * float * int
                 | SphereInstr of int
                 | CameraInstr of int
                 | PointLightInstr of index * float
                 | DirectionalLightInstr of index * float
                 | SettingsInstr of bool * bool * bool * int * float
                 | IgnoreInstr of string

type vertex = { x : float;
                y : float;
                z : float;
                dir : Vector.vector }

type light = Directional of index * float
           | Point of index * float

type element = Sphere of index * fcolor * fcolor * (fcolor * int)

type camera = { idx : index;
                frame_x : int;
                frame_y : int;
                }

type settings = { diffuse  : bool;
                  specular : bool;
                  shadows  : bool;
                  reflect_depth : int;
                  ambient_int  : float;
                }

type scene = { vertexes : vertex array;
               lights   : light list;
               camera   : camera;
               settings : settings;
               elements : element list;
               }

let create_scene instrs units_x units_y =
  let ambient_material = ref (0.2, 0.2, 0.2) in
  let diffuse_material = ref (1.0, 1.0, 1.0) in
  let specular_material = ref ((1.0, 1.0, 1.0), 64) in
  let settings = ref { diffuse = false;
                       specular = false;
                       shadows = false;
                       reflect_depth = 0;
                       ambient_int = 1.0 }
  in
  let create ((vtxs, camera, lights, elements) as accum) = function
    | VertexInstr (x, y, z, dx, dy, dz) ->
        let vtx = {x = x; y = y; z = z; dir = (dx, dy, dz)} in
        (vtx :: vtxs, camera, lights, elements)
    | AmbientMaterialInstr (r, g, b) ->
        begin
          ambient_material := (r, g, b);
          accum
        end
    | DiffuseMaterialInstr (r, g, b) ->
        begin
          diffuse_material := (r, g, b);
          accum
        end
    | SpecularMaterialInstr (r, g, b, p) ->
        begin
           specular_material := ((r, g, b), p);
           accum
        end
    | SphereInstr idx ->
        let element = Sphere (idx + 1,
                              !ambient_material,
                              !diffuse_material,
                              !specular_material)
        in
        (vtxs, camera, lights, element :: elements)
    | CameraInstr idx -> (vtxs, idx + 1, lights, elements)
    | PointLightInstr (idx, intens) ->
        (vtxs, camera, (Point (idx + 1, intens)) :: lights, elements)
    | DirectionalLightInstr (idx, intens) ->
        (vtxs, camera, (Directional (idx + 1, intens)) :: lights, elements)
    | SettingsInstr (diff, spec, shadows, ref, amb) ->
        begin
          settings := { diffuse = diff;
                        specular = spec;
                        shadows = shadows;
                        reflect_depth = ref;
                        ambient_int = amb;
                      };
          accum
        end
    | _ -> accum
  in
  let cam_origin = {x = 0.0; y = 0.0; z = 0.0; dir = (0.0, 0.0, 1.0)} in
  let (vtxs, cam, lights, els) = List.fold_left create ([cam_origin], 0, [], []) instrs
  in
  { vertexes  = Array.of_list (List.rev vtxs);
    lights    = lights;
    camera    = { idx = cam; frame_x = units_x; frame_y = units_y; };
    settings  = !settings;
    elements  = els }

let cast_ray_into_scene scene x y =
  let get_vertex = Array.get scene.vertexes in
  let {idx = camera_idx; frame_x = ix; frame_y = iy} = scene.camera in
  let {x = cam_x; y = cam_y; z = cam_z; dir = cam_dir} = get_vertex camera_idx in
  (*let _ = Printf.printf "Camera: %.2f %.2f %.2f\n" cam_x cam_y cam_z in*)
  (*let _ = print_endline ("Camera Dir: " ^ (Vector.to_string cam_dir)) in*)
  let e = (cam_x, cam_y, cam_z) in
  let ((bzx, bzy, bzz) as cbasis_z) = Vector.normalize (Vector.mult (-1.0) cam_dir) in
  let ((bxx, bxy, bxz) as cbasis_x) = Vector.normalize (Vector.cross cam_dir (0.0, 1.0, 0.0)) in
  let ((byx, byy, byz) as cbasis_y) = Vector.cross cbasis_z cbasis_x in
  (*let _ = print_endline "Camera Basis" in*)
  (*let _ = print_endline (Vector.to_string cbasis_x) in*)
  (*let _ = print_endline (Vector.to_string cbasis_y) in*)
  (*let _ = print_endline (Vector.to_string cbasis_z) in*)
  let frame_x, frame_y = float ix, float iy in
  let ((px, py, pz) as point_in_camera) = ((float x) /. frame_x -. 0.5,
                                           (-1.0 *. frame_y) /. frame_x *. ((float y) /. frame_y -. 0.5),
                                           (sqrt 3.0) /. -2.0)
  in
  (*let _ = print_endline "Point in camera" in*)
  (*let _ = print_endline (Vector.to_string point_in_camera) in*)
  let ((cam_dx, cam_dy, cam_dz) as p) = (bxx *. px +. byx *. py +. bzx *. pz,
                                         bxy *. px +. byy *. py +. bzy *. pz,
                                         bxz *. px +. byz *. py +. bzz *. pz)
  in
  (*let _ = print_endline "Point in world" in*)
  (*let _ = print_endline (Vector.to_string p) in*)
  (*let d = Vector.sub p e in*)
  let d = p in
  (*let _ = print_endline "Ray" in*)
  (*let _ = print_endline (Vector.to_string d) in*)
  let intensity = scene.settings.ambient_int in
  let closest_hit_element last_hit element =
    match element with
    | Sphere (idx, (amr, amg, amb), (diffr, diffg, diffb), ((specr, specg, specb), phong)) ->
        let {x = cx; y = cy; z = cz; dir = cdir} = get_vertex idx in
        let c = (cx, cy, cz) in
        let r = Vector.size cdir in
        (*let _ = print_endline ("Circle" ^ (Vector.to_string c) ^ " " ^
         * (string_of_float r ^ "\n")) in*)
        let dd = Vector.dot d d in
        let eminc = Vector.sub e c in
        let dec = Vector.dot d eminc in
        let determinant = (dec ** 2.0 -. (dd *. ((Vector.dot eminc eminc) -. (r ** 2.0)))) in
        (*let _ = Printf.printf "Determinant: %.3f\n" determinant in*)
        let to_color c = c *. intensity in
        let ambient_color = to_color amr, to_color amg, to_color amb in
        if determinant < 0.0
          then last_hit
          else
            let mint = (((-1.0 *. dec) -. (sqrt determinant)) /. dd) in
            let plust = (((-1.0 *. dec) -. (sqrt determinant)) /. dd) in
            let t = min mint plust in
            let intersect = Vector.add e (Vector.mult t d) in
            let n = Vector.sub intersect c in
            let n = Vector.normalize n in
            let apply_diffuse_light (prev_r, prev_g, prev_b) = function
              | Directional (idx, intensity) ->
                  let {x = _; y = _; z = _; dir = ldir} = get_vertex idx in
                  let l = Vector.normalize (Vector.mult (-1.0) ldir) in
                  let to_color c = c *. intensity *. (max 0.0 (Vector.dot n l)) in
                  let red = to_color diffr in
                  let green = to_color diffg in
                  let blue = to_color diffb in
                  (prev_r +. red, prev_g +. green, prev_b +. blue)
              | Point (idx, intensity) ->
                  let {x = px; y = py; z = pz; dir = _} = get_vertex idx in
                  let l = Vector.normalize (Vector.sub (px, py, pz) intersect) in
                  let to_color c = c *. intensity *. (max 0.0 (Vector.dot n l)) in
                  let red = to_color diffr in
                  let green = to_color diffg in
                  let blue = to_color diffb in
                  (prev_r +. red, prev_g +. green, prev_b +. blue)
            in
            let apply_specular_light (prev_r, prev_g, prev_b) = function
              | Directional (idx, intensity) ->
                  let {x = _; y = _; z = _; dir = ldir} = get_vertex idx in
                  let l = Vector.normalize (Vector.mult (-1.0) ldir) in
                  let v = Vector.normalize (Vector.mult (-1.0) d) in
                  let h = Vector.normalize (Vector.add v l) in
                  let to_color c = c *. intensity *. ((max 0.0 (Vector.dot n h)) ** (float phong)) in
                  let red = to_color specr in
                  let green = to_color specg in
                  let blue = to_color specb in
                  (prev_r +. red, prev_g +. green, prev_b +. blue)
              | Point (idx, intensity) ->
                  let {x = px; y = py; z = pz; dir = _} = get_vertex idx in
                  let l = Vector.normalize (Vector.sub (px, py, pz) intersect) in
                  let v = Vector.normalize (Vector.mult (-1.0) d) in
                  let h = Vector.normalize (Vector.add v l) in
                  let to_color c = c *. intensity *. ((max 0.0 (Vector.dot n h)) ** (float phong)) in
                  let red = to_color specr in
                  let green = to_color specg in
                  let blue = to_color specb in
                  (prev_r +. red, prev_g +. green, prev_b +. blue)
            in
            Some (List.fold_left apply_specular_light
                                (List.fold_left apply_diffuse_light ambient_color scene.lights)
                                scene.lights)
  in
  let to_color c = int_of_float (min 255.0 (floor (255.0 *. c))) in
  match List.fold_left closest_hit_element None scene.elements with
  | Some (r, g, b) -> (to_color r, to_color g, to_color b)
  | None -> (0, 0, 0)
