(* Module for manipulating vectors *)
module Vector = struct
  type vector = float * float * float

  let values (x, y, z) = (x, y, z)
  let cross (x1, y1, z1) (x2, y2, z2) =
    (y1 *. z2 -. z1 *. y2,
     z1 *. x2 -. x1 *. z2,
     x1 *. y2 -. y1 *. x2)

  let dot (x1, y1, z1) (x2, y2, z2) =
    x1 *. x2 +. y1 *. y2 +. z1 *. z2

  let size (x, y, z) = sqrt (x ** 2.0 +. y ** 2.0 +. z ** 2.0)

  let norm ((x, y, z) as v) =
    let len = size v in
      (x /. len, y /. len, z /. len)

  let mult factor (x, y, z) =
    (factor *. x, factor *. y, factor *. z)

  let sub (x1, y1, z1) (x2, y2, z2) = (x1 -. x2, y1 -. y2, z1 -. z2)

  let add (x1, y1, z1) (x2, y2, z2) = (x1 +. x2, y1 +. y2, z1 +. z2)

  let invert = mult (-1.0)

  let to_string (x, y, z) =
    Printf.sprintf "Vector x:%.4f y:%.4f z:%.4f" x y z
end

type index  = int
type color  = int * int * int
type fcolor = float * float * float

type instruction =
  | CommentInstr
  | VertexInstr of float * float * float * float * float * float
  | AmbientMaterialInstr of float * float * float
  | DiffuseMaterialInstr of float * float * float
  | SpecularMaterialInstr of float * float * float * int
  | SphereInstr of int
  | PlaneInstr of index
  | TriangleInstr of index * index * index
  | CameraInstr of int
  | PointLightInstr of index * float
  | DirectionalLightInstr of index * float
  | SettingsInstr of bool * bool * bool * int * float
  | IgnoreInstr of string

(* Vertexes read in from the configuration *)
type vertex = { pos : Vector.vector;
                dir : Vector.vector }

(* The types of light our ray tracer can have in a scene *)
type light =
  | Directional of index * float
  | Point of index * float

(* The types of elements our ray tracer can have in the scene.
 * Each element has an index of a vertex, and materials for
 * diffuse, specular and ambient shading *)
type element =
  | Sphere of index * fcolor * fcolor * (fcolor * int)
  | Plane of index * fcolor * fcolor * (fcolor * int)
  | Triangle of index * index * index * fcolor * fcolor * (fcolor * int)

type camera = { idx : index;
                frame_x : int;
                frame_y : int }

type settings = { diffuse  : bool;
                  specular : bool;
                  shadows  : bool;
                  reflect_depth : int;
                  ambient_int  : float }

type scene = { vertexes : vertex array;
               lights   : light list;
               camera   : camera;
               settings : settings;
               elements : element list }

let element_to_string vertexes =
  let get_vertex = Array.get vertexes in function
    | Sphere (idx, _, _, _) ->
        let {pos = (x, y, z); dir = _} = get_vertex idx in
          Printf.sprintf "Sphere : %d : %.2f %.2f %.2f" idx x y z
    | Plane (idx, _, _, _) ->
        let {pos = (x, y, z); dir = _} = get_vertex idx in
          Printf.sprintf "Plane : %d : %.2f %.2f %.2f" idx x y z
    | Triangle (idxa, idxb, idxc, _, _, _) ->
        let {pos = a; dir = _} = get_vertex idxa in
        let {pos = b; dir = _} = get_vertex idxb in
        let {pos = c; dir = _} = get_vertex idxc in
          Printf.sprintf "Triangle : %d %d %d: %s %s %s" idxa idxb idxc
            (Vector.to_string a)
            (Vector.to_string b)
            (Vector.to_string c)

let light_to_string vertexes = function
  | Point (idx, _) ->
      let {pos = (x, y, z); dir = _} = Array.get vertexes idx in
      Printf.sprintf "Point : %d : %.2f %.2f %.2f" idx x y z
  | Directional (idx, _) ->
      let {pos = _; dir = (x, y, z)} = Array.get vertexes idx in
      Printf.sprintf "Directional : %d : %.2f %.2f %.2f" idx x y z

let create_scene instrs units_x units_y =
  let ambient_material = ref (0.2, 0.2, 0.2) in
  let diffuse_material = ref (1.0, 1.0, 1.0) in
  let specular_material = ref ((1.0, 1.0, 1.0), 64) in
  let settings = ref { diffuse = true;
                       specular = true;
                       shadows = true;
                       reflect_depth = 1;
                       ambient_int = 1.0 }
  in

  let create ((vtxs, camera, lights, elements) as accum) = function
    | VertexInstr (x, y, z, dx, dy, dz) ->
        let vtx = {pos = (x, y, z); dir = (dx, dy, dz)} in
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
    | PlaneInstr idx ->
        let element = Plane (idx + 1,
                             !ambient_material,
                             !diffuse_material,
                             !specular_material)
        in
          (vtxs, camera, lights, element :: elements)
    | TriangleInstr (idx1, idx2, idx3) ->
        let element = Triangle(idx1 + 1, idx2 + 1, idx3 + 1,
                             !ambient_material,
                             !diffuse_material,
                             !specular_material)
        in
          (vtxs, camera, lights, element :: elements)
    | CameraInstr idx ->
        (vtxs, idx + 1, lights, elements)
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
                        ambient_int = amb };
          accum
        end
    | _ -> accum
  in

  let cam_origin = {pos = (0.0, 0.0, 0.0); dir = (0.0, 0.0, 1.0)} in
  let (vtxs, cam, lights, els) = List.fold_left
                                   create
                                   ([cam_origin], 0, [], [])
                                   instrs
  in
    { vertexes  = Array.of_list (List.rev vtxs);
      lights    = lights;
      camera    = {idx = cam; frame_x = units_x; frame_y = units_y};
      settings  = !settings;
      elements  = els }

let find_intersect vertexes start direction element =
  let get_vertex = Array.get vertexes in
    match element with
      | Sphere (idx, _, _, _) ->
          let {pos = c; dir = cdir} = get_vertex idx in
          let r = Vector.size cdir in
          let dd = Vector.dot direction direction in
          let eminc = Vector.sub start c in
          let dec = Vector.dot direction eminc in
          let descrim = (dec ** 2.0 -.
                         (dd *. ((Vector.dot eminc eminc) -. (r ** 2.0)))) in
            if descrim < 0.0
            then None
            else
              let mint = (((-1.0 *. dec) -. (sqrt descrim)) /. dd) in
              let plust = (((-1.0 *. dec) -. (sqrt descrim)) /. dd) in
                if mint < 0.0 && plust < 0.0
                then None
                else
                  let t =
                    if mint < 0.0
                    then plust
                    else
                      if plust < 0.0
                      then mint
                      else min mint plust
                  in
                    Some t
      | Plane (idx, _, _, _) ->
          let {pos = p; dir = pdir} = get_vertex idx in
          let n = Vector.norm pdir in
          let p' = Vector.invert p in
          let d' = (-1.0) *. (Vector.dot p' n) in
          let ddotn = Vector.dot direction n in
            if ddotn = 0.0
            then None
            else
              let t = (d' -. (Vector.dot start n)) /. ddotn in
                if t <= 0.000001
                then None
                else
                    Some t
      | Triangle (idxa, idxb, idxc, _, _, _) ->
          let xe, ye, ze = start in
          let xd, yd, zd = direction in
          let {pos = (xa, ya, za); dir = _} = get_vertex idxa in
          let {pos = (xb, yb, zb); dir = _} = get_vertex idxb in
          let {pos = (xc, yc, zc); dir = _} = get_vertex idxc in
          let a, d, g, j = (xa -. xb), (xa -. xc), xd, (xa -. xe) in
          let b, e, h, k = (ya -. yb), (ya -. yc), yd, (ya -. ye) in
          let c, f, i, l = (za -. zb), (za -. zc), zd, (za -. ze) in
          let ei_min_hf = e *. i -. h *. f in
          let gf_min_di = g *. f -. d *. i in
          let dh_min_eg = d *. h -. e *. g in
          let m = a *. ei_min_hf +. b *. gf_min_di +. c *. dh_min_eg in
          let ak_min_jb = a *. k -. j *. b in
          let jc_min_al = j *. c -. a *. l in
          let bl_min_kc = b *. l -. k *. c in
          let t = -1.0 *. (f *. ak_min_jb +. e *. jc_min_al +. d *. bl_min_kc) /. m in
            if t <= 0.000001
            then None
            else
              let gamma = (i *. ak_min_jb +. h *. jc_min_al +. g *. bl_min_kc) /. m in
                if gamma < 0.0 || gamma > 1.0
                then None
                else
                  let beta = (j *. ei_min_hf +. h *. gf_min_di +. l *. dh_min_eg) /. m in
                    if beta < 0.0 || (beta +. gamma) > 1.0
                    then None
                    else Some t

let cast_ray_into_scene scene x y =
  (*let _ = Printf.printf "x %d y %d\n" x y in*)
  let get_vertex = Array.get scene.vertexes in
  let {idx = camera_idx; frame_x = ix; frame_y = iy} = scene.camera in
  let {pos = e; dir = cam_dir} = get_vertex camera_idx in
  let ((bzx, bzy, bzz) as bas_z) = Vector.norm (Vector.invert cam_dir) in
  let ((bxx, bxy, bxz) as bas_x) = Vector.norm (Vector.cross cam_dir
                                                  (0.0, 1.0, 0.0)) in
  let byx, byy, byz = Vector.cross bas_z bas_x in
  let frame_x, frame_y = float ix, float iy in
  let px, py, pz =
    (((float x) /. frame_x) -. 0.5,
     (-1. *. (frame_y /. frame_x)) *.  (((float y) /. frame_y) -. 0.5),
     (sqrt 3.0) /. (-2.0))
  in
  let d = ((bxx *. px) +. (byx *. py) +. (bzx *. pz),
           (bxy *. px) +. (byy *. py) +. (bzy *. pz),
           (bxz *. px) +. (byz *. py) +. (bzz *. pz))
  in

  let ambient_lighting (ka_red, ka_green, ka_blue) =
    let intensity = scene.settings.ambient_int in
      (ka_red *. intensity, ka_green *. intensity, ka_blue *. intensity)
  in

  let diffuse_lighting material normal intersect light =
    let kd_red, kd_green, kd_blue = material in
      match light with
        | Directional (idx, intens) ->
            let {pos = _; dir = ldir} = get_vertex idx in
            let l = Vector.norm (Vector.invert ldir) in
            let to_color c = c *. intens *. (max 0.0 (Vector.dot normal l)) in
              (to_color kd_red, to_color kd_green, to_color kd_blue)
        | Point (idx, intens) ->
            let {pos = p; dir = _} = get_vertex idx in
            let l = Vector.norm (Vector.sub p intersect) in
            let to_color c = c *. intens *. (max 0.0 (Vector.dot normal l)) in
              (to_color kd_red, to_color kd_green, to_color kd_blue)
  in

  let specular_lighting material normal intersect light =
    let (ks_red, ks_green, ks_blue), phong = material in
    let spec_color intensity light view hue =
      let h = Vector.norm (Vector.add view light) in
      let contrib = (max 0.0 (Vector.dot normal h)) in
        hue *. intensity *. (contrib ** (float phong))
    in
      match light with
        | Directional (idx, intensity) ->
            let {pos = _; dir = ldir} = get_vertex idx in
            let l = Vector.norm (Vector.invert ldir) in
            let v = Vector.norm (Vector.invert d) in
            let to_color = spec_color intensity l v in
              (to_color ks_red, to_color ks_green, to_color ks_blue)
        | Point (idx, intensity) ->
            let {pos = p; dir = _} = get_vertex idx in
            let l = Vector.norm (Vector.sub p intersect) in
            let v = Vector.norm (Vector.invert d) in
            let to_color = spec_color intensity l v in
              (to_color ks_red, to_color ks_green, to_color ks_blue)
  in

  let hit = List.fold_left
              (fun accum element ->
                 let intersect = find_intersect scene.vertexes e d element in
                   match intersect, accum with
                     | Some t, Some (t', _) ->
                         (*let _ = print_endline ("New hit: " ^
                          * (element_to_string scene.vertexes element)) in*)
                         (*let _ = if t < t' then (print_endline "Replacing last
                          * hit") else () in*)
                         if t < t' then Some (t, element) else accum
                     | Some t, _ ->
                         (*let _ = print_endline ("New hit: " ^
                          * (element_to_string scene.vertexes element)) in*)
                         (*let _ = (print_endline "Replacing last hit") in*)
                           Some (t, element)
                     | None, _ -> accum)
              None
              scene.elements
  in

  let calculate_color (intersect, normal, amb_mat, diff_mat, spec_mat) =
    let base = (0.0, 0.0, 0.0) in
    let ambient = ambient_lighting amb_mat in
    let ray_towards_light = function
      | Directional (idx, _) ->
          let {pos = _; dir = ldir} = get_vertex idx in
            Vector.invert ldir, infinity
      | Point (idx, _) ->
          let {pos = p; dir = _} = get_vertex idx in
            Vector.sub p intersect, 1.0
    in
    let in_shadow light =
      scene.settings.shadows &&
      (*let _ = print_endline (light_to_string scene.vertexes light) in*)
      let direction, endpoint = ray_towards_light light in
      let p_above = (Vector.add intersect (Vector.mult 0.001 direction)) in
        List.exists
          (fun element ->
             match find_intersect scene.vertexes p_above direction element with
               | Some t -> t >= 0.0000001 && t <= endpoint
               | None -> false)
          scene.elements
    in
    let diff_shade = diffuse_lighting diff_mat normal intersect in
    let diffuse =
      if scene.settings.diffuse
      then List.fold_left
             (fun diff light ->
                if in_shadow light
                then diff
                else Vector.add (diff_shade light) diff)
             base
             scene.lights
      else base
    in
    let spec_shade = specular_lighting spec_mat normal intersect in
    let specular =
      if scene.settings.specular
      then List.fold_left
             (fun spec light ->
                if in_shadow light
                then spec
                else Vector.add (spec_shade light) spec)
             base
             scene.lights
      else base
    in
      Vector.add ambient (Vector.add diffuse specular)
  in

  let normalize_color c = int_of_float (min 255.0 (floor (255.0 *. c))) in
    match hit with
      | None -> (0, 0, 0)
      | Some (t, element) ->
          let intersect = Vector.add e (Vector.mult t d) in
          let normal, amb, diff, spec = match element with
            | Sphere (idx, amb, diff, spec) ->
                let {pos = center; dir = cdir} = get_vertex idx in
                let normal = Vector.mult
                               (1.0 /. (Vector.size cdir))
                               (Vector.sub intersect center) in
                  (normal, amb, diff, spec)
            | Plane  (idx, amb, diff, spec) ->
                let {pos = _; dir = normal} = get_vertex idx in
                let normal = Vector.norm normal in
                  (normal, amb, diff, spec)
            | Triangle (idxa, idxb, idxc, amb, diff, spec) ->
                let {pos = a; dir = _} = get_vertex idxa in
                let {pos = b; dir = _} = get_vertex idxb in
                let {pos = c; dir = _} = get_vertex idxc in
                let normal = Vector.cross (Vector.sub c a) (Vector.sub b a) in
                  (normal, amb, diff, spec)
          in
          let r, g, b = calculate_color (intersect, normal, amb, diff, spec) in
            (normalize_color r, normalize_color g, normalize_color b)
