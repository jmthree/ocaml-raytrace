module P = Parser

type index  = int
type color  = int * int * int
type vector = float * float * float
type vertex = { x : float;
                y : float;
                z : float;
                direction : vector }

type element = Sphere of index

type scene = { vertexes : vertex array;
               camera   : index;
               elements : element list }

let create_scene instrs =
  let create ((vtxs, camera, elements) as accum) = function
    | P.Vertex (x, y, z, dx, dy, dz) ->
        let vtx = {x = x; y = y; z = z; direction = (dx, dy, dz)} in
        (vtx :: vtxs, camera, elements)
    | P.Sphere idx -> (vtxs, camera, (Sphere idx) :: elements)
    | P.Camera idx -> (vtxs, Some idx, elements)
    | _ -> accum
  in
  let (vtxs, cam, els) = List.fold_left create ([], None, []) instrs in
  let vtxs = List.rev vtxs in
  match cam with
  | None -> failwith "Missing camera. Should never happen"
  | Some idx -> {vertexes = Array.of_list vtxs; camera = idx; elements = els}

let cast_ray_into_scene {vertexes = vtxs; camera = cam; elements = elms} x y =
  (0, 0, 0)

let print_scene {vertexes=vtxs; camera=cam; elements=els} = begin
  print_endline "Scene:";
  print_endline ("\tCamera: " ^ (string_of_int cam));
  print_endline "\tVertexes:";
  Array.iteri (fun i v -> Printf.printf "\t\t%d - %6.2f, %6.2f, %6.2f\n" i v.x v.y v.z) vtxs;
  print_endline "\tElements:";
  List.iter (function Sphere idx -> print_endline ("\t\tSphere: " ^ (string_of_int idx))) els
end
