type index = int
type color = int * int * int

(* Abstract scene type *)
type scene

(* Instructions used to initialize elements and create scenes *)
type instruction =
  | CommentInstr
  | VertexInstr of float * float * float * float * float * float
  | AmbientMaterialInstr of float * float * float
  | DiffuseMaterialInstr of float * float * float
  | SpecularMaterialInstr of float * float * float * int
  | SphereInstr of index
  | PlaneInstr of index
  | CameraInstr of index
  | PointLightInstr of index * float
  | DirectionalLightInstr of index * float
  | SettingsInstr of bool * bool * bool * int * float
  | IgnoreInstr of string

(* Takes in a list of instructions and the width and height of the
 * intended scene, and creates a scene from that information *)
val create_scene : instruction list -> int -> int -> scene

(* Takes a created scene and an x and y coordinate and produces
 * the color which the ray tracer renders at that point *)
val cast_ray_into_scene : scene -> int -> int -> color
