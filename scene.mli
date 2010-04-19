type index = int
type color = int * int * int

type instruction = CommentInstr
                 | VertexInstr of float * float * float * float * float * float
                 | AmbientMaterialInstr of float * float * float
                 | DiffuseMaterialInstr of float * float * float
                 | SpecularMaterialInstr of float * float * float * int
                 | SphereInstr of index
                 | CameraInstr of index
                 | PointLightInstr of index * float
                 | DirectionalLightInstr of index * float
                 | SettingsInstr of bool * bool * bool * int * float
                 | IgnoreInstr of string

type scene

val create_scene : instruction list -> int -> int -> scene

val cast_ray_into_scene : scene -> int -> int -> color
