OCAMLMAKEFILE = ./OCamlMakefile

OCAMLFLAGS = -w A -warn-error a
LIBS = str graphics
SOURCES = scene.mli scene.ml parser.ml render.ml
RESULT  = trace

include $(OCAMLMAKEFILE)
