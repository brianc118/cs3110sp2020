#
# Pure OCaml, no packages, no _tags
#

# bin-annot is required for Merlin and other IDE-like tools

.PHONY:	all clean byte native profile debug test

OCB_FLAGS = -tag bin_annot
OCB = 		ocamlbuild $(OCB_FLAGS)

all: native byte # profile debug

clean:
	$(OCB) -clean

native:
	$(OCB) exercises.native

byte:
	$(OCB) exercises.byte

profile:
	$(OCB) -tag profile exercises.native

debug:
	$(OCB) -tag debug exercises.byte

test: native
	./main.native "OCaml" "OCamlBuild" "users"