#
# Pure OCaml, no packages, no _tags
#

# bin-annot is required for Merlin and other IDE-like tools

.PHONY:	all clean byte native profile debug test

OCB_FLAGS = -tag bin_annot -tag thread -use-ocamlfind -pkg lwt -pkg lwt.unix
OCB = 		ocamlbuild $(OCB_FLAGS)

all: native byte # profile debug

clean:
	$(OCB) -clean

native:
	$(OCB) exercises.native
	$(OCB) filemon.native

byte:
	$(OCB) exercises.byte
	$(OCB) filemon.byte

profile:
	$(OCB) -tag profile exercises.native
	$(OCB) -tag profile filemon.native

debug:
	$(OCB) -tag debug exercises.byte
	$(OCB) -tag debug filemon.byte

test: native
	./main.native "OCaml" "OCamlBuild" "users"