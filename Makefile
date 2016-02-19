OPT = -O2 -g

MLGEN = kgen.ml simp.ml share.ml jumpopt.ml

all: $(MLGEN) build

build:
	ocamlbuild main.native
	mv main.native ppc

tools/nodexp: tools/%:
	$(MAKE) -C tools $*

%.ml: %.mlp tools/nodexp
	tools/nodexp $< >$@
	
build_test:
	ocamlbuild -use-ocamlfind -package oUnit unittest.native
	
test: build_test
	mv unittest.native unittest
	./unittest

force:

clean: force
	rm -f *.cmi *.cmo *.o *.output
	rm -f ppc unittest
	rm -f $(MLGEN)
	rm -rf _build
