OPT = -O2 -g

MLGEN = keiko.ml kgen.ml tran.ml simp.ml share.ml jumpopt.ml

all: $(MLGEN) build

build:
	ocamlbuild main.native
	mv main.native ppc

tools/nodexp: tools/%:
	$(MAKE) -C tools $*

%.ml: %.mlp tools/nodexp
	tools/nodexp $< >$@

force:

clean: force
	rm -f *.cmi *.cmo *.o *.output
	rm -f ppc
	rm -f $(MLGEN)
	rm -rf _build
