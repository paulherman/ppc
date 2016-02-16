# lab2/Makefile
# $Id: Makefile 39 2005-05-03 15:11:44Z mike $

all: ppc

PPC = print.cmo source.cmo util.cmo mach.cmo keiko.cmo \
	dict.cmo tree.cmo lexer.cmo parser.cmo check.cmo target.cmo \
	regs.cmo simp.cmo share.cmo jumpopt.cmo tran.cmo kgen.cmo gen.cmo regalloc.cmo arm.cmo \
	main.cmo

ppc: $(PPC)
	ocamlc -g -o $@ $^

parser.ml parser.mli: parser.mly
	ocamlyacc -v parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

%.ml: %.mlp tools/nodexp
	tools/nodexp $< >$@

%.cmi: %.mli
	ocamlc -c -g $<

%.cmo: %.ml
	ocamlc -c -g $<

tools/nodexp tools/pibake: tools/%: 
	$(MAKE) -C tools $*

TESTSRC := $(wildcard test/*.p)
OPT = -O2

# test0 -- compile tests and diff object code
test0 : $(TESTSRC:test/%.p=test0-%)

# test1 -- compile tests and execute with QEMU
test1 : $(TESTSRC:test/%.p=test1-%)

# test2 -- compile tests and execute using remote RPi
test2 : $(TESTSRC:test/%.p=test2-%)

SCRIPT1 = -e '1,/^(\*\[\[/d' -e '/^]]\*)/q' -e p
SCRIPT2 = -e '1,/^(\*<</d' -e '/^>>\*)/q' -e p

ARMGCC = arm-linux-gnueabihf-gcc -marm -march=armv6
QEMU = qemu-arm

test0-%: force
	@echo "*** Test $*.p"
	./ppc $(OPT) test/$*.p >b.s
	-sed -n $(SCRIPT1) test/$*.p | diff -u -b - b.s
	@echo

test1-%: pas0.o force
	@echo "*** Test $*.p"
	./ppc $(OPT) test/$*.p >b.s
	-sed -n $(SCRIPT1) test/$*.p | diff -u -b - b.s
	$(ARMGCC) b.s pas0.o -static -o b.out 
	$(QEMU) ./b.out >b.test
	sed -n $(SCRIPT2) test/$*.p | diff - b.test
	@echo "*** Passed"; echo

pas0.o: pas0.c
	$(ARMGCC) -c $< -o $@

test2-%: tools/pibake force
	@echo "*** Test $*.p"
	./ppc $(OPT) test/$*.p >b.s
	-sed -n $(SCRIPT1) test/$*.p | diff -u -b - b.s
	tools/pibake b.s >b.test
	sed -n $(SCRIPT2) test/$*.p | diff - b.test
	@echo "*** Passed"; echo


promote: $(TESTSRC:test/%.p=promote-%)

promote-%: force
	./ppc $(OPT) test/$*.p >b.s
	sed -f promote.sed test/$*.p >test/$*.new
	mv test/$*.new test/$*.p

force:

MLGEN = parser.mli parser.ml lexer.ml keiko.ml kgen.ml tran.ml simp.ml \
	share.ml jumpopt.ml

ML = $(MLGEN) check.ml check.mli dict.ml dict.mli arm.ml arm.mli \
	lexer.mli mach.ml mach.mli main.ml keiko.mli \
	print.ml print.mli source.ml source.mli kgen.mli tree.ml \
	tree.mli util.ml tran.mli target.mli target.ml \
	simp.mli share.mli regs.mli regs.ml jumpopt.mli gen.ml gen.mli regalloc.ml regalloc.mli \


clean: force
	rm -f *.cmi *.cmo *.o *.output
	rm -f $(MLGEN)
	rm -f ppc b.out b.s b.test

depend: $(ML) force
	(sed '/^###/q' Makefile; echo; ocamldep $(ML)) >new
	mv new Makefile

###

parser.cmi : tree.cmi keiko.cmi dict.cmi
parser.cmo : tree.cmi lexer.cmi keiko.cmi dict.cmi parser.cmi
parser.cmx : tree.cmx lexer.cmx keiko.cmx dict.cmx parser.cmi
lexer.cmo : util.cmo source.cmi print.cmi parser.cmi keiko.cmi dict.cmi \
    lexer.cmi
lexer.cmx : util.cmx source.cmx print.cmx parser.cmx keiko.cmx dict.cmx \
    lexer.cmi
keiko.cmo : source.cmi print.cmi keiko.cmi
keiko.cmx : source.cmx print.cmx keiko.cmi
kgen.cmo : tree.cmi tran.cmi target.cmi source.cmi simp.cmi share.cmi \
    regs.cmi print.cmi mach.cmi lexer.cmi keiko.cmi jumpopt.cmi dict.cmi arm.cmi \
    kgen.cmi
kgen.cmx : tree.cmx tran.cmx target.cmx source.cmx simp.cmx share.cmx \
    regs.cmx print.cmx mach.cmx lexer.cmx keiko.cmx jumpopt.cmx dict.cmx arm.cmx \
    kgen.cmi
tran.cmo : target.cmi source.cmi regs.cmi print.cmi keiko.cmi tran.cmi
tran.cmx : target.cmx source.cmx regs.cmx print.cmx keiko.cmx tran.cmi
simp.cmo : util.cmo keiko.cmi simp.cmi
simp.cmx : util.cmx keiko.cmx simp.cmi
share.cmo : regs.cmi print.cmi mach.cmi keiko.cmi share.cmi
share.cmx : regs.cmx print.cmx mach.cmx keiko.cmx share.cmi
jumpopt.cmo : util.cmo keiko.cmi jumpopt.cmi
jumpopt.cmx : util.cmx keiko.cmx jumpopt.cmi
check.cmo : tree.cmi print.cmi mach.cmi lexer.cmi keiko.cmi dict.cmi \
    check.cmi
check.cmx : tree.cmx print.cmx mach.cmx lexer.cmx keiko.cmx dict.cmx \
    check.cmi
check.cmi : tree.cmi print.cmi
dict.cmo : print.cmi mach.cmi keiko.cmi dict.cmi
dict.cmx : print.cmx mach.cmx keiko.cmx dict.cmi
dict.cmi : print.cmi mach.cmi keiko.cmi
lexer.cmi : parser.cmi keiko.cmi dict.cmi
mach.cmo : mach.cmi
mach.cmx : mach.cmi
mach.cmi :
main.cmo : tree.cmi tran.cmi source.cmi print.cmi parser.cmi mach.cmi \
    lexer.cmi kgen.cmi check.cmi gen.cmi regalloc.cmi
main.cmx : tree.cmx tran.cmx source.cmx print.cmx parser.cmx mach.cmx \
    lexer.cmx kgen.cmx check.cmx gen.cmx regalloc.cmx
keiko.cmi : print.cmi
print.cmo : print.cmi
print.cmx : print.cmi
print.cmi :
source.cmo : print.cmi source.cmi
source.cmx : print.cmx source.cmi
source.cmi : print.cmi
kgen.cmi : tree.cmi
tree.cmo : print.cmi keiko.cmi dict.cmi tree.cmi
tree.cmx : print.cmx keiko.cmx dict.cmx tree.cmi
tree.cmi : keiko.cmi dict.cmi
util.cmo :
util.cmx :
tran.cmi : keiko.cmi
target.cmi : print.cmi keiko.cmi
target.cmo : print.cmi keiko.cmi target.cmi
target.cmx : print.cmx keiko.cmx target.cmi
simp.cmi : keiko.cmi
share.cmi : keiko.cmi
regs.cmi : target.cmi
regs.cmo : util.cmo target.cmi print.cmi regs.cmi
regs.cmx : util.cmx target.cmx print.cmx regs.cmi
jumpopt.cmi : keiko.cmi
gen.cmo : keiko.cmi gen.cmi
gen.cmx : keiko.cmi gen.cmi
gen.cmi : keiko.cmi
arm.cmo : arm.cmi gen.cmi keiko.cmi regalloc.cmi
arm.cmx : arm.cmi gen.cmx keiko.cmx regalloc.cmx
arm.cmi : gen.cmi keiko.cmi regalloc.cmi
regalloc.cmo : regalloc.cmi
regalloc.cmx : regalloc.cmi
regalloc.cmi :

