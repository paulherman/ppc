# Pico Pascal Compiler
- based on the Keiko virtual machine for IR
- implements an ARM backend
- based on the PPC written by [Mike Spivey](http://spivey.oriel.ox.ac.uk/corner/Welcome_to_Spivey%27s_Corner) for the [compilers course at University of Oxford](http://spivey.oriel.ox.ac.uk/corner/Compilers "Compilers Course on Spivey's Corner")

#### How to build
- you need ocamlbuild with ocamllex and ocamllyacc
- you need the [ounit](http://ounit.forge.ocamlcore.org/) library for tests
- you can see examples in the `examples` folder
- command line options `ppc (-d <level>) (-O) (-O2) <file>`
- run `make test` to run the tests (currently inexistent)

#### Things to do
- allow in, out registers to be specified at each instructions instead of when declared (i.e. map from vreg to list of pregs); maybe not needed as a register is used twice only in the case of DEFTMP combined with USETEMP, but then we can generate optional moves
- allow to specify two vregs to be allocated to the same preg; example: add dest, src <=> dest = dest + src <=> out = in0; possible solution: append optional move instruction
