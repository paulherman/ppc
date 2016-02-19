# Pico Pascal Compiler
- based on the Keiko virtual machine for IR
- implements an ARM backend
- based on the PPC written by [Mike Spivey](http://spivey.oriel.ox.ac.uk/corner/Compilers "Compilers Course on Spivey's Corner") for the compilers course at University of Oxford

#### How to build?
- you need ocamlbuild with ocamllex and ocamllyacc
- you need the [ounit](http://ounit.forge.ocamlcore.org/) library for tests
- you can see examples in the `examples` folder
- command line options: ppc (-d <level>) (-O) (-O2) <file>
