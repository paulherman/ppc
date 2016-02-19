# Pico Pascal Compiler
- based on the Keiko virtual machine for IR
- implements an ARM backend
- based on the PPC written by [Mike Spivey](http://spivey.oriel.ox.ac.uk/corner/Compilers "Compilers Course on Spivey's Corner") for the compilers course at University of Oxford

#### Example programs

```pascal
var f, n, i : integer;

begin
    f := 1;
    n := 10;
    i := 1;
    while i <> n do
        f := f * i;
        i := i + 1;
    end
end.
```

