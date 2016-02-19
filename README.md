# Pico Pascal Compiler
- based on the Keiko virtual machine for IR
- implements an ARM backend

=== Example programs

```
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
