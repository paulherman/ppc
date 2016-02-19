proc foo(var a: array 10000 of integer);
  var c: array 256 of integer;
begin 
  a[5000] := 3 
end;

var b: array 10000 of integer;
begin foo(b) end.

(*[[
@ picoPascal compiler output
        .global pmain

@ proc foo(var a: array 10000 of integer);
        .text
_foo:
        mov ip, sp
        stmfd sp!, {r0-r1}
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
        sub sp, sp, #1024
@   a[5000] := 3 
        mov r0, #3
        ldr r1, [fp, #40]
        ldr r2, =20000
        add r1, r1, r2
        str r0, [r1]
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

pmain:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@ begin foo(b) end.
        ldr r0, =_b
        bl _foo
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

        .comm _b, 40000, 4
@ End
]]*)
