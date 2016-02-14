var x: integer;

proc p();
var y, z: integer;
begin
  y := 1;
  z := y + 1;
  z := 3;
  z := y + 1;
  x := z
end;

begin
  p();
  print_num(x);
  newline()
end.

(*<<
2
>>*)

(*[[
@ picoPascal compiler output
        .global pmain

@ proc p();
        .text
_p:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
        sub sp, sp, #8
@   y := 1;
        mov r5, #1
        str r5, [fp, #-4]
@   z := y + 1;
        add r5, r5, #1
        str r5, [fp, #-8]
@   z := 3;
        mov r0, #3
        str r0, [fp, #-8]
@   z := y + 1;
        str r5, [fp, #-8]
@   x := z
        ldr r0, =_x
        str r5, [r0]
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

pmain:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   p();
        bl _p
@   print_num(x);
        ldr r0, =_x
        ldr r0, [r0]
        bl print_num
@   newline()
        bl newline
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

        .comm _x, 4, 4
@ End
]]*)
