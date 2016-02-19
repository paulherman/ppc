proc f(x: integer): integer;
begin return x end;

proc mmm(x: integer): integer;
  var y: integer;
begin 
  y := f(x); 
  (* Translated to mul r0, r0, r0: *)
  return y*y 
end;

var x, y, z: integer;

begin
  x := 3;
  y := 5;
  z := x * y;
  print_num(z);
  newline();
  print_num(mmm(12));
  newline()
end.

(*<<
15
144
>>*)

(*[[
@ picoPascal compiler output
        .global pmain

@ proc f(x: integer): integer;
        .text
_f:
        mov ip, sp
        stmfd sp!, {r0-r1}
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@ begin return x end;
        ldr r0, [fp, #40]
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@ proc mmm(x: integer): integer;
_mmm:
        mov ip, sp
        stmfd sp!, {r0-r1}
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
        sub sp, sp, #8
@   y := f(x); 
        ldr r0, [fp, #40]
        bl _f
        str r0, [fp, #-4]
@   return y*y 
        mul r0, r0, r0
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

pmain:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   x := 3;
        mov r5, #3
        ldr r0, =_x
        str r5, [r0]
@   y := 5;
        mov r6, #5
        ldr r0, =_y
        str r6, [r0]
@   z := x * y;
        mul r5, r5, r6
        ldr r0, =_z
        str r5, [r0]
@   print_num(z);
        mov r0, r5
        bl print_num
@   newline();
        bl newline
@   print_num(mmm(12));
        mov r0, #12
        bl _mmm
        bl print_num
@   newline()
        bl newline
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

        .comm _x, 4, 4
        .comm _y, 4, 4
        .comm _z, 4, 4
@ End
]]*)
