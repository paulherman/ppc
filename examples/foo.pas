(* foo.p *)

proc gcd(u, v: integer): integer;
  var x, y: reg integer;
begin
  x := u; y := v;
  while x <> y do
    if x < y then
      y := y - x
    else
      x := x - y
    end
  end;
  return x
end;
  
var z: integer;
begin
  z := gcd(3*37, 5*37);
  print_string("The final answer is calculated as ");
  print_num(z); newline()
end.

(*<<
The final answer is calculated as 37
>>*)

(*[[
@ picoPascal compiler output
        .global pmain

@ proc gcd(u, v: integer): integer;
        .text
_gcd:
        mov ip, sp
        stmfd sp!, {r0-r1}
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   x := u; y := v;
        ldr r5, [fp, #40]
        ldr r6, [fp, #44]
        b .L4
.L3:
@     if x < y then
        cmp r5, r6
        bge .L7
@       y := y - x
        sub r6, r6, r5
        b .L4
.L7:
@       x := x - y
        sub r5, r5, r6
.L4:
@   while x <> y do
        cmp r5, r6
        bne .L3
@   return x
        mov r0, r5
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

pmain:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   z := gcd(3*37, 5*37);
        mov r1, #185
        mov r0, #111
        bl _gcd
        ldr r5, =_z
        str r0, [r5]
@   print_string("The final answer is calculated as ");
        mov r1, #34
        ldr r0, =g1
        bl print_string
@   print_num(z); newline()
        ldr r0, [r5]
        bl print_num
        bl newline
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

        .comm _z, 4, 4
        .data
g1:
        .byte 84, 104, 101, 32, 102, 105, 110, 97, 108, 32
        .byte 97, 110, 115, 119, 101, 114, 32, 105, 115, 32
        .byte 99, 97, 108, 99, 117, 108, 97, 116, 101, 100
        .byte 32, 97, 115, 32
        .byte 0
@ End
]]*)
