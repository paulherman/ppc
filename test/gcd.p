(* gcd.p *)

var x, y: integer;

begin
  x := 3 * 37; y := 5 * 37;
  while x <> y do
    if x > y then
      x := x - y
    else
      y := y - x
    end
  end;
  print_num(x); newline()
end.

(*<<
37
>>*)

(*[[
@ picoPascal compiler output
        .global pmain

        .text
pmain:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   x := 3 * 37; y := 5 * 37;
        mov r0, #111
        ldr r1, =_x
        str r0, [r1]
        mov r0, #185
        ldr r1, =_y
        str r0, [r1]
        b .L3
.L2:
@     if x > y then
        ldr r5, =_x
        ldr r6, [r5]
        ldr r0, =_y
        ldr r7, [r0]
        cmp r6, r7
        ble .L6
@       x := x - y
        sub r0, r6, r7
        str r0, [r5]
        b .L3
.L6:
@       y := y - x
        ldr r5, =_y
        ldr r0, [r5]
        ldr r1, =_x
        ldr r1, [r1]
        sub r0, r0, r1
        str r0, [r5]
.L3:
@   while x <> y do
        ldr r0, =_x
        ldr r5, [r0]
        ldr r0, =_y
        ldr r0, [r0]
        cmp r5, r0
        bne .L2
@   print_num(x); newline()
        mov r0, r5
        bl print_num
        bl newline
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

        .comm _x, 4, 4
        .comm _y, 4, 4
@ End
]]*)
