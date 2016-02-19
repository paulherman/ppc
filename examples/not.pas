var b: boolean;

begin
  b := not b;
  if b then print_string("ok"); newline() end
end.

(*<<
ok
>>*)

(*[[
@ picoPascal compiler output
        .global pmain

        .text
pmain:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   b := not b;
        ldr r5, =_b
        ldrb r0, [r5]
        eor r6, r0, #1
        strb r6, [r5]
@   if b then print_string("ok"); newline() end
        cmp r6, #0
        beq .L2
        mov r1, #2
        ldr r0, =g1
        bl print_string
        bl newline
.L2:
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

        .comm _b, 1, 4
        .data
g1:
        .byte 111, 107
        .byte 0
@ End
]]*)
