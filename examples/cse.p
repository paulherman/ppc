proc p(x, y: integer);
begin
  print_num((x-y)*(x-y)+x); newline()
end;

begin
  p(9, 5)
end.

(*<<
25
>>*)

(*[[
@ picoPascal compiler output
        .global pmain

@ proc p(x, y: integer);
        .text
_p:
        mov ip, sp
        stmfd sp!, {r0-r1}
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   print_num((x-y)*(x-y)+x); newline()
        ldr r5, [fp, #40]
        ldr r0, [fp, #44]
        sub r6, r5, r0
        mul r0, r6, r6
        add r0, r0, r5
        bl print_num
        bl newline
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

pmain:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   p(9, 5)
        mov r1, #5
        mov r0, #9
        bl _p
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@ End
]]*)
