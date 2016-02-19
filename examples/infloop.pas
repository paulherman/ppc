proc foo();
begin
  while true do newline() end
end;

begin end.

(*[[
@ picoPascal compiler output
        .global pmain

@ proc foo();
        .text
_foo:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
.L2:
@   while true do newline() end
        bl newline
        b .L2
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

pmain:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@ End
]]*)
