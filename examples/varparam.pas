(* varparam.p *)

proc one(var x: integer);
  proc two(); begin x := 37 end;
begin
  two()
end;

proc three();
  var y: integer;
begin
  one(y);
  print_num(y);
  newline()
end;

begin
  three()
end.

(*<<
37
>>*)

(*[[
@ picoPascal compiler output
        .global pmain

@ proc one(var x: integer);
        .text
_one:
        mov ip, sp
        stmfd sp!, {r0-r1}
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   two()
        mov r4, fp
        bl _two
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@   proc two(); begin x := 37 end;
_two:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   proc two(); begin x := 37 end;
        mov r0, #37
        ldr r1, [fp]
        ldr r1, [r1, #40]
        str r0, [r1]
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@ proc three();
_three:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
        sub sp, sp, #8
@   one(y);
        add r0, fp, #-4
        bl _one
@   print_num(y);
        ldr r0, [fp, #-4]
        bl print_num
@   newline()
        bl newline
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

pmain:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   three()
        bl _three
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@ End
]]*)
