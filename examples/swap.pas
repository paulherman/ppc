const n = 10;

var a: array n of integer; 

proc swap(i, j: integer);
  var t: reg integer;
begin
  t := a[i]; 
  a[i] := a[j]; 
  a[j] := t
end;

proc main();
  var i: integer;
begin
  for i := 0 to n-1 do a[i] := i end;
  swap(3, 6);
  for i := 0 to n-1 do print_num(a[i]) end;
  newline()
end;

begin 
  main()
end.

(*<<
0126453789
>>*)

(*[[
@ picoPascal compiler output
        .global pmain

@ proc swap(i, j: integer);
        .text
_swap:
        mov ip, sp
        stmfd sp!, {r0-r1}
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   t := a[i]; 
        ldr r6, =_a
        ldr r0, [fp, #40]
        lsl r0, r0, #2
        add r7, r6, r0
        ldr r5, [r7]
@   a[i] := a[j]; 
        ldr r0, [fp, #44]
        lsl r0, r0, #2
        add r6, r6, r0
        ldr r0, [r6]
        str r0, [r7]
@   a[j] := t
        str r5, [r6]
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@ proc main();
_main:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
        sub sp, sp, #8
@   for i := 0 to n-1 do a[i] := i end;
        mov r0, #0
        str r0, [fp, #-4]
        b .L4
.L3:
        ldr r5, [fp, #-4]
        ldr r0, =_a
        lsl r1, r5, #2
        add r0, r0, r1
        str r5, [r0]
        add r0, r5, #1
        str r0, [fp, #-4]
.L4:
        ldr r0, [fp, #-4]
        cmp r0, #9
        ble .L3
@   swap(3, 6);
        mov r1, #6
        mov r0, #3
        bl _swap
@   for i := 0 to n-1 do print_num(a[i]) end;
        mov r0, #0
        str r0, [fp, #-4]
        b .L6
.L5:
        ldr r0, =_a
        ldr r1, [fp, #-4]
        lsl r1, r1, #2
        add r0, r0, r1
        ldr r0, [r0]
        bl print_num
        ldr r0, [fp, #-4]
        add r0, r0, #1
        str r0, [fp, #-4]
.L6:
        ldr r0, [fp, #-4]
        cmp r0, #9
        ble .L5
@   newline()
        bl newline
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

pmain:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   main()
        bl _main
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

        .comm _a, 40, 4
@ End
]]*)
