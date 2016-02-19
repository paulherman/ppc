(* Pascal's triangle *)

const n = 10;

proc pascal2();
  var i, j: integer;
  var a: array n of array n+1 of integer;
begin
  i := 0;
  while i < n do
    a[i][0] := 1; j := 1;
    print_num(a[i][0]);
    while j <= i do
      a[i][j] := a[i-1][j-1] + a[i-1][j];
      print_char(' '); print_num(a[i][j]);
      j := j+1
    end;
    a[i][i+1] := 0;
    newline();
    i := i+1
  end
end;

begin
  pascal2()
end.

(*<<
1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
1 5 10 10 5 1
1 6 15 20 15 6 1
1 7 21 35 35 21 7 1
1 8 28 56 70 56 28 8 1
1 9 36 84 126 126 84 36 9 1
>>*)

(*[[
@ picoPascal compiler output
        .global pmain

@ proc pascal2();
        .text
_pascal2:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
        sub sp, sp, #448
@   i := 0;
        mov r0, #0
        str r0, [fp, #-4]
        b .L3
.L2:
@     a[i][0] := 1; j := 1;
        mov r0, #1
        add r1, fp, #-448
        ldr r2, [fp, #-4]
        mov r3, #44
        mul r2, r2, r3
        add r1, r1, r2
        str r0, [r1]
        mov r0, #1
        str r0, [fp, #-8]
@     print_num(a[i][0]);
        add r0, fp, #-448
        ldr r1, [fp, #-4]
        mov r2, #44
        mul r1, r1, r2
        add r0, r0, r1
        ldr r0, [r0]
        bl print_num
        b .L6
.L5:
@       a[i][j] := a[i-1][j-1] + a[i-1][j];
        add r0, fp, #-448
        ldr r1, [fp, #-4]
        mov r2, #44
        mul r1, r1, r2
        add r5, r0, r1
        ldr r0, [fp, #-8]
        lsl r6, r0, #2
        add r0, r5, #-44
        add r7, r0, r6
        ldr r0, [r7, #-4]
        ldr r1, [r7]
        add r0, r0, r1
        add r1, r5, r6
        str r0, [r1]
@       print_char(' '); print_num(a[i][j]);
        mov r0, #32
        bl print_char
        add r0, fp, #-448
        ldr r1, [fp, #-4]
        mov r2, #44
        mul r1, r1, r2
        add r0, r0, r1
        ldr r1, [fp, #-8]
        lsl r1, r1, #2
        add r0, r0, r1
        ldr r0, [r0]
        bl print_num
@       j := j+1
        ldr r0, [fp, #-8]
        add r0, r0, #1
        str r0, [fp, #-8]
.L6:
@     while j <= i do
        ldr r5, [fp, #-4]
        ldr r0, [fp, #-8]
        cmp r0, r5
        ble .L5
@     a[i][i+1] := 0;
        mov r0, #0
        add r1, fp, #-448
        mov r2, #44
        mul r2, r5, r2
        add r1, r1, r2
        lsl r2, r5, #2
        add r1, r1, r2
        str r0, [r1, #4]
@     newline();
        bl newline
@     i := i+1
        ldr r0, [fp, #-4]
        add r0, r0, #1
        str r0, [fp, #-4]
.L3:
@   while i < n do
        ldr r0, [fp, #-4]
        cmp r0, #10
        blt .L2
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

pmain:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   pascal2()
        bl _pascal2
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@ End
]]*)
