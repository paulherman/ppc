(* N queens with array of choices *)

const N = 8;

proc queens(k: integer; var choice: array N of integer);
  var y, j, q: integer; ok: boolean;
begin
  if k = N then
    print(choice)
  else
    y := 0;
    while y < N do
      j := 0; ok := true;
      while ok and (j < k) do
        q := choice[j];
        ok := (q <> y) and (q+j <> y+k) and (q-j <> y-k);
        j := j+1
      end;
      if ok then 
        choice[k] := y;
        queens(k+1, choice)
      end;
      y := y+1
    end
  end
end;

proc print(var choice: array N of integer);
  var x: integer;
begin
  x := 0;
  while x < N do
    print_num(choice[x]+1);
    x := x+1
  end;
  newline()
end;

var choice: array N of integer;

begin
  queens(0, choice)
end.

(*<<
15863724
16837425
17468253
17582463
24683175
25713864
25741863
26174835
26831475
27368514
27581463
28613574
31758246
35281746
35286471
35714286
35841726
36258174
36271485
36275184
36418572
36428571
36814752
36815724
36824175
37285146
37286415
38471625
41582736
41586372
42586137
42736815
42736851
42751863
42857136
42861357
46152837
46827135
46831752
47185263
47382516
47526138
47531682
48136275
48157263
48531726
51468273
51842736
51863724
52468317
52473861
52617483
52814736
53168247
53172864
53847162
57138642
57142863
57248136
57263148
57263184
57413862
58413627
58417263
61528374
62713584
62714853
63175824
63184275
63185247
63571428
63581427
63724815
63728514
63741825
64158273
64285713
64713528
64718253
68241753
71386425
72418536
72631485
73168524
73825164
74258136
74286135
75316824
82417536
82531746
83162574
84136275
>>*)

(*[[
@ picoPascal compiler output
        .global pmain

@ proc queens(k: integer; var choice: array N of integer);
        .text
_queens:
        mov ip, sp
        stmfd sp!, {r0-r1}
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
        sub sp, sp, #16
@   if k = N then
        ldr r0, [fp, #40]
        cmp r0, #8
        bne .L3
@     print(choice)
        ldr r0, [fp, #44]
        bl _print
        b .L1
.L3:
@     y := 0;
        mov r0, #0
        str r0, [fp, #-4]
        b .L6
.L5:
@       j := 0; ok := true;
        mov r0, #0
        str r0, [fp, #-8]
        mov r0, #1
        strb r0, [fp, #-13]
        b .L9
.L8:
@       q := choice[j];
        ldr r5, [fp, #-8]
        ldr r0, [fp, #44]
        lsl r1, r5, #2
        add r0, r0, r1
        ldr r6, [r0]
        str r6, [fp, #-12]
@       ok := (q <> y) and (q+j <> y+k) and (q-j <> y-k);
        ldr r7, [fp, #-4]
        ldr r8, [fp, #40]
        cmp r6, r7
        mov r0, #0
        movne r0, #1
        add r1, r6, r5
        add r2, r7, r8
        cmp r1, r2
        mov r1, #0
        movne r1, #1
        and r0, r0, r1
        sub r1, r6, r5
        sub r2, r7, r8
        cmp r1, r2
        mov r1, #0
        movne r1, #1
        and r0, r0, r1
        strb r0, [fp, #-13]
@         j := j+1
        add r0, r5, #1
        str r0, [fp, #-8]
.L9:
@       while ok and (j < k) do
        ldrb r0, [fp, #-13]
        cmp r0, #0
        beq .L10
        ldr r0, [fp, #-8]
        ldr r1, [fp, #40]
        cmp r0, r1
        blt .L8
.L10:
@       if ok then 
        ldrb r0, [fp, #-13]
        cmp r0, #0
        beq .L14
@       choice[k] := y;
        ldr r0, [fp, #-4]
        ldr r1, [fp, #44]
        ldr r2, [fp, #40]
        lsl r2, r2, #2
        add r1, r1, r2
        str r0, [r1]
@       queens(k+1, choice)
        ldr r1, [fp, #44]
        ldr r0, [fp, #40]
        add r0, r0, #1
        bl _queens
.L14:
@       y := y+1
        ldr r0, [fp, #-4]
        add r0, r0, #1
        str r0, [fp, #-4]
.L6:
@     while y < N do
        ldr r0, [fp, #-4]
        cmp r0, #8
        blt .L5
.L1:
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@ proc print(var choice: array N of integer);
_print:
        mov ip, sp
        stmfd sp!, {r0-r1}
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
        sub sp, sp, #8
@   x := 0;
        mov r0, #0
        str r0, [fp, #-4]
        b .L17
.L16:
@     print_num(choice[x]+1);
        ldr r0, [fp, #40]
        ldr r1, [fp, #-4]
        lsl r1, r1, #2
        add r0, r0, r1
        ldr r0, [r0]
        add r0, r0, #1
        bl print_num
@     x := x+1
        ldr r0, [fp, #-4]
        add r0, r0, #1
        str r0, [fp, #-4]
.L17:
@   while x < N do
        ldr r0, [fp, #-4]
        cmp r0, #8
        blt .L16
@   newline()
        bl newline
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

pmain:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   queens(0, choice)
        ldr r1, =_choice
        mov r0, #0
        bl _queens
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

        .comm _choice, 32, 4
@ End
]]*)
