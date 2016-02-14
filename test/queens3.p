(* N queens with bitmap arrays *)

const N = 8;

var 
  choice: array N of integer;
  rank: array N of boolean;
  diagup, diagdown: array 2 * N - 1 of boolean;

proc queens(k: integer);
  var y, j, q: integer; ok: boolean;
begin
  if k = N then
    print()
  else
    y := 0;
    while y < N do
      if rank[y] and diagup[k+y] and diagdown[k-y+N-1] then
        rank[y] := false; diagup[k+y] := false; diagdown[k-y+N-1] := false;
        choice[k] := y; queens(k+1);
        rank[y] := true; diagup[k+y] := true; diagdown[k-y+N-1] := true;
      end;
      y := y+1
    end
  end
end;

proc print();
  var x: integer;
begin
  x := 0;
  while x < N do
    print_num(choice[x]+1);
    x := x+1
  end;
  newline()
end;

proc init();
  var i: integer;
begin
  i := 0; 
  while i < N do 
    rank[i] := true; 
    i := i+1 
  end;
  i := 0; 
  while i < 2*N-1 do 
    diagup[i] := true; diagdown[i] := true ;
    i := i+1
  end
end;

begin
  init();
  queens(0)
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

@ proc queens(k: integer);
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
@     print()
        bl _print
        b .L1
.L3:
@     y := 0;
        mov r0, #0
        str r0, [fp, #-4]
        b .L6
.L5:
@       if rank[y] and diagup[k+y] and diagdown[k-y+N-1] then
        ldr r5, =_rank
        ldr r6, [fp, #-4]
        add r7, r5, r6
        ldrb r0, [r7]
        cmp r0, #0
        beq .L10
        ldr r8, =_diagup
        ldr r9, [fp, #40]
        add r0, r8, r9
        add r10, r0, r6
        ldrb r0, [r10]
        cmp r0, #0
        beq .L10
        ldr r0, =_diagdown
        sub r1, r9, r6
        add r1, r0, r1
        add r1, r1, #7
        ldrb r2, [r1]
        cmp r2, #0
        beq .L10
@       rank[y] := false; diagup[k+y] := false; diagdown[k-y+N-1] := false;
        mov r2, #0
        strb r2, [r7]
        mov r2, #0
        strb r2, [r10]
        mov r2, #0
        strb r2, [r1]
@       choice[k] := y; queens(k+1);
        ldr r1, =_choice
        lsl r2, r9, #2
        add r1, r1, r2
        str r6, [r1]
        mov r6, r0
        add r0, r9, #1
        bl _queens
@       rank[y] := true; diagup[k+y] := true; diagdown[k-y+N-1] := true;
        ldr r7, [fp, #-4]
        mov r0, #1
        add r1, r5, r7
        strb r0, [r1]
        ldr r5, [fp, #40]
        mov r0, #1
        add r1, r8, r5
        add r1, r1, r7
        strb r0, [r1]
        mov r0, #1
        sub r1, r5, r7
        add r1, r6, r1
        strb r0, [r1, #7]
.L10:
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

@ proc print();
_print:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
        sub sp, sp, #8
@   x := 0;
        mov r0, #0
        str r0, [fp, #-4]
        b .L15
.L14:
@     print_num(choice[x]+1);
        ldr r0, =_choice
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
.L15:
@   while x < N do
        ldr r0, [fp, #-4]
        cmp r0, #8
        blt .L14
@   newline()
        bl newline
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@ proc init();
_init:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
        sub sp, sp, #8
@   i := 0; 
        mov r0, #0
        str r0, [fp, #-4]
        b .L19
.L18:
@     rank[i] := true; 
        ldr r5, [fp, #-4]
        mov r0, #1
        ldr r1, =_rank
        add r1, r1, r5
        strb r0, [r1]
@     i := i+1 
        add r0, r5, #1
        str r0, [fp, #-4]
.L19:
@   while i < N do 
        ldr r0, [fp, #-4]
        cmp r0, #8
        blt .L18
@   i := 0; 
        mov r0, #0
        str r0, [fp, #-4]
        b .L22
.L21:
@     diagup[i] := true; diagdown[i] := true ;
        ldr r5, [fp, #-4]
        mov r0, #1
        ldr r1, =_diagup
        add r1, r1, r5
        strb r0, [r1]
        mov r0, #1
        ldr r1, =_diagdown
        add r1, r1, r5
        strb r0, [r1]
@     i := i+1
        add r0, r5, #1
        str r0, [fp, #-4]
.L22:
@   while i < 2*N-1 do 
        ldr r0, [fp, #-4]
        cmp r0, #15
        blt .L21
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

pmain:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   init();
        bl _init
@   queens(0)
        mov r0, #0
        bl _queens
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

        .comm _choice, 32, 4
        .comm _rank, 8, 4
        .comm _diagup, 15, 4
        .comm _diagdown, 15, 4
@ End
]]*)
