(* search.p *)

const target = "abracadabra";

var i: integer; found: boolean;

begin
  i := 0; found := false;
  while not found do
    found := target[i] = 'd';
    i := i + 1
  end;
  print_num(i);
  newline()
end.

(*<<
7
>>*)

(*[[
@ picoPascal compiler output
        .global pmain

        .text
pmain:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   i := 0; found := false;
        mov r0, #0
        ldr r1, =_i
        str r0, [r1]
        mov r0, #0
        ldr r1, =_found
        strb r0, [r1]
        b .L4
.L3:
@     found := target[i] = 'd';
        ldr r5, =_i
        ldr r6, [r5]
        ldr r0, =g1
        add r0, r0, r6
        ldrb r0, [r0]
        cmp r0, #100
        mov r0, #0
        moveq r0, #1
        ldr r1, =_found
        strb r0, [r1]
@     i := i + 1
        add r0, r6, #1
        str r0, [r5]
.L4:
@   while not found do
        ldr r0, =_found
        ldrb r0, [r0]
        cmp r0, #0
        beq .L3
@   print_num(i);
        ldr r0, =_i
        ldr r0, [r0]
        bl print_num
@   newline()
        bl newline
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

        .comm _i, 4, 4
        .comm _found, 1, 4
        .data
g1:
        .byte 97, 98, 114, 97, 99, 97, 100, 97, 98, 114
        .byte 97
        .byte 0
@ End
]]*)
