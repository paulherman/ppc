type ptr = pointer to rec;
  rec = record 
      data: integer; 
      next: ptr; 
    end;

proc sum(p: ptr): integer;
  var q: reg ptr; s: reg integer;
begin
  q := p; s := 0;
  while q <> nil do
    s := s + q^.data;
    q := q^.next
  end;
  return s
end;

proc main();
  const input = "3141592650";
  var i: integer; p, q: ptr;
begin
  i := 0;
  while input[i] <> '0' do i := i+1 end;

  p := nil;
  while i > 0 do
    i := i-1;
    q := p;
    new(p);
    p^.data := ord(input[i]) - ord('0');
    p^.next := q
  end;

  print_num(sum(p)); newline()
end;

begin
  main()
end.

(*<<
36
>>*)

(*[[
@ picoPascal compiler output
        .global pmain

@ proc sum(p: ptr): integer;
        .text
_sum:
        mov ip, sp
        stmfd sp!, {r0-r1}
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   q := p; s := 0;
        ldr r5, [fp, #40]
        mov r6, #0
        b .L4
.L3:
@     s := s + q^.data;
        ldr r0, [r5]
        add r6, r6, r0
@     q := q^.next
        ldr r5, [r5, #4]
.L4:
@   while q <> nil do
        cmp r5, #0
        bne .L3
@   return s
        mov r0, r6
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@ proc main();
_main:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
        sub sp, sp, #16
@   i := 0;
        mov r0, #0
        str r0, [fp, #-4]
        b .L8
.L7:
@   while input[i] <> '0' do i := i+1 end;
        ldr r0, [fp, #-4]
        add r0, r0, #1
        str r0, [fp, #-4]
.L8:
        ldr r0, =g1
        ldr r1, [fp, #-4]
        add r0, r0, r1
        ldrb r0, [r0]
        cmp r0, #48
        bne .L7
@   p := nil;
        mov r0, #0
        str r0, [fp, #-8]
        b .L11
.L10:
@     i := i-1;
        ldr r0, [fp, #-4]
        sub r0, r0, #1
        str r0, [fp, #-4]
@     q := p;
        ldr r0, [fp, #-8]
        str r0, [fp, #-12]
@     new(p);
        mov r1, #8
        add r0, fp, #-8
        bl new
@     p^.data := ord(input[i]) - ord('0');
        ldr r0, =g1
        ldr r1, [fp, #-4]
        add r0, r0, r1
        ldrb r0, [r0]
        sub r0, r0, #48
        ldr r1, [fp, #-8]
        str r0, [r1]
@     p^.next := q
        ldr r0, [fp, #-12]
        ldr r1, [fp, #-8]
        str r0, [r1, #4]
.L11:
@   while i > 0 do
        ldr r0, [fp, #-4]
        cmp r0, #0
        bgt .L10
@   print_num(sum(p)); newline()
        ldr r0, [fp, #-8]
        bl _sum
        bl print_num
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

        .data
g1:
        .byte 51, 49, 52, 49, 53, 57, 50, 54, 53, 48
        .byte 0
@ End
]]*)
