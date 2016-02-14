type 
  tree = pointer to node;
  node = record left, right: tree end;

proc build(n: integer): tree;
  var t: tree;
begin
  if n <= 1 then
    return nil
  else
    new(t);
    t^.left := build(n-2);
    t^.right := build(n-1);
    return t
  end
end;

proc print(t: tree);
  var tt: reg tree;
begin
  tt := t;
  if tt = nil then
    print_char('.')
  else
    print_char('(');
    print(tt^.left);
    print(tt^.right);
    print_char(')')
  end
end;

proc display(t: tree);
  var buf: array 20 of char;

  proc indent(i: integer);
    var j: reg integer;
  begin
    j := 0;
    while j <= i do print_char(' '); print_char(buf[j]); j := j+1 end
  end;

  proc disp(i: integer; t: tree);
  begin
    if t = nil then
      print_char('$');
      newline()
    else
      print_char('+'); print_char('-');
      buf[i] := '|';
      disp(i+1, t^.right);
      indent(i-1);
      print_char(' '); print_char(' '); print_char('\');
      newline();
      buf[i] := ' ';
      indent(i); print_char(' ');
      disp(i+1, t^.left)
    end
  end;

begin
  print_char('-'); disp(0, t)
end;

proc count(t: tree): integer;
  var tt: reg tree;
begin
  tt := t;
  if tt = nil then
    return 1
  else
    return count(tt^.left) + count(tt^.right)
  end
end;

var p: tree;

begin 
  p := build(7);
  print(p); newline();
  (* newline(); display(p); newline(); *)
  print_string("Count = "); print_num(count(p)); newline()
end.

(*<<
(((.(..))((..)(.(..))))(((..)(.(..)))((.(..))((..)(.(..))))))
Count = 21
>>*)

(*[[
@ picoPascal compiler output
        .global pmain

@ proc build(n: integer): tree;
        .text
_build:
        mov ip, sp
        stmfd sp!, {r0-r1}
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
        sub sp, sp, #8
@   if n <= 1 then
        ldr r0, [fp, #40]
        cmp r0, #1
        bgt .L4
@     return nil
        mov r0, #0
        b .L2
.L4:
@     new(t);
        mov r1, #8
        add r0, fp, #-4
        bl new
@     t^.left := build(n-2);
        ldr r0, [fp, #40]
        sub r0, r0, #2
        bl _build
        ldr r1, [fp, #-4]
        str r0, [r1]
@     t^.right := build(n-1);
        ldr r0, [fp, #40]
        sub r0, r0, #1
        bl _build
        ldr r1, [fp, #-4]
        str r0, [r1, #4]
@     return t
        ldr r0, [fp, #-4]
.L2:
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@ proc print(t: tree);
_print:
        mov ip, sp
        stmfd sp!, {r0-r1}
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   tt := t;
        ldr r5, [fp, #40]
@   if tt = nil then
        cmp r5, #0
        bne .L8
@     print_char('.')
        mov r0, #46
        bl print_char
        b .L6
.L8:
@     print_char('(');
        mov r0, #40
        bl print_char
@     print(tt^.left);
        ldr r0, [r5]
        bl _print
@     print(tt^.right);
        ldr r0, [r5, #4]
        bl _print
@     print_char(')')
        mov r0, #41
        bl print_char
.L6:
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@ proc display(t: tree);
_display:
        mov ip, sp
        stmfd sp!, {r0-r1}
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
        sub sp, sp, #24
@   print_char('-'); disp(0, t)
        mov r0, #45
        bl print_char
        ldr r1, [fp, #40]
        mov r0, #0
        mov r4, fp
        bl _disp
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@   proc indent(i: integer);
_indent:
        mov ip, sp
        stmfd sp!, {r0-r1}
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@     j := 0;
        mov r5, #0
        b .L13
.L12:
@     while j <= i do print_char(' '); print_char(buf[j]); j := j+1 end
        mov r0, #32
        bl print_char
        ldr r0, [fp]
        add r0, r0, #-20
        add r0, r0, r5
        ldrb r0, [r0]
        bl print_char
        add r5, r5, #1
.L13:
        ldr r0, [fp, #40]
        cmp r5, r0
        ble .L12
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@   proc disp(i: integer; t: tree);
_disp:
        mov ip, sp
        stmfd sp!, {r0-r1}
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@     if t = nil then
        ldr r0, [fp, #44]
        cmp r0, #0
        bne .L17
@       print_char('$');
        mov r0, #36
        bl print_char
@       newline()
        bl newline
        b .L15
.L17:
@       print_char('+'); print_char('-');
        mov r0, #43
        bl print_char
        mov r0, #45
        bl print_char
@       buf[i] := '|';
        mov r0, #124
        ldr r1, [fp]
        add r1, r1, #-20
        ldr r2, [fp, #40]
        add r1, r1, r2
        strb r0, [r1]
@       disp(i+1, t^.right);
        ldr r0, [fp, #44]
        ldr r1, [r0, #4]
        ldr r0, [fp, #40]
        add r0, r0, #1
        ldr r4, [fp]
        bl _disp
@       indent(i-1);
        ldr r0, [fp, #40]
        sub r0, r0, #1
        ldr r4, [fp]
        bl _indent
@       print_char(' '); print_char(' '); print_char('\');
        mov r0, #32
        bl print_char
        mov r0, #32
        bl print_char
        mov r0, #92
        bl print_char
@       newline();
        bl newline
@       buf[i] := ' ';
        mov r0, #32
        ldr r1, [fp]
        add r1, r1, #-20
        ldr r2, [fp, #40]
        add r1, r1, r2
        strb r0, [r1]
@       indent(i); print_char(' ');
        ldr r0, [fp, #40]
        ldr r4, [fp]
        bl _indent
        mov r0, #32
        bl print_char
@       disp(i+1, t^.left)
        ldr r0, [fp, #44]
        ldr r1, [r0]
        ldr r0, [fp, #40]
        add r0, r0, #1
        ldr r4, [fp]
        bl _disp
.L15:
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@ proc count(t: tree): integer;
_count:
        mov ip, sp
        stmfd sp!, {r0-r1}
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   tt := t;
        ldr r5, [fp, #40]
@   if tt = nil then
        cmp r5, #0
        bne .L21
@     return 1
        mov r0, #1
        b .L19
.L21:
@     return count(tt^.left) + count(tt^.right)
        ldr r0, [r5]
        bl _count
        mov r6, r0
        ldr r0, [r5, #4]
        bl _count
        add r0, r6, r0
.L19:
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

pmain:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   p := build(7);
        mov r0, #7
        bl _build
        ldr r5, =_p
        str r0, [r5]
@   print(p); newline();
        bl _print
        bl newline
@   print_string("Count = "); print_num(count(p)); newline()
        mov r1, #8
        ldr r0, =g1
        bl print_string
        ldr r0, [r5]
        bl _count
        bl print_num
        bl newline
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

        .comm _p, 4, 4
        .data
g1:
        .byte 67, 111, 117, 110, 116, 32, 61, 32
        .byte 0
@ End
]]*)
