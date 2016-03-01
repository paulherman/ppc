proc args(a, b, c, d, e, f: integer): integer;
var x, y : integer;
begin
    x := 39;
    return a + b + c + d + e + f
end;

var f: integer;

begin
  f := args(1, 2, 3, 4, 5, 6);
  print_num(f);
  newline()
end.
