(* ppc/main.ml *)

open Print
open Mach
open Source

let debug = ref 0

let usage = "Usage: ppc0 [-v] [-t] file"

let spec =
  Arg.align
    ["-b", Arg.Unit (fun () -> Kgen.boundchk := true), 
        " enable bound checks";
      "-d", Arg.Int (function x -> debug := x), "n set debug level";
      "-O", Arg.Unit (fun () -> Kgen.optlevel := 1), " enable optimiser";
      "-O2", Arg.Unit (fun () -> Kgen.optlevel := 2), " more optimisation"]
      
let main () =
  let fns = ref [] in
  Arg.parse spec (function s -> fns := !fns @ [s]) usage;
  if List.length !fns <> 1 then begin
    fprintf stderr "$\n" [fStr usage]; exit 2
  end;
  let in_file = List.hd !fns in
  let in_chan = open_in in_file in
  let lexbuf = Lexing.from_channel in_chan in
  Source.init in_file in_chan;
  Kgen.debug := !debug;
  ignore (Parsing.set_trace (!debug > 2));

  (* Parse the program *)
  let prog = 
    try Parser.program Lexer.token lexbuf with
      Parsing.Parse_error ->
        let tok = Lexing.lexeme lexbuf in
        Source.err_message "syntax error at token '$'" [fStr tok] !Util.lineno;
        exit 1 in

  if !debug > 2 then begin
    printf "$Abstract syntax tree:\n" [fStr Mach.comment];
    Tree.print_tree stdout Mach.comment prog;
    printf "\n" []
  end;

  (* Semantic analysis *)
  begin try Check.annotate prog with
    Check.Sem_error (fmt, args, ln) ->
      Source.err_message fmt args ln;
      exit 1
  end;
  
  (* Translate the program *)
  Kgen.translate prog;
  exit 0

let ppc = main ()
