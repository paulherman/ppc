(* ppc/lexer.mli *)

open Dict

(*
The lexer is generated from a camllex script.  It takes an input
buffer, reads a token, and returns the |token| value that corresponds
to it.  The lexer maintains the current line number in |lineno| for
producing error messages.  Lexical errors result in the exception
|Lex_error|, whose arguments are a format and argument list suitable
for passing to |printf| to print the message.
*)

(* |token| -- scan a token and return its code *)
val token : Lexing.lexbuf -> Parser.token

(* |intern| -- find an identifier *)
val intern : string -> ident

(* |string_table| -- fetch table of string constants *)
val string_table : unit -> (Keiko.symbol * string) list
