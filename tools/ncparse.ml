type token =
  | WORD of (string)
  | OPEN of (string)
  | CHAR of (char)
  | LPAREN
  | RPAREN
  | CLOSE
  | EOF
  | COMMA
  | SPACE
  | ATSIGN

open Parsing;;
let _ = parse_error;;
# 15 "ncparse.mly"
open Print

let outch ch = print_char ch

let out s = print_string s

let out_frag f = printf "$" [f]

let fJoin f1 f2 = fExt (fun prf -> prf "$$" [f1; f2])
# 26 "ncparse.ml"
let yytransl_const = [|
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* CLOSE *);
    0 (* EOF *);
  263 (* COMMA *);
  264 (* SPACE *);
  265 (* ATSIGN *);
    0|]

let yytransl_block = [|
  257 (* WORD *);
  258 (* OPEN *);
  259 (* CHAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\003\000\004\000\005\000\005\000\005\000\
\008\000\008\000\006\000\006\000\006\000\009\000\009\000\009\000\
\009\000\009\000\009\000\007\000\007\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\004\000\001\000\000\000\003\000\004\000\
\001\000\003\000\000\000\002\000\004\000\001\000\001\000\001\000\
\001\000\001\000\001\000\000\000\001\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\030\000\000\000\003\000\019\000\010\000\006\000\
\007\000\005\000\001\000\008\000\004\000\009\000\011\000\000\000\
\000\000\000\000\000\000\022\000\023\000\019\000\025\000\024\000\
\026\000\027\000\020\000\029\000\000\000\012\000\000\000\019\000\
\000\000\000\000\021\000\000\000\019\000\000\000"

let yydgoto = "\002\000\
\003\000\004\000\026\000\016\000\019\000\017\000\029\000\034\000\
\027\000"

let yysindex = "\013\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\251\254\
\033\255\013\255\034\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\038\255\000\000\015\255\000\000\
\033\255\044\255\000\000\024\255\000\000\033\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\255\
\006\255\002\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\042\255\000\000\000\000\000\000\
\023\255\047\255\000\000\048\255\000\000\032\255"

let yygindex = "\000\000\
\000\000\000\000\051\000\000\000\000\000\234\255\000\000\000\000\
\000\000"

let yytablesize = 266
let yytable = "\031\000\
\011\000\018\000\028\000\028\000\028\000\028\000\033\000\028\000\
\028\000\036\000\028\000\013\000\013\000\001\000\038\000\020\000\
\006\000\021\000\022\000\035\000\028\000\023\000\024\000\025\000\
\020\000\006\000\021\000\022\000\017\000\017\000\023\000\024\000\
\025\000\020\000\006\000\021\000\022\000\018\000\018\000\030\000\
\024\000\025\000\019\000\019\000\019\000\019\000\032\000\019\000\
\019\000\019\000\037\000\014\000\015\000\016\000\015\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\005\000\006\000\007\000\008\000\009\000\010\000\012\000\
\013\000\014\000"

let yycheck = "\022\000\
\000\000\007\001\001\001\002\001\003\001\004\001\029\000\006\001\
\007\001\032\000\009\001\006\001\007\001\001\000\037\000\001\001\
\002\001\003\001\004\001\005\001\008\001\007\001\008\001\009\001\
\001\001\002\001\003\001\004\001\006\001\007\001\007\001\008\001\
\009\001\001\001\002\001\003\001\004\001\006\001\007\001\006\001\
\008\001\009\001\001\001\002\001\003\001\004\001\009\001\006\001\
\007\001\008\001\007\001\006\001\006\001\006\001\004\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  CLOSE\000\
  EOF\000\
  COMMA\000\
  SPACE\000\
  ATSIGN\000\
  "

let yynames_block = "\
  WORD\000\
  OPEN\000\
  CHAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    Obj.repr(
# 29 "ncparse.mly"
                                ( () )
# 180 "ncparse.ml"
               : unit))
; (fun __caml_parser_env ->
    Obj.repr(
# 32 "ncparse.mly"
                                ( () )
# 186 "ncparse.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 33 "ncparse.mly"
                                ( out _2 )
# 194 "ncparse.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    Obj.repr(
# 34 "ncparse.mly"
                                ( out " " )
# 201 "ncparse.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    Obj.repr(
# 35 "ncparse.mly"
                                ( out ">" )
# 208 "ncparse.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    Obj.repr(
# 36 "ncparse.mly"
                                ( out "(" )
# 215 "ncparse.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    Obj.repr(
# 37 "ncparse.mly"
                                ( out ")" )
# 222 "ncparse.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    Obj.repr(
# 38 "ncparse.mly"
                                ( out "," )
# 229 "ncparse.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    Obj.repr(
# 39 "ncparse.mly"
                                ( out "@" )
# 236 "ncparse.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 40 "ncparse.mly"
                                ( outch _2 )
# 244 "ncparse.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'node) in
    Obj.repr(
# 41 "ncparse.mly"
                                ( out_frag _2 )
# 252 "ncparse.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'bal1) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 44 "ncparse.mly"
                                ( fMeta "(Node ($$, $))" [fStr _1; _2; _3] )
# 261 "ncparse.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bal) in
    Obj.repr(
# 47 "ncparse.mly"
                                ( _1 )
# 268 "ncparse.ml"
               : 'bal1))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "ncparse.mly"
                                ( fStr "[]" )
# 274 "ncparse.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'blank) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arglist) in
    Obj.repr(
# 51 "ncparse.mly"
                                ( fMeta "[$]" [_3] )
# 282 "ncparse.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'blank) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'bal) in
    Obj.repr(
# 52 "ncparse.mly"
                                ( _4 )
# 290 "ncparse.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bal) in
    Obj.repr(
# 55 "ncparse.mly"
                                ( _1 )
# 297 "ncparse.ml"
               : 'arglist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arglist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bal) in
    Obj.repr(
# 56 "ncparse.mly"
                                  ( fMeta "$; $" [_1; _3] )
# 305 "ncparse.ml"
               : 'arglist))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "ncparse.mly"
                                ( fStr "" )
# 311 "ncparse.ml"
               : 'bal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'bal) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'item) in
    Obj.repr(
# 60 "ncparse.mly"
                                ( fJoin _1 _2 )
# 319 "ncparse.ml"
               : 'bal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'bal) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'bal) in
    Obj.repr(
# 61 "ncparse.mly"
                                ( fMeta "$($)" [_1; _3] )
# 327 "ncparse.ml"
               : 'bal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "ncparse.mly"
                                ( fStr _1 )
# 334 "ncparse.ml"
               : 'item))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 65 "ncparse.mly"
                                ( fChr _1 )
# 341 "ncparse.ml"
               : 'item))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "ncparse.mly"
                                ( fStr " " )
# 347 "ncparse.ml"
               : 'item))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "ncparse.mly"
                                ( fStr "," )
# 353 "ncparse.ml"
               : 'item))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "ncparse.mly"
                                ( fStr "@" )
# 359 "ncparse.ml"
               : 'item))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'node) in
    Obj.repr(
# 69 "ncparse.mly"
                                ( _1 )
# 366 "ncparse.ml"
               : 'item))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "ncparse.mly"
                                ( () )
# 372 "ncparse.ml"
               : 'blank))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "ncparse.mly"
                                ( () )
# 378 "ncparse.ml"
               : 'blank))
(* Entry text *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let text (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
