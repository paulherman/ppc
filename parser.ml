type token =
  | IDENT of (Dict.ident)
  | MULOP of (Keiko.op)
  | ADDOP of (Keiko.op)
  | RELOP of (Keiko.op)
  | NUMBER of (int)
  | CHAR of (char)
  | STRING of (Keiko.symbol * int)
  | SEMI
  | DOT
  | COLON
  | LPAR
  | RPAR
  | COMMA
  | SUB
  | BUS
  | EQUAL
  | MINUS
  | ASSIGN
  | VBAR
  | ARROW
  | BADTOK
  | IMPOSSIBLE
  | ARRAY
  | BEGIN
  | CONST
  | DO
  | ELSE
  | END
  | IF
  | OF
  | PROC
  | RECORD
  | RETURN
  | THEN
  | TO
  | TYPE
  | VAR
  | WHILE
  | NOT
  | POINTER
  | NIL
  | REG
  | REPEAT
  | UNTIL
  | FOR
  | ELSIF
  | CASE

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Keiko
open Dict
open Tree
# 57 "parser.ml"
let yytransl_const = [|
  264 (* SEMI *);
  265 (* DOT *);
  266 (* COLON *);
  267 (* LPAR *);
  268 (* RPAR *);
  269 (* COMMA *);
  270 (* SUB *);
  271 (* BUS *);
  272 (* EQUAL *);
  273 (* MINUS *);
  274 (* ASSIGN *);
  275 (* VBAR *);
  276 (* ARROW *);
  277 (* BADTOK *);
  278 (* IMPOSSIBLE *);
  279 (* ARRAY *);
  280 (* BEGIN *);
  281 (* CONST *);
  282 (* DO *);
  283 (* ELSE *);
  284 (* END *);
  285 (* IF *);
  286 (* OF *);
  287 (* PROC *);
  288 (* RECORD *);
  289 (* RETURN *);
  290 (* THEN *);
  291 (* TO *);
  292 (* TYPE *);
  293 (* VAR *);
  294 (* WHILE *);
  295 (* NOT *);
  296 (* POINTER *);
  297 (* NIL *);
  298 (* REG *);
  299 (* REPEAT *);
  300 (* UNTIL *);
  301 (* FOR *);
  302 (* ELSIF *);
  303 (* CASE *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
  258 (* MULOP *);
  259 (* ADDOP *);
  260 (* RELOP *);
  261 (* NUMBER *);
  262 (* CHAR *);
  263 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\005\000\005\000\005\000\005\000\
\006\000\006\000\010\000\009\000\009\000\012\000\007\000\007\000\
\014\000\016\000\016\000\008\000\017\000\019\000\019\000\021\000\
\021\000\022\000\022\000\022\000\020\000\020\000\004\000\023\000\
\023\000\024\000\024\000\025\000\026\000\026\000\026\000\026\000\
\026\000\026\000\026\000\026\000\026\000\030\000\030\000\030\000\
\031\000\031\000\033\000\032\000\032\000\015\000\015\000\029\000\
\029\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\028\000\
\028\000\034\000\034\000\027\000\027\000\027\000\027\000\013\000\
\013\000\013\000\013\000\035\000\035\000\036\000\037\000\037\000\
\018\000\000\000"

let yylen = "\002\000\
\002\000\004\000\000\000\002\000\002\000\002\000\001\000\002\000\
\001\000\002\000\004\000\001\000\002\000\004\000\001\000\002\000\
\005\000\000\000\001\000\004\000\004\000\002\000\003\000\001\000\
\003\000\003\000\004\000\001\000\000\000\002\000\001\000\001\000\
\003\000\002\000\001\000\000\000\000\000\003\000\002\000\002\000\
\006\000\005\000\004\000\009\000\006\000\000\000\002\000\006\000\
\001\000\003\000\003\000\000\000\002\000\001\000\003\000\000\000\
\001\000\001\000\001\000\001\000\001\000\001\000\002\000\002\000\
\002\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\003\000\001\000\003\000\001\000\004\000\003\000\002\000\001\000\
\004\000\003\000\003\000\002\000\003\000\003\000\001\000\000\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\090\000\000\000\
\000\000\000\000\007\000\000\000\000\000\005\000\000\000\089\000\
\000\000\000\000\008\000\000\000\000\000\006\000\000\000\000\000\
\001\000\000\000\004\000\000\000\000\000\010\000\000\000\000\000\
\000\000\013\000\000\000\016\000\000\000\035\000\000\000\031\000\
\000\000\000\000\000\000\059\000\061\000\060\000\000\000\000\000\
\000\000\062\000\000\000\000\000\000\000\022\000\000\000\000\000\
\028\000\000\000\000\000\000\000\021\000\000\000\000\000\000\000\
\000\000\080\000\055\000\019\000\000\000\002\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\034\000\000\000\
\020\000\000\000\065\000\064\000\000\000\000\000\000\000\011\000\
\000\000\000\000\000\000\063\000\000\000\000\000\079\000\000\000\
\000\000\023\000\000\000\030\000\000\000\000\000\000\000\000\000\
\000\000\014\000\000\000\033\000\000\000\000\000\040\000\000\000\
\000\000\000\000\000\000\039\000\000\000\071\000\066\000\000\000\
\000\000\000\000\000\000\072\000\000\000\000\000\078\000\000\000\
\000\000\026\000\025\000\000\000\000\000\082\000\000\000\084\000\
\083\000\017\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\073\000\077\000\027\000\081\000\086\000\085\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\075\000\000\000\
\036\000\000\000\042\000\000\000\000\000\000\000\000\000\000\000\
\047\000\000\000\041\000\000\000\051\000\053\000\045\000\050\000\
\000\000\000\000\000\000\000\000\000\000\044\000\048\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\039\000\010\000\014\000\022\000\011\000\
\019\000\015\000\125\000\020\000\065\000\023\000\024\000\069\000\
\012\000\052\000\032\000\061\000\058\000\059\000\040\000\041\000\
\042\000\079\000\053\000\092\000\111\000\162\000\157\000\167\000\
\158\000\126\000\103\000\104\000\136\000"

let yysindex = "\012\000\
\185\255\000\000\013\255\021\255\022\255\023\255\000\000\016\255\
\003\255\185\255\000\000\029\255\024\255\000\000\013\255\000\000\
\018\255\026\255\000\000\022\255\017\255\000\000\023\255\039\255\
\000\000\028\255\000\000\185\255\210\000\000\000\086\255\042\255\
\046\255\000\000\023\255\000\000\012\255\000\000\011\255\000\000\
\047\255\062\255\056\255\000\000\000\000\000\000\210\000\210\000\
\210\000\000\000\002\001\055\255\110\255\000\000\023\255\057\255\
\000\000\059\255\072\255\046\255\000\000\210\000\023\255\048\255\
\073\255\000\000\000\000\000\000\046\255\000\000\028\255\210\000\
\210\000\210\000\028\255\021\255\210\000\055\255\000\000\211\255\
\000\000\011\001\000\000\000\000\210\000\210\000\210\000\000\000\
\210\000\210\000\197\000\000\000\021\255\210\000\000\000\075\255\
\046\255\000\000\089\255\000\000\203\000\079\255\054\255\091\255\
\046\255\000\000\098\255\000\000\117\000\041\001\000\000\237\000\
\050\255\092\255\226\000\000\000\210\000\000\000\000\000\109\255\
\014\255\014\255\109\255\000\000\018\001\115\255\000\000\022\001\
\046\255\000\000\000\000\046\255\046\255\000\000\023\255\000\000\
\000\000\000\000\028\255\028\255\210\000\210\000\210\000\041\001\
\210\000\000\000\000\000\000\000\000\000\000\000\000\000\231\254\
\093\255\041\001\177\255\038\001\095\255\114\255\000\000\028\255\
\000\000\101\255\000\000\210\000\028\255\028\255\106\255\210\000\
\000\000\210\000\000\000\242\000\000\000\000\000\000\000\000\000\
\152\000\028\255\028\255\121\255\231\254\000\000\000\000"

let yyrindex = "\000\000\
\118\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\118\255\000\000\000\000\000\000\000\000\134\255\000\000\
\000\000\000\000\000\000\216\255\128\255\000\000\025\001\000\000\
\000\000\190\255\000\000\118\255\000\000\000\000\000\000\253\254\
\000\000\000\000\000\000\000\000\061\255\000\000\000\000\000\000\
\144\000\007\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\247\255\026\000\000\000\000\000\000\000\
\000\000\000\000\131\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\117\255\000\000\
\129\255\000\000\201\255\000\000\000\000\218\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\123\255\
\000\000\000\000\000\000\000\000\000\000\180\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\061\000\
\131\000\166\000\096\000\000\000\140\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\125\255\000\000\
\000\000\000\000\139\255\190\255\000\000\000\000\000\000\004\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\126\255\
\000\000\039\000\000\000\000\000\146\255\235\254\000\000\190\255\
\000\000\000\000\000\000\000\000\168\255\190\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\190\255\139\255\000\000\126\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\150\000\173\000\193\255\000\000\174\000\167\000\000\000\
\172\000\000\000\227\255\000\000\197\255\000\000\229\255\000\000\
\236\255\255\255\000\000\000\000\101\000\000\000\132\000\000\000\
\043\000\000\000\163\000\136\000\000\000\036\000\074\000\000\000\
\000\000\098\000\119\000\000\000\000\000"

let yytablesize = 574
let yytable = "\051\000\
\100\000\160\000\017\000\056\000\029\000\049\000\049\000\067\000\
\029\000\107\000\057\000\113\000\001\000\013\000\037\000\085\000\
\086\000\082\000\083\000\084\000\161\000\016\000\018\000\021\000\
\025\000\037\000\026\000\096\000\031\000\035\000\090\000\066\000\
\101\000\037\000\037\000\102\000\028\000\130\000\070\000\029\000\
\078\000\033\000\109\000\110\000\112\000\137\000\016\000\115\000\
\037\000\038\000\037\000\060\000\037\000\068\000\071\000\119\000\
\120\000\121\000\066\000\122\000\123\000\018\000\016\000\081\000\
\128\000\091\000\097\000\066\000\062\000\148\000\098\000\056\000\
\149\000\150\000\114\000\152\000\153\000\063\000\057\000\099\000\
\106\000\134\000\105\000\018\000\129\000\064\000\021\000\144\000\
\133\000\021\000\072\000\127\000\018\000\141\000\073\000\066\000\
\169\000\054\000\135\000\074\000\018\000\173\000\174\000\066\000\
\075\000\138\000\076\000\102\000\077\000\142\000\085\000\154\000\
\155\000\156\000\180\000\181\000\004\000\036\000\093\000\004\000\
\163\000\166\000\055\000\094\000\036\000\055\000\146\000\066\000\
\171\000\095\000\066\000\066\000\168\000\175\000\172\000\036\000\
\056\000\054\000\156\000\036\000\177\000\003\000\024\000\036\000\
\036\000\036\000\036\000\056\000\182\000\036\000\088\000\074\000\
\087\000\046\000\036\000\056\000\056\000\009\000\009\000\036\000\
\036\000\036\000\036\000\036\000\009\000\036\000\036\000\036\000\
\036\000\009\000\009\000\036\000\056\000\052\000\056\000\036\000\
\036\000\043\000\085\000\086\000\087\000\036\000\027\000\036\000\
\036\000\036\000\036\000\057\000\030\000\036\000\036\000\034\000\
\089\000\090\000\036\000\036\000\036\000\036\000\057\000\131\000\
\036\000\036\000\108\000\170\000\080\000\036\000\057\000\057\000\
\036\000\003\000\036\000\164\000\036\000\116\000\036\000\004\000\
\183\000\036\000\036\000\093\000\005\000\006\000\036\000\057\000\
\094\000\057\000\076\000\036\000\117\000\036\000\095\000\076\000\
\036\000\036\000\036\000\076\000\036\000\076\000\036\000\012\000\
\012\000\176\000\159\000\036\000\036\000\036\000\012\000\036\000\
\076\000\076\000\076\000\012\000\012\000\151\000\076\000\076\000\
\076\000\000\000\076\000\076\000\076\000\076\000\076\000\076\000\
\000\000\076\000\076\000\038\000\000\000\000\000\000\000\000\000\
\076\000\076\000\076\000\000\000\076\000\000\000\038\000\000\000\
\076\000\076\000\000\000\058\000\058\000\058\000\038\000\038\000\
\000\000\058\000\076\000\058\000\076\000\058\000\058\000\000\000\
\058\000\058\000\058\000\000\000\058\000\000\000\043\000\038\000\
\000\000\038\000\000\000\058\000\058\000\058\000\000\000\058\000\
\000\000\043\000\000\000\058\000\058\000\000\000\000\000\067\000\
\067\000\043\000\043\000\000\000\067\000\058\000\067\000\058\000\
\067\000\067\000\000\000\067\000\067\000\067\000\000\000\067\000\
\000\000\000\000\043\000\000\000\043\000\000\000\067\000\067\000\
\067\000\000\000\067\000\000\000\000\000\000\000\067\000\067\000\
\000\000\000\000\068\000\068\000\000\000\000\000\000\000\068\000\
\067\000\068\000\067\000\068\000\068\000\000\000\068\000\068\000\
\068\000\000\000\068\000\000\000\000\000\000\000\085\000\086\000\
\087\000\068\000\068\000\068\000\000\000\068\000\000\000\000\000\
\000\000\068\000\068\000\000\000\089\000\090\000\069\000\000\000\
\000\000\000\000\069\000\068\000\069\000\068\000\069\000\069\000\
\000\000\069\000\069\000\000\000\000\000\069\000\139\000\000\000\
\000\000\085\000\086\000\087\000\069\000\069\000\069\000\000\000\
\069\000\000\000\032\000\000\000\069\000\069\000\000\000\089\000\
\090\000\070\000\032\000\032\000\000\000\070\000\069\000\070\000\
\069\000\070\000\070\000\000\000\070\000\070\000\000\000\000\000\
\070\000\179\000\000\000\032\000\000\000\032\000\000\000\070\000\
\070\000\070\000\000\000\070\000\000\000\016\000\000\000\070\000\
\070\000\044\000\045\000\046\000\085\000\086\000\087\000\047\000\
\124\000\070\000\016\000\070\000\000\000\048\000\044\000\045\000\
\046\000\000\000\089\000\090\000\047\000\000\000\000\000\000\000\
\000\000\000\000\048\000\085\000\086\000\087\000\000\000\000\000\
\132\000\000\000\000\000\049\000\000\000\050\000\085\000\086\000\
\087\000\089\000\090\000\085\000\086\000\087\000\000\000\000\000\
\049\000\000\000\050\000\000\000\089\000\090\000\000\000\143\000\
\000\000\089\000\090\000\085\000\086\000\087\000\140\000\000\000\
\000\000\088\000\000\000\178\000\085\000\086\000\087\000\000\000\
\000\000\089\000\090\000\085\000\086\000\087\000\118\000\085\000\
\086\000\087\000\089\000\090\000\000\000\000\000\145\000\000\000\
\000\000\089\000\090\000\000\000\147\000\089\000\090\000\085\000\
\086\000\087\000\085\000\086\000\087\000\000\000\000\000\165\000\
\015\000\015\000\000\000\000\000\000\000\089\000\090\000\015\000\
\089\000\090\000\000\000\000\000\015\000\015\000"

let yycheck = "\029\000\
\060\000\027\001\004\000\031\000\008\001\027\001\028\001\035\000\
\012\001\069\000\031\000\075\000\001\000\001\001\008\001\002\001\
\003\001\047\000\048\000\049\000\046\001\001\001\001\001\001\001\
\009\001\019\001\024\001\055\000\011\001\013\001\017\001\033\000\
\062\000\027\001\028\001\063\000\008\001\097\000\028\001\016\001\
\042\000\016\001\072\000\073\000\074\000\105\000\001\001\077\000\
\010\001\022\001\044\001\010\001\046\001\042\001\008\001\085\000\
\086\000\087\000\060\000\089\000\090\000\001\001\001\001\008\001\
\094\000\011\001\010\001\069\000\023\001\129\000\012\001\099\000\
\132\000\133\000\076\000\139\000\140\000\032\001\099\000\008\001\
\008\001\028\001\035\001\023\001\010\001\040\001\001\001\117\000\
\010\001\001\001\029\001\093\000\032\001\044\001\033\001\097\000\
\160\000\012\001\008\001\038\001\040\001\165\000\166\000\105\000\
\043\001\008\001\045\001\135\000\047\001\018\001\002\001\141\000\
\142\000\143\000\178\000\179\000\031\001\001\001\009\001\031\001\
\028\001\027\001\037\001\014\001\008\001\037\001\012\001\129\000\
\028\001\020\001\132\000\133\000\019\001\028\001\164\000\019\001\
\008\001\010\001\168\000\001\001\170\000\024\001\012\001\027\001\
\028\001\029\001\008\001\019\001\028\001\033\001\028\001\012\001\
\028\001\028\001\038\001\027\001\028\001\024\001\025\001\043\001\
\044\001\045\001\046\001\047\001\031\001\027\001\028\001\029\001\
\001\001\036\001\037\001\033\001\044\001\028\001\046\001\008\001\
\038\001\028\000\002\001\003\001\004\001\043\001\010\000\045\001\
\046\001\047\001\019\001\008\001\015\000\023\000\001\001\020\000\
\016\001\017\001\027\001\028\001\029\001\008\001\019\001\099\000\
\033\001\001\001\071\000\161\000\042\000\038\001\027\001\028\001\
\008\001\025\001\043\001\035\001\045\001\078\000\047\001\031\001\
\181\000\028\001\029\001\009\001\036\001\037\001\033\001\044\001\
\014\001\046\001\009\001\038\001\018\001\029\001\020\001\014\001\
\043\001\033\001\045\001\018\001\047\001\020\001\038\001\024\001\
\025\001\168\000\145\000\043\001\044\001\045\001\031\001\047\001\
\002\001\003\001\004\001\036\001\037\001\135\000\008\001\009\001\
\010\001\255\255\012\001\013\001\014\001\015\001\016\001\017\001\
\255\255\019\001\020\001\008\001\255\255\255\255\255\255\255\255\
\026\001\027\001\028\001\255\255\030\001\255\255\019\001\255\255\
\034\001\035\001\255\255\002\001\003\001\004\001\027\001\028\001\
\255\255\008\001\044\001\010\001\046\001\012\001\013\001\255\255\
\015\001\016\001\017\001\255\255\019\001\255\255\008\001\044\001\
\255\255\046\001\255\255\026\001\027\001\028\001\255\255\030\001\
\255\255\019\001\255\255\034\001\035\001\255\255\255\255\003\001\
\004\001\027\001\028\001\255\255\008\001\044\001\010\001\046\001\
\012\001\013\001\255\255\015\001\016\001\017\001\255\255\019\001\
\255\255\255\255\044\001\255\255\046\001\255\255\026\001\027\001\
\028\001\255\255\030\001\255\255\255\255\255\255\034\001\035\001\
\255\255\255\255\003\001\004\001\255\255\255\255\255\255\008\001\
\044\001\010\001\046\001\012\001\013\001\255\255\015\001\016\001\
\017\001\255\255\019\001\255\255\255\255\255\255\002\001\003\001\
\004\001\026\001\027\001\028\001\255\255\030\001\255\255\255\255\
\255\255\034\001\035\001\255\255\016\001\017\001\004\001\255\255\
\255\255\255\255\008\001\044\001\010\001\046\001\012\001\013\001\
\255\255\015\001\016\001\255\255\255\255\019\001\034\001\255\255\
\255\255\002\001\003\001\004\001\026\001\027\001\028\001\255\255\
\030\001\255\255\019\001\255\255\034\001\035\001\255\255\016\001\
\017\001\004\001\027\001\028\001\255\255\008\001\044\001\010\001\
\046\001\012\001\013\001\255\255\015\001\016\001\255\255\255\255\
\019\001\034\001\255\255\044\001\255\255\046\001\255\255\026\001\
\027\001\028\001\255\255\030\001\255\255\001\001\255\255\034\001\
\035\001\005\001\006\001\007\001\002\001\003\001\004\001\011\001\
\012\001\044\001\001\001\046\001\255\255\017\001\005\001\006\001\
\007\001\255\255\016\001\017\001\011\001\255\255\255\255\255\255\
\255\255\255\255\017\001\002\001\003\001\004\001\255\255\255\255\
\030\001\255\255\255\255\039\001\255\255\041\001\002\001\003\001\
\004\001\016\001\017\001\002\001\003\001\004\001\255\255\255\255\
\039\001\255\255\041\001\255\255\016\001\017\001\255\255\030\001\
\255\255\016\001\017\001\002\001\003\001\004\001\026\001\255\255\
\255\255\008\001\255\255\026\001\002\001\003\001\004\001\255\255\
\255\255\016\001\017\001\002\001\003\001\004\001\012\001\002\001\
\003\001\004\001\016\001\017\001\255\255\255\255\013\001\255\255\
\255\255\016\001\017\001\255\255\015\001\016\001\017\001\002\001\
\003\001\004\001\002\001\003\001\004\001\255\255\255\255\010\001\
\024\001\025\001\255\255\255\255\255\255\016\001\017\001\031\001\
\016\001\017\001\255\255\255\255\036\001\037\001"

let yynames_const = "\
  SEMI\000\
  DOT\000\
  COLON\000\
  LPAR\000\
  RPAR\000\
  COMMA\000\
  SUB\000\
  BUS\000\
  EQUAL\000\
  MINUS\000\
  ASSIGN\000\
  VBAR\000\
  ARROW\000\
  BADTOK\000\
  IMPOSSIBLE\000\
  ARRAY\000\
  BEGIN\000\
  CONST\000\
  DO\000\
  ELSE\000\
  END\000\
  IF\000\
  OF\000\
  PROC\000\
  RECORD\000\
  RETURN\000\
  THEN\000\
  TO\000\
  TYPE\000\
  VAR\000\
  WHILE\000\
  NOT\000\
  POINTER\000\
  NIL\000\
  REG\000\
  REPEAT\000\
  UNTIL\000\
  FOR\000\
  ELSIF\000\
  CASE\000\
  "

let yynames_block = "\
  IDENT\000\
  MULOP\000\
  ADDOP\000\
  RELOP\000\
  NUMBER\000\
  CHAR\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    Obj.repr(
# 38 "parser.mly"
                                        ( Prog (_1, ref []) )
# 437 "parser.ml"
               : Tree.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'decl_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 41 "parser.mly"
                                        ( makeBlock (_1, _3) )
# 445 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
                                        ( [] )
# 451 "parser.ml"
               : 'decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl_list) in
    Obj.repr(
# 45 "parser.mly"
                                        ( _1 @ _2 )
# 459 "parser.ml"
               : 'decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'const_decls) in
    Obj.repr(
# 48 "parser.mly"
                                        ( _2 )
# 466 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_decls) in
    Obj.repr(
# 49 "parser.mly"
                                        ( _2 )
# 473 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proc_decl) in
    Obj.repr(
# 50 "parser.mly"
                                        ( [_1] )
# 480 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_decls) in
    Obj.repr(
# 51 "parser.mly"
                                        ( [TypeDecl _2] )
# 487 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const_decl) in
    Obj.repr(
# 54 "parser.mly"
                                        ( [_1] )
# 494 "parser.ml"
               : 'const_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'const_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'const_decls) in
    Obj.repr(
# 55 "parser.mly"
                                        ( _1 :: _2 )
# 502 "parser.ml"
               : 'const_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Dict.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
                                        ( ConstDecl (_1, _3) )
# 510 "parser.ml"
               : 'const_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_decl) in
    Obj.repr(
# 61 "parser.mly"
                                        ( [_1] )
# 517 "parser.ml"
               : 'type_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_decls) in
    Obj.repr(
# 62 "parser.mly"
                                        ( _1 :: _2 )
# 525 "parser.ml"
               : 'type_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Dict.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typexpr) in
    Obj.repr(
# 65 "parser.mly"
                                        ( (_1, _3) )
# 533 "parser.ml"
               : 'type_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var_decl) in
    Obj.repr(
# 68 "parser.mly"
                                        ( [_1] )
# 540 "parser.ml"
               : 'var_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_decls) in
    Obj.repr(
# 69 "parser.mly"
                                        ( _1 :: _2 )
# 548 "parser.ml"
               : 'var_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'ident_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typexpr) in
    Obj.repr(
# 72 "parser.mly"
                                        ( VarDecl (_3, _1, _4) )
# 557 "parser.ml"
               : 'var_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
                                        ( VarDef )
# 563 "parser.ml"
               : 'reg))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                                        ( RegDef )
# 569 "parser.ml"
               : 'reg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'proc_heading) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    Obj.repr(
# 79 "parser.mly"
                                        ( ProcDecl (_1, _3) )
# 577 "parser.ml"
               : 'proc_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'params) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'return_type) in
    Obj.repr(
# 82 "parser.mly"
                                        ( Heading (_2, _3, _4) )
# 586 "parser.ml"
               : 'proc_heading))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
                                        ( [] )
# 592 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formal_decls) in
    Obj.repr(
# 86 "parser.mly"
                                        ( _2 )
# 599 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_decl) in
    Obj.repr(
# 89 "parser.mly"
                                        ( [_1] )
# 606 "parser.ml"
               : 'formal_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_decl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal_decls) in
    Obj.repr(
# 90 "parser.mly"
                                        ( _1 :: _3 )
# 614 "parser.ml"
               : 'formal_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typexpr) in
    Obj.repr(
# 93 "parser.mly"
                                        ( VarDecl (CParamDef, _1, _3) )
# 622 "parser.ml"
               : 'formal_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'typexpr) in
    Obj.repr(
# 94 "parser.mly"
                                        ( VarDecl (VParamDef, _2, _4) )
# 630 "parser.ml"
               : 'formal_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proc_heading) in
    Obj.repr(
# 95 "parser.mly"
                                        ( PParamDecl _1 )
# 637 "parser.ml"
               : 'formal_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
                                        ( None )
# 643 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typexpr) in
    Obj.repr(
# 99 "parser.mly"
                                        ( Some _2 )
# 650 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 102 "parser.mly"
                                        ( match _1 with [x] -> x
                                            | xs -> makeStmt (Seq _1, 0) )
# 658 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 106 "parser.mly"
                                        ( [_1] )
# 665 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 107 "parser.mly"
                                        ( _1 :: _3 )
# 673 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'line) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt1) in
    Obj.repr(
# 110 "parser.mly"
                                        ( makeStmt (_2, _1) )
# 681 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
                                        ( failwith "impossible" )
# 687 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
                                        ( !Lexer.lineno )
# 693 "parser.ml"
               : 'line))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
                                        ( Skip )
# 699 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'variable) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                                        ( Assign (_1, _3) )
# 707 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'name) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'actuals) in
    Obj.repr(
# 120 "parser.mly"
                                        ( ProcCall (_1, _2) )
# 715 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr_opt) in
    Obj.repr(
# 121 "parser.mly"
                                        ( Return _2 )
# 722 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'stmts) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'elses) in
    Obj.repr(
# 122 "parser.mly"
                                        ( IfStmt (_2, _4, _5) )
# 731 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 123 "parser.mly"
                                        ( WhileStmt (_2, _4) )
# 739 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'stmts) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                                        ( RepeatStmt (_2, _4) )
# 747 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 126 "parser.mly"
                                        ( let v = makeExpr (Variable _2) in
                                          ForStmt (v, _4, _6, _8) )
# 758 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'arms) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'else_part) in
    Obj.repr(
# 128 "parser.mly"
                                        ( CaseStmt (_2, _4, _5) )
# 767 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
                                        ( makeStmt (Skip, 0) )
# 773 "parser.ml"
               : 'elses))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 132 "parser.mly"
                                        ( _2 )
# 780 "parser.ml"
               : 'elses))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'line) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'elses) in
    Obj.repr(
# 133 "parser.mly"
                                        ( makeStmt (IfStmt (_3, _5, _6), _2) )
# 790 "parser.ml"
               : 'elses))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arm) in
    Obj.repr(
# 136 "parser.mly"
                                        ( [_1] )
# 797 "parser.ml"
               : 'arms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arms) in
    Obj.repr(
# 137 "parser.mly"
                                        ( _1 :: _3 )
# 805 "parser.ml"
               : 'arms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 140 "parser.mly"
                                        ( (_1, _3) )
# 813 "parser.ml"
               : 'arm))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "parser.mly"
                                        ( makeStmt (Skip, 0) )
# 819 "parser.ml"
               : 'else_part))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 144 "parser.mly"
                                        ( _2 )
# 826 "parser.ml"
               : 'else_part))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Dict.ident) in
    Obj.repr(
# 147 "parser.mly"
                                        ( [_1] )
# 833 "parser.ml"
               : 'ident_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Dict.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ident_list) in
    Obj.repr(
# 148 "parser.mly"
                                        ( _1 :: _3 )
# 841 "parser.ml"
               : 'ident_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 151 "parser.mly"
                                        ( None )
# 847 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 152 "parser.mly"
                                        ( Some _1 )
# 854 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'variable) in
    Obj.repr(
# 155 "parser.mly"
                                        ( _1 )
# 861 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 156 "parser.mly"
                                        ( makeExpr (Number _1) )
# 868 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Keiko.symbol * int) in
    Obj.repr(
# 157 "parser.mly"
                                        ( let (lab, len) = _1 in
                                          makeExpr (String (lab, len)) )
# 876 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 159 "parser.mly"
                                        ( makeExpr (Char _1) )
# 883 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 160 "parser.mly"
                                        ( makeExpr Nil )
# 889 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'name) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'actuals) in
    Obj.repr(
# 161 "parser.mly"
                                        ( makeExpr (FuncCall (_1, _2)) )
# 897 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 162 "parser.mly"
                                        ( makeExpr (Monop (Not, _2)) )
# 904 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 163 "parser.mly"
                                        ( makeExpr (Monop (Uminus, _2)) )
# 911 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Keiko.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 164 "parser.mly"
                                        ( makeExpr (Binop (_2, _1, _3)) )
# 920 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Keiko.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 165 "parser.mly"
                                        ( makeExpr (Binop (_2, _1, _3)) )
# 929 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 166 "parser.mly"
                                        ( makeExpr (Binop (Minus, _1, _3)) )
# 937 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Keiko.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 167 "parser.mly"
                                        ( makeExpr (Binop (_2, _1, _3)) )
# 946 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 168 "parser.mly"
                                        ( makeExpr (Binop (Eq, _1, _3)) )
# 954 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 169 "parser.mly"
                                        ( _2 )
# 961 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 172 "parser.mly"
                                        ( [] )
# 967 "parser.ml"
               : 'actuals))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 173 "parser.mly"
                                        ( _2 )
# 974 "parser.ml"
               : 'actuals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 176 "parser.mly"
                                        ( [_1] )
# 981 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 177 "parser.mly"
                                        ( _1 :: _3 )
# 989 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 180 "parser.mly"
                                        ( makeExpr (Variable _1) )
# 996 "parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'variable) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 181 "parser.mly"
                                        ( makeExpr (Sub (_1, _3)) )
# 1004 "parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'variable) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 182 "parser.mly"
                                        ( makeExpr (Select (_1, _3)) )
# 1012 "parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'variable) in
    Obj.repr(
# 183 "parser.mly"
                                        ( makeExpr (Deref _1) )
# 1019 "parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 186 "parser.mly"
                                        ( TypeName _1 )
# 1026 "parser.ml"
               : 'typexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'typexpr) in
    Obj.repr(
# 187 "parser.mly"
                                        ( Array (_2, _4) )
# 1034 "parser.ml"
               : 'typexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fields) in
    Obj.repr(
# 188 "parser.mly"
                                        ( Record _2 )
# 1041 "parser.ml"
               : 'typexpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typexpr) in
    Obj.repr(
# 189 "parser.mly"
                                        ( Pointer _3 )
# 1048 "parser.ml"
               : 'typexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'field_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt_semi) in
    Obj.repr(
# 192 "parser.mly"
                                        ( [_1] )
# 1056 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'field_decl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fields) in
    Obj.repr(
# 193 "parser.mly"
                                        ( _1 :: _3 )
# 1064 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typexpr) in
    Obj.repr(
# 196 "parser.mly"
                                        ( VarDecl (FieldDef, _1, _3) )
# 1072 "parser.ml"
               : 'field_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 199 "parser.mly"
                                        ( () )
# 1078 "parser.ml"
               : 'opt_semi))
; (fun __caml_parser_env ->
    Obj.repr(
# 200 "parser.mly"
                                        ( () )
# 1084 "parser.ml"
               : 'opt_semi))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Dict.ident) in
    Obj.repr(
# 203 "parser.mly"
                                        ( makeName (_1, !Lexer.lineno) )
# 1091 "parser.ml"
               : 'name))
(* Entry program *)
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
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Tree.program)
