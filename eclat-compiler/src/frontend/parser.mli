
(* The type of tokens. *)

type token = 
  | XOR
  | WITH
  | VECTOR_MAPI
  | VECTOR_CREATE
  | UP_IDENT of (string)
  | UNROLL
  | TYPE
  | TVAR_IDENT of (string)
  | TUPLE_UPDATE
  | TUPLE_OF_INT
  | TUPLE_GET
  | TO
  | TIMES
  | THEN
  | STRING_LIT of (string)
  | STATIC
  | SIZE_CREATE
  | SHARP_PIPE_LBRACKET
  | SET
  | SEMI_SEMI
  | SEMI
  | RPAREN
  | RIGHT_ARROW
  | RESIZE_INT
  | REGISTER
  | REF
  | REC
  | RBRACKET
  | QUOTE
  | PLUS
  | PIPE_RBRACKET
  | PIPE_PIPE
  | PIPE_COMMA_PIPE
  | PIPE
  | PARFOR
  | OR
  | OF
  | NOT
  | NODE
  | NEQ
  | MOD
  | MINUS
  | MATCH
  | MACRO_GENERATE
  | MACRO_FOR
  | LXOR
  | LT
  | LSR
  | LSL
  | LPAREN
  | LOR
  | LET
  | LENGTH
  | LEFT_ARROW
  | LE
  | LBRACKET_PIPE
  | LBRACKET
  | LAST
  | LAND
  | INT_OF_TUPLE
  | INT_MAPI
  | INT_LIT of (int)
  | INIT_TUPLE
  | INIT_INT
  | IN
  | IMPLY
  | IMMEDIATE
  | IF
  | IDENT of (string)
  | HAT
  | GT
  | GET
  | GE
  | FUN
  | FOR
  | FIX
  | EXIT_REPL
  | EXEC
  | EQ_EQ
  | EQ
  | EOF
  | END
  | ELSE
  | DOT_LENGTH
  | DOT
  | DONE
  | DO
  | DIV
  | DEFAULT
  | CREATE
  | COMMA
  | COL_EQ
  | COL
  | BOOL_LIT of (bool)
  | BANG
  | AT_AT
  | AT
  | ASR
  | ARRAY_LENGTH
  | ARRAY_CREATE
  | AND
  | AMP_AMP
  | AMP

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val pi: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.sz) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list)

val exp_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.e_static)

val decl_opt: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (((Ast.p * Ast.e_static) * Prelude.loc) option)
