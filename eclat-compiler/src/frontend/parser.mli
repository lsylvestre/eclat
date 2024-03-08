
(* The type of tokens. *)

type token = 
  | XOR
  | WITH
  | UP_IDENT of (string)
  | TYPE
  | TVAR_IDENT of (string)
  | TUPLE_OF_INT
  | TO
  | TIMES
  | THEN
  | STRING_LIT of (string)
  | STATIC
  | SHARP_PIPE_LBRACKET
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
  | LEFT_ARROW
  | LE
  | LBRACKET
  | LAST
  | LAND
  | INT_LIT of (int)
  | IN
  | IMPLY
  | IMMEDIATE
  | IF
  | IDENT of (string)
  | HAT
  | GT
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
  | COMMA
  | COL_EQ
  | COL
  | BOOL_LIT of (bool)
  | BANG
  | ASR
  | AND
  | AMP_AMP
  | AMP

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val pi: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.ty) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list)

val exp_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.e_static)

val decl_opt: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (((Ast.p * Ast.e_static) * Prelude.loc) option)
