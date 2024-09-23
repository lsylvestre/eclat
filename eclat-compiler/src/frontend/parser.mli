
(* The type of tokens. *)

type token = 
  | XOR
  | WITH_SIZES
  | WITH
  | WHEN
  | VECT_CREATE
  | VECTOR_MAPI
  | UP_IDENT of (string)
  | UNROLL
  | TYPE
  | TYB_VAR_IDENT of (string)
  | TVAR_IDENT of (string)
  | TUPLE_UPDATE
  | TUPLE_OF_INT
  | TUPLE_GET
  | TO
  | TIMES
  | THEN
  | SYM of (string)
  | STRING_LIT of (string)
  | STATIC
  | SIZE_CREATE
  | SHARP_PIPE_LBRACKET
  | SHARED
  | SET
  | SEMI_SEMI
  | SEMI
  | RUN
  | RPAREN
  | RIGHT_ARROW
  | RESIZE_INT
  | RESET
  | REGISTER
  | REF
  | REC
  | RCUR
  | RBRACKET
  | QUOTE
  | QUESTION_MARK
  | PLUS
  | PIPE_RBRACKET
  | PIPE_PIPE
  | PIPE_COMMA_PIPE
  | PIPE
  | PARFOR
  | OR
  | OPERATOR_IDENT of (string)
  | OPERATOR
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
  | LCUR
  | LBRACKET_PIPE
  | LBRACKET
  | LAND
  | INT_OF_TUPLE
  | INT_MAPI
  | INT_LIT of (int)
  | INIT_TUPLE
  | INIT_INT
  | INIT
  | IN
  | IMPURE
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
  | EXTERNAL
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
  | ARRAY_MAKE
  | ARRAY_LENGTH
  | ARRAY_CREATE
  | ARRAY
  | AND
  | AMP_AMP
  | AMP

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val pi: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list)

val exp_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.e_static)
