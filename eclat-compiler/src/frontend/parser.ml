
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | XOR
    | WITH_SIZES
    | WITH
    | WHEN
    | VECT_CREATE
    | VECTOR_MAPI
    | UP_IDENT of (
# 79 "src/frontend/parser.mly"
       (string)
# 21 "src/frontend/parser.ml"
  )
    | UNROLL
    | TYPE
    | TYB_VAR_IDENT of (
# 79 "src/frontend/parser.mly"
       (string)
# 28 "src/frontend/parser.ml"
  )
    | TVAR_IDENT of (
# 79 "src/frontend/parser.mly"
       (string)
# 33 "src/frontend/parser.ml"
  )
    | TUPLE_UPDATE
    | TUPLE_OF_INT
    | TUPLE_GET
    | TO
    | TIMES
    | THEN
    | SYM of (
# 90 "src/frontend/parser.mly"
       (string)
# 44 "src/frontend/parser.ml"
  )
    | STRING_LIT of (
# 97 "src/frontend/parser.mly"
       (string)
# 49 "src/frontend/parser.ml"
  )
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
    | OPERATOR_IDENT of (
# 91 "src/frontend/parser.mly"
       (string)
# 80 "src/frontend/parser.ml"
  )
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
    | INT_LIT of (
# 81 "src/frontend/parser.mly"
       (int)
# 111 "src/frontend/parser.ml"
  )
    | INIT_TUPLE
    | INIT_INT
    | INIT
    | IN
    | IMPURE
    | IMPLY
    | IMMEDIATE
    | IF
    | IDENT of (
# 79 "src/frontend/parser.mly"
       (string)
# 124 "src/frontend/parser.ml"
  )
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
    | BOOL_LIT of (
# 80 "src/frontend/parser.mly"
       (bool)
# 154 "src/frontend/parser.ml"
  )
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
  
end

include MenhirBasics

# 1 "src/frontend/parser.mly"
  

  open Prelude
  open Operators
  open Types
  open Ast
  open Ast_mk

  (* location augmented we a file name *)
  let with_file loc =
    (!Current_filename.current_file_name, loc)

  let alias_types = Hashtbl.create 10

  let add_alias x ty loc =
    match Hashtbl.find_opt alias_types x with
    | Some (_,loc') ->
        let open Errors in
        error ~loc
              (fun fmt -> 
                 Format.fprintf fmt "type %s cannot be redefined. Previous definition at %a"
                    x (fun fmt -> emph_pp bold pp_loc fmt) loc')
    | _ ->
      Hashtbl.add alias_types x (ty,loc)

  let rec as_const loc e =
    match un_annot e with
    | E_const c -> c
    | E_tuple es -> C_tuple(List.map (as_const loc) es)
    | _ -> Format.fprintf Format.std_formatter "--->%a\n" Ast_pprint.pp_exp e;
    Prelude.Errors.raise_error ~loc:(with_file loc)
              ~msg:"this expression should be a constant" ()

  let hash_size_tvar = Hashtbl.create 10 ;;
  let decl_size_var x =
    if Hashtbl.mem hash_size_tvar x then
    Hashtbl.find hash_size_tvar x else
    (let v = (new_size_unknown ()) in
     Hashtbl.add hash_size_tvar x v; v)

  let hash_ty_tvar = Hashtbl.create 10 ;;
  let decl_ty_var x =
    if Hashtbl.mem hash_ty_tvar x then
    Hashtbl.find hash_ty_tvar x else
    (let v = (new_ty_unknown ()) in
     Hashtbl.add hash_ty_tvar x v; v)

  let hash_tyB_tvar = Hashtbl.create 10 ;;
  let decl_tyB_var x =
    if Hashtbl.mem hash_tyB_tvar x then
    Hashtbl.find hash_tyB_tvar x else
    (let v = (new_tyB_unknown ()) in
     Hashtbl.add hash_tyB_tvar x v; v)


  let clear_tyvar_constraints () =
    Hashtbl.clear hash_size_tvar;
    Hashtbl.clear hash_ty_tvar;
    Hashtbl.clear hash_tyB_tvar 

  let infix_operators : (x, x) Hashtbl.t = Hashtbl.create 10

  let get_arity ty = 
    match ty with
    | Ty_fun(Ty_base(TyB_tuple ts),_,_) -> List.length ts
    | Ty_fun(Ty_base _,_,_) -> 1
    | _ -> assert false (* todo *)


# 242 "src/frontend/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_exp_eof) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: exp_eof. *)

  | MenhirState002 : (('s, 'r) _menhir_cell1_VECT_CREATE, 'r) _menhir_state
    (** State 002.
        Stack shape : VECT_CREATE.
        Start symbol: <undetermined>. *)

  | MenhirState007 : (('s, 'r) _menhir_cell1_VECTOR_MAPI, 'r) _menhir_state
    (** State 007.
        Stack shape : VECTOR_MAPI.
        Start symbol: <undetermined>. *)

  | MenhirState017 : (('s, 'r) _menhir_cell1_SHARP_PIPE_LBRACKET, 'r) _menhir_state
    (** State 017.
        Stack shape : SHARP_PIPE_LBRACKET.
        Start symbol: <undetermined>. *)

  | MenhirState020 : (('s, 'r) _menhir_cell1_SET, 'r) _menhir_state
    (** State 020.
        Stack shape : SET.
        Start symbol: <undetermined>. *)

  | MenhirState022 : (('s, 'r) _menhir_cell1_RUN _menhir_cell0_UP_IDENT, 'r) _menhir_state
    (** State 022.
        Stack shape : RUN UP_IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState024 : (('s, 'r) _menhir_cell1_RESIZE_INT, 'r) _menhir_state
    (** State 024.
        Stack shape : RESIZE_INT.
        Start symbol: <undetermined>. *)

  | MenhirState028 : (('s, 'r) _menhir_cell1_MATCH, 'r) _menhir_state
    (** State 028.
        Stack shape : MATCH.
        Start symbol: <undetermined>. *)

  | MenhirState029 : (('s, 'r) _menhir_cell1_REGISTER, 'r) _menhir_state
    (** State 029.
        Stack shape : REGISTER.
        Start symbol: <undetermined>. *)

  | MenhirState030 : (('s, 'r) _menhir_cell1_REF, 'r) _menhir_state
    (** State 030.
        Stack shape : REF.
        Start symbol: <undetermined>. *)

  | MenhirState033 : (('s, 'r) _menhir_cell1_MACRO_FOR _menhir_cell0_IDENT, 'r) _menhir_state
    (** State 033.
        Stack shape : MACRO_FOR IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState034 : (('s, 'r) _menhir_cell1_NODE, 'r) _menhir_state
    (** State 034.
        Stack shape : NODE.
        Start symbol: <undetermined>. *)

  | MenhirState035 : ((('s, 'r) _menhir_cell1_NODE, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 035.
        Stack shape : NODE IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState036 : ((('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 036.
        Stack shape : IDENT LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState038 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 038.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState043 : (('s, 'r) _menhir_cell1_apat, 'r) _menhir_state
    (** State 043.
        Stack shape : apat.
        Start symbol: <undetermined>. *)

  | MenhirState046 : ((('s, 'r) _menhir_cell1_apat, 'r) _menhir_cell1_apat, 'r) _menhir_state
    (** State 046.
        Stack shape : apat apat.
        Start symbol: <undetermined>. *)

  | MenhirState051 : (((('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_apat, 'r) _menhir_state
    (** State 051.
        Stack shape : IDENT LPAREN apat.
        Start symbol: <undetermined>. *)

  | MenhirState054 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 054.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState055 : (('s, 'r) _menhir_cell1_LT, 'r) _menhir_state
    (** State 055.
        Stack shape : LT.
        Start symbol: <undetermined>. *)

  | MenhirState058 : (('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 058.
        Stack shape : IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState059 : (('s, 'r) _menhir_cell1_LT, 'r) _menhir_state
    (** State 059.
        Stack shape : LT.
        Start symbol: <undetermined>. *)

  | MenhirState061 : (('s, 'r) _menhir_cell1_size, 'r) _menhir_state
    (** State 061.
        Stack shape : size.
        Start symbol: <undetermined>. *)

  | MenhirState068 : (('s, 'r) _menhir_cell1_ty_next, 'r) _menhir_state
    (** State 068.
        Stack shape : ty_next.
        Start symbol: <undetermined>. *)

  | MenhirState070 : ((('s, 'r) _menhir_cell1_ty_next, 'r) _menhir_cell1_ty_next, 'r) _menhir_state
    (** State 070.
        Stack shape : ty_next ty_next.
        Start symbol: <undetermined>. *)

  | MenhirState072 : (('s, 'r) _menhir_cell1_tyB_next _menhir_cell0_IDENT, 'r) _menhir_state
    (** State 072.
        Stack shape : tyB_next IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState076 : (('s, 'r) _menhir_cell1_ty_next, 'r) _menhir_state
    (** State 076.
        Stack shape : ty_next.
        Start symbol: <undetermined>. *)

  | MenhirState077 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 077.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState079 : (('s, 'r) _menhir_cell1_tyB_next, 'r) _menhir_state
    (** State 079.
        Stack shape : tyB_next.
        Start symbol: <undetermined>. *)

  | MenhirState081 : ((('s, 'r) _menhir_cell1_tyB_next, 'r) _menhir_cell1_tyB_next, 'r) _menhir_state
    (** State 081.
        Stack shape : tyB_next tyB_next.
        Start symbol: <undetermined>. *)

  | MenhirState086 : (('s, 'r) _menhir_cell1_tyB, 'r) _menhir_state
    (** State 086.
        Stack shape : tyB.
        Start symbol: <undetermined>. *)

  | MenhirState091 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_separated_nonempty_list_COMMA_tyB_ _menhir_cell0_RPAREN _menhir_cell0_IDENT, 'r) _menhir_state
    (** State 091.
        Stack shape : LPAREN separated_nonempty_list(COMMA,tyB) RPAREN IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState095 : (('s, 'r) _menhir_cell1_ty_next, 'r) _menhir_state
    (** State 095.
        Stack shape : ty_next.
        Start symbol: <undetermined>. *)

  | MenhirState097 : (('s, 'r) _menhir_cell1_ty_next, 'r) _menhir_state
    (** State 097.
        Stack shape : ty_next.
        Start symbol: <undetermined>. *)

  | MenhirState100 : (('s, 'r) _menhir_cell1_ty_next, 'r) _menhir_state
    (** State 100.
        Stack shape : ty_next.
        Start symbol: <undetermined>. *)

  | MenhirState109 : ((('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_arg_ty_atomic, 'r) _menhir_state
    (** State 109.
        Stack shape : IDENT arg_ty_atomic.
        Start symbol: <undetermined>. *)

  | MenhirState111 : (((('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_arg_ty_atomic, 'r) _menhir_cell1_COL, 'r) _menhir_state
    (** State 111.
        Stack shape : IDENT arg_ty_atomic COL.
        Start symbol: <undetermined>. *)

  | MenhirState114 : (((('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_arg_ty_atomic, 'r) _menhir_cell1_ret_ty_annot_eq, 'r) _menhir_state
    (** State 114.
        Stack shape : IDENT arg_ty_atomic ret_ty_annot_eq.
        Start symbol: <undetermined>. *)

  | MenhirState115 : (('s, 'r) _menhir_cell1_MINUS, 'r) _menhir_state
    (** State 115.
        Stack shape : MINUS.
        Start symbol: <undetermined>. *)

  | MenhirState116 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 116.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState128 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_MINUS, 'r) _menhir_state
    (** State 128.
        Stack shape : LPAREN MINUS.
        Start symbol: <undetermined>. *)

  | MenhirState130 : (('s, 'r) _menhir_cell1_LCUR, 'r) _menhir_state
    (** State 130.
        Stack shape : LCUR.
        Start symbol: <undetermined>. *)

  | MenhirState131 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 131.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState132 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_MINUS, 'r) _menhir_state
    (** State 132.
        Stack shape : LPAREN MINUS.
        Start symbol: <undetermined>. *)

  | MenhirState168 : (('s, 'r) _menhir_cell1_const, 'r) _menhir_state
    (** State 168.
        Stack shape : const.
        Start symbol: <undetermined>. *)

  | MenhirState174 : (('s, 'r) _menhir_cell1_INT_MAPI, 'r) _menhir_state
    (** State 174.
        Stack shape : INT_MAPI.
        Start symbol: <undetermined>. *)

  | MenhirState178 : (('s, 'r) _menhir_cell1_FOR _menhir_cell0_IDENT, 'r) _menhir_state
    (** State 178.
        Stack shape : FOR IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState179 : (('s, 'r) _menhir_cell1_MACRO_GENERATE, 'r) _menhir_state
    (** State 179.
        Stack shape : MACRO_GENERATE.
        Start symbol: <undetermined>. *)

  | MenhirState180 : (('s, 'r) _menhir_cell1_BANG, 'r) _menhir_state
    (** State 180.
        Stack shape : BANG.
        Start symbol: <undetermined>. *)

  | MenhirState184 : ((('s, 'r) _menhir_cell1_MACRO_GENERATE, 'r) _menhir_cell1_aexp, 'r) _menhir_state
    (** State 184.
        Stack shape : MACRO_GENERATE aexp.
        Start symbol: <undetermined>. *)

  | MenhirState185 : (((('s, 'r) _menhir_cell1_MACRO_GENERATE, 'r) _menhir_cell1_aexp, 'r) _menhir_cell1_aexp, 'r) _menhir_state
    (** State 185.
        Stack shape : MACRO_GENERATE aexp aexp.
        Start symbol: <undetermined>. *)

  | MenhirState187 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 187.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState188 : (('s, 'r) _menhir_cell1_LET, 'r) _menhir_state
    (** State 188.
        Stack shape : LET.
        Start symbol: <undetermined>. *)

  | MenhirState189 : ((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_REC, 'r) _menhir_state
    (** State 189.
        Stack shape : LET REC.
        Start symbol: <undetermined>. *)

  | MenhirState190 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 190.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState191 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 191.
        Stack shape : LPAREN IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState192 : ((('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_COL, 'r) _menhir_state
    (** State 192.
        Stack shape : IDENT COL.
        Start symbol: <undetermined>. *)

  | MenhirState196 : (((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_REC, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 196.
        Stack shape : LET REC IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState197 : ((((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_REC, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_arg_ty_atomic, 'r) _menhir_state
    (** State 197.
        Stack shape : LET REC IDENT arg_ty_atomic.
        Start symbol: <undetermined>. *)

  | MenhirState198 : (((((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_REC, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_arg_ty_atomic, 'r) _menhir_cell1_ret_ty_annot_eq, 'r) _menhir_state
    (** State 198.
        Stack shape : LET REC IDENT arg_ty_atomic ret_ty_annot_eq.
        Start symbol: <undetermined>. *)

  | MenhirState199 : (('s, 'r) _menhir_cell1_LENGTH, 'r) _menhir_state
    (** State 199.
        Stack shape : LENGTH.
        Start symbol: <undetermined>. *)

  | MenhirState201 : (('s, 'r) _menhir_cell1_IF, 'r) _menhir_state
    (** State 201.
        Stack shape : IF.
        Start symbol: <undetermined>. *)

  | MenhirState203 : (('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 203.
        Stack shape : IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState204 : (('s, 'r) _menhir_cell1_FUN, 'r) _menhir_state
    (** State 204.
        Stack shape : FUN.
        Start symbol: <undetermined>. *)

  | MenhirState205 : ((('s, 'r) _menhir_cell1_FUN, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 205.
        Stack shape : FUN LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState210 : (('s, 'r) _menhir_cell1_apat, 'r) _menhir_state
    (** State 210.
        Stack shape : apat.
        Start symbol: <undetermined>. *)

  | MenhirState216 : ((('s, 'r) _menhir_cell1_FUN, 'r) _menhir_cell1_arg_ty, 'r) _menhir_state
    (** State 216.
        Stack shape : FUN arg_ty.
        Start symbol: <undetermined>. *)

  | MenhirState218 : (('s, 'r) _menhir_cell1_FIX _menhir_cell0_IDENT, 'r) _menhir_state
    (** State 218.
        Stack shape : FIX IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState219 : (('s, 'r) _menhir_cell1_EXEC, 'r) _menhir_state
    (** State 219.
        Stack shape : EXEC.
        Start symbol: <undetermined>. *)

  | MenhirState221 : (('s, 'r) _menhir_cell1_CREATE, 'r) _menhir_state
    (** State 221.
        Stack shape : CREATE.
        Start symbol: <undetermined>. *)

  | MenhirState227 : (('s, 'r) _menhir_cell1_ARRAY_MAKE, 'r) _menhir_state
    (** State 227.
        Stack shape : ARRAY_MAKE.
        Start symbol: <undetermined>. *)

  | MenhirState229 : ((('s, 'r) _menhir_cell1_ARRAY_MAKE, 'r) _menhir_cell1_size _menhir_cell0_GT, 'r) _menhir_state
    (** State 229.
        Stack shape : ARRAY_MAKE size GT.
        Start symbol: <undetermined>. *)

  | MenhirState231 : (('s, 'r) _menhir_cell1_ARRAY_LENGTH, 'r) _menhir_state
    (** State 231.
        Stack shape : ARRAY_LENGTH.
        Start symbol: <undetermined>. *)

  | MenhirState234 : (('s, 'r) _menhir_cell1_ARRAY_CREATE, 'r) _menhir_state
    (** State 234.
        Stack shape : ARRAY_CREATE.
        Start symbol: <undetermined>. *)

  | MenhirState241 : (('s, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 241.
        Stack shape : lexp.
        Start symbol: <undetermined>. *)

  | MenhirState246 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 246.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState248 : (('s, 'r) _menhir_cell1_aexp, 'r) _menhir_state
    (** State 248.
        Stack shape : aexp.
        Start symbol: <undetermined>. *)

  | MenhirState249 : ((('s, 'r) _menhir_cell1_aexp, 'r) _menhir_cell1_COL_EQ, 'r) _menhir_state
    (** State 249.
        Stack shape : aexp COL_EQ.
        Start symbol: <undetermined>. *)

  | MenhirState251 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 251.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState253 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 253.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState255 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 255.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState257 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 257.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState259 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 259.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState261 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 261.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState263 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 263.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState265 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 265.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState267 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 267.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState269 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 269.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState271 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 271.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState273 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 273.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState275 : (('s, 'r) _menhir_cell1_app_exp _menhir_cell0_MINUS, 'r) _menhir_state
    (** State 275.
        Stack shape : app_exp MINUS.
        Start symbol: <undetermined>. *)

  | MenhirState277 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 277.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState279 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 279.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState281 : (('s, 'r) _menhir_cell1_app_exp _menhir_cell0_GT, 'r) _menhir_state
    (** State 281.
        Stack shape : app_exp GT.
        Start symbol: <undetermined>. *)

  | MenhirState283 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 283.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState285 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 285.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState287 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 287.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState289 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 289.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState291 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 291.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState294 : ((('s, 'r) _menhir_cell1_aexp, 'r) _menhir_cell1_aexp, 'r) _menhir_state
    (** State 294.
        Stack shape : aexp aexp.
        Start symbol: <undetermined>. *)

  | MenhirState295 : (((('s, 'r) _menhir_cell1_aexp, 'r) _menhir_cell1_aexp, 'r) _menhir_cell1_AT_AT, 'r) _menhir_state
    (** State 295.
        Stack shape : aexp aexp AT_AT.
        Start symbol: <undetermined>. *)

  | MenhirState296 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 296.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState297 : (('s, 'r) _menhir_cell1_FUN, 'r) _menhir_state
    (** State 297.
        Stack shape : FUN.
        Start symbol: <undetermined>. *)

  | MenhirState299 : ((('s, 'r) _menhir_cell1_FUN, 'r) _menhir_cell1_arg_ty, 'r) _menhir_state
    (** State 299.
        Stack shape : FUN arg_ty.
        Start symbol: <undetermined>. *)

  | MenhirState305 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_lvalue, 'r) _menhir_state
    (** State 305.
        Stack shape : LPAREN lvalue.
        Start symbol: <undetermined>. *)

  | MenhirState308 : ((('s, 'r) _menhir_cell1_lvalue, 'r) _menhir_cell1_lvalue, 'r) _menhir_state
    (** State 308.
        Stack shape : lvalue lvalue.
        Start symbol: <undetermined>. *)

  | MenhirState313 : (((('s, 'r) _menhir_cell1_aexp, 'r) _menhir_cell1_aexp, 'r) _menhir_cell1_AT, 'r) _menhir_state
    (** State 313.
        Stack shape : aexp aexp AT.
        Start symbol: <undetermined>. *)

  | MenhirState316 : (((('s, 'r) _menhir_cell1_aexp, 'r) _menhir_cell1_aexp, 'r) _menhir_cell1_aexp, 'r) _menhir_state
    (** State 316.
        Stack shape : aexp aexp aexp.
        Start symbol: <undetermined>. *)

  | MenhirState317 : (('s, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 317.
        Stack shape : lexp.
        Start symbol: <undetermined>. *)

  | MenhirState320 : ((('s, 'r) _menhir_cell1_lexp, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 320.
        Stack shape : lexp lexp.
        Start symbol: <undetermined>. *)

  | MenhirState323 : ((('s, 'r) _menhir_cell1_EXEC, 'r) _menhir_cell1_exp _menhir_cell0_DEFAULT, 'r) _menhir_state
    (** State 323.
        Stack shape : EXEC exp DEFAULT.
        Start symbol: <undetermined>. *)

  | MenhirState325 : (((('s, 'r) _menhir_cell1_EXEC, 'r) _menhir_cell1_exp _menhir_cell0_DEFAULT, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 325.
        Stack shape : EXEC exp DEFAULT lexp.
        Start symbol: <undetermined>. *)

  | MenhirState331 : ((('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_exp _menhir_cell0_RBRACKET, 'r) _menhir_state
    (** State 331.
        Stack shape : IDENT exp RBRACKET.
        Start symbol: <undetermined>. *)

  | MenhirState335 : (('s, 'r) _menhir_cell1_IDENT _menhir_cell0_LPAREN, 'r) _menhir_state
    (** State 335.
        Stack shape : IDENT LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState339 : (('s, 'r) _menhir_cell1_IDENT _menhir_cell0_dot_get, 'r) _menhir_state
    (** State 339.
        Stack shape : IDENT dot_get.
        Start symbol: <undetermined>. *)

  | MenhirState342 : ((('s, 'r) _menhir_cell1_IF, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 342.
        Stack shape : IF exp.
        Start symbol: <undetermined>. *)

  | MenhirState344 : (((('s, 'r) _menhir_cell1_IF, 'r) _menhir_cell1_exp, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 344.
        Stack shape : IF exp lexp.
        Start symbol: <undetermined>. *)

  | MenhirState351 : (((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_REC, 'r) _menhir_cell1_ty_annot_IDENT_, 'r) _menhir_state
    (** State 351.
        Stack shape : LET REC ty_annot(IDENT).
        Start symbol: <undetermined>. *)

  | MenhirState354 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 354.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState358 : (('s, 'r) _menhir_cell1_apat, 'r) _menhir_state
    (** State 358.
        Stack shape : apat.
        Start symbol: <undetermined>. *)

  | MenhirState360 : ((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 360.
        Stack shape : LET IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState362 : (('s, 'r) _menhir_cell1_ty_annot_apat_, 'r) _menhir_state
    (** State 362.
        Stack shape : ty_annot(apat).
        Start symbol: <undetermined>. *)

  | MenhirState370 : (('s, 'r) _menhir_cell1_binding_apat_exp_, 'r) _menhir_state
    (** State 370.
        Stack shape : binding(apat,exp).
        Start symbol: <undetermined>. *)

  | MenhirState373 : (('s, 'r) _menhir_cell1_apat, 'r) _menhir_state
    (** State 373.
        Stack shape : apat.
        Start symbol: <undetermined>. *)

  | MenhirState375 : ((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_after_let_IN_, 'r) _menhir_state
    (** State 375.
        Stack shape : LET after_let(IN).
        Start symbol: <undetermined>. *)

  | MenhirState378 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 378.
        Stack shape : LPAREN lexp.
        Start symbol: <undetermined>. *)

  | MenhirState382 : ((('s, 'r) _menhir_cell1_lexp, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 382.
        Stack shape : lexp lexp.
        Start symbol: <undetermined>. *)

  | MenhirState384 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 384.
        Stack shape : LPAREN lexp.
        Start symbol: <undetermined>. *)

  | MenhirState388 : ((('s, 'r) _menhir_cell1_lexp, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 388.
        Stack shape : lexp lexp.
        Start symbol: <undetermined>. *)

  | MenhirState392 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 392.
        Stack shape : LPAREN exp.
        Start symbol: <undetermined>. *)

  | MenhirState396 : ((('s, 'r) _menhir_cell1_FOR _menhir_cell0_IDENT, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 396.
        Stack shape : FOR IDENT exp.
        Start symbol: <undetermined>. *)

  | MenhirState398 : (((('s, 'r) _menhir_cell1_FOR _menhir_cell0_IDENT, 'r) _menhir_cell1_exp, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 398.
        Stack shape : FOR IDENT exp exp.
        Start symbol: <undetermined>. *)

  | MenhirState405 : ((('s, 'r) _menhir_cell1_NODE, 'r) _menhir_cell1_fun_decl_IN_, 'r) _menhir_state
    (** State 405.
        Stack shape : NODE fun_decl(IN).
        Start symbol: <undetermined>. *)

  | MenhirState408 : ((('s, 'r) _menhir_cell1_MACRO_FOR _menhir_cell0_IDENT, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 408.
        Stack shape : MACRO_FOR IDENT exp.
        Start symbol: <undetermined>. *)

  | MenhirState410 : (((('s, 'r) _menhir_cell1_MACRO_FOR _menhir_cell0_IDENT, 'r) _menhir_cell1_exp, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 410.
        Stack shape : MACRO_FOR IDENT exp exp.
        Start symbol: <undetermined>. *)

  | MenhirState415 : ((('s, 'r) _menhir_cell1_REGISTER, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 415.
        Stack shape : REGISTER IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState418 : ((('s, 'r) _menhir_cell1_REGISTER, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 418.
        Stack shape : REGISTER exp.
        Start symbol: <undetermined>. *)

  | MenhirState423 : ((('s, 'r) _menhir_cell1_MATCH, 'r) _menhir_cell1_exp _menhir_cell0_option_PIPE_, 'r) _menhir_state
    (** State 423.
        Stack shape : MATCH exp option(PIPE).
        Start symbol: <undetermined>. *)

  | MenhirState424 : (((('s, 'r) _menhir_cell1_MATCH, 'r) _menhir_cell1_exp _menhir_cell0_option_PIPE_, 'r) _menhir_cell1_UP_IDENT, 'r) _menhir_state
    (** State 424.
        Stack shape : MATCH exp option(PIPE) UP_IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState426 : ((('s, 'r) _menhir_cell1_UP_IDENT, 'r) _menhir_cell1_apat, 'r) _menhir_state
    (** State 426.
        Stack shape : UP_IDENT apat.
        Start symbol: <undetermined>. *)

  | MenhirState429 : (('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 429.
        Stack shape : IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState433 : (('s, 'r) _menhir_cell1_separated_nonempty_list_PIPE_const_, 'r) _menhir_state
    (** State 433.
        Stack shape : separated_nonempty_list(PIPE,const).
        Start symbol: <undetermined>. *)

  | MenhirState438 : (('s, 'r) _menhir_cell1_match_case_const, 'r) _menhir_state
    (** State 438.
        Stack shape : match_case_const.
        Start symbol: <undetermined>. *)

  | MenhirState441 : (('s, 'r) _menhir_cell1_const, 'r) _menhir_state
    (** State 441.
        Stack shape : const.
        Start symbol: <undetermined>. *)

  | MenhirState444 : (('s, 'r) _menhir_cell1_match_case, 'r) _menhir_state
    (** State 444.
        Stack shape : match_case.
        Start symbol: <undetermined>. *)

  | MenhirState445 : ((('s, 'r) _menhir_cell1_match_case, 'r) _menhir_cell1_UP_IDENT, 'r) _menhir_state
    (** State 445.
        Stack shape : match_case UP_IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState449 : (((('s, 'r) _menhir_cell1_MATCH, 'r) _menhir_cell1_exp _menhir_cell0_option_PIPE_, 'r) _menhir_cell1_list_match_case_const_ _menhir_cell0_IDENT, 'r) _menhir_state
    (** State 449.
        Stack shape : MATCH exp option(PIPE) list(match_case_const) IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState458 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 458.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState464 : ('s, _menhir_box_pi) _menhir_state
    (** State 464.
        Stack shape : .
        Start symbol: pi. *)

  | MenhirState465 : (('s, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_state
    (** State 465.
        Stack shape : TYPE.
        Start symbol: pi. *)

  | MenhirState466 : ((('s, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_state
    (** State 466.
        Stack shape : TYPE IDENT.
        Start symbol: pi. *)

  | MenhirState467 : (((('s, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_EQ, _menhir_box_pi) _menhir_state
    (** State 467.
        Stack shape : TYPE IDENT EQ.
        Start symbol: pi. *)

  | MenhirState469 : (('s, _menhir_box_pi) _menhir_cell1_UP_IDENT, _menhir_box_pi) _menhir_state
    (** State 469.
        Stack shape : UP_IDENT.
        Start symbol: pi. *)

  | MenhirState472 : (('s, _menhir_box_pi) _menhir_cell1_ty_case, _menhir_box_pi) _menhir_state
    (** State 472.
        Stack shape : ty_case.
        Start symbol: pi. *)

  | MenhirState474 : ((((('s, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_EQ, _menhir_box_pi) _menhir_cell1_tyB, _menhir_box_pi) _menhir_state
    (** State 474.
        Stack shape : TYPE IDENT EQ tyB.
        Start symbol: pi. *)

  | MenhirState477 : ((((('s, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_EQ, _menhir_box_pi) _menhir_cell1_separated_nonempty_list_PIPE_ty_case_, _menhir_box_pi) _menhir_state
    (** State 477.
        Stack shape : TYPE IDENT EQ separated_nonempty_list(PIPE,ty_case).
        Start symbol: pi. *)

  | MenhirState479 : ((('s, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_tyB, _menhir_box_pi) _menhir_state
    (** State 479.
        Stack shape : TYPE tyB.
        Start symbol: pi. *)

  | MenhirState481 : (((('s, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_tyB, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_state
    (** State 481.
        Stack shape : TYPE tyB IDENT.
        Start symbol: pi. *)

  | MenhirState483 : (('s, _menhir_box_pi) _menhir_cell1_INT_LIT, _menhir_box_pi) _menhir_state
    (** State 483.
        Stack shape : INT_LIT.
        Start symbol: pi. *)

  | MenhirState486 : ((((('s, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_tyB, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_separated_nonempty_list_COMMA_INT_LIT_ _menhir_cell0_RBRACKET, _menhir_box_pi) _menhir_state
    (** State 486.
        Stack shape : TYPE tyB IDENT separated_nonempty_list(COMMA,INT_LIT) RBRACKET.
        Start symbol: pi. *)

  | MenhirState489 : (((('s, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_tyB, _menhir_box_pi) _menhir_cell1_AT _menhir_cell0_IDENT, _menhir_box_pi) _menhir_state
    (** State 489.
        Stack shape : TYPE tyB AT IDENT.
        Start symbol: pi. *)

  | MenhirState495 : (('s, _menhir_box_pi) _menhir_cell1_SHARED _menhir_cell0_EXTERNAL _menhir_cell0_IDENT, _menhir_box_pi) _menhir_state
    (** State 495.
        Stack shape : SHARED EXTERNAL IDENT.
        Start symbol: pi. *)

  | MenhirState501 : (('s, _menhir_box_pi) _menhir_cell1_OPERATOR _menhir_cell0_OPERATOR_IDENT, _menhir_box_pi) _menhir_state
    (** State 501.
        Stack shape : OPERATOR OPERATOR_IDENT.
        Start symbol: pi. *)

  | MenhirState508 : (('s, _menhir_box_pi) _menhir_cell1_OPERATOR _menhir_cell0_OPERATOR_IDENT, _menhir_box_pi) _menhir_state
    (** State 508.
        Stack shape : OPERATOR OPERATOR_IDENT.
        Start symbol: pi. *)

  | MenhirState514 : (('s, _menhir_box_pi) _menhir_cell1_NODE, _menhir_box_pi) _menhir_state
    (** State 514.
        Stack shape : NODE.
        Start symbol: pi. *)

  | MenhirState515 : ((('s, _menhir_box_pi) _menhir_cell1_NODE, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_state
    (** State 515.
        Stack shape : NODE IDENT.
        Start symbol: pi. *)

  | MenhirState516 : ((('s, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic, _menhir_box_pi) _menhir_state
    (** State 516.
        Stack shape : IDENT arg_ty_atomic.
        Start symbol: pi. *)

  | MenhirState517 : (((('s, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic, _menhir_box_pi) _menhir_cell1_ret_ty_annot_eq, _menhir_box_pi) _menhir_state
    (** State 517.
        Stack shape : IDENT arg_ty_atomic ret_ty_annot_eq.
        Start symbol: pi. *)

  | MenhirState521 : (('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_state
    (** State 521.
        Stack shape : LET.
        Start symbol: pi. *)

  | MenhirState524 : ((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_STATIC _menhir_cell0_IDENT, _menhir_box_pi) _menhir_state
    (** State 524.
        Stack shape : LET STATIC IDENT.
        Start symbol: pi. *)

  | MenhirState526 : (((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_STATIC _menhir_cell0_IDENT, _menhir_box_pi) _menhir_cell1_aexp, _menhir_box_pi) _menhir_state
    (** State 526.
        Stack shape : LET STATIC IDENT aexp.
        Start symbol: pi. *)

  | MenhirState530 : ((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_STATIC _menhir_cell0_IDENT, _menhir_box_pi) _menhir_state
    (** State 530.
        Stack shape : LET STATIC IDENT.
        Start symbol: pi. *)

  | MenhirState533 : ((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_state
    (** State 533.
        Stack shape : LET REC.
        Start symbol: pi. *)

  | MenhirState534 : (((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_state
    (** State 534.
        Stack shape : LET REC IDENT.
        Start symbol: pi. *)

  | MenhirState535 : ((((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic, _menhir_box_pi) _menhir_state
    (** State 535.
        Stack shape : LET REC IDENT arg_ty_atomic.
        Start symbol: pi. *)

  | MenhirState536 : (((((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic, _menhir_box_pi) _menhir_cell1_ret_ty_annot_eq, _menhir_box_pi) _menhir_state
    (** State 536.
        Stack shape : LET REC IDENT arg_ty_atomic ret_ty_annot_eq.
        Start symbol: pi. *)

  | MenhirState540 : (((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_ty_annot_IDENT_, _menhir_box_pi) _menhir_state
    (** State 540.
        Stack shape : LET REC ty_annot(IDENT).
        Start symbol: pi. *)

  | MenhirState543 : ((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_state
    (** State 543.
        Stack shape : LET IDENT.
        Start symbol: pi. *)

  | MenhirState551 : (('s, _menhir_box_pi) _menhir_cell1_EXTERNAL _menhir_cell0_IDENT, _menhir_box_pi) _menhir_state
    (** State 551.
        Stack shape : EXTERNAL IDENT.
        Start symbol: pi. *)

  | MenhirState555 : (('s, _menhir_box_pi) _menhir_cell1_type_alias, _menhir_box_pi) _menhir_state
    (** State 555.
        Stack shape : type_alias.
        Start symbol: pi. *)

  | MenhirState556 : (('s, _menhir_box_pi) _menhir_cell1_typ_sum, _menhir_box_pi) _menhir_state
    (** State 556.
        Stack shape : typ_sum.
        Start symbol: pi. *)

  | MenhirState557 : (('s, _menhir_box_pi) _menhir_cell1_static, _menhir_box_pi) _menhir_state
    (** State 557.
        Stack shape : static.
        Start symbol: pi. *)

  | MenhirState559 : (('s, _menhir_box_pi) _menhir_cell1_ext_fun, _menhir_box_pi) _menhir_state
    (** State 559.
        Stack shape : ext_fun.
        Start symbol: pi. *)

  | MenhirState561 : (('s, _menhir_box_pi) _menhir_cell1_ext_circ, _menhir_box_pi) _menhir_state
    (** State 561.
        Stack shape : ext_circ.
        Start symbol: pi. *)

  | MenhirState566 : (('s, _menhir_box_pi) _menhir_cell1_decl, _menhir_box_pi) _menhir_state
    (** State 566.
        Stack shape : decl.
        Start symbol: pi. *)


and ('s, 'r) _menhir_cell1_aexp = 
  | MenhirCell1_aexp of 's * ('s, 'r) _menhir_state * (Ast.e_static) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_after_let_IN_ = 
  | MenhirCell1_after_let_IN_ of 's * ('s, 'r) _menhir_state * (Ast.p * Ast.e_static)

and ('s, 'r) _menhir_cell1_apat = 
  | MenhirCell1_apat of 's * ('s, 'r) _menhir_state * (Ast.p)

and ('s, 'r) _menhir_cell1_app_exp = 
  | MenhirCell1_app_exp of 's * ('s, 'r) _menhir_state * (Ast.e) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_arg_ty = 
  | MenhirCell1_arg_ty of 's * ('s, 'r) _menhir_state * (Ast.p * Types.ty option)

and ('s, 'r) _menhir_cell1_arg_ty_atomic = 
  | MenhirCell1_arg_ty_atomic of 's * ('s, 'r) _menhir_state * (Ast.p * Types.ty option)

and ('s, 'r) _menhir_cell1_binding_apat_exp_ = 
  | MenhirCell1_binding_apat_exp_ of 's * ('s, 'r) _menhir_state * (Ast.p * Ast.e_static)

and ('s, 'r) _menhir_cell1_bindings_apat_exp_ = 
  | MenhirCell1_bindings_apat_exp_ of 's * ('s, 'r) _menhir_state * (Ast.p * Ast.e_static)

and ('s, 'r) _menhir_cell1_const = 
  | MenhirCell1_const of 's * ('s, 'r) _menhir_state * (Ast.c) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_decl = 
  | MenhirCell1_decl of 's * ('s, 'r) _menhir_state * ((Ast.p * Ast.e_static) * Prelude.loc)

and 's _menhir_cell0_dot_get = 
  | MenhirCell0_dot_get of 's * (Ast.e_static) * Lexing.position

and ('s, 'r) _menhir_cell1_exp = 
  | MenhirCell1_exp of 's * ('s, 'r) _menhir_state * (Ast.e_static) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_ext_circ = 
  | MenhirCell1_ext_circ of 's * ('s, 'r) _menhir_state * (Ast.x * (Types.ty * bool)) * Lexing.position

and ('s, 'r) _menhir_cell1_ext_fun = 
  | MenhirCell1_ext_fun of 's * ('s, 'r) _menhir_state * (Ast.x * (Types.ty * (bool * int * bool)))

and ('s, 'r) _menhir_cell1_fun_decl_IN_ = 
  | MenhirCell1_fun_decl_IN_ of 's * ('s, 'r) _menhir_state * (Ast.p * Ast.e_static)

and ('s, 'r) _menhir_cell1_lexp = 
  | MenhirCell1_lexp of 's * ('s, 'r) _menhir_state * (Ast.e_static) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_list_match_case_const_ = 
  | MenhirCell1_list_match_case_const_ of 's * ('s, 'r) _menhir_state * ((Ast.c list * Ast.e_static) list)

and ('s, 'r) _menhir_cell1_lvalue = 
  | MenhirCell1_lvalue of 's * ('s, 'r) _menhir_state * (Ast.e_static) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_match_case = 
  | MenhirCell1_match_case of 's * ('s, 'r) _menhir_state * (Ast.x * (Ast.p * Ast.e_static))

and ('s, 'r) _menhir_cell1_match_case_const = 
  | MenhirCell1_match_case_const of 's * ('s, 'r) _menhir_state * (Ast.c list * Ast.e_static)

and 's _menhir_cell0_option_PIPE_ = 
  | MenhirCell0_option_PIPE_ of 's * (unit option)

and ('s, 'r) _menhir_cell1_pat = 
  | MenhirCell1_pat of 's * ('s, 'r) _menhir_state * (Ast.p)

and ('s, 'r) _menhir_cell1_ret_ty_annot_eq = 
  | MenhirCell1_ret_ty_annot_eq of 's * ('s, 'r) _menhir_state * (Types.ty option)

and ('s, 'r) _menhir_cell1_separated_nonempty_list_COMMA_INT_LIT_ = 
  | MenhirCell1_separated_nonempty_list_COMMA_INT_LIT_ of 's * ('s, 'r) _menhir_state * (int list)

and ('s, 'r) _menhir_cell1_separated_nonempty_list_COMMA_tyB_ = 
  | MenhirCell1_separated_nonempty_list_COMMA_tyB_ of 's * ('s, 'r) _menhir_state * (Types.tyB list)

and ('s, 'r) _menhir_cell1_separated_nonempty_list_PIPE_const_ = 
  | MenhirCell1_separated_nonempty_list_PIPE_const_ of 's * ('s, 'r) _menhir_state * (Ast.c list)

and ('s, 'r) _menhir_cell1_separated_nonempty_list_PIPE_ty_case_ = 
  | MenhirCell1_separated_nonempty_list_PIPE_ty_case_ of 's * ('s, 'r) _menhir_state * ((Ast.x * Types.tyB) list)

and ('s, 'r) _menhir_cell1_size = 
  | MenhirCell1_size of 's * ('s, 'r) _menhir_state * (Types.size) * Lexing.position

and ('s, 'r) _menhir_cell1_static = 
  | MenhirCell1_static of 's * ('s, 'r) _menhir_state * (Ast.x * Ast.static)

and ('s, 'r) _menhir_cell1_tyB = 
  | MenhirCell1_tyB of 's * ('s, 'r) _menhir_state * (Types.tyB)

and ('s, 'r) _menhir_cell1_tyB_next = 
  | MenhirCell1_tyB_next of 's * ('s, 'r) _menhir_state * (Types.tyB) * Lexing.position

and ('s, 'r) _menhir_cell1_ty_annot_IDENT_ = 
  | MenhirCell1_ty_annot_IDENT_ of 's * ('s, 'r) _menhir_state * (Types.x * Types.ty option) * Lexing.position

and ('s, 'r) _menhir_cell1_ty_annot_apat_ = 
  | MenhirCell1_ty_annot_apat_ of 's * ('s, 'r) _menhir_state * (Ast.p * Types.ty option)

and ('s, 'r) _menhir_cell1_ty_case = 
  | MenhirCell1_ty_case of 's * ('s, 'r) _menhir_state * (Ast.x * Types.tyB)

and ('s, 'r) _menhir_cell1_ty_next = 
  | MenhirCell1_ty_next of 's * ('s, 'r) _menhir_state * (Types.ty) * Lexing.position

and ('s, 'r) _menhir_cell1_typ_sum = 
  | MenhirCell1_typ_sum of 's * ('s, 'r) _menhir_state * (Ast.x * (Ast.x * Types.tyB) list)

and ('s, 'r) _menhir_cell1_type_alias = 
  | MenhirCell1_type_alias of 's * ('s, 'r) _menhir_state * (unit)

and ('s, 'r) _menhir_cell1_ARRAY_CREATE = 
  | MenhirCell1_ARRAY_CREATE of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_ARRAY_LENGTH = 
  | MenhirCell1_ARRAY_LENGTH of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_ARRAY_MAKE = 
  | MenhirCell1_ARRAY_MAKE of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_AT = 
  | MenhirCell1_AT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_AT_AT = 
  | MenhirCell1_AT_AT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_BANG = 
  | MenhirCell1_BANG of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_COL = 
  | MenhirCell1_COL of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_COL_EQ = 
  | MenhirCell1_COL_EQ of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_CREATE = 
  | MenhirCell1_CREATE of 's * ('s, 'r) _menhir_state * Lexing.position

and 's _menhir_cell0_DEFAULT = 
  | MenhirCell0_DEFAULT of 's * Lexing.position

and ('s, 'r) _menhir_cell1_EQ = 
  | MenhirCell1_EQ of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_EXEC = 
  | MenhirCell1_EXEC of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_EXTERNAL = 
  | MenhirCell1_EXTERNAL of 's * ('s, 'r) _menhir_state * Lexing.position

and 's _menhir_cell0_EXTERNAL = 
  | MenhirCell0_EXTERNAL of 's * Lexing.position

and ('s, 'r) _menhir_cell1_FIX = 
  | MenhirCell1_FIX of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_FOR = 
  | MenhirCell1_FOR of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_FUN = 
  | MenhirCell1_FUN of 's * ('s, 'r) _menhir_state * Lexing.position

and 's _menhir_cell0_GT = 
  | MenhirCell0_GT of 's * Lexing.position

and ('s, 'r) _menhir_cell1_IDENT = 
  | MenhirCell1_IDENT of 's * ('s, 'r) _menhir_state * (
# 79 "src/frontend/parser.mly"
       (string)
# 1340 "src/frontend/parser.ml"
) * Lexing.position * Lexing.position

and 's _menhir_cell0_IDENT = 
  | MenhirCell0_IDENT of 's * (
# 79 "src/frontend/parser.mly"
       (string)
# 1347 "src/frontend/parser.ml"
) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_INT_LIT = 
  | MenhirCell1_INT_LIT of 's * ('s, 'r) _menhir_state * (
# 81 "src/frontend/parser.mly"
       (int)
# 1357 "src/frontend/parser.ml"
) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_INT_MAPI = 
  | MenhirCell1_INT_MAPI of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_LCUR = 
  | MenhirCell1_LCUR of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_LENGTH = 
  | MenhirCell1_LENGTH of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_LET = 
  | MenhirCell1_LET of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state * Lexing.position

and 's _menhir_cell0_LPAREN = 
  | MenhirCell0_LPAREN of 's * Lexing.position

and ('s, 'r) _menhir_cell1_LT = 
  | MenhirCell1_LT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_MACRO_FOR = 
  | MenhirCell1_MACRO_FOR of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_MACRO_GENERATE = 
  | MenhirCell1_MACRO_GENERATE of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_MATCH = 
  | MenhirCell1_MATCH of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_MINUS = 
  | MenhirCell1_MINUS of 's * ('s, 'r) _menhir_state * Lexing.position

and 's _menhir_cell0_MINUS = 
  | MenhirCell0_MINUS of 's * Lexing.position

and ('s, 'r) _menhir_cell1_NODE = 
  | MenhirCell1_NODE of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_OPERATOR = 
  | MenhirCell1_OPERATOR of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_OPERATOR_IDENT = 
  | MenhirCell0_OPERATOR_IDENT of 's * (
# 91 "src/frontend/parser.mly"
       (string)
# 1406 "src/frontend/parser.ml"
) * Lexing.position * Lexing.position

and 's _menhir_cell0_RBRACKET = 
  | MenhirCell0_RBRACKET of 's * Lexing.position

and ('s, 'r) _menhir_cell1_REC = 
  | MenhirCell1_REC of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_REF = 
  | MenhirCell1_REF of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_REGISTER = 
  | MenhirCell1_REGISTER of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_RESIZE_INT = 
  | MenhirCell1_RESIZE_INT of 's * ('s, 'r) _menhir_state * Lexing.position

and 's _menhir_cell0_RPAREN = 
  | MenhirCell0_RPAREN of 's * Lexing.position

and ('s, 'r) _menhir_cell1_RUN = 
  | MenhirCell1_RUN of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_SET = 
  | MenhirCell1_SET of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_SHARED = 
  | MenhirCell1_SHARED of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_SHARP_PIPE_LBRACKET = 
  | MenhirCell1_SHARP_PIPE_LBRACKET of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_STATIC = 
  | MenhirCell1_STATIC of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_TYPE = 
  | MenhirCell1_TYPE of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_UP_IDENT = 
  | MenhirCell1_UP_IDENT of 's * ('s, 'r) _menhir_state * (
# 79 "src/frontend/parser.mly"
       (string)
# 1449 "src/frontend/parser.ml"
) * Lexing.position * Lexing.position

and 's _menhir_cell0_UP_IDENT = 
  | MenhirCell0_UP_IDENT of 's * (
# 79 "src/frontend/parser.mly"
       (string)
# 1456 "src/frontend/parser.ml"
) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_VECTOR_MAPI = 
  | MenhirCell1_VECTOR_MAPI of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_VECT_CREATE = 
  | MenhirCell1_VECT_CREATE of 's * ('s, 'r) _menhir_state * Lexing.position

and _menhir_box_pi = 
  | MenhirBox_pi of (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list) [@@unboxed]

and _menhir_box_exp_eof = 
  | MenhirBox_exp_eof of (Ast.e_static) [@@unboxed]

let _menhir_action_002 =
  fun _endpos_e_ _startpos_e_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    let _loc = (_startpos, _endpos) in
    (
# 628 "src/frontend/parser.mly"
              ( mk_loc (with_file _loc) e )
# 1482 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_003 =
  fun ex ->
    (
# 631 "src/frontend/parser.mly"
               ( E_get(ex) )
# 1490 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_004 =
  fun e ->
    (
# 632 "src/frontend/parser.mly"
                      ( e )
# 1498 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_005 =
  fun e ty ->
    (
# 633 "src/frontend/parser.mly"
                                ( ty_annot ~ty e )
# 1506 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_006 =
  fun c ->
    (
# 634 "src/frontend/parser.mly"
          ( E_const c )
# 1514 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_007 =
  fun k ->
    (
# 636 "src/frontend/parser.mly"
                          ( E_const (Op(Runtime(Resize_int k))) )
# 1522 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_008 =
  fun k ->
    (
# 637 "src/frontend/parser.mly"
                               ( E_const (Op(Runtime(Tuple_of_int k))) )
# 1530 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_009 =
  fun k ->
    (
# 638 "src/frontend/parser.mly"
                               ( E_const (Op(Runtime(Int_of_tuple k))) )
# 1538 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_010 =
  fun e ->
    (
# 639 "src/frontend/parser.mly"
                     (
                let loc = loc_of e in
                match Ast_undecorated.remove_deco e with
                | E_tuple[v;e] -> assert (is_variable v || evaluated v); (* todo: error *)
                                  let x = Ast.gensym () in
                                  E_vector_mapi(false,(P_var x,E_app(v,E_var x)), 
                                     mk_loc loc e, new_size_unknown ())
                | _ -> assert false (* todo error *) )
# 1553 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_011 =
  fun e ->
    (
# 647 "src/frontend/parser.mly"
                  (
                match Ast_undecorated.remove_deco e with
                | E_tuple[v;e] -> assert (is_variable v || evaluated v); (* todo: error *)
                                  let x = Ast.gensym () in
                                  E_int_mapi(false,(P_var x,E_app(v,E_var x)), e, new_size_unknown ())
                | _ -> assert false (* todo error *) )
# 1566 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_012 =
  fun k x ->
    (
# 653 "src/frontend/parser.mly"
                        ( E_const (Op(Runtime(Unroll k))) )
# 1574 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_013 =
  fun l ->
    (
# 654 "src/frontend/parser.mly"
                           ( E_const (Op(Runtime(Vector_create l))) )
# 1582 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_014 =
  fun _endpos_x_ _startpos_x_ x ->
    let _endpos = _endpos_x_ in
    let _startpos = _startpos_x_ in
    let _loc = (_startpos, _endpos) in
    (
# 655 "src/frontend/parser.mly"
          ( match x with
            | "abs" -> E_const (Op(Runtime(External_fun("Int.absv",new_ty_unknown ()))))
            | "not" -> E_const (Op(Runtime(External_fun("Bool.lnot",new_ty_unknown ()))))
            | "print" -> E_const (Op(Runtime(Print)))
            | "print_string" -> E_const (Op(Runtime(Print_string)))
            | "print_int" -> E_const (Op(Runtime(Print_int)))
            | "print_newline" -> E_const (Op(Runtime(Print_newline)))
            | "string_length" -> E_const (Op(Runtime(String_length)))
            | "assert" -> E_const (Op(Runtime(Assert)))
            | "get_bit"| "nth_bit" -> E_const (Op(Runtime(GetBit)))
            | "update_bit" -> E_const (Op(Runtime(UpdateBit)))
           (*  | "size_of_val" -> E_const (Op(Runtime(Size_of_val (unknown(),unknown())))) *)
            | "get" -> let x = gensym () in let y = gensym () in
                       E_fun(P_tuple[P_var x;P_var y], E_array_get(x,E_var y))
            | "bvect_of_int" -> E_const (Op(Runtime(Bvect_of_int)))
            | "int_of_bvect" -> E_const (Op(Runtime(Int_of_bvect)))
            | "_" -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                         ~msg:"wildcard \"_\" not expected." ()
            | _ -> E_var x )
# 1611 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_015 =
  fun xs ->
    let _2 = 
# 241 "<standard.mly>"
    ( xs )
# 1619 "src/frontend/parser.ml"
     in
    (
# 675 "src/frontend/parser.mly"
    ( (* Buffer n *) assert false (*todo*)  )
# 1624 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_016 =
  fun cases e otherwise ->
    (
# 680 "src/frontend/parser.mly"
      ( E_case(e,cases,otherwise) )
# 1632 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_017 =
  fun e rev_cases ->
    (
# 684 "src/frontend/parser.mly"
      ( let (hs,eo) = rev_cases in
        E_match(e,List.rev hs,eo) )
# 1641 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_018 =
  fun e e1 e2 i ->
    (
# 687 "src/frontend/parser.mly"
      ( let loop = gensym ~prefix:"loop" () in
        let n0 = gensym ~prefix:"n0" () in
        let n = gensym ~prefix:"n" () in
        E_letIn(P_var n0, e1,
        E_letIn(P_var n, e2,
        E_letIn(P_var loop,E_fix(loop,(P_var i,
                              E_if(E_app(E_const(Op(Runtime(External_fun("Int.gt",new_ty_unknown ())))),E_tuple[E_var i;E_var n]),
                                   E_const(Unit),
                                   E_letIn(P_unit,e,E_app(E_var loop,
                                    E_app(E_const(Op(Runtime(External_fun("Int.add",new_ty_unknown ())))),
                                        E_tuple[E_var i;E_const (Int (1,new_size_unknown()))])))))), 
                 E_app(E_var loop,E_var n0))))
                )
# 1661 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_019 =
  fun _endpos__9_ _startpos__1_ e e_st1 e_st2 x ->
    let _endpos = _endpos__9_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 702 "src/frontend/parser.mly"
      ( E_for(x,e_st1,e_st2,e,with_file _loc) )
# 1672 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_020 =
  fun b ->
    (
# 264 "src/frontend/parser.mly"
                             ( b )
# 1680 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_021 =
  fun b ->
    (
# 266 "src/frontend/parser.mly"
        ( b )
# 1688 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_022 =
  fun e ->
    (
# 268 "src/frontend/parser.mly"
        ( e )
# 1696 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_023 =
  fun _endpos_e1_ _startpos_f_ty_opt_ e1 f_ty_opt ->
    (
# 270 "src/frontend/parser.mly"
        (
            let f,ty_opt = f_ty_opt in
            let loc_fun = with_file (_startpos_f_ty_opt_,_endpos_e1_) in
            let v = mk_fix f (ty_annot_opt ~ty:ty_opt e1) loc_fun in
            P_var f, v
        )
# 1709 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_024 =
  fun b ->
    (
# 264 "src/frontend/parser.mly"
                             ( b )
# 1717 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_025 =
  fun b ->
    (
# 266 "src/frontend/parser.mly"
        ( b )
# 1725 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_026 =
  fun e ->
    (
# 268 "src/frontend/parser.mly"
        ( e )
# 1733 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_027 =
  fun _endpos_e1_ _startpos_f_ty_opt_ e1 f_ty_opt ->
    (
# 270 "src/frontend/parser.mly"
        (
            let f,ty_opt = f_ty_opt in
            let loc_fun = with_file (_startpos_f_ty_opt_,_endpos_e1_) in
            let v = mk_fix f (ty_annot_opt ~ty:ty_opt e1) loc_fun in
            P_var f, v
        )
# 1746 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_028 =
  fun () ->
    (
# 730 "src/frontend/parser.mly"
                ( P_unit )
# 1754 "src/frontend/parser.ml"
     : (Ast.p))

let _menhir_action_029 =
  fun p ->
    (
# 731 "src/frontend/parser.mly"
                      ( p )
# 1762 "src/frontend/parser.ml"
     : (Ast.p))

let _menhir_action_030 =
  fun x ->
    (
# 732 "src/frontend/parser.mly"
          ( P_var x )
# 1770 "src/frontend/parser.ml"
     : (Ast.p))

let _menhir_action_031 =
  fun _endpos_e_ _startpos_e_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    let _loc = (_startpos, _endpos) in
    (
# 514 "src/frontend/parser.mly"
                 ( mk_loc (with_file _loc) e )
# 1781 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_032 =
  fun _endpos_e_ _startpos__1_ e sz ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 517 "src/frontend/parser.mly"
                                  ( E_array_make(sz, e, with_file _loc) )
# 1792 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_033 =
  fun _endpos__6_ _startpos__1_ sz ->
    let _endpos = _endpos__6_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 519 "src/frontend/parser.mly"
                                     ( E_array_create(sz, with_file _loc) )
# 1803 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_034 =
  fun _endpos__6_ _startpos__1_ sz ->
    let _endpos = _endpos__6_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 519 "src/frontend/parser.mly"
                                     ( E_array_create(sz, with_file _loc) )
# 1814 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_035 =
  fun e1 e2 v ->
    (
# 522 "src/frontend/parser.mly"
    ( 
        E_app(e1,E_tuple[e2;v]) 
    )
# 1824 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_036 =
  fun e1 e2 v ->
    (
# 522 "src/frontend/parser.mly"
    ( 
        E_app(e1,E_tuple[e2;v]) 
    )
# 1834 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_037 =
  fun e ex ->
    (
# 525 "src/frontend/parser.mly"
                           ( E_set(ex,e) )
# 1842 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_038 =
  fun e ->
    (
# 526 "src/frontend/parser.mly"
                        ( E_ref e )
# 1850 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_039 =
  fun _endpos_e_ _startpos__1_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 531 "src/frontend/parser.mly"
             ( match Ast_undecorated.remove_deco e with 
               | E_tuple[E_var x;e1;e2] -> E_array_set(x,e1,e2) 
               | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                   ~msg:"... array set" () )
# 1864 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_040 =
  fun e1 x ->
    (
# 537 "src/frontend/parser.mly"
   ( E_array_get(x,e1) )
# 1872 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_041 =
  fun e1 x ->
    (
# 537 "src/frontend/parser.mly"
   ( E_array_get(x,e1) )
# 1880 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_042 =
  fun e1 x ->
    (
# 538 "src/frontend/parser.mly"
                     ( E_array_get(x,e1) )
# 1888 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_043 =
  fun x ->
    (
# 539 "src/frontend/parser.mly"
                     ( E_array_length x )
# 1896 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_044 =
  fun _endpos_a_ _startpos__1_ a ->
    let _endpos = _endpos_a_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 541 "src/frontend/parser.mly"
                ( match un_annot a with
                  | E_var x -> E_array_length x 
                  | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                          ~msg:"array length: should be a variable......" () )
# 1910 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_045 =
  fun _endpos_a_ _startpos__1_ a ->
    let _endpos = _endpos_a_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 541 "src/frontend/parser.mly"
                ( match un_annot a with
                  | E_var x -> E_array_length x 
                  | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                          ~msg:"array length: should be a variable......" () )
# 1924 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_046 =
  fun e1 e2 x ->
    (
# 547 "src/frontend/parser.mly"
  ( E_array_set(x,e1,e2) )
# 1932 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_047 =
  fun e1 e2 x ->
    (
# 547 "src/frontend/parser.mly"
  ( E_array_set(x,e1,e2) )
# 1940 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_048 =
  fun n ->
    (
# 548 "src/frontend/parser.mly"
                        ( E_const(C_size n) )
# 1948 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_049 =
  fun _endpos_es_ _startpos_e_ e es ->
    let _endpos = _endpos_es_ in
    let _startpos = _startpos_e_ in
    let _loc = (_startpos, _endpos) in
    (
# 550 "src/frontend/parser.mly"
      ( match e::es with
        | [e1;e2] -> (match un_annot e1 with
                      | E_var _ | E_const _ | E_fun _ -> 
                        E_app(e1,e2)
                      | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                   ~msg:"expression in functional position should be a variable or a constante" ())
        | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                   ~msg:"All functions and primitives should be unary. Hints: use a tuple as argument" () )
# 1966 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_050 =
  fun e1 ->
    (
# 558 "src/frontend/parser.mly"
                                       ( E_app(E_const(Op(Runtime(External_fun("Int.neg",new_ty_unknown ())))),e1) )
# 1974 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_051 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 767 "src/frontend/parser.mly"
             ( External_fun("Int.add",new_ty_unknown ()) )
# 1982 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 1992 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_052 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 768 "src/frontend/parser.mly"
             ( External_fun("Int.sub",new_ty_unknown ()) )
# 2000 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2010 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_053 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 769 "src/frontend/parser.mly"
             ( External_fun("Int.mul",new_ty_unknown ()) )
# 2018 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2028 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_054 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 770 "src/frontend/parser.mly"
             ( External_fun("Int.div",new_ty_unknown ()) )
# 2036 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2046 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_055 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 771 "src/frontend/parser.mly"
             ( External_fun("Int.modulo",new_ty_unknown ()) )
# 2054 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2064 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_056 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 772 "src/frontend/parser.mly"
             ( External_fun("Int.lt",new_ty_unknown ()) )
# 2072 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2082 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_057 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 773 "src/frontend/parser.mly"
             ( External_fun("Int.gt",new_ty_unknown ()) )
# 2090 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2100 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_058 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 774 "src/frontend/parser.mly"
             ( External_fun("Int.le",new_ty_unknown ()) )
# 2108 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2118 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_059 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 775 "src/frontend/parser.mly"
             ( External_fun("Int.ge",new_ty_unknown ()) )
# 2126 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2136 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_060 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 776 "src/frontend/parser.mly"
             ( External_fun("Int.eq",new_ty_unknown ()) )
# 2144 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2154 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_061 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 776 "src/frontend/parser.mly"
             ( External_fun("Int.eq",new_ty_unknown ()) )
# 2162 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2172 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_062 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 777 "src/frontend/parser.mly"
             ( External_fun("Int.neq",new_ty_unknown ()) )
# 2180 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2190 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_063 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 778 "src/frontend/parser.mly"
             ( External_fun("Bool.land",new_ty_unknown ()) )
# 2198 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2208 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_064 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 779 "src/frontend/parser.mly"
             ( External_fun("Bool.lxor",new_ty_unknown ()) )
# 2216 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2226 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_065 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 780 "src/frontend/parser.mly"
             ( External_fun("Int.lxor",new_ty_unknown ()) )
# 2234 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2244 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_066 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 781 "src/frontend/parser.mly"
             ( External_fun("Int.land",new_ty_unknown ()) )
# 2252 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2262 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_067 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 782 "src/frontend/parser.mly"
             ( External_fun("Int.lor",new_ty_unknown ()) )
# 2270 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2280 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_068 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 783 "src/frontend/parser.mly"
             ( External_fun("Int.lsl",new_ty_unknown ()) )
# 2288 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2298 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_069 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 784 "src/frontend/parser.mly"
             ( External_fun("Int.lsr",new_ty_unknown ()) )
# 2306 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2316 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_070 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 785 "src/frontend/parser.mly"
             ( External_fun("Int.asr",new_ty_unknown ()) )
# 2324 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 560 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2334 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_071 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 564 "src/frontend/parser.mly"
        ( let e3 = mk_loc (with_file _loc) @@ E_const (Bool false) in
          E_if(e1,e2,e3)
        )
# 2347 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_072 =
  fun _endpos_e3_ _startpos_e1_ e1 e3 ->
    let _endpos = _endpos_e3_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 568 "src/frontend/parser.mly"
        ( let e2 = mk_loc (with_file _loc) @@ E_const (Bool true) in
          E_if(e1,e2,e3)
        )
# 2360 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_073 =
  fun _endpos_e0_ _startpos__1_ e0 ev ->
    let _endpos = _endpos_e0_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 572 "src/frontend/parser.mly"
       ( match un_annot ev with
         | E_fun(p,e1) -> E_reg((p,e1),e0,Ast.gensym ())
         | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                               ~msg:"This expression should be a function" ()
       )
# 2375 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_074 =
  fun e0 f ->
    (
# 578 "src/frontend/parser.mly"
       (
        let y = gensym () in
         E_reg((P_var y,E_app(E_var f,E_var y)),e0,Ast.gensym ())
       )
# 2386 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_075 =
  fun e1 e2 ->
    (
# 583 "src/frontend/parser.mly"
       ( E_exec(e1,e2,None,"") )
# 2394 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_076 =
  fun e1 e2 e3 ->
    (
# 585 "src/frontend/parser.mly"
       ( E_exec(e1,e2,Some e3,"") )
# 2402 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_077 =
  fun _endpos_e_st3_ _startpos__1_ e_init2 e_st3 ef1 ->
    let _endpos = _endpos_e_st3_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 587 "src/frontend/parser.mly"
  ( let z = Ast.gensym () in
    E_generate((P_var z,E_app(ef1,E_var z)),e_init2,e_st3,with_file _loc) )
# 2414 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_078 =
  fun _endpos_e1_ _startpos__1_ e1 ->
    let _endpos = _endpos_e1_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 591 "src/frontend/parser.mly"
    (
        Prelude.Errors.raise_error ~loc:(with_file _loc)
            ~msg:"missing ``default'' close; `exec e default e` expected" ()
    )
# 2428 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_079 =
  fun _endpos__3_ _startpos__1_ e1 ->
    let _endpos = _endpos__3_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 596 "src/frontend/parser.mly"
    (
        Prelude.Errors.raise_error ~loc:(with_file _loc)
            ~msg:"missing expression after keyword ``default''; `exec e default e` expected" ()
    )
# 2442 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_080 =
  fun e i ->
    (
# 616 "src/frontend/parser.mly"
     ( E_run(i, e) )
# 2450 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_081 =
  fun e ->
    (
# 620 "src/frontend/parser.mly"
         ( e )
# 2458 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_082 =
  fun a ->
    (
# 443 "src/frontend/parser.mly"
                                 ( a )
# 2466 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_083 =
  fun a ->
    (
# 443 "src/frontend/parser.mly"
                                 ( a )
# 2474 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_084 =
  fun p ->
    (
# 446 "src/frontend/parser.mly"
                      ( p, None )
# 2482 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_085 =
  fun p ty ->
    (
# 447 "src/frontend/parser.mly"
                                 (p, Some ty)
# 2490 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_086 =
  fun p ->
    (
# 448 "src/frontend/parser.mly"
         ( p, None )
# 2498 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_087 =
  fun p ->
    (
# 438 "src/frontend/parser.mly"
        ( p, None )
# 2506 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_088 =
  fun p ty ->
    (
# 439 "src/frontend/parser.mly"
                     (p, Some ty)
# 2514 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_089 =
  fun ty ->
    (
# 396 "src/frontend/parser.mly"
      ( ty )
# 2522 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_090 =
  fun e ->
    (
# 415 "src/frontend/parser.mly"
                        ( e )
# 2530 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_091 =
  fun c ->
    (
# 416 "src/frontend/parser.mly"
          ( E_const c )
# 2538 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_092 =
  fun e p_ty_opt ->
    (
# 506 "src/frontend/parser.mly"
        (
            let p,ty_opt = p_ty_opt in
            p,ty_annot_opt ~ty:ty_opt e
        )
# 2549 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_093 =
  fun e p ->
    (
# 511 "src/frontend/parser.mly"
    ( p,e )
# 2557 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_094 =
  fun w ->
    (
# 490 "src/frontend/parser.mly"
  ( match w with
    | [],_ | _,[] -> assert false
    | [p],[e] -> (p,e)
    | ps,es -> (P_tuple ps, E_par es) )
# 2568 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_095 =
  fun b ->
    (
# 496 "src/frontend/parser.mly"
                 ( let (p,e) = b in ([p],[e]) )
# 2576 "src/frontend/parser.ml"
     : (Ast.p list * Ast.e_static list))

let _menhir_action_096 =
  fun b1 bs ->
    (
# 498 "src/frontend/parser.mly"
   ( let (p1,e1) = b1 in
     let (ps,es) = bs in
     (p1::ps,e1::es) )
# 2586 "src/frontend/parser.ml"
     : (Ast.p list * Ast.e_static list))

let _menhir_action_097 =
  fun () ->
    (
# 741 "src/frontend/parser.mly"
                ( Unit )
# 2594 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_098 =
  fun b ->
    (
# 742 "src/frontend/parser.mly"
                ( Bool b )
# 2602 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_099 =
  fun n ->
    (
# 743 "src/frontend/parser.mly"
            (
    Int (n,new_size_unknown()) )
# 2611 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_100 =
  fun _endpos_k_ _startpos_n_ k n ->
    let _endpos = _endpos_k_ in
    let _startpos = _startpos_n_ in
    let _loc = (_startpos, _endpos) in
    (
# 746 "src/frontend/parser.mly"
    ( if Float.log2 (float n) >= float (k-1) then
       Prelude.Errors.raise_error ~loc:(with_file _loc)
          ~msg:("Integer literal "^
                string_of_int n^
                " exceeds the range of representable integers of type int<"^
                string_of_int k ^">") ()
      else Int (n,Sz_lit k) )
# 2628 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_101 =
  fun s ->
    (
# 753 "src/frontend/parser.mly"
                         ( String s )
# 2636 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_102 =
  fun x ->
    (
# 754 "src/frontend/parser.mly"
                   ( Op(Runtime(External_fun(x,new_ty_unknown()))) )
# 2644 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_103 =
  fun x ->
    (
# 755 "src/frontend/parser.mly"
                         ( Inj x )
# 2652 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_104 =
  fun () ->
    let op = 
# 767 "src/frontend/parser.mly"
             ( External_fun("Int.add",new_ty_unknown ()) )
# 2660 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2665 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_105 =
  fun () ->
    let op = 
# 768 "src/frontend/parser.mly"
             ( External_fun("Int.sub",new_ty_unknown ()) )
# 2673 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2678 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_106 =
  fun () ->
    let op = 
# 769 "src/frontend/parser.mly"
             ( External_fun("Int.mul",new_ty_unknown ()) )
# 2686 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2691 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_107 =
  fun () ->
    let op = 
# 770 "src/frontend/parser.mly"
             ( External_fun("Int.div",new_ty_unknown ()) )
# 2699 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2704 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_108 =
  fun () ->
    let op = 
# 771 "src/frontend/parser.mly"
             ( External_fun("Int.modulo",new_ty_unknown ()) )
# 2712 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2717 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_109 =
  fun () ->
    let op = 
# 772 "src/frontend/parser.mly"
             ( External_fun("Int.lt",new_ty_unknown ()) )
# 2725 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2730 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_110 =
  fun () ->
    let op = 
# 773 "src/frontend/parser.mly"
             ( External_fun("Int.gt",new_ty_unknown ()) )
# 2738 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2743 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_111 =
  fun () ->
    let op = 
# 774 "src/frontend/parser.mly"
             ( External_fun("Int.le",new_ty_unknown ()) )
# 2751 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2756 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_112 =
  fun () ->
    let op = 
# 775 "src/frontend/parser.mly"
             ( External_fun("Int.ge",new_ty_unknown ()) )
# 2764 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2769 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_113 =
  fun () ->
    let op = 
# 776 "src/frontend/parser.mly"
             ( External_fun("Int.eq",new_ty_unknown ()) )
# 2777 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2782 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_114 =
  fun () ->
    let op = 
# 776 "src/frontend/parser.mly"
             ( External_fun("Int.eq",new_ty_unknown ()) )
# 2790 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2795 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_115 =
  fun () ->
    let op = 
# 777 "src/frontend/parser.mly"
             ( External_fun("Int.neq",new_ty_unknown ()) )
# 2803 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2808 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_116 =
  fun () ->
    let op = 
# 778 "src/frontend/parser.mly"
             ( External_fun("Bool.land",new_ty_unknown ()) )
# 2816 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2821 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_117 =
  fun () ->
    let op = 
# 779 "src/frontend/parser.mly"
             ( External_fun("Bool.lxor",new_ty_unknown ()) )
# 2829 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2834 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_118 =
  fun () ->
    let op = 
# 780 "src/frontend/parser.mly"
             ( External_fun("Int.lxor",new_ty_unknown ()) )
# 2842 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2847 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_119 =
  fun () ->
    let op = 
# 781 "src/frontend/parser.mly"
             ( External_fun("Int.land",new_ty_unknown ()) )
# 2855 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2860 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_120 =
  fun () ->
    let op = 
# 782 "src/frontend/parser.mly"
             ( External_fun("Int.lor",new_ty_unknown ()) )
# 2868 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2873 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_121 =
  fun () ->
    let op = 
# 783 "src/frontend/parser.mly"
             ( External_fun("Int.lsl",new_ty_unknown ()) )
# 2881 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2886 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_122 =
  fun () ->
    let op = 
# 784 "src/frontend/parser.mly"
             ( External_fun("Int.lsr",new_ty_unknown ()) )
# 2894 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2899 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_123 =
  fun () ->
    let op = 
# 785 "src/frontend/parser.mly"
             ( External_fun("Int.asr",new_ty_unknown ()) )
# 2907 "src/frontend/parser.ml"
     in
    (
# 756 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2912 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_124 =
  fun cs ->
    (
# 758 "src/frontend/parser.mly"
    ( C_vector cs )
# 2920 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_125 =
  fun d ->
    (
# 203 "src/frontend/parser.mly"
  ( clear_tyvar_constraints();
    d )
# 2929 "src/frontend/parser.ml"
     : ((Ast.p * Ast.e_static) * Prelude.loc))

let _menhir_action_126 =
  fun _endpos_b_ _startpos__1_ b ->
    let _endpos = _endpos_b_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 208 "src/frontend/parser.mly"
        ( b,(with_file _loc) )
# 2940 "src/frontend/parser.ml"
     : ((Ast.p * Ast.e_static) * Prelude.loc))

let _menhir_action_127 =
  fun _endpos_b_ _startpos__1_ b ->
    let _endpos = _endpos_b_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 210 "src/frontend/parser.mly"
        ( enforce_node b,(with_file _loc) )
# 2951 "src/frontend/parser.ml"
     : ((Ast.p * Ast.e_static) * Prelude.loc))

let _menhir_action_128 =
  fun _endpos__2_ _startpos_e_ e ->
    let _endpos = _endpos__2_ in
    let _startpos = _startpos_e_ in
    let _loc = (_startpos, _endpos) in
    (
# 212 "src/frontend/parser.mly"
                  ( ((P_var "_", e),(with_file _loc))  )
# 2962 "src/frontend/parser.ml"
     : ((Ast.p * Ast.e_static) * Prelude.loc))

let _menhir_action_129 =
  fun e ->
    (
# 624 "src/frontend/parser.mly"
                        ( e )
# 2970 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_130 =
  fun _endpos_e_ _startpos_e_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    let _loc = (_startpos, _endpos) in
    (
# 421 "src/frontend/parser.mly"
             ( mk_loc (with_file _loc) e )
# 2981 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_131 =
  fun e1 e2 ->
    (
# 427 "src/frontend/parser.mly"
        (
            E_letIn(P_unit,e1,e2)
        )
# 2991 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_132 =
  fun e es ->
    (
# 431 "src/frontend/parser.mly"
        (
            E_tuple (e::es)
        )
# 3001 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_133 =
  fun e ->
    (
# 435 "src/frontend/parser.mly"
         ( e )
# 3009 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_134 =
  fun e ->
    (
# 199 "src/frontend/parser.mly"
            (e)
# 3017 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_135 =
  fun t x ->
    (
# 163 "src/frontend/parser.mly"
                                      ( (x, (t, false)) )
# 3025 "src/frontend/parser.ml"
     : (Ast.x * (Types.ty * bool)))

let _menhir_action_136 =
  fun t x ->
    (
# 164 "src/frontend/parser.mly"
                                             ( (x, (t, true)) )
# 3033 "src/frontend/parser.ml"
     : (Ast.x * (Types.ty * bool)))

let _menhir_action_137 =
  fun t x ->
    let imp = 
# 159 "src/frontend/parser.mly"
            ( true )
# 3041 "src/frontend/parser.ml"
     in
    (
# 152 "src/frontend/parser.mly"
   ( Hashtbl.clear hash_size_tvar;
     (x, (t,(false,get_arity t,imp))) )
# 3047 "src/frontend/parser.ml"
     : (Ast.x * (Types.ty * (bool * int * bool))))

let _menhir_action_138 =
  fun t x ->
    let imp = 
# 160 "src/frontend/parser.mly"
  (false)
# 3055 "src/frontend/parser.ml"
     in
    (
# 152 "src/frontend/parser.mly"
   ( Hashtbl.clear hash_size_tvar;
     (x, (t,(false,get_arity t,imp))) )
# 3061 "src/frontend/parser.ml"
     : (Ast.x * (Types.ty * (bool * int * bool))))

let _menhir_action_139 =
  fun t x ->
    let imp = 
# 159 "src/frontend/parser.mly"
            ( true )
# 3069 "src/frontend/parser.ml"
     in
    (
# 155 "src/frontend/parser.mly"
   ( Hashtbl.clear hash_size_tvar;
     (x, (t,(true,get_arity t,imp))) )
# 3075 "src/frontend/parser.ml"
     : (Ast.x * (Types.ty * (bool * int * bool))))

let _menhir_action_140 =
  fun t x ->
    let imp = 
# 160 "src/frontend/parser.mly"
  (false)
# 3083 "src/frontend/parser.ml"
     in
    (
# 155 "src/frontend/parser.mly"
   ( Hashtbl.clear hash_size_tvar;
     (x, (t,(true,get_arity t,imp))) )
# 3089 "src/frontend/parser.ml"
     : (Ast.x * (Types.ty * (bool * int * bool))))

let _menhir_action_141 =
  fun _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt_ret ->
    (
# 254 "src/frontend/parser.mly"
        (
            let ef = mk_let_fun ~loc:(with_file (_startpos_f_,_endpos_e1_))
                                ~p_ty_opt
                                ~ty_opt_ret
                        e1
            in
            (P_var f,ef)
        )
# 3104 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_142 =
  fun _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt_ret ->
    (
# 254 "src/frontend/parser.mly"
        (
            let ef = mk_let_fun ~loc:(with_file (_startpos_f_,_endpos_e1_))
                                ~p_ty_opt
                                ~ty_opt_ret
                        e1
            in
            (P_var f,ef)
        )
# 3119 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_143 =
  fun _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt ->
    (
# 278 "src/frontend/parser.mly"
        (
            let p_ty_opt_f =
              let open Types in
              match p_ty_opt with
              | p,None -> p,None
              | p,Some t -> p,Some (Ty_fun(t,new_dur_unknown(),new_tyB_unknown()))
            in
            let loc_fun = with_file (_startpos_f_,_endpos_e1_) in
            let (p,ty_f_opt) = p_ty_opt_f in
            let ef = mk_fun_ty_annot p ty_f_opt (ty_annot_opt ~ty:ty_opt e1)
                   |> mk_loc loc_fun in
            let v = mk_fix f ef loc_fun in
            (*match o with 
            | None -> *)P_var f, v
            (* | Some _ -> P_var f, E_letIn(P_var f,v,e1)*)
        )
# 3142 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_144 =
  fun _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt ->
    (
# 278 "src/frontend/parser.mly"
        (
            let p_ty_opt_f =
              let open Types in
              match p_ty_opt with
              | p,None -> p,None
              | p,Some t -> p,Some (Ty_fun(t,new_dur_unknown(),new_tyB_unknown()))
            in
            let loc_fun = with_file (_startpos_f_,_endpos_e1_) in
            let (p,ty_f_opt) = p_ty_opt_f in
            let ef = mk_fun_ty_annot p ty_f_opt (ty_annot_opt ~ty:ty_opt e1)
                   |> mk_loc loc_fun in
            let v = mk_fix f ef loc_fun in
            (*match o with 
            | None -> *)P_var f, v
            (* | Some _ -> P_var f, E_letIn(P_var f,v,e1)*)
        )
# 3165 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_145 =
  fun e2 ->
    (
# 481 "src/frontend/parser.mly"
          ( e2,E_const Unit )
# 3173 "src/frontend/parser.ml"
     : (Ast.e_static * Ast.e_static))

let _menhir_action_146 =
  fun e2 e3 ->
    (
# 482 "src/frontend/parser.mly"
                       ( e2, e3 )
# 3181 "src/frontend/parser.ml"
     : (Ast.e_static * Ast.e_static))

let _menhir_action_147 =
  fun _endpos_e_ _startpos_e_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    let _loc = (_startpos, _endpos) in
    (
# 452 "src/frontend/parser.mly"
              ( mk_loc (with_file _loc) e )
# 3192 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_148 =
  fun e ->
    (
# 455 "src/frontend/parser.mly"
            ( e )
# 3200 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_149 =
  fun _endpos_e_ _startpos__1_ e f ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 457 "src/frontend/parser.mly"
        ( mk_fix f e (with_file _loc) )
# 3211 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_150 =
  fun e p_ty_opt ->
    (
# 459 "src/frontend/parser.mly"
        ( let (p,ty_p_opt) = p_ty_opt in
          mk_fun_ty_annot_p p ty_p_opt e )
# 3220 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_151 =
  fun e1 e2_e3 ->
    (
# 462 "src/frontend/parser.mly"
        ( let (e2,e3) = e2_e3 in E_if(e1,e2,e3) )
# 3228 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_152 =
  fun b e2 ->
    (
# 464 "src/frontend/parser.mly"
        ( let (p,e1) = b in
          E_letIn(p,e1,e2) )
# 3237 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_153 =
  fun b e2 ->
    (
# 468 "src/frontend/parser.mly"
        ( let (p,e1) = enforce_node b in
          E_letIn(p,e1,e2) )
# 3246 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_154 =
  fun e1 es ->
    (
# 476 "src/frontend/parser.mly"
        (
            E_par(e1::es)
        )
# 3256 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_155 =
  fun e1 es ->
    (
# 476 "src/frontend/parser.mly"
        (
            E_par(e1::es)
        )
# 3266 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_156 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 3274 "src/frontend/parser.ml"
     : ((Ast.c list * Ast.e_static) list))

let _menhir_action_157 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 3282 "src/frontend/parser.ml"
     : ((Ast.c list * Ast.e_static) list))

let _menhir_action_158 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 3290 "src/frontend/parser.ml"
     : (Ast.e list))

let _menhir_action_159 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 3298 "src/frontend/parser.ml"
     : (Ast.e list))

let _menhir_action_160 =
  fun v ->
    (
# 407 "src/frontend/parser.mly"
           ( v )
# 3306 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_161 =
  fun e p_ty_opt ->
    (
# 409 "src/frontend/parser.mly"
      (
        let p,ty_opt = p_ty_opt in
        mk_fun_ty_annot p ty_opt e
    )
# 3317 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_162 =
  fun e p x ->
    (
# 714 "src/frontend/parser.mly"
                                      ( (x,(p,e)) )
# 3325 "src/frontend/parser.ml"
     : (Ast.x * (Ast.p * Ast.e_static)))

let _menhir_action_163 =
  fun cs e ->
    (
# 706 "src/frontend/parser.mly"
                                                                ( (cs,e) )
# 3333 "src/frontend/parser.ml"
     : (Ast.c list * Ast.e_static))

let _menhir_action_164 =
  fun e ->
    (
# 709 "src/frontend/parser.mly"
                                          ( [],Some e )
# 3341 "src/frontend/parser.ml"
     : ((Ast.x * (Ast.p * Ast.e_static)) list * Ast.e_static option))

let _menhir_action_165 =
  fun h ->
    (
# 710 "src/frontend/parser.mly"
                                          ( [h],None )
# 3349 "src/frontend/parser.ml"
     : ((Ast.x * (Ast.p * Ast.e_static)) list * Ast.e_static option))

let _menhir_action_166 =
  fun h rev_cases ->
    (
# 711 "src/frontend/parser.mly"
                                          ( let (hs,eo) = rev_cases in h::hs,eo )
# 3357 "src/frontend/parser.ml"
     : ((Ast.x * (Ast.p * Ast.e_static)) list * Ast.e_static option))

let _menhir_action_167 =
  fun x ->
    (
# 228 "<standard.mly>"
    ( [ x ] )
# 3365 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_168 =
  fun x xs ->
    (
# 231 "<standard.mly>"
    ( x :: xs )
# 3373 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_169 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 3381 "src/frontend/parser.ml"
     : (unit option))

let _menhir_action_170 =
  fun x ->
    (
# 114 "<standard.mly>"
    ( Some x )
# 3389 "src/frontend/parser.ml"
     : (unit option))

let _menhir_action_171 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 3397 "src/frontend/parser.ml"
     : (unit option))

let _menhir_action_172 =
  fun x ->
    (
# 114 "<standard.mly>"
    ( Some x )
# 3405 "src/frontend/parser.ml"
     : (unit option))

let _menhir_action_173 =
  fun p ->
    (
# 725 "src/frontend/parser.mly"
         ( p )
# 3413 "src/frontend/parser.ml"
     : (Ast.p))

let _menhir_action_174 =
  fun p ps ->
    (
# 727 "src/frontend/parser.mly"
  ( P_tuple (p::ps) )
# 3421 "src/frontend/parser.ml"
     : (Ast.p))

let _menhir_action_175 =
  fun _endpos_pi_ _startpos_ec_ ec pi ->
    let _endpos = _endpos_pi_ in
    let _startpos = _startpos_ec_ in
    let _loc = (_startpos, _endpos) in
    (
# 133 "src/frontend/parser.mly"
                      ( let (ecs,efs),gs,ts,ds = pi in
                        let (x,t) = ec in
                        let f = String.capitalize_ascii x in
                        let arg = gensym ~prefix:"arg" () in
                        let v = E_fun(P_var arg, E_run(f,E_var arg)) in
                        (((f,t)::ecs,efs), gs,    ts,    (((P_var x,v),with_file _loc)::ds)) )
# 3437 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_176 =
  fun ef pi ->
    (
# 139 "src/frontend/parser.mly"
                      ( let (ecs,efs),gs,ts,ds = pi in ((ecs,ef::efs), gs,    ts,    ds   ) )
# 3448 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_177 =
  fun g pi ->
    (
# 140 "src/frontend/parser.mly"
                      ( let (ecs,efs),gs,ts,ds = pi in ((ecs,    efs), g::gs, ts,    ds   ) )
# 3459 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_178 =
  fun d pi ->
    (
# 141 "src/frontend/parser.mly"
                      ( let (ecs,efs),gs,ts,ds = pi in ((ecs,    efs), gs,    d::ts, ds   ) )
# 3470 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_179 =
  fun d pi ->
    (
# 142 "src/frontend/parser.mly"
                      ( let (ecs,efs),gs,ts,ds = pi in ((ecs,    efs), gs,    ts,    d::ds) )
# 3481 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_180 =
  fun pi ->
    (
# 143 "src/frontend/parser.mly"
                      ( pi )
# 3492 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_181 =
  fun () ->
    (
# 144 "src/frontend/parser.mly"
                      ( ([],[]),[],[],[] )
# 3503 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_182 =
  fun () ->
    (
# 485 "src/frontend/parser.mly"
     ( None )
# 3514 "src/frontend/parser.ml"
     : (Types.ty option))

let _menhir_action_183 =
  fun ty ->
    (
# 486 "src/frontend/parser.mly"
               ( Some ty )
# 3522 "src/frontend/parser.ml"
     : (Types.ty option))

let _menhir_action_184 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3530 "src/frontend/parser.ml"
     : (int list))

let _menhir_action_185 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3538 "src/frontend/parser.ml"
     : (int list))

let _menhir_action_186 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3546 "src/frontend/parser.ml"
     : (Ast.p list))

let _menhir_action_187 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3554 "src/frontend/parser.ml"
     : (Ast.p list))

let _menhir_action_188 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3562 "src/frontend/parser.ml"
     : (Ast.e list))

let _menhir_action_189 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3570 "src/frontend/parser.ml"
     : (Ast.e list))

let _menhir_action_190 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3578 "src/frontend/parser.ml"
     : (Ast.c list))

let _menhir_action_191 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3586 "src/frontend/parser.ml"
     : (Ast.c list))

let _menhir_action_192 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3594 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_193 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3602 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_194 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3610 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_195 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3618 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_196 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3626 "src/frontend/parser.ml"
     : (Types.size list))

let _menhir_action_197 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3634 "src/frontend/parser.ml"
     : (Types.size list))

let _menhir_action_198 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3642 "src/frontend/parser.ml"
     : (Types.tyB list))

let _menhir_action_199 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3650 "src/frontend/parser.ml"
     : (Types.tyB list))

let _menhir_action_200 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3658 "src/frontend/parser.ml"
     : (Ast.c list))

let _menhir_action_201 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3666 "src/frontend/parser.ml"
     : (Ast.c list))

let _menhir_action_202 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3674 "src/frontend/parser.ml"
     : ((Ast.x * Types.tyB) list))

let _menhir_action_203 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3682 "src/frontend/parser.ml"
     : ((Ast.x * Types.tyB) list))

let _menhir_action_204 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3690 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_205 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3698 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_206 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3706 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_207 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3714 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_208 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3722 "src/frontend/parser.ml"
     : (Types.tyB list))

let _menhir_action_209 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3730 "src/frontend/parser.ml"
     : (Types.tyB list))

let _menhir_action_210 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3738 "src/frontend/parser.ml"
     : (Types.ty list))

let _menhir_action_211 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3746 "src/frontend/parser.ml"
     : (Types.ty list))

let _menhir_action_212 =
  fun n ->
    (
# 390 "src/frontend/parser.mly"
            ( Sz_lit n )
# 3754 "src/frontend/parser.ml"
     : (Types.size))

let _menhir_action_213 =
  fun x ->
    (
# 391 "src/frontend/parser.mly"
               (
    decl_size_var x
  )
# 3764 "src/frontend/parser.ml"
     : (Types.size))

let _menhir_action_214 =
  fun sz ->
    (
# 355 "src/frontend/parser.mly"
          ( [sz] )
# 3772 "src/frontend/parser.ml"
     : (Types.size list))

let _menhir_action_215 =
  fun szs ->
    (
# 356 "src/frontend/parser.mly"
                                                ( szs )
# 3780 "src/frontend/parser.ml"
     : (Types.size list))

let _menhir_action_216 =
  fun _endpos__7_ _startpos__1_ e ec x ->
    let _endpos = _endpos__7_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 169 "src/frontend/parser.mly"
    ( let to_int e =
        match un_annot e with
        | E_const (Int(n,_)) -> n
        | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                          ~msg:("dimension for "^x^" should be an integer") ()
      in
      x,Static_array(e2c ec,to_int e)
  )
# 3798 "src/frontend/parser.ml"
     : (Ast.x * Ast.static))

let _menhir_action_217 =
  fun _endpos__6_ _startpos__1_ ty x ->
    let _endpos = _endpos__6_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 179 "src/frontend/parser.mly"
    (
      let loc = with_file _loc in
      if Types.no_unknown_in_ty ty then (
         Prelude.Errors.raise_error ~loc
           ~msg:"this type annotation should not contain type unknowns"
      ) ();
      x,Static_array_of (ty,loc)
    )
# 3816 "src/frontend/parser.ml"
     : (Ast.x * Ast.static))

let _menhir_action_218 =
  fun e ->
    (
# 192 "src/frontend/parser.mly"
                           ( e )
# 3824 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_219 =
  fun ty tys ->
    (
# 365 "src/frontend/parser.mly"
    ( Ty_tuple (ty::tys) )
# 3832 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_220 =
  fun ty ->
    (
# 366 "src/frontend/parser.mly"
             (ty)
# 3840 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_221 =
  fun sz ->
    (
# 323 "src/frontend/parser.mly"
                ( TyB_size(sz) )
# 3848 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_222 =
  fun tyB tyBs ->
    (
# 325 "src/frontend/parser.mly"
    ( TyB_tuple (tyB::tyBs) )
# 3856 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_223 =
  fun tyB ->
    (
# 326 "src/frontend/parser.mly"
               (tyB)
# 3864 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_224 =
  fun x ->
    (
# 307 "src/frontend/parser.mly"
          ( match x with
            | "unit" -> TyB_unit
            | "bool" -> TyB_bool
            | "int" -> TyB_int(Sz_lit 32)
            | "string" -> TyB_string (new_size_unknown()) 
            | s -> 
                (match Hashtbl.find_opt alias_types s with
                    | Some (t,_) -> t
                    | None ->  TyB_abstract(x,[Sz_lit 1],[])) (* Prelude.Errors.raise_error ~loc:(with_file $loc)
                              ~msg:("unbound type constructor "^s) ()) *) )
# 3881 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_225 =
  fun x ->
    (
# 329 "src/frontend/parser.mly"
                  ( decl_tyB_var x )
# 3889 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_226 =
  fun tyB ->
    (
# 330 "src/frontend/parser.mly"
                ( tyB )
# 3897 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_227 =
  fun tyB ->
    (
# 331 "src/frontend/parser.mly"
                        ( tyB )
# 3905 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_228 =
  fun _endpos_szs_ _startpos_x_ szs x ->
    let _endpos = _endpos_szs_ in
    let _startpos = _startpos_x_ in
    let _loc = (_startpos, _endpos) in
    (
# 333 "src/frontend/parser.mly"
     ( match x with
       | "int" -> (match szs with 
                   | sz::[] -> 
                      TyB_int(sz)
                   | _ -> assert false) 
       | ("array" (* | "vect"*)) -> 
            Prelude.Errors.raise_error ~loc:(with_file _loc)
                 ~msg:("type parameter expected for type constructor "^x) ()
       | _ -> TyB_abstract(x,szs,[]) )
# 3924 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_229 =
  fun _endpos_szs_ _startpos_tyB_ szs tyB x ->
    let tyBs = 
# 359 "src/frontend/parser.mly"
               ( [tyB] )
# 3932 "src/frontend/parser.ml"
     in
    let _startpos_tyBs_ = _startpos_tyB_ in
    let _endpos = _endpos_szs_ in
    let _startpos = _startpos_tyBs_ in
    let _loc = (_startpos, _endpos) in
    (
# 343 "src/frontend/parser.mly"
     ( match x with
       | "array" -> Prelude.Errors.raise_error ~loc:(with_file _loc) ()
                 ~msg:(x^" is not a basic type constructor")
       (* | "vect" -> (match szs,tyBs with 
                   | sz::[],tyB::[] -> 
                      TyB_vector(sz,tyB)
                   | _ -> assert false)*)
       | x -> TyB_abstract(x,szs,tyBs)
       (* Prelude.Errors.raise_error ~loc:(with_file $loc) ()
                 ~msg:("unknown type constructor "^x) *) )
# 3950 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_230 =
  fun _endpos_szs_ _startpos__1_ szs tyBs x ->
    let tyBs = 
# 360 "src/frontend/parser.mly"
                                                        ( tyBs )
# 3958 "src/frontend/parser.ml"
     in
    let _startpos_tyBs_ = _startpos__1_ in
    let _endpos = _endpos_szs_ in
    let _startpos = _startpos_tyBs_ in
    let _loc = (_startpos, _endpos) in
    (
# 343 "src/frontend/parser.mly"
     ( match x with
       | "array" -> Prelude.Errors.raise_error ~loc:(with_file _loc) ()
                 ~msg:(x^" is not a basic type constructor")
       (* | "vect" -> (match szs,tyBs with 
                   | sz::[],tyB::[] -> 
                      TyB_vector(sz,tyB)
                   | _ -> assert false)*)
       | x -> TyB_abstract(x,szs,tyBs)
       (* Prelude.Errors.raise_error ~loc:(with_file $loc) ()
                 ~msg:("unknown type constructor "^x) *) )
# 3976 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_231 =
  fun x ->
    (
# 300 "src/frontend/parser.mly"
        ( x,None )
# 3984 "src/frontend/parser.ml"
     : (Types.x * Types.ty option))

let _menhir_action_232 =
  fun ty x ->
    (
# 302 "src/frontend/parser.mly"
        ( x,Some ty )
# 3992 "src/frontend/parser.ml"
     : (Types.x * Types.ty option))

let _menhir_action_233 =
  fun x_ty_opt ->
    (
# 304 "src/frontend/parser.mly"
        ( x_ty_opt )
# 4000 "src/frontend/parser.ml"
     : (Types.x * Types.ty option))

let _menhir_action_234 =
  fun x ->
    (
# 300 "src/frontend/parser.mly"
        ( x,None )
# 4008 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_235 =
  fun ty x ->
    (
# 302 "src/frontend/parser.mly"
        ( x,Some ty )
# 4016 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_236 =
  fun x_ty_opt ->
    (
# 304 "src/frontend/parser.mly"
        ( x_ty_opt )
# 4024 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_237 =
  fun tyB x ->
    (
# 248 "src/frontend/parser.mly"
                        ( x,tyB )
# 4032 "src/frontend/parser.ml"
     : (Ast.x * Types.tyB))

let _menhir_action_238 =
  fun x ->
    (
# 370 "src/frontend/parser.mly"
               ( decl_ty_var x )
# 4040 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_239 =
  fun _endpos__5_ _startpos_ty_tyB_ sz ty_tyB ->
    let _endpos = _endpos__5_ in
    let _startpos = _startpos_ty_tyB_ in
    let _loc = (_startpos, _endpos) in
    (
# 372 "src/frontend/parser.mly"
    ( let tyB = as_tyB ~loc:(with_file _loc) ty_tyB in
      Ty_array(sz,tyB) )
# 4052 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_240 =
  fun ty tyB ->
    (
# 383 "src/frontend/parser.mly"
                                 ( Ty_fun(ty,Dur_one,tyB) )
# 4060 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_241 =
  fun ty tyB ->
    (
# 384 "src/frontend/parser.mly"
                                               ( Ty_fun(ty,new_dur_unknown(),tyB) )
# 4068 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_242 =
  fun ty tyB ->
    (
# 385 "src/frontend/parser.mly"
                           ( Ty_fun(ty,Dur_zero,tyB) )
# 4076 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_243 =
  fun tyB ->
    (
# 386 "src/frontend/parser.mly"
               ( Ty_base tyB )
# 4084 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_244 =
  fun ty ->
    (
# 387 "src/frontend/parser.mly"
                      ( ty )
# 4092 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_245 =
  fun _endpos__5_ _startpos__1_ ts x ->
    let _endpos = _endpos__5_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 243 "src/frontend/parser.mly"
   ( add_alias x (TyB_sum ts) (with_file _loc);
     clear_tyvar_constraints();
     x,ts )
# 4105 "src/frontend/parser.ml"
     : (Ast.x * (Ast.x * Types.tyB) list))

let _menhir_action_246 =
  fun _endpos__5_ _startpos__1_ tyB x ->
    let _endpos = _endpos__5_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 218 "src/frontend/parser.mly"
                                     ( add_alias x tyB (with_file _loc) )
# 4116 "src/frontend/parser.ml"
     : (unit))

let _menhir_action_247 =
  fun _endpos__1_inlined1_ _startpos__1_ tyB ->
    let r = 
# 235 "src/frontend/parser.mly"
             ( None )
# 4124 "src/frontend/parser.ml"
     in
    let _endpos_r_ = _endpos__1_inlined1_ in
    let _endpos = _endpos_r_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 220 "src/frontend/parser.mly"
    ( 
      let x,szs = (match tyB with
                  | TyB_abstract(x,szs,_) -> x,szs
                      (* take the type declaration into account *)
                  | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc) 
                           ~msg:"type definition expected" ()
                  ) in
      match r with
      | None -> ()
      | Some (op,intl) ->
          Hashtbl.add Ast.typ_decl_abstract x (op,szs,intl)
   ;
   clear_tyvar_constraints () 
   )
# 4146 "src/frontend/parser.ml"
     : (unit))

let _menhir_action_248 =
  fun _endpos__3_ _startpos__1_ op tyB ->
    let r = 
# 236 "src/frontend/parser.mly"
                         ( Some (op,[]) )
# 4154 "src/frontend/parser.ml"
     in
    let _endpos_r_ = _endpos__3_ in
    let _endpos = _endpos_r_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 220 "src/frontend/parser.mly"
    ( 
      let x,szs = (match tyB with
                  | TyB_abstract(x,szs,_) -> x,szs
                      (* take the type declaration into account *)
                  | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc) 
                           ~msg:"type definition expected" ()
                  ) in
      match r with
      | None -> ()
      | Some (op,intl) ->
          Hashtbl.add Ast.typ_decl_abstract x (op,szs,intl)
   ;
   clear_tyvar_constraints () 
   )
# 4176 "src/frontend/parser.ml"
     : (unit))

let _menhir_action_249 =
  fun _endpos__5_ _startpos__1_ intl op tyB ->
    let r = 
# 238 "src/frontend/parser.mly"
   ( Some (op,intl) )
# 4184 "src/frontend/parser.ml"
     in
    let _endpos_r_ = _endpos__5_ in
    let _endpos = _endpos_r_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 220 "src/frontend/parser.mly"
    ( 
      let x,szs = (match tyB with
                  | TyB_abstract(x,szs,_) -> x,szs
                      (* take the type declaration into account *)
                  | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc) 
                           ~msg:"type definition expected" ()
                  ) in
      match r with
      | None -> ()
      | Some (op,intl) ->
          Hashtbl.add Ast.typ_decl_abstract x (op,szs,intl)
   ;
   clear_tyvar_constraints () 
   )
# 4206 "src/frontend/parser.ml"
     : (unit))

let _menhir_action_250 =
  fun _endpos_v_ _startpos_v_ v ->
    let _endpos = _endpos_v_ in
    let _startpos = _startpos_v_ in
    let _loc = (_startpos, _endpos) in
    (
# 399 "src/frontend/parser.mly"
               ( mk_loc (with_file _loc) v )
# 4217 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_251 =
  fun e es ->
    (
# 403 "src/frontend/parser.mly"
    ( E_tuple (e::es) )
# 4225 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_252 =
  fun e ->
    (
# 404 "src/frontend/parser.mly"
           ( e )
# 4233 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_253 =
  fun _endpos_e_ _startpos_x_ e x ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos_x_ in
    let _loc = (_startpos, _endpos) in
    (
# 718 "src/frontend/parser.mly"
        ( match x with
          | "_" -> e
          | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                  ~msg:"the wildcard_case should be named \"_\"" () )
# 4247 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | AMP ->
        "AMP"
    | AMP_AMP ->
        "AMP_AMP"
    | AND ->
        "AND"
    | ARRAY ->
        "ARRAY"
    | ARRAY_CREATE ->
        "ARRAY_CREATE"
    | ARRAY_LENGTH ->
        "ARRAY_LENGTH"
    | ARRAY_MAKE ->
        "ARRAY_MAKE"
    | ASR ->
        "ASR"
    | AT ->
        "AT"
    | AT_AT ->
        "AT_AT"
    | BANG ->
        "BANG"
    | BOOL_LIT _ ->
        "BOOL_LIT"
    | COL ->
        "COL"
    | COL_EQ ->
        "COL_EQ"
    | COMMA ->
        "COMMA"
    | CREATE ->
        "CREATE"
    | DEFAULT ->
        "DEFAULT"
    | DIV ->
        "DIV"
    | DO ->
        "DO"
    | DONE ->
        "DONE"
    | DOT ->
        "DOT"
    | DOT_LENGTH ->
        "DOT_LENGTH"
    | ELSE ->
        "ELSE"
    | END ->
        "END"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | EQ_EQ ->
        "EQ_EQ"
    | EXEC ->
        "EXEC"
    | EXIT_REPL ->
        "EXIT_REPL"
    | EXTERNAL ->
        "EXTERNAL"
    | FIX ->
        "FIX"
    | FOR ->
        "FOR"
    | FUN ->
        "FUN"
    | GE ->
        "GE"
    | GET ->
        "GET"
    | GT ->
        "GT"
    | HAT ->
        "HAT"
    | IDENT _ ->
        "IDENT"
    | IF ->
        "IF"
    | IMMEDIATE ->
        "IMMEDIATE"
    | IMPLY ->
        "IMPLY"
    | IMPURE ->
        "IMPURE"
    | IN ->
        "IN"
    | INIT ->
        "INIT"
    | INIT_INT ->
        "INIT_INT"
    | INIT_TUPLE ->
        "INIT_TUPLE"
    | INT_LIT _ ->
        "INT_LIT"
    | INT_MAPI ->
        "INT_MAPI"
    | INT_OF_TUPLE ->
        "INT_OF_TUPLE"
    | LAND ->
        "LAND"
    | LBRACKET ->
        "LBRACKET"
    | LBRACKET_PIPE ->
        "LBRACKET_PIPE"
    | LCUR ->
        "LCUR"
    | LE ->
        "LE"
    | LEFT_ARROW ->
        "LEFT_ARROW"
    | LENGTH ->
        "LENGTH"
    | LET ->
        "LET"
    | LOR ->
        "LOR"
    | LPAREN ->
        "LPAREN"
    | LSL ->
        "LSL"
    | LSR ->
        "LSR"
    | LT ->
        "LT"
    | LXOR ->
        "LXOR"
    | MACRO_FOR ->
        "MACRO_FOR"
    | MACRO_GENERATE ->
        "MACRO_GENERATE"
    | MATCH ->
        "MATCH"
    | MINUS ->
        "MINUS"
    | MOD ->
        "MOD"
    | NEQ ->
        "NEQ"
    | NODE ->
        "NODE"
    | NOT ->
        "NOT"
    | OF ->
        "OF"
    | OPERATOR ->
        "OPERATOR"
    | OPERATOR_IDENT _ ->
        "OPERATOR_IDENT"
    | OR ->
        "OR"
    | PARFOR ->
        "PARFOR"
    | PIPE ->
        "PIPE"
    | PIPE_COMMA_PIPE ->
        "PIPE_COMMA_PIPE"
    | PIPE_PIPE ->
        "PIPE_PIPE"
    | PIPE_RBRACKET ->
        "PIPE_RBRACKET"
    | PLUS ->
        "PLUS"
    | QUESTION_MARK ->
        "QUESTION_MARK"
    | QUOTE ->
        "QUOTE"
    | RBRACKET ->
        "RBRACKET"
    | RCUR ->
        "RCUR"
    | REC ->
        "REC"
    | REF ->
        "REF"
    | REGISTER ->
        "REGISTER"
    | RESET ->
        "RESET"
    | RESIZE_INT ->
        "RESIZE_INT"
    | RIGHT_ARROW ->
        "RIGHT_ARROW"
    | RPAREN ->
        "RPAREN"
    | RUN ->
        "RUN"
    | SEMI ->
        "SEMI"
    | SEMI_SEMI ->
        "SEMI_SEMI"
    | SET ->
        "SET"
    | SHARED ->
        "SHARED"
    | SHARP_PIPE_LBRACKET ->
        "SHARP_PIPE_LBRACKET"
    | SIZE_CREATE ->
        "SIZE_CREATE"
    | STATIC ->
        "STATIC"
    | STRING_LIT _ ->
        "STRING_LIT"
    | SYM _ ->
        "SYM"
    | THEN ->
        "THEN"
    | TIMES ->
        "TIMES"
    | TO ->
        "TO"
    | TUPLE_GET ->
        "TUPLE_GET"
    | TUPLE_OF_INT ->
        "TUPLE_OF_INT"
    | TUPLE_UPDATE ->
        "TUPLE_UPDATE"
    | TVAR_IDENT _ ->
        "TVAR_IDENT"
    | TYB_VAR_IDENT _ ->
        "TYB_VAR_IDENT"
    | TYPE ->
        "TYPE"
    | UNROLL ->
        "UNROLL"
    | UP_IDENT _ ->
        "UP_IDENT"
    | VECTOR_MAPI ->
        "VECTOR_MAPI"
    | VECT_CREATE ->
        "VECT_CREATE"
    | WHEN ->
        "WHEN"
    | WITH ->
        "WITH"
    | WITH_SIZES ->
        "WITH_SIZES"
    | XOR ->
        "XOR"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_570 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_pi =
    fun _menhir_stack _v ->
      MenhirBox_pi _v
  
  let rec _menhir_goto_pi : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _endpos _v _menhir_s ->
      match _menhir_s with
      | MenhirState464 ->
          _menhir_run_570 _menhir_stack _v
      | MenhirState555 ->
          _menhir_run_569 _menhir_stack _endpos _v
      | MenhirState556 ->
          _menhir_run_568 _menhir_stack _endpos _v
      | MenhirState566 ->
          _menhir_run_567 _menhir_stack _endpos _v
      | MenhirState561 ->
          _menhir_run_562 _menhir_stack _endpos _v
      | MenhirState559 ->
          _menhir_run_560 _menhir_stack _endpos _v
      | MenhirState557 ->
          _menhir_run_558 _menhir_stack _endpos _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_569 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_type_alias -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _endpos _v ->
      let MenhirCell1_type_alias (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let (_endpos_pi_, pi) = (_endpos, _v) in
      let _v = _menhir_action_180 pi in
      _menhir_goto_pi _menhir_stack _endpos_pi_ _v _menhir_s
  
  and _menhir_run_568 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_typ_sum -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _endpos _v ->
      let MenhirCell1_typ_sum (_menhir_stack, _menhir_s, d) = _menhir_stack in
      let (_endpos_pi_, pi) = (_endpos, _v) in
      let _v = _menhir_action_178 d pi in
      _menhir_goto_pi _menhir_stack _endpos_pi_ _v _menhir_s
  
  and _menhir_run_567 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_decl -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _endpos _v ->
      let MenhirCell1_decl (_menhir_stack, _menhir_s, d) = _menhir_stack in
      let (_endpos_pi_, pi) = (_endpos, _v) in
      let _v = _menhir_action_179 d pi in
      _menhir_goto_pi _menhir_stack _endpos_pi_ _v _menhir_s
  
  and _menhir_run_562 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_ext_circ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _endpos _v ->
      let MenhirCell1_ext_circ (_menhir_stack, _menhir_s, ec, _startpos_ec_) = _menhir_stack in
      let (_endpos_pi_, pi) = (_endpos, _v) in
      let _v = _menhir_action_175 _endpos_pi_ _startpos_ec_ ec pi in
      _menhir_goto_pi _menhir_stack _endpos_pi_ _v _menhir_s
  
  and _menhir_run_560 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_ext_fun -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _endpos _v ->
      let MenhirCell1_ext_fun (_menhir_stack, _menhir_s, ef) = _menhir_stack in
      let (_endpos_pi_, pi) = (_endpos, _v) in
      let _v = _menhir_action_176 ef pi in
      _menhir_goto_pi _menhir_stack _endpos_pi_ _v _menhir_s
  
  and _menhir_run_558 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_static -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _endpos _v ->
      let MenhirCell1_static (_menhir_stack, _menhir_s, g) = _menhir_stack in
      let (_endpos_pi_, pi) = (_endpos, _v) in
      let _v = _menhir_action_177 g pi in
      _menhir_goto_pi _menhir_stack _endpos_pi_ _v _menhir_s
  
  let _menhir_run_554 : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_s ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _endpos__1_ = _endpos in
      let _v = _menhir_action_181 () in
      _menhir_goto_pi _menhir_stack _endpos__1_ _v _menhir_s
  
  let _menhir_run_462 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_exp_eof =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let e = _v in
          let _v = _menhir_action_134 e in
          MenhirBox_exp_eof _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_001 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_VECT_CREATE (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LT ->
          let _menhir_s = MenhirState002 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TVAR_IDENT _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INT_LIT _v ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_003 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_x_, x) = (_endpos, _v) in
      let _v = _menhir_action_213 x in
      _menhir_goto_size _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _v _menhir_s _tok
  
  and _menhir_goto_size : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState234 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState227 ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState221 ->
          _menhir_run_222 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState100 ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState466 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState059 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState024 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState002 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_235 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ARRAY_CREATE -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | GT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | RPAREN ->
                  let _endpos_1 = _menhir_lexbuf.Lexing.lex_curr_p in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let MenhirCell1_ARRAY_CREATE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
                  let (sz, _endpos__6_) = (_v, _endpos_1) in
                  let _v = _menhir_action_033 _endpos__6_ _startpos__1_ sz in
                  _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__6_ _startpos__1_ _v _menhir_s _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_app_exp_desc : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_031 _endpos_e_ _startpos_e_ e in
      _menhir_goto_app_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v _menhir_s _tok
  
  and _menhir_goto_app_exp : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState458 ->
          _menhir_run_457 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState017 ->
          _menhir_run_457 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState339 ->
          _menhir_run_340 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState331 ->
          _menhir_run_332 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_326 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_292 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState289 ->
          _menhir_run_290 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState287 ->
          _menhir_run_288 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState285 ->
          _menhir_run_286 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState283 ->
          _menhir_run_284 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState281 ->
          _menhir_run_282 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState279 ->
          _menhir_run_280 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState277 ->
          _menhir_run_278 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState275 ->
          _menhir_run_276 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState273 ->
          _menhir_run_274 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState271 ->
          _menhir_run_272 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState269 ->
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState267 ->
          _menhir_run_268 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState265 ->
          _menhir_run_266 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState263 ->
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState261 ->
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState259 ->
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState257 ->
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState255 ->
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState253 ->
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState251 ->
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState246 ->
          _menhir_run_247 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState464 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState555 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState556 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState557 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState559 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState561 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState566 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState540 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState536 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState517 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState020 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState028 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState449 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState433 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState429 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState426 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState408 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState410 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState405 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState178 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState396 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState398 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState187 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState384 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState388 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState378 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState382 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState375 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState373 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState362 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState351 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState198 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState201 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState342 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState344 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState335 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState203 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState216 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState218 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState219 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState320 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState299 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState241 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_457 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_271 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_273 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_277 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_279 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_281 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_283 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_285 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_287 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState458 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP_AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_289 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PIPE_RBRACKET ->
          let x = _v in
          let _v = _menhir_action_188 x in
          _menhir_goto_separated_nonempty_list_COMMA_app_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_246 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState246 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_007 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_VECTOR_MAPI (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState007 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_008 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_x_, _startpos_x_, x) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_103 x in
      _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _startpos_x_ _v _menhir_s _tok
  
  and _menhir_goto_const : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState423 ->
          _menhir_run_440 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState441 ->
          _menhir_run_440 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState438 ->
          _menhir_run_440 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState313 ->
          _menhir_run_310 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState295 ->
          _menhir_run_310 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState296 ->
          _menhir_run_310 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState305 ->
          _menhir_run_310 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_310 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState464 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState555 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState556 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState557 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState559 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState561 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState566 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState540 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState536 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState526 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState524 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState517 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState007 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState458 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState017 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState020 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState022 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState028 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState449 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState433 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState429 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState426 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState418 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState415 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState030 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState408 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState410 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState405 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState115 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState178 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState396 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState398 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState187 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState384 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState388 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState378 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState382 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState375 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState373 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState362 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState351 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState198 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState201 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState342 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState344 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState339 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState335 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState203 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState331 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState216 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState218 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState219 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState320 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState316 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState294 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState299 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState248 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState289 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState287 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState285 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState283 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState281 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState279 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState277 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState275 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState273 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState271 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState269 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState267 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState265 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState263 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState261 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState259 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState257 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState255 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState253 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState251 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState246 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState241 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState231 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState229 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState199 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState185 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState184 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState179 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState180 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_440 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PIPE ->
          let _menhir_stack = MenhirCell1_const (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState441 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RIGHT_ARROW ->
          let x = _v in
          let _v = _menhir_action_200 x in
          _menhir_goto_separated_nonempty_list_PIPE_const_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_016 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_s_, _startpos_s_, s) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_101 s in
      _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_s_ _startpos_s_ _v _menhir_s _tok
  
  and _menhir_run_027 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_x_, _startpos_x_, x) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_102 x in
      _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _startpos_x_ _v _menhir_s _tok
  
  and _menhir_run_131 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState131 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LXOR ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_155 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_117 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let _endpos__3_ = _endpos in
          let _v = _menhir_action_117 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_119 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let _endpos__3_ = _endpos in
          let _v = _menhir_action_106 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_121 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__2_ = _endpos in
      let _v = _menhir_action_097 () in
      _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_122 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let _endpos__3_ = _endpos in
          let _v = _menhir_action_104 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_124 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let _endpos__3_ = _endpos in
          let _v = _menhir_action_115 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_126 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let _endpos__3_ = _endpos in
          let _v = _menhir_action_108 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_132 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_129 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_MINUS -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_MINUS (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__3_ = _endpos in
      let _v = _menhir_action_105 () in
      _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_133 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let _endpos__3_ = _endpos in
          let _v = _menhir_action_118 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_135 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let _endpos__3_ = _endpos in
          let _v = _menhir_action_109 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_137 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let _endpos__3_ = _endpos in
          let _v = _menhir_action_122 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_139 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let _endpos__3_ = _endpos in
          let _v = _menhir_action_121 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_141 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let _endpos__3_ = _endpos in
          let _v = _menhir_action_120 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_143 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let _endpos__3_ = _endpos in
          let _v = _menhir_action_111 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_145 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let _endpos__3_ = _endpos in
          let _v = _menhir_action_119 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_147 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let _endpos__3_ = _endpos_0 in
          let _v = _menhir_action_110 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_149 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let _endpos__3_ = _endpos in
          let _v = _menhir_action_112 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_151 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let _endpos__3_ = _endpos in
          let _v = _menhir_action_114 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_153 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let _endpos__3_ = _endpos in
          let _v = _menhir_action_113 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_155 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let _endpos__3_ = _endpos in
          let _v = _menhir_action_107 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_157 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let _endpos__3_ = _endpos in
          let _v = _menhir_action_123 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_159 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let _endpos__3_ = _endpos in
          let _v = _menhir_action_116 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_130 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LCUR (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState130 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_161 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | QUOTE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | INT_LIT _v_0 ->
              let _endpos_2 = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let (_startpos_n_, n, _endpos_k_, k) = (_startpos, _v, _endpos_2, _v_0) in
              let _v = _menhir_action_100 _endpos_k_ _startpos_n_ k n in
              _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_k_ _startpos_n_ _v _menhir_s _tok
          | _ ->
              _eRR ())
      | AMP | AMP_AMP | AND | ASR | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_MAPI | INT_OF_TUPLE | LAND | LCUR | LE | LOR | LPAREN | LSL | LSR | LT | LXOR | MACRO_FOR | MATCH | MINUS | MOD | NEQ | OPERATOR_IDENT _ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RCUR | RESET | RESIZE_INT | RIGHT_ARROW | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TIMES | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH | XOR ->
          let (_endpos_n_, _startpos_n_, n) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_099 n in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_n_ _startpos_n_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_164 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_b_, _startpos_b_, b) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_098 b in
      _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_b_ _startpos_b_ _v _menhir_s _tok
  
  and _menhir_goto_separated_nonempty_list_PIPE_const_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState441 ->
          _menhir_run_442 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState438 ->
          _menhir_run_432 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState423 ->
          _menhir_run_432 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_442 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_const -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_const (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_201 x xs in
      _menhir_goto_separated_nonempty_list_PIPE_const_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_432 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_separated_nonempty_list_PIPE_const_ (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState433 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NODE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_009 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | AT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | INT_LIT _v ->
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let (_startpos_x_, x, _endpos_k_, k) = (_startpos, (), _endpos, _v) in
              let _v = _menhir_action_012 k x in
              _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_k_ _startpos_x_ _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_aexp_desc : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_002 _endpos_e_ _startpos_e_ e in
      _menhir_goto_aexp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v _menhir_s _tok
  
  and _menhir_goto_aexp : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState526 ->
          _menhir_run_527 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState524 ->
          _menhir_run_525 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState007 ->
          _menhir_run_460 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState022 ->
          _menhir_run_452 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState418 ->
          _menhir_run_419 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState415 ->
          _menhir_run_416 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState030 ->
          _menhir_run_413 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState115 ->
          _menhir_run_402 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState128 ->
          _menhir_run_402 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState174 ->
          _menhir_run_401 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState316 ->
          _menhir_run_316 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState294 ->
          _menhir_run_316 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState248 ->
          _menhir_run_294 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState464 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState555 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState556 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState557 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState559 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState561 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState566 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState540 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState536 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState517 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState017 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState458 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState020 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState028 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState449 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState433 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState429 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState426 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState408 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState410 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState405 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState178 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState396 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState398 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState187 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState384 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState388 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState378 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState382 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState375 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState373 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState362 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState351 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState198 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState201 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState342 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState344 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState339 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState335 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState203 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState331 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState216 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState218 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState219 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState320 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState241 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState299 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState271 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState289 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState287 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState285 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState283 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState281 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState279 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState277 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState273 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState275 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState263 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState265 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState267 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState269 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState251 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState261 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState259 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState257 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState255 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState253 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState246 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState231 ->
          _menhir_run_232 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState229 ->
          _menhir_run_230 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState199 ->
          _menhir_run_200 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState185 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState184 ->
          _menhir_run_185 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState179 ->
          _menhir_run_184 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState180 ->
          _menhir_run_183 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_527 : type  ttv_stack. (((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_STATIC _menhir_cell0_IDENT, _menhir_box_pi) _menhir_cell1_aexp -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_218 e in
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_aexp (_menhir_stack, _, ec, _, _) = _menhir_stack in
          let MenhirCell0_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
          let MenhirCell1_STATIC (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (e, _endpos__7_) = (_v, _endpos) in
          let _v = _menhir_action_216 _endpos__7_ _startpos__1_ e ec x in
          _menhir_goto_static _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_static : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_static (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState557
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | TYPE ->
          _menhir_run_465 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState557
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | SHARED ->
          _menhir_run_492 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState557
      | OPERATOR ->
          _menhir_run_498 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | NODE ->
          _menhir_run_514 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | LET ->
          _menhir_run_521 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | INT_LIT _v_3 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState557
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState557
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | EXTERNAL ->
          _menhir_run_549 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | EOF ->
          _menhir_run_554 _menhir_stack _menhir_lexbuf MenhirState557
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | BOOL_LIT _v_5 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState557
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | _ ->
          _eRR ()
  
  and _menhir_run_465 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_TYPE (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState465
      | LT ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState465
      | LPAREN ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState465
      | IDENT _v ->
          let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TVAR_IDENT _v_1 ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState465, _v, _startpos_0, _endpos) in
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState466
          | LT ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState465, _v, _startpos_0, _endpos) in
              _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState466
          | INT_LIT _v_2 ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState465, _v, _startpos_0, _endpos) in
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState466
          | EQ ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState465, _v, _startpos_0, _endpos) in
              let _menhir_stack = MenhirCell1_EQ (_menhir_stack, MenhirState466) in
              let _menhir_s = MenhirState467 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | UP_IDENT _v ->
                  _menhir_run_468 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TYB_VAR_IDENT _v ->
                  _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | LT ->
                  _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | AT | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | INT_MAPI | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_FOR | MACRO_GENERATE | MATCH | MINUS | NODE | OPERATOR | OPERATOR_IDENT _ | REF | REGISTER | RESIZE_INT | RUN | SEMI_SEMI | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TIMES | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
              let (_menhir_s, _startpos_x_, x) = (MenhirState465, _startpos_0, _v) in
              let _v = _menhir_action_224 x in
              _menhir_goto_tyB_ident _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_x_ _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_052 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_startpos_x_, x) = (_startpos, _v) in
      let _v = _menhir_action_225 x in
      _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_x_ _v _menhir_s _tok
  
  and _menhir_goto_tyB_next : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState054 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState081 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState079 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState465 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState467 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState469 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState076 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState551 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState530 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState508 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState501 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState495 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState392 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState358 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState210 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState192 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState068 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState070 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_104 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | COMMA | RPAREN ->
          let tyB = _v in
          let _v = _menhir_action_223 tyB in
          _menhir_goto_tyB _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | ARRAY | IMPLY | QUESTION_MARK | RIGHT_ARROW ->
          let (_startpos_tyB_, tyB) = (_startpos, _v) in
          let _v = _menhir_action_243 tyB in
          _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_tyB_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_079 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB_next -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState079 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_077 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState077 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LT ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_055 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState055 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TVAR_IDENT _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INT_LIT _v ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_004 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_n_, n) = (_endpos, _v) in
      let _v = _menhir_action_212 n in
      _menhir_goto_size _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_n_ _v _menhir_s _tok
  
  and _menhir_run_058 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TVAR_IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState058
      | LT ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState058
      | INT_LIT _v_1 ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState058
      | ARRAY | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | AT | BANG | BOOL_LIT _ | COMMA | CREATE | EOF | EQ | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | IMPLY | INT_MAPI | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_FOR | MACRO_GENERATE | MATCH | MINUS | NODE | OPERATOR | OPERATOR_IDENT _ | PIPE | QUESTION_MARK | REF | REGISTER | RESIZE_INT | RIGHT_ARROW | RPAREN | RUN | SEMI_SEMI | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TIMES | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let (_startpos_x_, x) = (_startpos, _v) in
          let _v = _menhir_action_224 x in
          _menhir_goto_tyB_ident _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_x_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_059 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState059 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TVAR_IDENT _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INT_LIT _v ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_tyB_ident : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_startpos_tyB_, tyB) = (_startpos, _v) in
      let _v = _menhir_action_226 tyB in
      _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_tyB_ _v _menhir_s _tok
  
  and _menhir_run_072 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB_next -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v, _startpos, _endpos) in
      let _menhir_s = MenhirState072 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TVAR_IDENT _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LT ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_tyB : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState465 ->
          _menhir_run_479 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState467 ->
          _menhir_run_474 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState469 ->
          _menhir_run_470 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState097 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState095 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState076 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState086 ->
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState054 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_479 : type  ttv_stack. ((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_tyB (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          _menhir_run_475 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState479
      | IDENT _v_0 ->
          let (_v, _menhir_s) = (_v_0, MenhirState479) in
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACKET ->
              let _menhir_s = MenhirState481 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | INT_LIT _v ->
                  _menhir_run_482 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | AT ->
          let _menhir_stack = MenhirCell1_AT (_menhir_stack, MenhirState479) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v_2 ->
              let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v_2, _startpos, _endpos) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | SEMI_SEMI ->
                  _menhir_run_475 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState489
              | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | INT_LIT _ | INT_MAPI | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_FOR | MACRO_GENERATE | MATCH | MINUS | NODE | OPERATOR | OPERATOR_IDENT _ | REF | REGISTER | RESIZE_INT | RUN | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
                  let _ = _menhir_action_171 () in
                  _menhir_run_490 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IF | INT_LIT _ | INT_MAPI | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_FOR | MACRO_GENERATE | MATCH | MINUS | NODE | OPERATOR | OPERATOR_IDENT _ | REF | REGISTER | RESIZE_INT | RUN | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let _ = _menhir_action_171 () in
          _menhir_run_491 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_475 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_x_, x) = (_endpos, ()) in
      let _ = _menhir_action_172 x in
      _menhir_goto_option_SEMI_SEMI_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _menhir_s _tok
  
  and _menhir_goto_option_SEMI_SEMI_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _menhir_s _tok ->
      match _menhir_s with
      | MenhirState479 ->
          _menhir_run_491 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | MenhirState489 ->
          _menhir_run_490 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | MenhirState486 ->
          _menhir_run_487 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | MenhirState477 ->
          _menhir_run_478 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | MenhirState474 ->
          _menhir_run_476 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_491 : type  ttv_stack. ((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_tyB -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok ->
      let MenhirCell1_tyB (_menhir_stack, _, tyB) = _menhir_stack in
      let MenhirCell1_TYPE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__1_inlined1_ = _endpos in
      let _v = _menhir_action_247 _endpos__1_inlined1_ _startpos__1_ tyB in
      _menhir_goto_type_alias _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_type_alias : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_alias (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState555
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | TYPE ->
          _menhir_run_465 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState555
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | SHARED ->
          _menhir_run_492 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState555
      | OPERATOR ->
          _menhir_run_498 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | NODE ->
          _menhir_run_514 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | LET ->
          _menhir_run_521 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | INT_LIT _v_3 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState555
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState555
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | EXTERNAL ->
          _menhir_run_549 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | EOF ->
          _menhir_run_554 _menhir_stack _menhir_lexbuf MenhirState555
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | BOOL_LIT _v_5 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState555
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState555
      | _ ->
          _eRR ()
  
  and _menhir_run_012 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | INT_LIT _v ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | GT ->
                  let _endpos_1 = _menhir_lexbuf.Lexing.lex_curr_p in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let (_startpos__1_, k, _endpos__4_) = (_startpos, _v, _endpos_1) in
                  let _v = _menhir_action_008 k in
                  _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__4_ _startpos__1_ _v _menhir_s _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_018 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | INT_LIT _v ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_endpos_n_, n, _startpos__1_) = (_endpos, _v, _startpos) in
          let _v = _menhir_action_048 n in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_n_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_017 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_SHARP_PIPE_LBRACKET (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState017 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PIPE_RBRACKET ->
          let _v = _menhir_action_158 () in
          _menhir_goto_loption_separated_nonempty_list_COMMA_app_exp__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_020 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_SET (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState020 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NODE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_021 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_RUN (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UP_IDENT _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_UP_IDENT (_menhir_stack, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState022 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_023 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_RESIZE_INT (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LT ->
          let _menhir_s = MenhirState024 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TVAR_IDENT _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INT_LIT _v ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_028 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_MATCH (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState028 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NODE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_029 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_REGISTER (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState029
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState029
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState029
      | NODE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | LET ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState029
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | IDENT _v ->
          let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACKET ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState029, _v, _startpos_0, _endpos) in
              _menhir_run_203 _menhir_stack _menhir_lexbuf _menhir_lexer
          | INIT ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState029, _v, _startpos_0, _endpos) in
              let _menhir_s = MenhirState415 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VECT_CREATE ->
                  _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | VECTOR_MAPI ->
                  _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | UP_IDENT _v ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | UNROLL ->
                  _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TUPLE_OF_INT ->
                  _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | STRING_LIT _v ->
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | SHARP_PIPE_LBRACKET ->
                  _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | RESIZE_INT ->
                  _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPERATOR_IDENT _v ->
                  _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | MATCH ->
                  _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_FOR ->
                  _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LCUR ->
                  _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_OF_TUPLE ->
                  _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_MAPI ->
                  _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_LIT _v ->
                  _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IDENT _v ->
                  _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FOR ->
                  _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL_LIT _v ->
                  _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | DOT_LENGTH ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState029, _v, _startpos_0, _endpos) in
              _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer
          | DOT ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState029, _v, _startpos_0, _endpos) in
              _menhir_run_334 _menhir_stack _menhir_lexbuf _menhir_lexer
          | AMP | AMP_AMP | ASR | BANG | BOOL_LIT _ | COL_EQ | COMMA | DIV | EQ | EQ_EQ | FOR | GE | GT | IDENT _ | INT_LIT _ | INT_MAPI | INT_OF_TUPLE | LAND | LCUR | LE | LOR | LPAREN | LSL | LSR | LT | LXOR | MACRO_FOR | MATCH | MINUS | MOD | NEQ | OPERATOR_IDENT _ | OR | PLUS | RESIZE_INT | SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | TIMES | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | XOR ->
              let (_menhir_s, _endpos_x_, _startpos_x_, x) = (MenhirState029, _endpos, _startpos_0, _v) in
              let _v = _menhir_action_014 _endpos_x_ _startpos_x_ x in
              _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _startpos_x_ _v _menhir_s _tok
          | _ ->
              _eRR ())
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState029
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState029
      | _ ->
          _eRR ()
  
  and _menhir_run_030 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_REF (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState030 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_031 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_MACRO_FOR (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | EQ ->
              let _menhir_s = MenhirState033 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VECT_CREATE ->
                  _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | VECTOR_MAPI ->
                  _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | UP_IDENT _v ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | UNROLL ->
                  _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TUPLE_OF_INT ->
                  _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | STRING_LIT _v ->
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | SIZE_CREATE ->
                  _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | SHARP_PIPE_LBRACKET ->
                  _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | SET ->
                  _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | RUN ->
                  _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | RESIZE_INT ->
                  _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | REGISTER ->
                  _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | REF ->
                  _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPERATOR_IDENT _v ->
                  _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | NODE ->
                  _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MINUS ->
                  _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MATCH ->
                  _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_GENERATE ->
                  _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_FOR ->
                  _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LET ->
                  _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LENGTH ->
                  _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LCUR ->
                  _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_OF_TUPLE ->
                  _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_MAPI ->
                  _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_LIT _v ->
                  _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IF ->
                  _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FUN ->
                  _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FOR ->
                  _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FIX ->
                  _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | EXEC ->
                  _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CREATE ->
                  _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL_LIT _v ->
                  _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_MAKE ->
                  _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_LENGTH ->
                  _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_CREATE ->
                  _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_034 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_NODE (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState034 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState035 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_036 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState036 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_037 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _v = _menhir_action_028 () in
      _menhir_goto_apat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_apat : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState445 ->
          _menhir_run_425 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState424 ->
          _menhir_run_425 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState521 ->
          _menhir_run_372 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState188 ->
          _menhir_run_372 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState370 ->
          _menhir_run_372 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState354 ->
          _menhir_run_357 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState543 ->
          _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState534 ->
          _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState515 ->
          _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState360 ->
          _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState196 ->
          _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState204 ->
          _menhir_run_209 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState297 ->
          _menhir_run_209 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState205 ->
          _menhir_run_209 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState036 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState046 ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState043 ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState038 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_425 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_UP_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW ->
          let _menhir_s = MenhirState426 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_115 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState115 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_116 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState116 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TIMES ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RPAREN ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NODE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NEQ ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LXOR ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LOR ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LET ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LE ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAND ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | GT ->
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EQ_EQ ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_155 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASR ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_128 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState128 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RPAREN ->
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_170 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | INT_LIT _v ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | GT ->
                  let _endpos_1 = _menhir_lexbuf.Lexing.lex_curr_p in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let (_startpos__1_, k, _endpos__4_) = (_startpos, _v, _endpos_1) in
                  let _v = _menhir_action_009 k in
                  _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__4_ _startpos__1_ _v _menhir_s _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_174 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_INT_MAPI (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState174 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_175 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_x_, _startpos_x_, x) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_014 _endpos_x_ _startpos_x_ x in
      _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _startpos_x_ _v _menhir_s _tok
  
  and _menhir_run_176 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_FOR (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | EQ ->
              let _menhir_s = MenhirState178 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VECT_CREATE ->
                  _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | VECTOR_MAPI ->
                  _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | UP_IDENT _v ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | UNROLL ->
                  _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TUPLE_OF_INT ->
                  _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | STRING_LIT _v ->
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | SIZE_CREATE ->
                  _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | SHARP_PIPE_LBRACKET ->
                  _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | SET ->
                  _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | RUN ->
                  _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | RESIZE_INT ->
                  _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | REGISTER ->
                  _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | REF ->
                  _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPERATOR_IDENT _v ->
                  _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | NODE ->
                  _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MINUS ->
                  _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MATCH ->
                  _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_GENERATE ->
                  _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_FOR ->
                  _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LET ->
                  _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LENGTH ->
                  _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LCUR ->
                  _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_OF_TUPLE ->
                  _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_MAPI ->
                  _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_LIT _v ->
                  _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IF ->
                  _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FUN ->
                  _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FOR ->
                  _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FIX ->
                  _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | EXEC ->
                  _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CREATE ->
                  _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL_LIT _v ->
                  _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_MAKE ->
                  _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_LENGTH ->
                  _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_CREATE ->
                  _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_179 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_MACRO_GENERATE (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState179 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_180 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_BANG (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState180 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_187 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState187 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TIMES ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RPAREN ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NODE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NEQ ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LXOR ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LOR ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LET ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LE ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAND ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | GT ->
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EQ_EQ ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_155 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASR ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_188 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | REC ->
          let _menhir_stack = MenhirCell1_REC (_menhir_stack, MenhirState188) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState189
          | IDENT _v ->
              let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LPAREN ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState189, _v, _startpos_0, _endpos) in
                  _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState196
              | IDENT _v_1 ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState189, _v, _startpos_0, _endpos) in
                  _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState196
              | COL ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState189, _v, _startpos_0, _endpos) in
                  _menhir_run_192 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState196
              | EQ ->
                  let _v =
                    let x = _v in
                    _menhir_action_231 x
                  in
                  _menhir_run_350 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_0 _v MenhirState189 _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | LPAREN ->
          _menhir_run_354 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState188
      | IDENT _v ->
          let _startpos_2 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState188, _v, _startpos_2, _endpos) in
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState360
          | IDENT _v_3 ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState188, _v, _startpos_2, _endpos) in
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState360
          | COL | EQ ->
              let _v =
                let x = _v in
                _menhir_action_030 x
              in
              _menhir_run_372 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState188 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_190 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState190
      | IDENT _v ->
          let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COL ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState190, _v, _startpos_0, _endpos) in
              _menhir_run_192 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState191
          | RPAREN ->
              let _v =
                let x = _v in
                _menhir_action_231 x
              in
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_192 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_COL (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState192 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TVAR_IDENT _v ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_053 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_startpos_x_, x) = (_startpos, _v) in
      let _v = _menhir_action_238 x in
      _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_x_ _v _menhir_s _tok
  
  and _menhir_goto_ty_next : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState070 ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState068 ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState551 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState530 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState508 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState501 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState495 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState392 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState358 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState210 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState192 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState054 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_069 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_ty_next as 'stack) -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          let _menhir_s = MenhirState070 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVAR_IDENT _v ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RIGHT_ARROW ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer
      | QUESTION_MARK ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IMPLY ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARRAY ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AT | EQ | RPAREN | SEMI_SEMI ->
          let x = _v in
          let _v = _menhir_action_210 x in
          _menhir_goto_separated_nonempty_list_TIMES_ty_next_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_054 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState054 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TVAR_IDENT _v ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LT ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_076 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState076 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LT ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_094 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW ->
          let _menhir_s = MenhirState095 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LT ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_097 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState097 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LT ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_099 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LT ->
          let _menhir_s = MenhirState100 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TVAR_IDENT _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INT_LIT _v ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_TIMES_ty_next_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_ty_next as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState068 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState070 ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_103 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty_next (_menhir_stack, _menhir_s, ty, _) = _menhir_stack in
      let tys = _v in
      let _v = _menhir_action_219 ty tys in
      _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_ty : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState551 ->
          _menhir_run_552 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState530 ->
          _menhir_run_531 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState508 ->
          _menhir_run_509 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState501 ->
          _menhir_run_502 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState495 ->
          _menhir_run_496 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState392 ->
          _menhir_run_393 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState358 ->
          _menhir_run_359 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState210 ->
          _menhir_run_211 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState192 ->
          _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState111 ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState051 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState054 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_552 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_EXTERNAL _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
          let MenhirCell1_EXTERNAL (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_135 t x in
          _menhir_goto_ext_circ _menhir_stack _menhir_lexbuf _menhir_lexer _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_ext_circ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ext_circ (_menhir_stack, _menhir_s, _v, _startpos) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState561
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | TYPE ->
          _menhir_run_465 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState561
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | SHARED ->
          _menhir_run_492 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState561
      | OPERATOR ->
          _menhir_run_498 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | NODE ->
          _menhir_run_514 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | LET ->
          _menhir_run_521 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | INT_LIT _v_3 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState561
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState561
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | EXTERNAL ->
          _menhir_run_549 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | EOF ->
          _menhir_run_554 _menhir_stack _menhir_lexbuf MenhirState561
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | BOOL_LIT _v_5 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState561
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState561
      | _ ->
          _eRR ()
  
  and _menhir_run_492 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_SHARED (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | EXTERNAL ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _menhir_stack = MenhirCell0_EXTERNAL (_menhir_stack, _startpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v, _startpos, _endpos) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | COL ->
                  let _menhir_s = MenhirState495 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | TYB_VAR_IDENT _v ->
                      _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | TVAR_IDENT _v ->
                      _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | LPAREN ->
                      _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IDENT _v ->
                      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_498 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_OPERATOR (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WITH_SIZES ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | OPERATOR_IDENT _v ->
              let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _menhir_stack = MenhirCell0_OPERATOR_IDENT (_menhir_stack, _v, _startpos, _endpos) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | COL ->
                  let _menhir_s = MenhirState501 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | TYB_VAR_IDENT _v ->
                      _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | TVAR_IDENT _v ->
                      _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | LPAREN ->
                      _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IDENT _v ->
                      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | OPERATOR_IDENT _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_OPERATOR_IDENT (_menhir_stack, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COL ->
              let _menhir_s = MenhirState508 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TYB_VAR_IDENT _v ->
                  _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TVAR_IDENT _v ->
                  _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | LPAREN ->
                  _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_514 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_NODE (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState514 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState515 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_039 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let x = _v in
      let _v = _menhir_action_030 x in
      _menhir_goto_apat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_521 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STATIC ->
          let _menhir_stack = MenhirCell1_STATIC (_menhir_stack, MenhirState521) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v, _startpos, _endpos) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | EQ ->
                  let _menhir_s = MenhirState524 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | VECT_CREATE ->
                      _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | VECTOR_MAPI ->
                      _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | UP_IDENT _v ->
                      _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | UNROLL ->
                      _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | TUPLE_OF_INT ->
                      _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | STRING_LIT _v ->
                      _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | SHARP_PIPE_LBRACKET ->
                      _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | RESIZE_INT ->
                      _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | OPERATOR_IDENT _v ->
                      _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | MATCH ->
                      _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | MACRO_FOR ->
                      _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LPAREN ->
                      _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LCUR ->
                      _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | INT_OF_TUPLE ->
                      _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | INT_MAPI ->
                      _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | INT_LIT _v ->
                      _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | IDENT _v ->
                      _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | FOR ->
                      _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | BOOL_LIT _v ->
                      _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | BANG ->
                      _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | _ ->
                      _eRR ())
              | COL ->
                  let _menhir_s = MenhirState530 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | TYB_VAR_IDENT _v ->
                      _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | TVAR_IDENT _v ->
                      _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | LPAREN ->
                      _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IDENT _v ->
                      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | REC ->
          let _menhir_stack = MenhirCell1_REC (_menhir_stack, MenhirState521) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState533
          | IDENT _v ->
              let _startpos_10 = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LPAREN ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState533, _v, _startpos_10, _endpos) in
                  _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState534
              | IDENT _v_11 ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState533, _v, _startpos_10, _endpos) in
                  _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_11 MenhirState534
              | COL ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState533, _v, _startpos_10, _endpos) in
                  _menhir_run_192 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState534
              | EQ ->
                  let _v =
                    let x = _v in
                    _menhir_action_231 x
                  in
                  _menhir_run_539 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_10 _v MenhirState533 _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | LPAREN ->
          _menhir_run_354 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState521
      | IDENT _v ->
          let _startpos_12 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState521, _v, _startpos_12, _endpos) in
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState543
          | IDENT _v_13 ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState521, _v, _startpos_12, _endpos) in
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_13 MenhirState543
          | COL | EQ ->
              let _v =
                let x = _v in
                _menhir_action_030 x
              in
              _menhir_run_372 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState521 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_539 : type  ttv_stack. (((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty_annot_IDENT_ (_menhir_stack, _menhir_s, _v, _startpos) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState540 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_199 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LENGTH (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState199 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_201 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState201 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NODE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_202 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_203 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT_LENGTH ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_334 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | ASR | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | IDENT _ | IN | INIT | INT_LIT _ | INT_MAPI | INT_OF_TUPLE | LAND | LCUR | LE | LOR | LPAREN | LSL | LSR | LT | LXOR | MACRO_FOR | MATCH | MINUS | MOD | NEQ | OPERATOR_IDENT _ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TIMES | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH | XOR ->
          let (_endpos_x_, _startpos_x_, x) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_014 _endpos_x_ _startpos_x_ x in
          _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _startpos_x_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_203 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_IDENT -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState203 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NODE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_204 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_FUN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState204 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_205 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_205 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState205 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_038 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState038 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_217 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_FIX (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState218 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_219 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_EXEC (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState219 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NODE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_220 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_CREATE (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LT ->
          let _menhir_s = MenhirState221 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TVAR_IDENT _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INT_LIT _v ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_226 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_ARRAY_MAKE (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LT ->
          let _menhir_s = MenhirState227 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TVAR_IDENT _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INT_LIT _v ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_231 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_ARRAY_LENGTH (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState231 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_233 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_ARRAY_CREATE (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LT ->
          let _menhir_s = MenhirState234 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TVAR_IDENT _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INT_LIT _v ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_333 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_IDENT -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
      let _endpos__2_ = _endpos in
      let _v = _menhir_action_043 x in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos_x_ _v _menhir_s _tok
  
  and _menhir_run_334 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_IDENT -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _menhir_stack = MenhirCell0_LPAREN (_menhir_stack, _startpos) in
          let _menhir_s = MenhirState335 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_354 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState354 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_354 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_372 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState373 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | COL ->
          _menhir_run_358 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_358 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_apat -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState358 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TVAR_IDENT _v ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_549 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_EXTERNAL (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COL ->
              let _menhir_s = MenhirState551 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TYB_VAR_IDENT _v ->
                  _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TVAR_IDENT _v ->
                  _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | LPAREN ->
                  _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_531 : type  ttv_stack. ((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_STATIC _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
          let MenhirCell1_STATIC (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (ty, _endpos__6_) = (_v, _endpos) in
          let _v = _menhir_action_217 _endpos__6_ _startpos__1_ ty x in
          _menhir_goto_static _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_509 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_OPERATOR _menhir_cell0_OPERATOR_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_OPERATOR_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
          let MenhirCell1_OPERATOR (_menhir_stack, _menhir_s) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_138 t x in
          _menhir_goto_ext_fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | AT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IMPURE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | SEMI_SEMI ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let MenhirCell0_OPERATOR_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
                  let MenhirCell1_OPERATOR (_menhir_stack, _menhir_s) = _menhir_stack in
                  let t = _v in
                  let _v = _menhir_action_137 t x in
                  _menhir_goto_ext_fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_ext_fun : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ext_fun (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState559
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | TYPE ->
          _menhir_run_465 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState559
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | SHARED ->
          _menhir_run_492 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState559
      | OPERATOR ->
          _menhir_run_498 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | NODE ->
          _menhir_run_514 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | LET ->
          _menhir_run_521 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | INT_LIT _v_3 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState559
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState559
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | EXTERNAL ->
          _menhir_run_549 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | EOF ->
          _menhir_run_554 _menhir_stack _menhir_lexbuf MenhirState559
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | BOOL_LIT _v_5 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState559
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState559
      | _ ->
          _eRR ()
  
  and _menhir_run_502 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_OPERATOR _menhir_cell0_OPERATOR_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_OPERATOR_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
          let MenhirCell1_OPERATOR (_menhir_stack, _menhir_s) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_140 t x in
          _menhir_goto_ext_fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | AT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IMPURE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | SEMI_SEMI ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let MenhirCell0_OPERATOR_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
                  let MenhirCell1_OPERATOR (_menhir_stack, _menhir_s) = _menhir_stack in
                  let t = _v in
                  let _v = _menhir_action_139 t x in
                  _menhir_goto_ext_fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_496 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_SHARED _menhir_cell0_EXTERNAL _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
          let MenhirCell0_EXTERNAL (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_SHARED (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_136 t x in
          _menhir_goto_ext_circ _menhir_stack _menhir_lexbuf _menhir_lexer _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_393 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_exp -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_exp (_menhir_stack, _, e, _, _) = _menhir_stack in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__5_, ty) = (_endpos, _v) in
          let _v = _menhir_action_005 e ty in
          _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_359 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_apat (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let ty = _v in
      let _v = _menhir_action_235 ty x in
      _menhir_goto_ty_annot_apat_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_ty_annot_apat_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState521 ->
          _menhir_run_361 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState370 ->
          _menhir_run_361 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState188 ->
          _menhir_run_361 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState354 ->
          _menhir_run_355 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_361 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty_annot_apat_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState362 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_355 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let x_ty_opt = _v in
          let _v = _menhir_action_236 x_ty_opt in
          _menhir_goto_ty_annot_apat_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_211 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let ty = _v in
      let _v = _menhir_action_089 ty in
      let MenhirCell1_apat (_menhir_stack, _menhir_s, p) = _menhir_stack in
      let ty = _v in
      let _v = _menhir_action_088 p ty in
      _menhir_goto_arg_ty_unparen _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_arg_ty_unparen : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState297 ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState204 ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState205 ->
          _menhir_run_207 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_214 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let a = _v in
      let _v = _menhir_action_082 a in
      _menhir_goto_arg_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_arg_ty : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState297 ->
          _menhir_run_298 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState204 ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_298 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_arg_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW ->
          let _menhir_s = MenhirState299 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_215 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_arg_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW ->
          let _menhir_s = MenhirState216 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_207 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let a = _v in
          let _v = _menhir_action_083 a in
          _menhir_goto_arg_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_193 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_COL -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_COL (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
      let ty = _v in
      let _v = _menhir_action_232 ty x in
      _menhir_goto_ty_annot_IDENT_ _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_x_ _v _menhir_s _tok
  
  and _menhir_goto_ty_annot_IDENT_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState533 ->
          _menhir_run_539 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState189 ->
          _menhir_run_350 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_350 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_REC as 'stack) -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty_annot_IDENT_ (_menhir_stack, _menhir_s, _v, _startpos) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState351 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_194 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let x_ty_opt = _v in
          let _v = _menhir_action_233 x_ty_opt in
          _menhir_goto_ty_annot_IDENT_ _menhir_stack _menhir_lexbuf _menhir_lexer _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_112 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic, ttv_result) _menhir_cell1_COL -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_COL (_menhir_stack, _menhir_s) = _menhir_stack in
          let ty = _v in
          let _v = _menhir_action_183 ty in
          _menhir_goto_ret_ty_annot_eq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_ret_ty_annot_eq : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState535 ->
          _menhir_run_536 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState516 ->
          _menhir_run_517 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState197 ->
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_536 : type  ttv_stack. (((((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ret_ty_annot_eq (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState536
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState536
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState536
      | NODE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | LET ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | INT_LIT _v_3 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState536
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState536
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | BOOL_LIT _v_5 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState536
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
      | _ ->
          _eRR ()
  
  and _menhir_run_517 : type  ttv_stack. (((ttv_stack, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ret_ty_annot_eq (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState517
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState517
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState517
      | NODE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | LET ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | INT_LIT _v_3 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState517
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState517
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | BOOL_LIT _v_5 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState517
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState517
      | _ ->
          _eRR ()
  
  and _menhir_run_198 : type  ttv_stack ttv_result. (((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_REC, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ret_ty_annot_eq (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState198
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState198
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState198
      | NODE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | LET ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | INT_LIT _v_3 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState198
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState198
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | BOOL_LIT _v_5 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState198
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | _ ->
          _eRR ()
  
  and _menhir_run_114 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ret_ty_annot_eq (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState114
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState114
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState114
      | NODE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | LET ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | INT_LIT _v_3 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState114
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState114
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | BOOL_LIT _v_5 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState114
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
      | _ ->
          _eRR ()
  
  and _menhir_run_107 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_apat (_menhir_stack, _, p) = _menhir_stack in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let ty = _v in
          let _v = _menhir_action_085 p ty in
          _menhir_goto_arg_ty_atomic _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_arg_ty_atomic : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState534 ->
          _menhir_run_535 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState543 ->
          _menhir_run_516 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState515 ->
          _menhir_run_516 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState196 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState360 ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_535 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_arg_ty_atomic (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState535
      | COL ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState535
      | _ ->
          _eRR ()
  
  and _menhir_run_110 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_182 () in
      _menhir_goto_ret_ty_annot_eq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_111 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_COL (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState111 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TVAR_IDENT _v ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_516 : type  ttv_stack. ((ttv_stack, _menhir_box_pi) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_arg_ty_atomic (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState516
      | COL ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState516
      | _ ->
          _eRR ()
  
  and _menhir_run_197 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_REC, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_arg_ty_atomic (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState197
      | COL ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState197
      | _ ->
          _eRR ()
  
  and _menhir_run_109 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_arg_ty_atomic (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
      | COL ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
      | _ ->
          _eRR ()
  
  and _menhir_run_105 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let ty = _v in
          let _v = _menhir_action_244 ty in
          _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_075 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_ty_next, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty_next (_menhir_stack, _menhir_s, x, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_211 x xs in
      _menhir_goto_separated_nonempty_list_TIMES_ty_next_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_067 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          let _menhir_s = MenhirState068 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVAR_IDENT _v ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RIGHT_ARROW ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer
      | QUESTION_MARK ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IMPLY ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARRAY ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AT | EQ | RPAREN | SEMI_SEMI ->
          let ty = _v in
          let _v = _menhir_action_220 ty in
          _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_357 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COL ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_358 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let x = _v in
          let _v = _menhir_action_234 x in
          _menhir_goto_ty_annot_apat_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_043 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_apat -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState043 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_349 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let p = _v in
      let _v = _menhir_action_086 p in
      _menhir_goto_arg_ty_atomic _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_209 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COL ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState210 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVAR_IDENT _v ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RIGHT_ARROW | RPAREN ->
          let p = _v in
          let _v = _menhir_action_173 p in
          _menhir_goto_pat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_pat : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState297 ->
          _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState204 ->
          _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState205 ->
          _menhir_run_206 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState036 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState354 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState038 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_213 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let p = _v in
      let _v = _menhir_action_087 p in
      _menhir_goto_arg_ty_unparen _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_206 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_pat (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_041 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_pat -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_pat (_menhir_stack, _, p) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _v = _menhir_action_029 p in
      _menhir_goto_apat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_048 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let p = _v in
          let _v = _menhir_action_084 p in
          _menhir_goto_arg_ty_atomic _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_040 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_pat (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_050 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COL ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState051 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVAR_IDENT _v ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let p = _v in
          let _v = _menhir_action_173 p in
          _menhir_goto_pat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_045 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_apat as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState046 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RIGHT_ARROW | RPAREN ->
          let x = _v in
          let _v = _menhir_action_186 x in
          _menhir_goto_separated_nonempty_list_COMMA_apat_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_apat_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_apat as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState046 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState043 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_047 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_apat, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_apat (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_187 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_apat_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_044 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_apat (_menhir_stack, _menhir_s, p) = _menhir_stack in
      let ps = _v in
      let _v = _menhir_action_174 p ps in
      _menhir_goto_pat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_042 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let p = _v in
          let _v = _menhir_action_173 p in
          _menhir_goto_pat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_loption_separated_nonempty_list_COMMA_app_exp__ : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_SHARP_PIPE_LBRACKET -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_SHARP_PIPE_LBRACKET (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__3_, xs) = (_endpos, _v) in
      let _v = _menhir_action_015 xs in
      _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_490 : type  ttv_stack. (((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_tyB, _menhir_box_pi) _menhir_cell1_AT _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok ->
      let MenhirCell0_IDENT (_menhir_stack, op, _, _) = _menhir_stack in
      let MenhirCell1_AT (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_tyB (_menhir_stack, _, tyB) = _menhir_stack in
      let MenhirCell1_TYPE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__3_ = _endpos in
      let _v = _menhir_action_248 _endpos__3_ _startpos__1_ op tyB in
      _menhir_goto_type_alias _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_487 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_tyB, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_separated_nonempty_list_COMMA_INT_LIT_ _menhir_cell0_RBRACKET -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok ->
      let MenhirCell0_RBRACKET (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_separated_nonempty_list_COMMA_INT_LIT_ (_menhir_stack, _, intl) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _, op, _, _) = _menhir_stack in
      let MenhirCell1_tyB (_menhir_stack, _, tyB) = _menhir_stack in
      let MenhirCell1_TYPE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__5_ = _endpos in
      let _v = _menhir_action_249 _endpos__5_ _startpos__1_ intl op tyB in
      _menhir_goto_type_alias _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_478 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_EQ, _menhir_box_pi) _menhir_cell1_separated_nonempty_list_PIPE_ty_case_ -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok ->
      let MenhirCell1_separated_nonempty_list_PIPE_ty_case_ (_menhir_stack, _, ts) = _menhir_stack in
      let MenhirCell1_EQ (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _, x, _, _) = _menhir_stack in
      let MenhirCell1_TYPE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__5_ = _endpos in
      let _v = _menhir_action_245 _endpos__5_ _startpos__1_ ts x in
      let _menhir_stack = MenhirCell1_typ_sum (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState556
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | TYPE ->
          _menhir_run_465 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState556
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | SHARED ->
          _menhir_run_492 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState556
      | OPERATOR ->
          _menhir_run_498 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | NODE ->
          _menhir_run_514 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | LET ->
          _menhir_run_521 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | INT_LIT _v_3 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState556
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState556
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | EXTERNAL ->
          _menhir_run_549 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | EOF ->
          _menhir_run_554 _menhir_stack _menhir_lexbuf MenhirState556
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | BOOL_LIT _v_5 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState556
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | _ ->
          _eRR ()
  
  and _menhir_run_476 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_EQ, _menhir_box_pi) _menhir_cell1_tyB -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok ->
      let MenhirCell1_tyB (_menhir_stack, _, tyB) = _menhir_stack in
      let MenhirCell1_EQ (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _, x, _, _) = _menhir_stack in
      let MenhirCell1_TYPE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__5_ = _endpos in
      let _v = _menhir_action_246 _endpos__5_ _startpos__1_ tyB x in
      _menhir_goto_type_alias _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_482 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_INT_LIT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState483 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | INT_LIT _v ->
              _menhir_run_482 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RBRACKET ->
          let x = _v in
          let _v = _menhir_action_184 x in
          _menhir_goto_separated_nonempty_list_COMMA_INT_LIT_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_INT_LIT_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState481 ->
          _menhir_run_485 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState483 ->
          _menhir_run_484 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_485 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_tyB, _menhir_box_pi) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_separated_nonempty_list_COMMA_INT_LIT_ (_menhir_stack, _menhir_s, _v) in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell0_RBRACKET (_menhir_stack, _endpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          _menhir_run_475 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState486
      | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | INT_LIT _ | INT_MAPI | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_FOR | MACRO_GENERATE | MATCH | MINUS | NODE | OPERATOR | OPERATOR_IDENT _ | REF | REGISTER | RESIZE_INT | RUN | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let _ = _menhir_action_171 () in
          _menhir_run_487 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_484 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_INT_LIT -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_INT_LIT (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_185 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_INT_LIT_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_474 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_EQ as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_tyB (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          _menhir_run_475 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState474
      | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | INT_LIT _ | INT_MAPI | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_FOR | MACRO_GENERATE | MATCH | MINUS | NODE | OPERATOR | OPERATOR_IDENT _ | REF | REGISTER | RESIZE_INT | RUN | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let _ = _menhir_action_171 () in
          _menhir_run_476 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_470 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_UP_IDENT -> _ -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_UP_IDENT (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let tyB = _v in
      let _v = _menhir_action_237 tyB x in
      match (_tok : MenhirBasics.token) with
      | PIPE ->
          let _menhir_stack = MenhirCell1_ty_case (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState472 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UP_IDENT _v ->
              _menhir_run_468 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | INT_LIT _ | INT_MAPI | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_FOR | MACRO_GENERATE | MATCH | MINUS | NODE | OPERATOR | OPERATOR_IDENT _ | REF | REGISTER | RESIZE_INT | RUN | SEMI_SEMI | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let x = _v in
          let _v = _menhir_action_202 x in
          _menhir_goto_separated_nonempty_list_PIPE_ty_case_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_468 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_UP_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | OF ->
          let _menhir_s = MenhirState469 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LT ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_PIPE_ty_case_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState467 ->
          _menhir_run_477 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState472 ->
          _menhir_run_473 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_477 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_EQ as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_separated_nonempty_list_PIPE_ty_case_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          _menhir_run_475 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState477
      | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | INT_LIT _ | INT_MAPI | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_FOR | MACRO_GENERATE | MATCH | MINUS | NODE | OPERATOR | OPERATOR_IDENT _ | REF | REGISTER | RESIZE_INT | RUN | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let _ = _menhir_action_171 () in
          _menhir_run_478 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_473 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_ty_case -> _ -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_ty_case (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_203 x xs in
      _menhir_goto_separated_nonempty_list_PIPE_ty_case_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
  
  and _menhir_run_098 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty_next (_menhir_stack, _menhir_s, ty, _startpos_ty_) = _menhir_stack in
      let tyB = _v in
      let _v = _menhir_action_242 ty tyB in
      _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_ty_ _v _menhir_s _tok
  
  and _menhir_run_096 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty_next (_menhir_stack, _menhir_s, ty, _startpos_ty_) = _menhir_stack in
      let tyB = _v in
      let _v = _menhir_action_241 ty tyB in
      _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_ty_ _v _menhir_s _tok
  
  and _menhir_run_093 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty_next (_menhir_stack, _menhir_s, ty, _startpos_ty_) = _menhir_stack in
      let tyB = _v in
      let _v = _menhir_action_240 ty tyB in
      _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_ty_ _v _menhir_s _tok
  
  and _menhir_run_087 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_tyB as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_tyB (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let x = _v in
          let _v = _menhir_action_198 x in
          _menhir_goto_separated_nonempty_list_COMMA_tyB_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_086 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState086 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LT ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_tyB_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState054 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState077 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState086 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_089 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_separated_nonempty_list_COMMA_tyB_ (_menhir_stack, _menhir_s, _v) in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell0_RPAREN (_menhir_stack, _endpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState091 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TVAR_IDENT _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LT ->
              _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_088 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_tyB (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_199 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_tyB_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_084 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let tyB = _v in
          let _v = _menhir_action_227 tyB in
          _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos__1_ _v _menhir_s _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_tyB (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_080 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_tyB_next as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          let _menhir_s = MenhirState081 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | IDENT _v_2 ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | ARRAY | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | AT | BANG | BOOL_LIT _ | COMMA | CREATE | EOF | EQ | EXEC | EXTERNAL | FIX | FOR | FUN | IF | IMPLY | INT_LIT _ | INT_MAPI | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_FOR | MACRO_GENERATE | MATCH | MINUS | NODE | OPERATOR | OPERATOR_IDENT _ | PIPE | QUESTION_MARK | REF | REGISTER | RESIZE_INT | RIGHT_ARROW | RPAREN | RUN | SEMI_SEMI | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let x = _v in
          let _v = _menhir_action_208 x in
          _menhir_goto_separated_nonempty_list_TIMES_tyB_next_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_TIMES_tyB_next_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_tyB_next as 'stack) -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState079 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState081 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_083 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB_next -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_tyB_next (_menhir_stack, _menhir_s, tyB, _) = _menhir_stack in
      let tyBs = _v in
      let _v = _menhir_action_222 tyB tyBs in
      _menhir_goto_tyB _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
  
  and _menhir_run_082 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_tyB_next, ttv_result) _menhir_cell1_tyB_next -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_tyB_next (_menhir_stack, _menhir_s, x, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_209 x xs in
      _menhir_goto_separated_nonempty_list_TIMES_tyB_next_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
  
  and _menhir_run_078 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ARRAY | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | AT | BANG | BOOL_LIT _ | COMMA | CREATE | EOF | EQ | EXEC | EXTERNAL | FIX | FOR | FUN | IF | IMPLY | INT_LIT _ | INT_MAPI | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_FOR | MACRO_GENERATE | MATCH | MINUS | NODE | OPERATOR | OPERATOR_IDENT _ | PIPE | QUESTION_MARK | REF | REGISTER | RESIZE_INT | RIGHT_ARROW | RPAREN | RUN | SEMI_SEMI | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let tyB = _v in
          let _v = _menhir_action_223 tyB in
          _menhir_goto_tyB _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_071 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ARRAY | AT | EQ | IMPLY | QUESTION_MARK | RIGHT_ARROW | RPAREN | SEMI_SEMI | TIMES ->
          let (_startpos_tyB_, tyB) = (_startpos, _v) in
          let _v = _menhir_action_243 tyB in
          _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_tyB_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_525 : type  ttv_stack. (((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_STATIC _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | HAT ->
          let _menhir_s = MenhirState526 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_460 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_VECTOR_MAPI -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_VECTOR_MAPI (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_010 e in
      _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_452 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_RUN _menhir_cell0_UP_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell0_UP_IDENT (_menhir_stack, i, _, _) = _menhir_stack in
      let MenhirCell1_RUN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_080 e i in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_419 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_REGISTER, ttv_result) _menhir_cell1_exp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_exp (_menhir_stack, _, ev, _, _) = _menhir_stack in
      let MenhirCell1_REGISTER (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e0_, e0) = (_endpos, _v) in
      let _v = _menhir_action_073 _endpos_e0_ _startpos__1_ e0 ev in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e0_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_416 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_REGISTER, ttv_result) _menhir_cell1_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_IDENT (_menhir_stack, _, f, _, _) = _menhir_stack in
      let MenhirCell1_REGISTER (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e0_, e0) = (_endpos, _v) in
      let _v = _menhir_action_074 e0 f in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e0_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_413 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_REF -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_REF (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_038 e in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_402 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_MINUS -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_MINUS (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e1_, e1) = (_endpos, _v) in
      let _v = _menhir_action_050 e1 in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_401 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_INT_MAPI -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_INT_MAPI (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_011 e in
      _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_316 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_aexp, ttv_result) _menhir_cell1_aexp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | VECTOR_MAPI ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | UP_IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState316
      | UNROLL ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | TUPLE_OF_INT ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | STRING_LIT _v_1 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState316
      | SHARP_PIPE_LBRACKET ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | RESIZE_INT ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | OPERATOR_IDENT _v_2 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState316
      | MATCH ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | MACRO_FOR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | LPAREN ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | LCUR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | INT_OF_TUPLE ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | INT_MAPI ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | INT_LIT _v_3 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState316
      | IDENT _v_4 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState316
      | FOR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | BOOL_LIT _v_5 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState316
      | BANG ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | AMP | AMP_AMP | AND | ASR | COL | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
          let (_endpos_x_, x) = (_endpos, _v) in
          let _v = _menhir_action_167 x in
          _menhir_goto_nonempty_list_aexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_nonempty_list_aexp_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_aexp as 'stack) -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState316 ->
          _menhir_run_315 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState294 ->
          _menhir_run_315 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState248 ->
          _menhir_run_293 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_315 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_aexp, ttv_result) _menhir_cell1_aexp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_aexp (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let (_endpos_xs_, xs) = (_endpos, _v) in
      let _v = _menhir_action_168 x xs in
      _menhir_goto_nonempty_list_aexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_xs_ _v _menhir_s _tok
  
  and _menhir_run_293 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_aexp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_aexp (_menhir_stack, _menhir_s, e, _startpos_e_, _) = _menhir_stack in
      let (_endpos_es_, es) = (_endpos, _v) in
      let _v = _menhir_action_049 _endpos_es_ _startpos_e_ e es in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_es_ _startpos_e_ _v _menhir_s _tok
  
  and _menhir_run_294 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_aexp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | VECTOR_MAPI ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | UP_IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState294
      | UNROLL ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | TUPLE_OF_INT ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | STRING_LIT _v_1 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState294
      | SHARP_PIPE_LBRACKET ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | RESIZE_INT ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | OPERATOR_IDENT _v_2 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState294
      | MATCH ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | MACRO_FOR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | LPAREN ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | LCUR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | INT_OF_TUPLE ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | INT_MAPI ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | INT_LIT _v_3 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState294
      | IDENT _v_4 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState294
      | FOR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | BOOL_LIT _v_5 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState294
      | BANG ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | AT_AT ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_stack = MenhirCell1_AT_AT (_menhir_stack, MenhirState294) in
          let _menhir_s = MenhirState295 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_296 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | AT ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_stack = MenhirCell1_AT (_menhir_stack, MenhirState294) in
          let _menhir_s = MenhirState313 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_296 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | AMP | AMP_AMP | AND | ASR | COL | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
          let (_endpos_x_, x) = (_endpos, _v) in
          let _v = _menhir_action_167 x in
          _menhir_goto_nonempty_list_aexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_296 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState296 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TIMES ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | RPAREN ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NEQ ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LXOR ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_296 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LOR ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAND ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | GT ->
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FUN ->
          _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EQ_EQ ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_155 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ASR ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_297 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_FUN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState297 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_205 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_248 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | VECTOR_MAPI ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | UP_IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState248
      | UNROLL ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | TUPLE_OF_INT ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | STRING_LIT _v_1 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState248
      | SHARP_PIPE_LBRACKET ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | RESIZE_INT ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | OPERATOR_IDENT _v_2 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState248
      | MATCH ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | MACRO_FOR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | LPAREN ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | LCUR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | INT_OF_TUPLE ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | INT_MAPI ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | INT_LIT _v_3 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState248
      | IDENT _v_4 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState248
      | FOR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | COL_EQ ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_stack = MenhirCell1_COL_EQ (_menhir_stack, MenhirState248) in
          let _menhir_s = MenhirState249 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | BOOL_LIT _v_11 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_11 MenhirState248
      | BANG ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | AMP | AMP_AMP | AND | ASR | COL | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
          let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_081 e in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_232 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ARRAY_LENGTH -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_ARRAY_LENGTH (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_a_, a) = (_endpos, _v) in
      let _v = _menhir_action_044 _endpos_a_ _startpos__1_ a in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_a_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_230 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_ARRAY_MAKE, ttv_result) _menhir_cell1_size _menhir_cell0_GT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell0_GT (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_size (_menhir_stack, _, sz, _) = _menhir_stack in
      let MenhirCell1_ARRAY_MAKE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_032 _endpos_e_ _startpos__1_ e sz in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_200 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LENGTH -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_LENGTH (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_a_, a) = (_endpos, _v) in
      let _v = _menhir_action_045 _endpos_a_ _startpos__1_ a in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_a_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_186 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_MACRO_GENERATE, ttv_result) _menhir_cell1_aexp, ttv_result) _menhir_cell1_aexp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_aexp (_menhir_stack, _, e_init2, _, _) = _menhir_stack in
      let MenhirCell1_aexp (_menhir_stack, _, ef1, _, _) = _menhir_stack in
      let MenhirCell1_MACRO_GENERATE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_st3_, e_st3) = (_endpos, _v) in
      let _v = _menhir_action_077 _endpos_e_st3_ _startpos__1_ e_init2 e_st3 ef1 in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_st3_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_185 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_MACRO_GENERATE, ttv_result) _menhir_cell1_aexp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState185
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState185
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState185
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | INT_LIT _v_3 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState185
      | IDENT _v_4 ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState185
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | BOOL_LIT _v_5 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState185
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | _ ->
          _eRR ()
  
  and _menhir_run_184 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_MACRO_GENERATE as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState184
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState184
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState184
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState184
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState184
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState184
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState184
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState184
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState184
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState184
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState184
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState184
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState184
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState184
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState184
      | INT_LIT _v_3 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState184
      | IDENT _v_4 ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState184
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState184
      | BOOL_LIT _v_5 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState184
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState184
      | _ ->
          _eRR ()
  
  and _menhir_run_183 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_BANG -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_BANG (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_ex_, ex) = (_endpos, _v) in
      let _v = _menhir_action_003 ex in
      _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_ex_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_310 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_c_, _startpos_c_, c) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_091 c in
      _menhir_goto_avalue _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_c_ _startpos_c_ _v _menhir_s _tok
  
  and _menhir_goto_avalue : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_v_, _startpos_v_, v) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_160 v in
      _menhir_goto_lvalue _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_v_ _startpos_v_ _v _menhir_s _tok
  
  and _menhir_goto_lvalue : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState313 ->
          _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState295 ->
          _menhir_run_312 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState308 ->
          _menhir_run_307 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState305 ->
          _menhir_run_307 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState296 ->
          _menhir_run_304 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_314 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_aexp, ttv_result) _menhir_cell1_aexp, ttv_result) _menhir_cell1_AT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_AT (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_aexp (_menhir_stack, _, e2, _, _) = _menhir_stack in
      let MenhirCell1_aexp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_v_, v) = (_endpos, _v) in
      let _v = _menhir_action_035 e1 e2 v in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_v_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_312 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_aexp, ttv_result) _menhir_cell1_aexp, ttv_result) _menhir_cell1_AT_AT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_AT_AT (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_aexp (_menhir_stack, _, e2, _, _) = _menhir_stack in
      let MenhirCell1_aexp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_v_, v) = (_endpos, _v) in
      let _v = _menhir_action_036 e1 e2 v in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_v_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_307 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lvalue as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_lvalue (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState308 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_296 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let (_endpos_x_, x) = (_endpos, _v) in
          let _v = _menhir_action_194 x in
          _menhir_goto_separated_nonempty_list_COMMA_lvalue_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_lvalue_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lvalue as 'stack) -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s ->
      match _menhir_s with
      | MenhirState308 ->
          _menhir_run_309 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v
      | MenhirState305 ->
          _menhir_run_306 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_309 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lvalue, ttv_result) _menhir_cell1_lvalue -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v ->
      let MenhirCell1_lvalue (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let (_endpos_xs_, xs) = (_endpos, _v) in
      let _v = _menhir_action_195 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_lvalue_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_xs_ _v _menhir_s
  
  and _menhir_run_306 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_lvalue -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v ->
      let MenhirCell1_lvalue (_menhir_stack, _, e, _startpos_e_, _) = _menhir_stack in
      let (_endpos_es_, es) = (_endpos, _v) in
      let _v = _menhir_action_251 e es in
      _menhir_goto_value_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_es_ _startpos_e_ _v
  
  and _menhir_goto_value_desc : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v ->
      let (_endpos_v_, _startpos_v_, v) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_250 _endpos_v_ _startpos_v_ v in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__3_, e) = (_endpos, _v) in
      let _v = _menhir_action_090 e in
      _menhir_goto_avalue _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_304 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_lvalue (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState305 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_296 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_252 e in
          _menhir_goto_value_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v
      | _ ->
          _eRR ()
  
  and _menhir_run_181 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_c_, _startpos_c_, c) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_006 c in
      _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_c_ _startpos_c_ _v _menhir_s _tok
  
  and _menhir_run_167 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_const (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState168 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RCUR ->
          let x = _v in
          let _v = _menhir_action_190 x in
          _menhir_goto_separated_nonempty_list_COMMA_const_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_const_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState168 ->
          _menhir_run_169 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState130 ->
          _menhir_run_165 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_169 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_const -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_const (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_191 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_const_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_165 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LCUR -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LCUR (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__3_, cs) = (_endpos, _v) in
      let _v = _menhir_action_124 cs in
      _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_251 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState251 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_263 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState263 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_271 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState271 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_273 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState273 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_253 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState253 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_275 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell0_MINUS (_menhir_stack, _startpos) in
      let _menhir_s = MenhirState275 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_255 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState255 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_277 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState277 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_265 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState265 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_267 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState267 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_257 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState257 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_279 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState279 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_259 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState259 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_281 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell0_GT (_menhir_stack, _endpos) in
      let _menhir_s = MenhirState281 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_283 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState283 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_285 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState285 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_287 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState287 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_261 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState261 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_269 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState269 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_289 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState289 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_291 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState291 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_app_exp_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState458 ->
          _menhir_run_459 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState017 ->
          _menhir_run_454 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_459 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_app_exp (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_189 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_app_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_454 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_SHARP_PIPE_LBRACKET -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let x = _v in
      let _v = _menhir_action_159 x in
      _menhir_goto_loption_separated_nonempty_list_COMMA_app_exp__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
  
  and _menhir_run_340 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT _menhir_cell0_dot_get as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_271 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_273 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_277 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_279 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_281 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_283 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_285 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_287 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP_AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_289 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | IN | INIT | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell0_dot_get (_menhir_stack, e1, _) = _menhir_stack in
          let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_047 e1 e2 x in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_x_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_332 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_exp _menhir_cell0_RBRACKET as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_271 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_273 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_277 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_279 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_281 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_283 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_285 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_287 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP_AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_289 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | IN | INIT | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell0_RBRACKET (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_exp (_menhir_stack, _, e1, _, _) = _menhir_stack in
          let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_046 e1 e2 x in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_x_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_326 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_EXEC, ttv_result) _menhir_cell1_exp _menhir_cell0_DEFAULT, ttv_result) _menhir_cell1_lexp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_271 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_273 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_277 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_279 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_281 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_283 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_285 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_287 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP_AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_289 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | IN | INIT | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_lexp (_menhir_stack, _, e2, _, _) = _menhir_stack in
          let MenhirCell0_DEFAULT (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_exp (_menhir_stack, _, e1, _, _) = _menhir_stack in
          let MenhirCell1_EXEC (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos_e3_, e3) = (_endpos, _v) in
          let _v = _menhir_action_076 e1 e2 e3 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_292 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_273 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_277 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_279 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_281 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_283 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_285 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_287 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP_AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_289 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | IN | INIT | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_063 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_290 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_273 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_277 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_279 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_281 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_283 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_285 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_287 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP_AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_289 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | IN | INIT | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_071 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_288 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_060 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_286 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_061 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_284 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_059 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_282 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp _menhir_cell0_GT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell0_GT (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_057 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_280 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_058 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_278 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_056 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_276 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp _menhir_cell0_MINUS as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | MINUS | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell0_MINUS (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_052 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_274 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_062 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_272 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_271 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_273 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_277 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_279 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_281 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_283 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_285 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_287 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP_AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_289 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | IN | INIT | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e3_, e3) = (_endpos, _v) in
          let _v = _menhir_action_072 _endpos_e3_ _startpos_e1_ e1 e3 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e3_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_270 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | MINUS | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_070 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_268 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | MINUS | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_068 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_266 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | MINUS | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_069 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_264 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | MINUS | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_051 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_262 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_054 _endpos_e2_ _startpos_e1_ e1 e2 in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_260 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_066 _endpos_e2_ _startpos_e1_ e1 e2 in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_258 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_067 _endpos_e2_ _startpos_e1_ e1 e2 in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_256 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_065 _endpos_e2_ _startpos_e1_ e1 e2 in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_254 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_055 _endpos_e2_ _startpos_e1_ e1 e2 in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_252 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | ASR | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LSL | LSR | LT | MINUS | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_053 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_250 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_aexp, ttv_result) _menhir_cell1_COL_EQ as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_271 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_273 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_277 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_279 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_281 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_283 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_285 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_287 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP_AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_289 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | IN | INIT | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_COL_EQ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_aexp (_menhir_stack, _menhir_s, ex, _startpos_ex_, _) = _menhir_stack in
          let (_endpos_e_, e) = (_endpos, _v) in
          let _v = _menhir_action_037 e ex in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_ex_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_247 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_064 _endpos_e2_ _startpos_e1_ e1 e2 in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_245 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_271 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_273 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_277 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_279 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_281 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_283 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_285 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_287 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP_AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_289 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | IN | INIT | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_148 e in
          _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_lexp_desc : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_147 _endpos_e_ _startpos_e_ e in
      _menhir_goto_lexp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v _menhir_s _tok
  
  and _menhir_goto_lexp : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState020 ->
          _menhir_run_453 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState388 ->
          _menhir_run_387 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState384 ->
          _menhir_run_387 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState382 ->
          _menhir_run_381 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState378 ->
          _menhir_run_381 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState187 ->
          _menhir_run_377 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState344 ->
          _menhir_run_345 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState342 ->
          _menhir_run_343 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_324 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState320 ->
          _menhir_run_319 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_319 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState464 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState555 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState556 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState566 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState561 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState559 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState557 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState540 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState536 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState517 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState449 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState433 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState429 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState426 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState028 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState410 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState408 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState405 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState398 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState396 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState178 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState375 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState373 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState362 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState351 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState198 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState201 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState335 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState203 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState216 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState218 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState299 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState241 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState219 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_453 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_SET -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_SET (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_039 _endpos_e_ _startpos__1_ e in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_387 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PIPE_COMMA_PIPE ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState388 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let x = _v in
          let _v = _menhir_action_204 x in
          _menhir_goto_separated_nonempty_list_PIPE_COMMA_PIPE_lexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_PIPE_COMMA_PIPE_lexp_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState388 ->
          _menhir_run_389 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState384 ->
          _menhir_run_385 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_389 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_lexp (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_205 x xs in
      _menhir_goto_separated_nonempty_list_PIPE_COMMA_PIPE_lexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_385 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_lexp (_menhir_stack, _, e1, _, _) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__5_, es) = (_endpos, _v) in
      let _v = _menhir_action_155 e1 es in
      _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_381 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PIPE_PIPE ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState382 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let x = _v in
          let _v = _menhir_action_206 x in
          _menhir_goto_separated_nonempty_list_PIPE_PIPE_lexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_PIPE_PIPE_lexp_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState382 ->
          _menhir_run_383 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState378 ->
          _menhir_run_379 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_383 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_lexp (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_207 x xs in
      _menhir_goto_separated_nonempty_list_PIPE_PIPE_lexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_379 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_lexp (_menhir_stack, _, e1, _, _) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__5_, es) = (_endpos, _v) in
      let _v = _menhir_action_154 e1 es in
      _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_377 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PIPE_PIPE ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState378 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | PIPE_COMMA_PIPE ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState384 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | COMMA ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_317 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COL | RPAREN ->
          let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_133 e in
          _menhir_goto_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_241 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_lexp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState241 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NODE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_317 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_lexp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState317 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NODE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_exp_desc : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_130 _endpos_e_ _startpos_e_ e in
      _menhir_goto_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v _menhir_s _tok
  
  and _menhir_goto_exp : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState464 ->
          _menhir_run_563 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState555 ->
          _menhir_run_563 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState556 ->
          _menhir_run_563 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState557 ->
          _menhir_run_563 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState559 ->
          _menhir_run_563 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState566 ->
          _menhir_run_563 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState561 ->
          _menhir_run_563 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState540 ->
          _menhir_run_541 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState536 ->
          _menhir_run_537 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState517 ->
          _menhir_run_518 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_462 _menhir_stack _v _tok
      | MenhirState449 ->
          _menhir_run_450 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState433 ->
          _menhir_run_434 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState429 ->
          _menhir_run_430 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState426 ->
          _menhir_run_427 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState028 ->
          _menhir_run_420 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_417 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState410 ->
          _menhir_run_411 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState408 ->
          _menhir_run_409 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_407 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState405 ->
          _menhir_run_406 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState114 ->
          _menhir_run_403 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState398 ->
          _menhir_run_399 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState396 ->
          _menhir_run_397 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState178 ->
          _menhir_run_395 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_390 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState187 ->
          _menhir_run_390 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState375 ->
          _menhir_run_376 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState373 ->
          _menhir_run_374 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState362 ->
          _menhir_run_363 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState351 ->
          _menhir_run_352 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState198 ->
          _menhir_run_347 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState201 ->
          _menhir_run_341 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState335 ->
          _menhir_run_336 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState203 ->
          _menhir_run_329 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState216 ->
          _menhir_run_328 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState218 ->
          _menhir_run_327 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState219 ->
          _menhir_run_322 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState299 ->
          _menhir_run_300 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState241 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_563 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_endpos__2_, _startpos_e_, e) = (_endpos_0, _startpos, _v) in
          let _v = _menhir_action_128 _endpos__2_ _startpos_e_ e in
          _menhir_goto_decl_all _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_decl_all : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let d = _v in
      let _v = _menhir_action_125 d in
      let _menhir_stack = MenhirCell1_decl (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState566
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | TYPE ->
          _menhir_run_465 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState566
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | SHARED ->
          _menhir_run_492 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState566
      | OPERATOR ->
          _menhir_run_498 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | NODE ->
          _menhir_run_514 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | LET ->
          _menhir_run_521 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | INT_LIT _v_3 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState566
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState566
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | EXTERNAL ->
          _menhir_run_549 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | EOF ->
          _menhir_run_554 _menhir_stack _menhir_lexbuf MenhirState566
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | BOOL_LIT _v_5 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState566
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState566
      | _ ->
          _eRR ()
  
  and _menhir_run_541 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_ty_annot_IDENT_ as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ty_annot_IDENT_ (_menhir_stack, _, f_ty_opt, _startpos_f_ty_opt_) = _menhir_stack in
          let MenhirCell1_REC (_menhir_stack, _) = _menhir_stack in
          let (_endpos__5_, _endpos_e1_, e1) = (_endpos_0, _endpos, _v) in
          let _v = _menhir_action_027 _endpos_e1_ _startpos_f_ty_opt_ e1 f_ty_opt in
          _menhir_goto_after_let_SEMI_SEMI_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _v _tok
      | IN ->
          let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_353 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_goto_after_let_SEMI_SEMI_ : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_LET -> _ -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_b_, b) = (_endpos, _v) in
      let _v = _menhir_action_126 _endpos_b_ _startpos__1_ b in
      _menhir_goto_decl_all _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_353 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_REC, ttv_result) _menhir_cell1_ty_annot_IDENT_, ttv_result) _menhir_cell1_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_exp (_menhir_stack, _, e1, _, _endpos_e1_) = _menhir_stack in
      let MenhirCell1_ty_annot_IDENT_ (_menhir_stack, _, f_ty_opt, _startpos_f_ty_opt_) = _menhir_stack in
      let MenhirCell1_REC (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_023 _endpos_e1_ _startpos_f_ty_opt_ e1 f_ty_opt in
      _menhir_goto_after_let_IN_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_after_let_IN_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_after_let_IN_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState375
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState375
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState375
      | NODE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | LET ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | INT_LIT _v_3 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState375
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState375
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | BOOL_LIT _v_5 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState375
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | _ ->
          _eRR ()
  
  and _menhir_run_537 : type  ttv_stack. ((((((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic, _menhir_box_pi) _menhir_cell1_ret_ty_annot_eq as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ret_ty_annot_eq (_menhir_stack, _, ty_opt) = _menhir_stack in
          let MenhirCell1_arg_ty_atomic (_menhir_stack, _, p_ty_opt) = _menhir_stack in
          let MenhirCell1_IDENT (_menhir_stack, _, f, _startpos_f_, _) = _menhir_stack in
          let MenhirCell1_REC (_menhir_stack, _) = _menhir_stack in
          let (_endpos_e1_, e1, _endpos__6_) = (_endpos, _v, _endpos_0) in
          let _v = _menhir_action_144 _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt in
          let _endpos = _endpos__6_ in
          let (_endpos_e_, e) = (_endpos, _v) in
          let _v = _menhir_action_026 e in
          _menhir_goto_after_let_SEMI_SEMI_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _v _tok
      | IN ->
          let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_348 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_348 : type  ttv_stack ttv_result. ((((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_REC, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic, ttv_result) _menhir_cell1_ret_ty_annot_eq, ttv_result) _menhir_cell1_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_exp (_menhir_stack, _, e1, _, _endpos_e1_) = _menhir_stack in
      let MenhirCell1_ret_ty_annot_eq (_menhir_stack, _, ty_opt) = _menhir_stack in
      let MenhirCell1_arg_ty_atomic (_menhir_stack, _, p_ty_opt) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _, f, _startpos_f_, _) = _menhir_stack in
      let MenhirCell1_REC (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_143 _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt in
      let e = _v in
      let _v = _menhir_action_022 e in
      _menhir_goto_after_let_IN_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_518 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic, _menhir_box_pi) _menhir_cell1_ret_ty_annot_eq as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ret_ty_annot_eq (_menhir_stack, _, ty_opt_ret) = _menhir_stack in
          let MenhirCell1_arg_ty_atomic (_menhir_stack, _, p_ty_opt) = _menhir_stack in
          let MenhirCell1_IDENT (_menhir_stack, _menhir_s, f, _startpos_f_, _) = _menhir_stack in
          let (_endpos__5_, _endpos_e1_, e1) = (_endpos_0, _endpos, _v) in
          let _v = _menhir_action_142 _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt_ret in
          _menhir_goto_fun_decl_SEMI_SEMI_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _v _menhir_s _tok
      | IN ->
          let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_404 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_goto_fun_decl_SEMI_SEMI_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState521 ->
          _menhir_run_545 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState514 ->
          _menhir_run_520 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_545 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_LET -> _ -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let (_endpos_b_, b) = (_endpos, _v) in
      let _v = _menhir_action_025 b in
      _menhir_goto_after_let_SEMI_SEMI_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_b_ _v _tok
  
  and _menhir_run_520 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_NODE -> _ -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_NODE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_b_, b) = (_endpos, _v) in
      let _v = _menhir_action_127 _endpos_b_ _startpos__1_ b in
      _menhir_goto_decl_all _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_404 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic, ttv_result) _menhir_cell1_ret_ty_annot_eq, ttv_result) _menhir_cell1_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_exp (_menhir_stack, _, e1, _, _endpos_e1_) = _menhir_stack in
      let MenhirCell1_ret_ty_annot_eq (_menhir_stack, _, ty_opt_ret) = _menhir_stack in
      let MenhirCell1_arg_ty_atomic (_menhir_stack, _, p_ty_opt) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, f, _startpos_f_, _) = _menhir_stack in
      let _v = _menhir_action_141 _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt_ret in
      _menhir_goto_fun_decl_IN_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_fun_decl_IN_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState514 ->
          _menhir_run_405 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_405 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState521 ->
          _menhir_run_365 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState188 ->
          _menhir_run_365 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_405 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_NODE as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_fun_decl_IN_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState405
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState405
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState405
      | NODE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | LET ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | INT_LIT _v_3 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState405
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState405
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | BOOL_LIT _v_5 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState405
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
      | _ ->
          _eRR ()
  
  and _menhir_run_365 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let b = _v in
      let _v = _menhir_action_021 b in
      _menhir_goto_after_let_IN_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_450 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_MATCH, ttv_result) _menhir_cell1_exp _menhir_cell0_option_PIPE_, ttv_result) _menhir_cell1_list_match_case_const_ _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | END ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_IDENT (_menhir_stack, _, _, _) = _menhir_stack in
          let MenhirCell1_list_match_case_const_ (_menhir_stack, _, cases) = _menhir_stack in
          let MenhirCell0_option_PIPE_ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_exp (_menhir_stack, _, e, _, _) = _menhir_stack in
          let MenhirCell1_MATCH (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__9_, otherwise) = (_endpos_0, _v) in
          let _v = _menhir_action_016 cases e otherwise in
          _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__9_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_434 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_separated_nonempty_list_PIPE_const_ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | PIPE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_separated_nonempty_list_PIPE_const_ (_menhir_stack, _menhir_s, cs) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_163 cs e in
          let _menhir_stack = MenhirCell1_match_case_const (_menhir_stack, _menhir_s, _v) in
          (match (_tok : MenhirBasics.token) with
          | UP_IDENT _v_0 ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState438
          | STRING_LIT _v_1 ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState438
          | OPERATOR_IDENT _v_2 ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState438
          | LPAREN ->
              _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState438
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState438
          | INT_LIT _v_3 ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState438
          | BOOL_LIT _v_4 ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState438
          | IDENT _ ->
              let _v_5 = _menhir_action_156 () in
              _menhir_run_439 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_439 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_match_case_const -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_match_case_const (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_157 x xs in
      _menhir_goto_list_match_case_const_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_list_match_case_const_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState423 ->
          _menhir_run_447 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState438 ->
          _menhir_run_439 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_447 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_MATCH, ttv_result) _menhir_cell1_exp _menhir_cell0_option_PIPE_ as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_list_match_case_const_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | IDENT _v_0 ->
          let _v = _v_0 in
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | RIGHT_ARROW ->
              let _menhir_s = MenhirState449 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VECT_CREATE ->
                  _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | VECTOR_MAPI ->
                  _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | UP_IDENT _v ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | UNROLL ->
                  _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TUPLE_OF_INT ->
                  _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | STRING_LIT _v ->
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | SIZE_CREATE ->
                  _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | SHARP_PIPE_LBRACKET ->
                  _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | SET ->
                  _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | RUN ->
                  _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | RESIZE_INT ->
                  _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | REGISTER ->
                  _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | REF ->
                  _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPERATOR_IDENT _v ->
                  _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | NODE ->
                  _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MINUS ->
                  _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MATCH ->
                  _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_GENERATE ->
                  _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_FOR ->
                  _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LET ->
                  _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LENGTH ->
                  _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LCUR ->
                  _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_OF_TUPLE ->
                  _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_MAPI ->
                  _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_LIT _v ->
                  _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IF ->
                  _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FUN ->
                  _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FOR ->
                  _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FIX ->
                  _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | EXEC ->
                  _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CREATE ->
                  _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL_LIT _v ->
                  _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_MAKE ->
                  _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_LENGTH ->
                  _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_CREATE ->
                  _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_430 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_253 _endpos_e_ _startpos_x_ e x in
      let e = _v in
      let _v = _menhir_action_164 e in
      _menhir_goto_match_cases _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_match_cases : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState444 ->
          _menhir_run_446 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState423 ->
          _menhir_run_436 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_446 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_match_case -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_match_case (_menhir_stack, _menhir_s, h) = _menhir_stack in
      let rev_cases = _v in
      let _v = _menhir_action_166 h rev_cases in
      _menhir_goto_match_cases _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_436 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_MATCH, ttv_result) _menhir_cell1_exp _menhir_cell0_option_PIPE_ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | END ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_option_PIPE_ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_exp (_menhir_stack, _, e, _, _) = _menhir_stack in
          let MenhirCell1_MATCH (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__6_, rev_cases) = (_endpos, _v) in
          let _v = _menhir_action_017 e rev_cases in
          _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__6_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_427 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_UP_IDENT, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_apat (_menhir_stack, _, p) = _menhir_stack in
      let MenhirCell1_UP_IDENT (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_162 e p x in
      match (_tok : MenhirBasics.token) with
      | PIPE ->
          let _menhir_stack = MenhirCell1_match_case (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState444 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UP_IDENT _v ->
              let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _menhir_stack = MenhirCell1_UP_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_s = MenhirState445 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LPAREN ->
                  _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | IDENT _v ->
              _menhir_run_428 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | END ->
          let h = _v in
          let _v = _menhir_action_165 h in
          _menhir_goto_match_cases _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_428 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW ->
          let _menhir_s = MenhirState429 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_420 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_MATCH as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | PIPE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let x = () in
              let _v = _menhir_action_170 x in
              _menhir_goto_option_PIPE_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BOOL_LIT _ | IDENT _ | INT_LIT _ | LCUR | LPAREN | OPERATOR_IDENT _ | STRING_LIT _ | UP_IDENT _ ->
              let _v = _menhir_action_169 () in
              _menhir_goto_option_PIPE_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_option_PIPE_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_MATCH, ttv_result) _menhir_cell1_exp -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_option_PIPE_ (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | UP_IDENT _v_0 ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _menhir_stack = MenhirCell1_UP_IDENT (_menhir_stack, MenhirState423, _v_0, _startpos, _endpos) in
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState424
          | IDENT _v_1 ->
              let _menhir_stack = MenhirCell1_UP_IDENT (_menhir_stack, MenhirState423, _v_0, _startpos, _endpos) in
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState424
          | PIPE | RIGHT_ARROW ->
              let _v_2 =
                let x = _v_0 in
                _menhir_action_103 x
              in
              _menhir_run_440 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v_2 MenhirState423 _tok
          | _ ->
              _eRR ())
      | STRING_LIT _v_3 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState423
      | OPERATOR_IDENT _v_4 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState423
      | LPAREN ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState423
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState423
      | INT_LIT _v_5 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState423
      | IDENT _v_6 ->
          _menhir_run_428 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState423
      | BOOL_LIT _v_7 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState423
      | _ ->
          _eRR ()
  
  and _menhir_run_417 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_REGISTER as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | INIT ->
          let _menhir_s = MenhirState418 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_411 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_MACRO_FOR _menhir_cell0_IDENT, ttv_result) _menhir_cell1_exp, ttv_result) _menhir_cell1_exp -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | DONE ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_exp (_menhir_stack, _, e_st2, _, _) = _menhir_stack in
          let MenhirCell1_exp (_menhir_stack, _, e_st1, _, _) = _menhir_stack in
          let MenhirCell0_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
          let MenhirCell1_MACRO_FOR (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (e, _endpos__9_) = (_v, _endpos_0) in
          let _v = _menhir_action_019 _endpos__9_ _startpos__1_ e e_st1 e_st2 x in
          _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__9_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_409 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_MACRO_FOR _menhir_cell0_IDENT, ttv_result) _menhir_cell1_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | DO ->
          let _menhir_s = MenhirState410 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_407 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_MACRO_FOR _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | TO ->
          let _menhir_s = MenhirState408 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_406 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_NODE, ttv_result) _menhir_cell1_fun_decl_IN_ -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_fun_decl_IN_ (_menhir_stack, _, b) = _menhir_stack in
      let MenhirCell1_NODE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_153 b e2 in
      _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_403 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic, ttv_result) _menhir_cell1_ret_ty_annot_eq as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | IN ->
          _menhir_run_404 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_399 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_FOR _menhir_cell0_IDENT, ttv_result) _menhir_cell1_exp, ttv_result) _menhir_cell1_exp -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | DONE ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_exp (_menhir_stack, _, e2, _, _) = _menhir_stack in
          let MenhirCell1_exp (_menhir_stack, _, e1, _, _) = _menhir_stack in
          let MenhirCell0_IDENT (_menhir_stack, i, _, _) = _menhir_stack in
          let MenhirCell1_FOR (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (e, _endpos__9_) = (_v, _endpos_0) in
          let _v = _menhir_action_018 e e1 e2 i in
          _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__9_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_397 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_FOR _menhir_cell0_IDENT, ttv_result) _menhir_cell1_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | DO ->
          let _menhir_s = MenhirState398 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_395 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FOR _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | TO ->
          let _menhir_s = MenhirState396 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_390 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__3_, e) = (_endpos_0, _v) in
          let _v = _menhir_action_004 e in
          _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | COL ->
          let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState392 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVAR_IDENT _v ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_376 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_after_let_IN_ -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_after_let_IN_ (_menhir_stack, _, b) = _menhir_stack in
      let MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_152 b e2 in
      _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_374 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_apat (_menhir_stack, _menhir_s, p) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_093 e p in
      _menhir_goto_binding_apat_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_binding_apat_exp_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | AND ->
          let _menhir_stack = MenhirCell1_binding_apat_exp_ (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState370 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_354 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | IN | SEMI_SEMI ->
          let b = _v in
          let _v = _menhir_action_095 b in
          _menhir_goto_bindings_and_apat_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_bindings_and_apat_exp_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState370 ->
          _menhir_run_371 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState521 ->
          _menhir_run_366 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState188 ->
          _menhir_run_366 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_371 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_binding_apat_exp_ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_binding_apat_exp_ (_menhir_stack, _menhir_s, b1) = _menhir_stack in
      let bs = _v in
      let _v = _menhir_action_096 b1 bs in
      _menhir_goto_bindings_and_apat_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_366 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let w = _v in
      let _v = _menhir_action_094 w in
      _menhir_goto_bindings_apat_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_bindings_apat_exp_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState521 ->
          _menhir_run_546 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState188 ->
          _menhir_run_367 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_546 : type  ttv_stack. ((ttv_stack, _menhir_box_pi) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (b, _endpos__2_) = (_v, _endpos) in
          let _v = _menhir_action_024 b in
          _menhir_goto_after_let_SEMI_SEMI_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _v _tok
      | IN ->
          let _menhir_stack = MenhirCell1_bindings_apat_exp_ (_menhir_stack, _menhir_s, _v) in
          _menhir_run_368 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_368 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_bindings_apat_exp_ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_bindings_apat_exp_ (_menhir_stack, _menhir_s, b) = _menhir_stack in
      let _v = _menhir_action_020 b in
      _menhir_goto_after_let_IN_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_367 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_bindings_apat_exp_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | IN ->
          _menhir_run_368 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_363 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_annot_apat_ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty_annot_apat_ (_menhir_stack, _menhir_s, p_ty_opt) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_092 e p_ty_opt in
      _menhir_goto_binding_apat_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_352 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_REC, ttv_result) _menhir_cell1_ty_annot_IDENT_ as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | IN ->
          _menhir_run_353 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_347 : type  ttv_stack ttv_result. ((((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_REC, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic, ttv_result) _menhir_cell1_ret_ty_annot_eq as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | IN ->
          _menhir_run_348 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_341 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | THEN ->
          let _menhir_s = MenhirState342 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_336 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_IDENT _menhir_cell0_LPAREN -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | AMP | AMP_AMP | AND | ASR | COL | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
              let MenhirCell0_LPAREN (_menhir_stack, _) = _menhir_stack in
              let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
              let (_endpos__5_, e1) = (_endpos_0, _v) in
              let _v = _menhir_action_041 e1 x in
              _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _startpos_x_ _v _menhir_s _tok
          | LEFT_ARROW ->
              let MenhirCell0_LPAREN (_menhir_stack, _) = _menhir_stack in
              let (_endpos__4_, e) = (_endpos_0, _v) in
              let _v = _menhir_action_129 e in
              let _endpos = _endpos__4_ in
              let _menhir_stack = MenhirCell0_dot_get (_menhir_stack, _v, _endpos) in
              let _menhir_s = MenhirState339 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VECT_CREATE ->
                  _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | VECTOR_MAPI ->
                  _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | UP_IDENT _v ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | UNROLL ->
                  _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TUPLE_OF_INT ->
                  _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | STRING_LIT _v ->
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | SIZE_CREATE ->
                  _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | SHARP_PIPE_LBRACKET ->
                  _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | SET ->
                  _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | RUN ->
                  _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | RESIZE_INT ->
                  _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | REGISTER ->
                  _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | REF ->
                  _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPERATOR_IDENT _v ->
                  _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | MINUS ->
                  _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MATCH ->
                  _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_GENERATE ->
                  _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_FOR ->
                  _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LENGTH ->
                  _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LCUR ->
                  _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_OF_TUPLE ->
                  _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_MAPI ->
                  _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_LIT _v ->
                  _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IDENT _v ->
                  _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FOR ->
                  _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | EXEC ->
                  _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CREATE ->
                  _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL_LIT _v ->
                  _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_MAKE ->
                  _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_LENGTH ->
                  _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_CREATE ->
                  _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_329 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RBRACKET ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LEFT_ARROW ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_RBRACKET (_menhir_stack, _endpos_0) in
              let _menhir_s = MenhirState331 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VECT_CREATE ->
                  _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | VECTOR_MAPI ->
                  _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | UP_IDENT _v ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | UNROLL ->
                  _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TUPLE_OF_INT ->
                  _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | STRING_LIT _v ->
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | SIZE_CREATE ->
                  _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | SHARP_PIPE_LBRACKET ->
                  _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | SET ->
                  _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | RUN ->
                  _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | RESIZE_INT ->
                  _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | REGISTER ->
                  _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | REF ->
                  _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPERATOR_IDENT _v ->
                  _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | MINUS ->
                  _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MATCH ->
                  _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_GENERATE ->
                  _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_FOR ->
                  _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LENGTH ->
                  _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LCUR ->
                  _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_OF_TUPLE ->
                  _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_MAPI ->
                  _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_LIT _v ->
                  _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IDENT _v ->
                  _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FOR ->
                  _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | EXEC ->
                  _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CREATE ->
                  _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL_LIT _v ->
                  _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_MAKE ->
                  _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_LENGTH ->
                  _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_CREATE ->
                  _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | AMP | AMP_AMP | AND | ASR | COL | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
              let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
              let (_endpos__4_, e1) = (_endpos_0, _v) in
              let _v = _menhir_action_040 e1 x in
              _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__4_ _startpos_x_ _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_328 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_arg_ty -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_arg_ty (_menhir_stack, _, p_ty_opt) = _menhir_stack in
      let MenhirCell1_FUN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_150 e p_ty_opt in
      _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_327 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_FIX _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell0_IDENT (_menhir_stack, f, _, _) = _menhir_stack in
      let MenhirCell1_FIX (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_149 _endpos_e_ _startpos__1_ e f in
      _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_322 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_EXEC as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | DEFAULT ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | VECTOR_MAPI ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | UP_IDENT _v_1 ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState323
          | UNROLL ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | TUPLE_OF_INT ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | STRING_LIT _v_2 ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState323
          | SIZE_CREATE ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | SHARP_PIPE_LBRACKET ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | SET ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | RUN ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | RESIZE_INT ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | REGISTER ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | REF ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | OPERATOR_IDENT _v_3 ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState323
          | NODE ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | MINUS ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | MATCH ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | MACRO_GENERATE ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | MACRO_FOR ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | LPAREN ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | LET ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | LENGTH ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | LCUR ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | INT_OF_TUPLE ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | INT_MAPI ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | INT_LIT _v_4 ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState323
          | IF ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | IDENT _v_5 ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState323
          | FUN ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | FOR ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | FIX ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | EXEC ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | CREATE ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | BOOL_LIT _v_6 ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState323
          | BANG ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | ARRAY_MAKE ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | ARRAY_LENGTH ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | ARRAY_CREATE ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | AMP | AMP_AMP | AND | ASR | COL | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
              let MenhirCell1_EXEC (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
              let (_endpos__3_, e1) = (_endpos_0, _v) in
              let _v = _menhir_action_079 _endpos__3_ _startpos__1_ e1 in
              _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
          | _ ->
              _eRR ())
      | AMP | AMP_AMP | AND | ASR | COL | COMMA | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
          let MenhirCell1_EXEC (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos_e1_, e1) = (_endpos, _v) in
          let _v = _menhir_action_078 _endpos_e1_ _startpos__1_ e1 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e1_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_300 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_arg_ty -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_arg_ty (_menhir_stack, _, p_ty_opt) = _menhir_stack in
      let MenhirCell1_FUN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_161 e p_ty_opt in
      _menhir_goto_lvalue _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_243 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_lexp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_131 e1 e2 in
      _menhir_goto_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_345 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IF, ttv_result) _menhir_cell1_exp, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_lexp (_menhir_stack, _, e2, _, _) = _menhir_stack in
      let (_endpos_e3_, e3) = (_endpos, _v) in
      let _v = _menhir_action_146 e2 e3 in
      _menhir_goto_if_end _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e3_ _v _tok
  
  and _menhir_goto_if_end : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IF, ttv_result) _menhir_cell1_exp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_exp (_menhir_stack, _, e1, _, _) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e2_e3_, e2_e3) = (_endpos, _v) in
      let _v = _menhir_action_151 e1 e2_e3 in
      _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_e3_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_343 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IF, ttv_result) _menhir_cell1_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState344 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AMP | AMP_AMP | AND | ASR | COL | COMMA | DEFAULT | DIV | DO | DONE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_145 e2 in
          _menhir_goto_if_end _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_324 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_EXEC, ttv_result) _menhir_cell1_exp _menhir_cell0_DEFAULT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RESET ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState325 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AMP | AMP_AMP | AND | ASR | COL | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
          let MenhirCell0_DEFAULT (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_exp (_menhir_stack, _, e1, _, _) = _menhir_stack in
          let MenhirCell1_EXEC (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_075 e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_319 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState320 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SIZE_CREATE ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SET ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RUN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REGISTER ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NODE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AMP | AMP_AMP | AND | ASR | COL | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
          let (_endpos_x_, x) = (_endpos, _v) in
          let _v = _menhir_action_192 x in
          _menhir_goto_separated_nonempty_list_COMMA_lexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_lexp_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp as 'stack) -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState320 ->
          _menhir_run_321 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState317 ->
          _menhir_run_318 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_321 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_lexp (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let (_endpos_xs_, xs) = (_endpos, _v) in
      let _v = _menhir_action_193 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_lexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_xs_ _v _menhir_s _tok
  
  and _menhir_run_318 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_lexp (_menhir_stack, _menhir_s, e, _startpos_e_, _) = _menhir_stack in
      let (_endpos_es_, es) = (_endpos, _v) in
      let _v = _menhir_action_132 e es in
      _menhir_goto_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_es_ _startpos_e_ _v _menhir_s _tok
  
  and _menhir_run_240 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_317 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | ASR | COL | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
          let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_133 e in
          _menhir_goto_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_228 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_ARRAY_MAKE as 'stack) -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_size (_menhir_stack, _menhir_s, _v, _endpos) in
      match (_tok : MenhirBasics.token) with
      | GT ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_GT (_menhir_stack, _endpos) in
          let _menhir_s = MenhirState229 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VECTOR_MAPI ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNROLL ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUPLE_OF_INT ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SHARP_PIPE_LBRACKET ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RESIZE_INT ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MATCH ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_FOR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_MAPI ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_222 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_CREATE -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | GT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | RPAREN ->
                  let _endpos_1 = _menhir_lexbuf.Lexing.lex_curr_p in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let MenhirCell1_CREATE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
                  let (sz, _endpos__6_) = (_v, _endpos_1) in
                  let _v = _menhir_action_034 _endpos__6_ _startpos__1_ sz in
                  _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__6_ _startpos__1_ _v _menhir_s _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_101 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | GT ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ty_next (_menhir_stack, _menhir_s, ty_tyB, _startpos_ty_tyB_) = _menhir_stack in
          let (sz, _endpos__5_) = (_v, _endpos_0) in
          let _v = _menhir_action_239 _endpos__5_ _startpos_ty_tyB_ sz ty_tyB in
          _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_ty_tyB_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_066 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let (_endpos_sz_, sz) = (_endpos, _v) in
      let _v = _menhir_action_214 sz in
      _menhir_goto_size_list _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_sz_ _v _menhir_s _tok
  
  and _menhir_goto_size_list : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState091 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState072 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState466 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState058 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_092 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_separated_nonempty_list_COMMA_tyB_ _menhir_cell0_RPAREN _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell0_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
      let MenhirCell0_RPAREN (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_separated_nonempty_list_COMMA_tyB_ (_menhir_stack, _, tyBs) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_szs_, szs) = (_endpos, _v) in
      let _v = _menhir_action_230 _endpos_szs_ _startpos__1_ szs tyBs x in
      _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_073 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB_next _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell0_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
      let MenhirCell1_tyB_next (_menhir_stack, _menhir_s, tyB, _startpos_tyB_) = _menhir_stack in
      let (_endpos_szs_, szs) = (_endpos, _v) in
      let _v = _menhir_action_229 _endpos_szs_ _startpos_tyB_ szs tyB x in
      _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_tyB_ _v _menhir_s _tok
  
  and _menhir_run_065 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
      let (_endpos_szs_, szs) = (_endpos, _v) in
      let _v = _menhir_action_228 _endpos_szs_ _startpos_x_ szs x in
      _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_x_ _v _menhir_s _tok
  
  and _menhir_run_060 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_size (_menhir_stack, _menhir_s, _v, _endpos) in
          let _menhir_s = MenhirState061 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TVAR_IDENT _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INT_LIT _v ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | GT ->
          let x = _v in
          let _v = _menhir_action_196 x in
          _menhir_goto_separated_nonempty_list_COMMA_size_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_size_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState059 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState061 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_063 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LT -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LT (_menhir_stack, _menhir_s) = _menhir_stack in
      let (_endpos__3_, szs) = (_endpos, _v) in
      let _v = _menhir_action_215 szs in
      _menhir_goto_size_list _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _v _menhir_s _tok
  
  and _menhir_run_062 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_size -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_size (_menhir_stack, _menhir_s, x, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_197 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_size_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_056 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LT -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | GT ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LT (_menhir_stack, _menhir_s) = _menhir_stack in
          let (_endpos, sz) = (_endpos_0, _v) in
          let _v = _menhir_action_221 sz in
          _menhir_goto_tyB _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_025 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_RESIZE_INT -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | GT ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_RESIZE_INT (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (k, _endpos__4_) = (_v, _endpos_0) in
          let _v = _menhir_action_007 k in
          _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__4_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_005 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_VECT_CREATE -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | GT ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_VECT_CREATE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__4_, l) = (_endpos_0, _v) in
          let _v = _menhir_action_013 l in
          _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__4_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_exp_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState000 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NODE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  let _menhir_run_464 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState464 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TYPE ->
          _menhir_run_465 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARED ->
          _menhir_run_492 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | OPERATOR ->
          _menhir_run_498 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NODE ->
          _menhir_run_514 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_FOR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_521 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_MAPI ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXTERNAL ->
          _menhir_run_549 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EOF ->
          _menhir_run_554 _menhir_stack _menhir_lexbuf _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
end

let pi =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_pi v = _menhir_run_464 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

let exp_eof =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_exp_eof v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
