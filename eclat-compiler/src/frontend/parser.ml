
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
    | PAUSE
    | PARFOR
    | OR
    | OPERATOR_IDENT of (
# 91 "src/frontend/parser.mly"
       (string)
# 81 "src/frontend/parser.ml"
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
# 112 "src/frontend/parser.ml"
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
# 125 "src/frontend/parser.ml"
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
# 155 "src/frontend/parser.ml"
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
    match Types.canon_ty ty with
    | Ty_fun(Ty_base(TyB_tuple ts),_,_) -> List.length ts
    | Ty_fun(Ty_base _,_,_) -> 1
    | _ -> assert false (* todo *)


# 243 "src/frontend/parser.ml"

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

  | MenhirState029 : (('s, 'r) _menhir_cell1_PARFOR _menhir_cell0_IDENT, 'r) _menhir_state
    (** State 029.
        Stack shape : PARFOR IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState030 : (('s, 'r) _menhir_cell1_REGISTER, 'r) _menhir_state
    (** State 030.
        Stack shape : REGISTER.
        Start symbol: <undetermined>. *)

  | MenhirState031 : (('s, 'r) _menhir_cell1_REF, 'r) _menhir_state
    (** State 031.
        Stack shape : REF.
        Start symbol: <undetermined>. *)

  | MenhirState033 : (('s, 'r) _menhir_cell1_MATCH, 'r) _menhir_state
    (** State 033.
        Stack shape : MATCH.
        Start symbol: <undetermined>. *)

  | MenhirState034 : (('s, 'r) _menhir_cell1_PAUSE, 'r) _menhir_state
    (** State 034.
        Stack shape : PAUSE.
        Start symbol: <undetermined>. *)

  | MenhirState035 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 035.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState047 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_MINUS, 'r) _menhir_state
    (** State 047.
        Stack shape : LPAREN MINUS.
        Start symbol: <undetermined>. *)

  | MenhirState049 : (('s, 'r) _menhir_cell1_LCUR, 'r) _menhir_state
    (** State 049.
        Stack shape : LCUR.
        Start symbol: <undetermined>. *)

  | MenhirState050 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 050.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState051 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_MINUS, 'r) _menhir_state
    (** State 051.
        Stack shape : LPAREN MINUS.
        Start symbol: <undetermined>. *)

  | MenhirState087 : (('s, 'r) _menhir_cell1_const, 'r) _menhir_state
    (** State 087.
        Stack shape : const.
        Start symbol: <undetermined>. *)

  | MenhirState096 : (('s, 'r) _menhir_cell1_FOR _menhir_cell0_IDENT, 'r) _menhir_state
    (** State 096.
        Stack shape : FOR IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState097 : (('s, 'r) _menhir_cell1_MINUS, 'r) _menhir_state
    (** State 097.
        Stack shape : MINUS.
        Start symbol: <undetermined>. *)

  | MenhirState098 : (('s, 'r) _menhir_cell1_BANG, 'r) _menhir_state
    (** State 098.
        Stack shape : BANG.
        Start symbol: <undetermined>. *)

  | MenhirState103 : (('s, 'r) _menhir_cell1_MACRO_GENERATE, 'r) _menhir_state
    (** State 103.
        Stack shape : MACRO_GENERATE.
        Start symbol: <undetermined>. *)

  | MenhirState104 : ((('s, 'r) _menhir_cell1_MACRO_GENERATE, 'r) _menhir_cell1_aexp, 'r) _menhir_state
    (** State 104.
        Stack shape : MACRO_GENERATE aexp.
        Start symbol: <undetermined>. *)

  | MenhirState105 : (((('s, 'r) _menhir_cell1_MACRO_GENERATE, 'r) _menhir_cell1_aexp, 'r) _menhir_cell1_aexp, 'r) _menhir_state
    (** State 105.
        Stack shape : MACRO_GENERATE aexp aexp.
        Start symbol: <undetermined>. *)

  | MenhirState107 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 107.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState108 : (('s, 'r) _menhir_cell1_LET, 'r) _menhir_state
    (** State 108.
        Stack shape : LET.
        Start symbol: <undetermined>. *)

  | MenhirState109 : ((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_REC, 'r) _menhir_state
    (** State 109.
        Stack shape : LET REC.
        Start symbol: <undetermined>. *)

  | MenhirState110 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 110.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState111 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 111.
        Stack shape : LPAREN IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState112 : ((('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_COL, 'r) _menhir_state
    (** State 112.
        Stack shape : IDENT COL.
        Start symbol: <undetermined>. *)

  | MenhirState115 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 115.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState116 : (('s, 'r) _menhir_cell1_LT, 'r) _menhir_state
    (** State 116.
        Stack shape : LT.
        Start symbol: <undetermined>. *)

  | MenhirState119 : (('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 119.
        Stack shape : IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState120 : (('s, 'r) _menhir_cell1_LT, 'r) _menhir_state
    (** State 120.
        Stack shape : LT.
        Start symbol: <undetermined>. *)

  | MenhirState122 : (('s, 'r) _menhir_cell1_size, 'r) _menhir_state
    (** State 122.
        Stack shape : size.
        Start symbol: <undetermined>. *)

  | MenhirState129 : (('s, 'r) _menhir_cell1_ty_next, 'r) _menhir_state
    (** State 129.
        Stack shape : ty_next.
        Start symbol: <undetermined>. *)

  | MenhirState131 : ((('s, 'r) _menhir_cell1_ty_next, 'r) _menhir_cell1_ty_next, 'r) _menhir_state
    (** State 131.
        Stack shape : ty_next ty_next.
        Start symbol: <undetermined>. *)

  | MenhirState134 : (('s, 'r) _menhir_cell1_tyB_next _menhir_cell0_IDENT, 'r) _menhir_state
    (** State 134.
        Stack shape : tyB_next IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState137 : (('s, 'r) _menhir_cell1_tyB_next, 'r) _menhir_state
    (** State 137.
        Stack shape : tyB_next.
        Start symbol: <undetermined>. *)

  | MenhirState142 : (('s, 'r) _menhir_cell1_ty_next, 'r) _menhir_state
    (** State 142.
        Stack shape : ty_next.
        Start symbol: <undetermined>. *)

  | MenhirState144 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 144.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState146 : (('s, 'r) _menhir_cell1_tyB_next, 'r) _menhir_state
    (** State 146.
        Stack shape : tyB_next.
        Start symbol: <undetermined>. *)

  | MenhirState148 : ((('s, 'r) _menhir_cell1_tyB_next, 'r) _menhir_cell1_tyB_next, 'r) _menhir_state
    (** State 148.
        Stack shape : tyB_next tyB_next.
        Start symbol: <undetermined>. *)

  | MenhirState154 : (('s, 'r) _menhir_cell1_tyB, 'r) _menhir_state
    (** State 154.
        Stack shape : tyB.
        Start symbol: <undetermined>. *)

  | MenhirState159 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_separated_nonempty_list_COMMA_tyB_ _menhir_cell0_RPAREN _menhir_cell0_IDENT, 'r) _menhir_state
    (** State 159.
        Stack shape : LPAREN separated_nonempty_list(COMMA,tyB) RPAREN IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState163 : (('s, 'r) _menhir_cell1_ty_next, 'r) _menhir_state
    (** State 163.
        Stack shape : ty_next.
        Start symbol: <undetermined>. *)

  | MenhirState165 : (('s, 'r) _menhir_cell1_ty_next, 'r) _menhir_state
    (** State 165.
        Stack shape : ty_next.
        Start symbol: <undetermined>. *)

  | MenhirState174 : (((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_REC, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 174.
        Stack shape : LET REC IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState175 : ((('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 175.
        Stack shape : IDENT LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState177 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 177.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState182 : (('s, 'r) _menhir_cell1_apat, 'r) _menhir_state
    (** State 182.
        Stack shape : apat.
        Start symbol: <undetermined>. *)

  | MenhirState185 : ((('s, 'r) _menhir_cell1_apat, 'r) _menhir_cell1_apat, 'r) _menhir_state
    (** State 185.
        Stack shape : apat apat.
        Start symbol: <undetermined>. *)

  | MenhirState190 : (((('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_apat, 'r) _menhir_state
    (** State 190.
        Stack shape : IDENT LPAREN apat.
        Start symbol: <undetermined>. *)

  | MenhirState193 : ((((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_REC, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_arg_ty_atomic, 'r) _menhir_state
    (** State 193.
        Stack shape : LET REC IDENT arg_ty_atomic.
        Start symbol: <undetermined>. *)

  | MenhirState195 : (((('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_arg_ty_atomic, 'r) _menhir_cell1_COL, 'r) _menhir_state
    (** State 195.
        Stack shape : IDENT arg_ty_atomic COL.
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

  | MenhirState361 : (((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_arg_ty_atomic, 'r) _menhir_state
    (** State 361.
        Stack shape : LET IDENT arg_ty_atomic.
        Start symbol: <undetermined>. *)

  | MenhirState362 : ((((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_arg_ty_atomic, 'r) _menhir_cell1_ret_ty_annot_eq, 'r) _menhir_state
    (** State 362.
        Stack shape : LET IDENT arg_ty_atomic ret_ty_annot_eq.
        Start symbol: <undetermined>. *)

  | MenhirState366 : (('s, 'r) _menhir_cell1_ty_annot_apat_, 'r) _menhir_state
    (** State 366.
        Stack shape : ty_annot(apat).
        Start symbol: <undetermined>. *)

  | MenhirState374 : (('s, 'r) _menhir_cell1_binding_apat_exp_, 'r) _menhir_state
    (** State 374.
        Stack shape : binding(apat,exp).
        Start symbol: <undetermined>. *)

  | MenhirState377 : (('s, 'r) _menhir_cell1_apat, 'r) _menhir_state
    (** State 377.
        Stack shape : apat.
        Start symbol: <undetermined>. *)

  | MenhirState379 : ((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_after_let_IN_, 'r) _menhir_state
    (** State 379.
        Stack shape : LET after_let(IN).
        Start symbol: <undetermined>. *)

  | MenhirState382 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 382.
        Stack shape : LPAREN lexp.
        Start symbol: <undetermined>. *)

  | MenhirState386 : ((('s, 'r) _menhir_cell1_lexp, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 386.
        Stack shape : lexp lexp.
        Start symbol: <undetermined>. *)

  | MenhirState388 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 388.
        Stack shape : LPAREN lexp.
        Start symbol: <undetermined>. *)

  | MenhirState392 : ((('s, 'r) _menhir_cell1_lexp, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 392.
        Stack shape : lexp lexp.
        Start symbol: <undetermined>. *)

  | MenhirState396 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 396.
        Stack shape : LPAREN exp.
        Start symbol: <undetermined>. *)

  | MenhirState400 : ((('s, 'r) _menhir_cell1_FOR _menhir_cell0_IDENT, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 400.
        Stack shape : FOR IDENT exp.
        Start symbol: <undetermined>. *)

  | MenhirState402 : (((('s, 'r) _menhir_cell1_FOR _menhir_cell0_IDENT, 'r) _menhir_cell1_exp, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 402.
        Stack shape : FOR IDENT exp exp.
        Start symbol: <undetermined>. *)

  | MenhirState409 : ((('s, 'r) _menhir_cell1_MATCH, 'r) _menhir_cell1_exp _menhir_cell0_option_PIPE_, 'r) _menhir_state
    (** State 409.
        Stack shape : MATCH exp option(PIPE).
        Start symbol: <undetermined>. *)

  | MenhirState410 : (((('s, 'r) _menhir_cell1_MATCH, 'r) _menhir_cell1_exp _menhir_cell0_option_PIPE_, 'r) _menhir_cell1_UP_IDENT, 'r) _menhir_state
    (** State 410.
        Stack shape : MATCH exp option(PIPE) UP_IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState412 : ((('s, 'r) _menhir_cell1_UP_IDENT, 'r) _menhir_cell1_apat, 'r) _menhir_state
    (** State 412.
        Stack shape : UP_IDENT apat.
        Start symbol: <undetermined>. *)

  | MenhirState415 : (('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 415.
        Stack shape : IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState419 : (('s, 'r) _menhir_cell1_separated_nonempty_list_PIPE_const_, 'r) _menhir_state
    (** State 419.
        Stack shape : separated_nonempty_list(PIPE,const).
        Start symbol: <undetermined>. *)

  | MenhirState422 : (((('s, 'r) _menhir_cell1_MATCH, 'r) _menhir_cell1_exp _menhir_cell0_option_PIPE_, 'r) _menhir_cell1_match_cases, 'r) _menhir_state
    (** State 422.
        Stack shape : MATCH exp option(PIPE) match_cases.
        Start symbol: <undetermined>. *)

  | MenhirState425 : (('s, 'r) _menhir_cell1_match_case_const, 'r) _menhir_state
    (** State 425.
        Stack shape : match_case_const.
        Start symbol: <undetermined>. *)

  | MenhirState428 : (('s, 'r) _menhir_cell1_const, 'r) _menhir_state
    (** State 428.
        Stack shape : const.
        Start symbol: <undetermined>. *)

  | MenhirState431 : (('s, 'r) _menhir_cell1_match_case, 'r) _menhir_state
    (** State 431.
        Stack shape : match_case.
        Start symbol: <undetermined>. *)

  | MenhirState432 : ((('s, 'r) _menhir_cell1_match_case, 'r) _menhir_cell1_UP_IDENT, 'r) _menhir_state
    (** State 432.
        Stack shape : match_case UP_IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState436 : (((('s, 'r) _menhir_cell1_MATCH, 'r) _menhir_cell1_exp _menhir_cell0_option_PIPE_, 'r) _menhir_cell1_list_match_case_const_ _menhir_cell0_IDENT, 'r) _menhir_state
    (** State 436.
        Stack shape : MATCH exp option(PIPE) list(match_case_const) IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState437 : ((((('s, 'r) _menhir_cell1_MATCH, 'r) _menhir_cell1_exp _menhir_cell0_option_PIPE_, 'r) _menhir_cell1_list_match_case_const_ _menhir_cell0_IDENT, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 437.
        Stack shape : MATCH exp option(PIPE) list(match_case_const) IDENT exp.
        Start symbol: <undetermined>. *)

  | MenhirState441 : ((('s, 'r) _menhir_cell1_REGISTER, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 441.
        Stack shape : REGISTER IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState444 : ((('s, 'r) _menhir_cell1_REGISTER, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 444.
        Stack shape : REGISTER exp.
        Start symbol: <undetermined>. *)

  | MenhirState447 : ((('s, 'r) _menhir_cell1_PARFOR _menhir_cell0_IDENT, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 447.
        Stack shape : PARFOR IDENT exp.
        Start symbol: <undetermined>. *)

  | MenhirState449 : (((('s, 'r) _menhir_cell1_PARFOR _menhir_cell0_IDENT, 'r) _menhir_cell1_exp, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 449.
        Stack shape : PARFOR IDENT exp exp.
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

  | MenhirState514 : (('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_state
    (** State 514.
        Stack shape : LET.
        Start symbol: pi. *)

  | MenhirState517 : ((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_STATIC _menhir_cell0_IDENT, _menhir_box_pi) _menhir_state
    (** State 517.
        Stack shape : LET STATIC IDENT.
        Start symbol: pi. *)

  | MenhirState519 : (((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_STATIC _menhir_cell0_IDENT, _menhir_box_pi) _menhir_cell1_aexp, _menhir_box_pi) _menhir_state
    (** State 519.
        Stack shape : LET STATIC IDENT aexp.
        Start symbol: pi. *)

  | MenhirState523 : ((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_STATIC _menhir_cell0_IDENT, _menhir_box_pi) _menhir_state
    (** State 523.
        Stack shape : LET STATIC IDENT.
        Start symbol: pi. *)

  | MenhirState526 : ((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_state
    (** State 526.
        Stack shape : LET REC.
        Start symbol: pi. *)

  | MenhirState527 : (((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_state
    (** State 527.
        Stack shape : LET REC IDENT.
        Start symbol: pi. *)

  | MenhirState528 : ((((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic, _menhir_box_pi) _menhir_state
    (** State 528.
        Stack shape : LET REC IDENT arg_ty_atomic.
        Start symbol: pi. *)

  | MenhirState529 : (((((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic, _menhir_box_pi) _menhir_cell1_ret_ty_annot_eq, _menhir_box_pi) _menhir_state
    (** State 529.
        Stack shape : LET REC IDENT arg_ty_atomic ret_ty_annot_eq.
        Start symbol: pi. *)

  | MenhirState533 : (((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_ty_annot_IDENT_, _menhir_box_pi) _menhir_state
    (** State 533.
        Stack shape : LET REC ty_annot(IDENT).
        Start symbol: pi. *)

  | MenhirState536 : ((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_state
    (** State 536.
        Stack shape : LET IDENT.
        Start symbol: pi. *)

  | MenhirState537 : (((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic, _menhir_box_pi) _menhir_state
    (** State 537.
        Stack shape : LET IDENT arg_ty_atomic.
        Start symbol: pi. *)

  | MenhirState538 : ((((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic, _menhir_box_pi) _menhir_cell1_ret_ty_annot_eq, _menhir_box_pi) _menhir_state
    (** State 538.
        Stack shape : LET IDENT arg_ty_atomic ret_ty_annot_eq.
        Start symbol: pi. *)

  | MenhirState548 : (('s, _menhir_box_pi) _menhir_cell1_EXTERNAL _menhir_cell0_IDENT, _menhir_box_pi) _menhir_state
    (** State 548.
        Stack shape : EXTERNAL IDENT.
        Start symbol: pi. *)

  | MenhirState552 : (('s, _menhir_box_pi) _menhir_cell1_type_alias, _menhir_box_pi) _menhir_state
    (** State 552.
        Stack shape : type_alias.
        Start symbol: pi. *)

  | MenhirState553 : (('s, _menhir_box_pi) _menhir_cell1_typ_sum, _menhir_box_pi) _menhir_state
    (** State 553.
        Stack shape : typ_sum.
        Start symbol: pi. *)

  | MenhirState554 : (('s, _menhir_box_pi) _menhir_cell1_static, _menhir_box_pi) _menhir_state
    (** State 554.
        Stack shape : static.
        Start symbol: pi. *)

  | MenhirState556 : (('s, _menhir_box_pi) _menhir_cell1_ext_fun, _menhir_box_pi) _menhir_state
    (** State 556.
        Stack shape : ext_fun.
        Start symbol: pi. *)

  | MenhirState558 : (('s, _menhir_box_pi) _menhir_cell1_ext_circ, _menhir_box_pi) _menhir_state
    (** State 558.
        Stack shape : ext_circ.
        Start symbol: pi. *)

  | MenhirState563 : (('s, _menhir_box_pi) _menhir_cell1_decl, _menhir_box_pi) _menhir_state
    (** State 563.
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

and ('s, 'r) _menhir_cell1_match_cases = 
  | MenhirCell1_match_cases of 's * ('s, 'r) _menhir_state * ((Ast.x * (Ast.p * Ast.e_static)) list * Ast.e_static option)

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
  | MenhirCell1_ty_next of 's * ('s, 'r) _menhir_state * (Types.ty)

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
# 1326 "src/frontend/parser.ml"
) * Lexing.position * Lexing.position

and 's _menhir_cell0_IDENT = 
  | MenhirCell0_IDENT of 's * (
# 79 "src/frontend/parser.mly"
       (string)
# 1333 "src/frontend/parser.ml"
) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_INT_LIT = 
  | MenhirCell1_INT_LIT of 's * ('s, 'r) _menhir_state * (
# 81 "src/frontend/parser.mly"
       (int)
# 1343 "src/frontend/parser.ml"
) * Lexing.position * Lexing.position

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

and ('s, 'r) _menhir_cell1_MACRO_GENERATE = 
  | MenhirCell1_MACRO_GENERATE of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_MATCH = 
  | MenhirCell1_MATCH of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_MINUS = 
  | MenhirCell1_MINUS of 's * ('s, 'r) _menhir_state * Lexing.position

and 's _menhir_cell0_MINUS = 
  | MenhirCell0_MINUS of 's * Lexing.position

and ('s, 'r) _menhir_cell1_OPERATOR = 
  | MenhirCell1_OPERATOR of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_OPERATOR_IDENT = 
  | MenhirCell0_OPERATOR_IDENT of 's * (
# 91 "src/frontend/parser.mly"
       (string)
# 1383 "src/frontend/parser.ml"
) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_PARFOR = 
  | MenhirCell1_PARFOR of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_PAUSE = 
  | MenhirCell1_PAUSE of 's * ('s, 'r) _menhir_state * Lexing.position

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
# 1432 "src/frontend/parser.ml"
) * Lexing.position * Lexing.position

and 's _menhir_cell0_UP_IDENT = 
  | MenhirCell0_UP_IDENT of 's * (
# 79 "src/frontend/parser.mly"
       (string)
# 1439 "src/frontend/parser.ml"
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
# 629 "src/frontend/parser.mly"
              ( mk_loc (with_file _loc) e )
# 1465 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_003 =
  fun ex ->
    (
# 632 "src/frontend/parser.mly"
               ( E_get(ex) )
# 1473 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_004 =
  fun e ->
    (
# 633 "src/frontend/parser.mly"
                      ( e )
# 1481 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_005 =
  fun e ty ->
    (
# 634 "src/frontend/parser.mly"
                                ( ty_annot ~ty e )
# 1489 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_006 =
  fun c ->
    (
# 635 "src/frontend/parser.mly"
          ( E_const c )
# 1497 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_007 =
  fun k ->
    (
# 637 "src/frontend/parser.mly"
                          ( E_const (Op(Runtime(Resize_int k))) )
# 1505 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_008 =
  fun k ->
    (
# 638 "src/frontend/parser.mly"
                               ( E_const (Op(Runtime(Tuple_of_int k))) )
# 1513 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_009 =
  fun k ->
    (
# 639 "src/frontend/parser.mly"
                               ( E_const (Op(Runtime(Int_of_tuple k))) )
# 1521 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_010 =
  fun e ->
    (
# 640 "src/frontend/parser.mly"
                     (
                let loc = loc_of e in
                match Ast_undecorated.remove_deco e with
                | E_tuple[v;e] -> assert (is_variable v || evaluated v); (* todo: error *)
                                  let x = Ast.gensym () in
                                  E_vector_mapi(false,(P_var x,(Types.new_tyB_unknown(),Types.new_tyB_unknown()),E_app(v,E_var x)), 
                                     mk_loc loc e, new_size_unknown ())
                | _ -> assert false (* todo error *) )
# 1536 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_011 =
  fun k x ->
    (
# 649 "src/frontend/parser.mly"
                        ( E_const (Op(Runtime(Unroll k))) )
# 1544 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_012 =
  fun l ->
    (
# 650 "src/frontend/parser.mly"
                           ( E_const (Op(Runtime(Vector_create l))) )
# 1552 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_013 =
  fun _endpos_x_ _startpos_x_ x ->
    let _endpos = _endpos_x_ in
    let _startpos = _startpos_x_ in
    let _loc = (_startpos, _endpos) in
    (
# 651 "src/frontend/parser.mly"
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
                       E_fun(P_tuple[P_var x;P_var y], (Types.new_ty_unknown(),Types.new_tyB_unknown()), 
                          E_array_get(x,E_var y))
            | "bvect_of_int" -> E_const (Op(Runtime(Bvect_of_int)))
            | "int_of_bvect" -> E_const (Op(Runtime(Int_of_bvect)))
            | "_" -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                         ~msg:"wildcard \"_\" not expected." ()
            | _ -> E_var x )
# 1582 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_014 =
  fun xs ->
    let _2 = 
# 241 "<standard.mly>"
    ( xs )
# 1590 "src/frontend/parser.ml"
     in
    (
# 672 "src/frontend/parser.mly"
    ( (* Buffer n *) assert false (*todo*)  )
# 1595 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_015 =
  fun cases e otherwise ->
    (
# 677 "src/frontend/parser.mly"
      ( E_case(e,cases,otherwise) )
# 1603 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_016 =
  fun e rev_cases ->
    (
# 681 "src/frontend/parser.mly"
      ( let (hs,eo) = rev_cases in
        E_match(e,List.rev hs,eo) )
# 1612 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_017 =
  fun e e1 e2 i ->
    (
# 684 "src/frontend/parser.mly"
      ( let loop = gensym ~prefix:"loop" () in
        let n0 = gensym ~prefix:"n0" () in
        let n = gensym ~prefix:"n" () in
        E_letIn(P_var n0,Types.new_ty_unknown(), e1,
        E_letIn(P_var n, Types.new_ty_unknown(), e2,
        E_letIn(P_var loop,Types.new_ty_unknown(),
                          E_fix(loop,(P_var i,(Types.new_ty_unknown(),Types.new_tyB_unknown()),
                              E_if(E_app(E_const(Op(Runtime(External_fun("Int.gt",new_ty_unknown ())))),E_tuple[E_var i;E_var n]),
                                   E_const(Unit),
                                   E_letIn(P_unit,Types.new_ty_unknown(), e,E_app(E_var loop,
                                    E_app(E_const(Op(Runtime(External_fun("Int.add",new_ty_unknown ())))),
                                        E_tuple[E_var i;E_const (Int (1,new_size_unknown()))])))))), 
                 E_app(E_var loop,E_var n0))))
                )
# 1633 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_018 =
  fun _endpos__9_ _startpos__1_ e e_st1 e_st2 x ->
    let _endpos = _endpos__9_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 700 "src/frontend/parser.mly"
      ( E_for(x,e_st1,e_st2,e,with_file _loc) )
# 1644 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_019 =
  fun b ->
    (
# 266 "src/frontend/parser.mly"
                             ( b )
# 1652 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_020 =
  fun b ->
    (
# 268 "src/frontend/parser.mly"
        ( b )
# 1660 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_021 =
  fun e ->
    (
# 270 "src/frontend/parser.mly"
        ( e )
# 1668 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_022 =
  fun _endpos_e1_ _startpos_f_ty_opt_ e1 f_ty_opt ->
    (
# 272 "src/frontend/parser.mly"
        (
            let f,ty_opt = f_ty_opt in
            let loc_fun = with_file (_startpos_f_ty_opt_,_endpos_e1_) in
            let v = mk_fix f (ty_annot_opt ~ty:ty_opt e1) loc_fun in
            P_var f, v
        )
# 1681 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_023 =
  fun b ->
    (
# 266 "src/frontend/parser.mly"
                             ( b )
# 1689 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_024 =
  fun b ->
    (
# 268 "src/frontend/parser.mly"
        ( b )
# 1697 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_025 =
  fun e ->
    (
# 270 "src/frontend/parser.mly"
        ( e )
# 1705 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_026 =
  fun _endpos_e1_ _startpos_f_ty_opt_ e1 f_ty_opt ->
    (
# 272 "src/frontend/parser.mly"
        (
            let f,ty_opt = f_ty_opt in
            let loc_fun = with_file (_startpos_f_ty_opt_,_endpos_e1_) in
            let v = mk_fix f (ty_annot_opt ~ty:ty_opt e1) loc_fun in
            P_var f, v
        )
# 1718 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_027 =
  fun () ->
    (
# 728 "src/frontend/parser.mly"
                ( P_unit )
# 1726 "src/frontend/parser.ml"
     : (Ast.p))

let _menhir_action_028 =
  fun p ->
    (
# 729 "src/frontend/parser.mly"
                      ( p )
# 1734 "src/frontend/parser.ml"
     : (Ast.p))

let _menhir_action_029 =
  fun x ->
    (
# 730 "src/frontend/parser.mly"
          ( P_var x )
# 1742 "src/frontend/parser.ml"
     : (Ast.p))

let _menhir_action_030 =
  fun _endpos_e_ _startpos_e_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    let _loc = (_startpos, _endpos) in
    (
# 514 "src/frontend/parser.mly"
                 ( mk_loc (with_file _loc) e )
# 1753 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_031 =
  fun _endpos_e_ _startpos__1_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 517 "src/frontend/parser.mly"
               ( mk_loc (with_file _loc) @@ E_pause e )
# 1764 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_032 =
  fun _endpos_e_ _startpos__1_ e sz ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 518 "src/frontend/parser.mly"
                                  ( E_array_make(sz, e, with_file _loc) )
# 1775 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_033 =
  fun _endpos__6_ _startpos__1_ sz ->
    let _endpos = _endpos__6_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 520 "src/frontend/parser.mly"
                                     ( E_array_create(sz, with_file _loc) )
# 1786 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_034 =
  fun _endpos__6_ _startpos__1_ sz ->
    let _endpos = _endpos__6_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 520 "src/frontend/parser.mly"
                                     ( E_array_create(sz, with_file _loc) )
# 1797 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_035 =
  fun e1 e2 v ->
    (
# 523 "src/frontend/parser.mly"
    ( 
        E_app(e1,E_tuple[e2;v]) 
    )
# 1807 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_036 =
  fun e1 e2 v ->
    (
# 523 "src/frontend/parser.mly"
    ( 
        E_app(e1,E_tuple[e2;v]) 
    )
# 1817 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_037 =
  fun e ex ->
    (
# 526 "src/frontend/parser.mly"
                           ( E_set(ex,e) )
# 1825 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_038 =
  fun e ->
    (
# 527 "src/frontend/parser.mly"
                        ( E_ref e )
# 1833 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_039 =
  fun _endpos_e_ _startpos__1_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 532 "src/frontend/parser.mly"
             ( match Ast_undecorated.remove_deco e with 
               | E_tuple[E_var x;e1;e2] -> E_array_set(x,e1,e2) 
               | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                   ~msg:"... array set" () )
# 1847 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_040 =
  fun e1 x ->
    (
# 538 "src/frontend/parser.mly"
   ( E_array_get(x,e1) )
# 1855 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_041 =
  fun e1 x ->
    (
# 538 "src/frontend/parser.mly"
   ( E_array_get(x,e1) )
# 1863 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_042 =
  fun e1 x ->
    (
# 539 "src/frontend/parser.mly"
                     ( E_array_get(x,e1) )
# 1871 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_043 =
  fun x ->
    (
# 540 "src/frontend/parser.mly"
                     ( E_array_length x )
# 1879 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_044 =
  fun _endpos_a_ _startpos__1_ a ->
    let _endpos = _endpos_a_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 542 "src/frontend/parser.mly"
                ( match un_annot a with
                  | E_var x -> E_array_length x 
                  | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                          ~msg:"array length: should be a variable......" () )
# 1893 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_045 =
  fun _endpos_a_ _startpos__1_ a ->
    let _endpos = _endpos_a_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 542 "src/frontend/parser.mly"
                ( match un_annot a with
                  | E_var x -> E_array_length x 
                  | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                          ~msg:"array length: should be a variable......" () )
# 1907 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_046 =
  fun e1 e2 x ->
    (
# 548 "src/frontend/parser.mly"
  ( E_array_set(x,e1,e2) )
# 1915 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_047 =
  fun e1 e2 x ->
    (
# 548 "src/frontend/parser.mly"
  ( E_array_set(x,e1,e2) )
# 1923 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_048 =
  fun n ->
    (
# 549 "src/frontend/parser.mly"
                        ( E_const(C_size n) )
# 1931 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_049 =
  fun _endpos_es_ _startpos_e_ e es ->
    let _endpos = _endpos_es_ in
    let _startpos = _startpos_e_ in
    let _loc = (_startpos, _endpos) in
    (
# 551 "src/frontend/parser.mly"
      ( match e::es with
        | [e1;e2] -> (match un_annot e1 with
                      | E_var _ | E_const _ | E_fun _ -> 
                        E_app(e1,e2)
                      | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                   ~msg:"expression in functional position should be a variable or a constante" ())
        | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                   ~msg:"All functions and primitives should be unary. Hints: use a tuple as argument" () )
# 1949 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_050 =
  fun e1 ->
    (
# 559 "src/frontend/parser.mly"
                                       ( E_app(E_const(Op(Runtime(External_fun("Int.neg",new_ty_unknown ())))),e1) )
# 1957 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_051 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 765 "src/frontend/parser.mly"
             ( External_fun("Int.add",new_ty_unknown ()) )
# 1965 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 1975 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_052 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 766 "src/frontend/parser.mly"
             ( External_fun("Int.sub",new_ty_unknown ()) )
# 1983 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 1993 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_053 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 767 "src/frontend/parser.mly"
             ( External_fun("Int.mul",new_ty_unknown ()) )
# 2001 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2011 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_054 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 768 "src/frontend/parser.mly"
             ( External_fun("Int.div",new_ty_unknown ()) )
# 2019 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2029 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_055 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 769 "src/frontend/parser.mly"
             ( External_fun("Int.modulo",new_ty_unknown ()) )
# 2037 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2047 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_056 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 770 "src/frontend/parser.mly"
             ( External_fun("Int.lt",new_ty_unknown ()) )
# 2055 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2065 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_057 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 771 "src/frontend/parser.mly"
             ( External_fun("Int.gt",new_ty_unknown ()) )
# 2073 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2083 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_058 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 772 "src/frontend/parser.mly"
             ( External_fun("Int.le",new_ty_unknown ()) )
# 2091 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2101 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_059 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 773 "src/frontend/parser.mly"
             ( External_fun("Int.ge",new_ty_unknown ()) )
# 2109 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2119 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_060 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 774 "src/frontend/parser.mly"
             ( External_fun("Int.eq",new_ty_unknown ()) )
# 2127 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2137 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_061 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 774 "src/frontend/parser.mly"
             ( External_fun("Int.eq",new_ty_unknown ()) )
# 2145 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2155 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_062 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 775 "src/frontend/parser.mly"
             ( External_fun("Int.neq",new_ty_unknown ()) )
# 2163 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2173 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_063 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 776 "src/frontend/parser.mly"
             ( External_fun("Bool.land",new_ty_unknown ()) )
# 2181 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2191 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_064 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 777 "src/frontend/parser.mly"
             ( External_fun("Bool.lxor",new_ty_unknown ()) )
# 2199 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2209 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_065 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 778 "src/frontend/parser.mly"
             ( External_fun("Int.lxor",new_ty_unknown ()) )
# 2217 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2227 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_066 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 779 "src/frontend/parser.mly"
             ( External_fun("Int.land",new_ty_unknown ()) )
# 2235 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2245 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_067 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 780 "src/frontend/parser.mly"
             ( External_fun("Int.lor",new_ty_unknown ()) )
# 2253 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2263 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_068 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 781 "src/frontend/parser.mly"
             ( External_fun("Int.lsl",new_ty_unknown ()) )
# 2271 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2281 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_069 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 782 "src/frontend/parser.mly"
             ( External_fun("Int.lsr",new_ty_unknown ()) )
# 2289 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2299 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_070 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 783 "src/frontend/parser.mly"
             ( External_fun("Int.asr",new_ty_unknown ()) )
# 2307 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 561 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2317 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_071 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 565 "src/frontend/parser.mly"
        ( let e3 = mk_loc (with_file _loc) @@ E_const (Bool false) in
          E_if(e1,e2,e3)
        )
# 2330 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_072 =
  fun _endpos_e3_ _startpos_e1_ e1 e3 ->
    let _endpos = _endpos_e3_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 569 "src/frontend/parser.mly"
        ( let e2 = mk_loc (with_file _loc) @@ E_const (Bool true) in
          E_if(e1,e2,e3)
        )
# 2343 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_073 =
  fun _endpos_e0_ _startpos__1_ e0 ev ->
    let _endpos = _endpos_e0_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 573 "src/frontend/parser.mly"
       ( match un_annot ev with
         | E_fun(p,(_,tyB),e1) -> E_reg((p,tyB,e1),e0,Ast.gensym ())
         | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                               ~msg:"This expression should be a function" ()
       )
# 2358 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_074 =
  fun e0 f ->
    (
# 579 "src/frontend/parser.mly"
       (
        let y = gensym () in
         E_reg((P_var y,Types.new_tyB_unknown(),E_app(E_var f,E_var y)),e0,Ast.gensym ())
       )
# 2369 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_075 =
  fun e1 e2 ->
    (
# 584 "src/frontend/parser.mly"
       ( E_exec(e1,e2,None,"") )
# 2377 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_076 =
  fun e1 e2 e3 ->
    (
# 586 "src/frontend/parser.mly"
       ( E_exec(e1,e2,Some e3,"") )
# 2385 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_077 =
  fun _endpos_e_st3_ _startpos__1_ e_init2 e_st3 ef1 ->
    let _endpos = _endpos_e_st3_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 588 "src/frontend/parser.mly"
  ( let z = Ast.gensym () in
    E_generate((P_var z,(Types.new_ty_unknown(),Types.new_tyB_unknown()),  E_app(ef1,E_var z)),e_init2,e_st3,with_file _loc) )
# 2397 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_078 =
  fun _endpos_e1_ _startpos__1_ e1 ->
    let _endpos = _endpos_e1_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 592 "src/frontend/parser.mly"
    (
        Prelude.Errors.raise_error ~loc:(with_file _loc)
            ~msg:"missing ``default'' close; `exec e default e` expected" ()
    )
# 2411 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_079 =
  fun _endpos__3_ _startpos__1_ e1 ->
    let _endpos = _endpos__3_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 597 "src/frontend/parser.mly"
    (
        Prelude.Errors.raise_error ~loc:(with_file _loc)
            ~msg:"missing expression after keyword ``default''; `exec e default e` expected" ()
    )
# 2425 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_080 =
  fun e i ->
    (
# 617 "src/frontend/parser.mly"
     ( E_run(i, e) )
# 2433 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_081 =
  fun e ->
    (
# 621 "src/frontend/parser.mly"
         ( e )
# 2441 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_082 =
  fun a ->
    (
# 443 "src/frontend/parser.mly"
                                 ( a )
# 2449 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_083 =
  fun a ->
    (
# 443 "src/frontend/parser.mly"
                                 ( a )
# 2457 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_084 =
  fun p ->
    (
# 446 "src/frontend/parser.mly"
                      ( p, None )
# 2465 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_085 =
  fun p ty ->
    (
# 447 "src/frontend/parser.mly"
                                 (p, Some ty)
# 2473 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_086 =
  fun p ->
    (
# 448 "src/frontend/parser.mly"
         ( p, None )
# 2481 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_087 =
  fun p ->
    (
# 438 "src/frontend/parser.mly"
        ( p, None )
# 2489 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_088 =
  fun p ty ->
    (
# 439 "src/frontend/parser.mly"
                     (p, Some ty)
# 2497 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_089 =
  fun ty ->
    (
# 396 "src/frontend/parser.mly"
      ( ty )
# 2505 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_090 =
  fun e ->
    (
# 415 "src/frontend/parser.mly"
                        ( e )
# 2513 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_091 =
  fun c ->
    (
# 416 "src/frontend/parser.mly"
          ( E_const c )
# 2521 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_092 =
  fun e p_ty_opt ->
    (
# 506 "src/frontend/parser.mly"
        (
            let p,ty_opt = p_ty_opt in
            p,ty_annot_opt ~ty:ty_opt e
        )
# 2532 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_093 =
  fun e p ->
    (
# 511 "src/frontend/parser.mly"
    ( p,e )
# 2540 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_094 =
  fun w ->
    (
# 490 "src/frontend/parser.mly"
  ( match w with
    | [],_ | _,[] -> assert false
    | [p],[e] -> (p,e)
    | ps,es -> (P_tuple ps, E_par es) )
# 2551 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_095 =
  fun b ->
    (
# 496 "src/frontend/parser.mly"
                 ( let (p,e) = b in ([p],[e]) )
# 2559 "src/frontend/parser.ml"
     : (Ast.p list * Ast.e_static list))

let _menhir_action_096 =
  fun b1 bs ->
    (
# 498 "src/frontend/parser.mly"
   ( let (p1,e1) = b1 in
     let (ps,es) = bs in
     (p1::ps,e1::es) )
# 2569 "src/frontend/parser.ml"
     : (Ast.p list * Ast.e_static list))

let _menhir_action_097 =
  fun () ->
    (
# 739 "src/frontend/parser.mly"
                ( Unit )
# 2577 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_098 =
  fun b ->
    (
# 740 "src/frontend/parser.mly"
                ( Bool b )
# 2585 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_099 =
  fun n ->
    (
# 741 "src/frontend/parser.mly"
            (
    Int (n,new_size_unknown()) )
# 2594 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_100 =
  fun _endpos_k_ _startpos_n_ k n ->
    let _endpos = _endpos_k_ in
    let _startpos = _startpos_n_ in
    let _loc = (_startpos, _endpos) in
    (
# 744 "src/frontend/parser.mly"
    ( if Float.log2 (float n) >= float (k-1) then
       Prelude.Errors.raise_error ~loc:(with_file _loc)
          ~msg:("Integer literal "^
                string_of_int n^
                " exceeds the range of representable integers of type int<"^
                string_of_int k ^">") ()
      else Int (n,Sz_lit k) )
# 2611 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_101 =
  fun s ->
    (
# 751 "src/frontend/parser.mly"
                         ( String s )
# 2619 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_102 =
  fun x ->
    (
# 752 "src/frontend/parser.mly"
                   ( Op(Runtime(External_fun(x,new_ty_unknown()))) )
# 2627 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_103 =
  fun x ->
    (
# 753 "src/frontend/parser.mly"
                         ( Inj x )
# 2635 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_104 =
  fun () ->
    let op = 
# 765 "src/frontend/parser.mly"
             ( External_fun("Int.add",new_ty_unknown ()) )
# 2643 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2648 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_105 =
  fun () ->
    let op = 
# 766 "src/frontend/parser.mly"
             ( External_fun("Int.sub",new_ty_unknown ()) )
# 2656 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2661 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_106 =
  fun () ->
    let op = 
# 767 "src/frontend/parser.mly"
             ( External_fun("Int.mul",new_ty_unknown ()) )
# 2669 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2674 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_107 =
  fun () ->
    let op = 
# 768 "src/frontend/parser.mly"
             ( External_fun("Int.div",new_ty_unknown ()) )
# 2682 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2687 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_108 =
  fun () ->
    let op = 
# 769 "src/frontend/parser.mly"
             ( External_fun("Int.modulo",new_ty_unknown ()) )
# 2695 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2700 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_109 =
  fun () ->
    let op = 
# 770 "src/frontend/parser.mly"
             ( External_fun("Int.lt",new_ty_unknown ()) )
# 2708 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2713 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_110 =
  fun () ->
    let op = 
# 771 "src/frontend/parser.mly"
             ( External_fun("Int.gt",new_ty_unknown ()) )
# 2721 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2726 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_111 =
  fun () ->
    let op = 
# 772 "src/frontend/parser.mly"
             ( External_fun("Int.le",new_ty_unknown ()) )
# 2734 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2739 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_112 =
  fun () ->
    let op = 
# 773 "src/frontend/parser.mly"
             ( External_fun("Int.ge",new_ty_unknown ()) )
# 2747 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2752 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_113 =
  fun () ->
    let op = 
# 774 "src/frontend/parser.mly"
             ( External_fun("Int.eq",new_ty_unknown ()) )
# 2760 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2765 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_114 =
  fun () ->
    let op = 
# 774 "src/frontend/parser.mly"
             ( External_fun("Int.eq",new_ty_unknown ()) )
# 2773 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2778 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_115 =
  fun () ->
    let op = 
# 775 "src/frontend/parser.mly"
             ( External_fun("Int.neq",new_ty_unknown ()) )
# 2786 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2791 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_116 =
  fun () ->
    let op = 
# 776 "src/frontend/parser.mly"
             ( External_fun("Bool.land",new_ty_unknown ()) )
# 2799 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2804 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_117 =
  fun () ->
    let op = 
# 777 "src/frontend/parser.mly"
             ( External_fun("Bool.lxor",new_ty_unknown ()) )
# 2812 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2817 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_118 =
  fun () ->
    let op = 
# 778 "src/frontend/parser.mly"
             ( External_fun("Int.lxor",new_ty_unknown ()) )
# 2825 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2830 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_119 =
  fun () ->
    let op = 
# 779 "src/frontend/parser.mly"
             ( External_fun("Int.land",new_ty_unknown ()) )
# 2838 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2843 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_120 =
  fun () ->
    let op = 
# 780 "src/frontend/parser.mly"
             ( External_fun("Int.lor",new_ty_unknown ()) )
# 2851 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2856 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_121 =
  fun () ->
    let op = 
# 781 "src/frontend/parser.mly"
             ( External_fun("Int.lsl",new_ty_unknown ()) )
# 2864 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2869 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_122 =
  fun () ->
    let op = 
# 782 "src/frontend/parser.mly"
             ( External_fun("Int.lsr",new_ty_unknown ()) )
# 2877 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2882 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_123 =
  fun () ->
    let op = 
# 783 "src/frontend/parser.mly"
             ( External_fun("Int.asr",new_ty_unknown ()) )
# 2890 "src/frontend/parser.ml"
     in
    (
# 754 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2895 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_124 =
  fun cs ->
    (
# 756 "src/frontend/parser.mly"
    ( C_vector cs )
# 2903 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_125 =
  fun d ->
    (
# 205 "src/frontend/parser.mly"
  ( clear_tyvar_constraints();
    d )
# 2912 "src/frontend/parser.ml"
     : ((Ast.p * Ast.e_static) * Prelude.loc))

let _menhir_action_126 =
  fun _endpos_b_ _startpos__1_ b ->
    let _endpos = _endpos_b_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 210 "src/frontend/parser.mly"
        ( b,(with_file _loc) )
# 2923 "src/frontend/parser.ml"
     : ((Ast.p * Ast.e_static) * Prelude.loc))

let _menhir_action_127 =
  fun _endpos__2_ _startpos_e_ e ->
    let _endpos = _endpos__2_ in
    let _startpos = _startpos_e_ in
    let _loc = (_startpos, _endpos) in
    (
# 214 "src/frontend/parser.mly"
                  ( ((P_var "_", e),(with_file _loc))  )
# 2934 "src/frontend/parser.ml"
     : ((Ast.p * Ast.e_static) * Prelude.loc))

let _menhir_action_128 =
  fun e ->
    (
# 625 "src/frontend/parser.mly"
                        ( e )
# 2942 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_129 =
  fun _endpos_e_ _startpos_e_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    let _loc = (_startpos, _endpos) in
    (
# 421 "src/frontend/parser.mly"
             ( mk_loc (with_file _loc) e )
# 2953 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_130 =
  fun e1 e2 ->
    (
# 427 "src/frontend/parser.mly"
        (
            E_letIn(P_unit,Ty_base TyB_unit, e1,e2)
        )
# 2963 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_131 =
  fun e es ->
    (
# 431 "src/frontend/parser.mly"
        (
            E_tuple (e::es)
        )
# 2973 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_132 =
  fun e ->
    (
# 435 "src/frontend/parser.mly"
         ( e )
# 2981 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_133 =
  fun e ->
    (
# 201 "src/frontend/parser.mly"
            (e)
# 2989 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_134 =
  fun t x ->
    (
# 165 "src/frontend/parser.mly"
                                      ( (x, (t, false)) )
# 2997 "src/frontend/parser.ml"
     : (Ast.x * (Types.ty * bool)))

let _menhir_action_135 =
  fun t x ->
    (
# 166 "src/frontend/parser.mly"
                                             ( (x, (t, true)) )
# 3005 "src/frontend/parser.ml"
     : (Ast.x * (Types.ty * bool)))

let _menhir_action_136 =
  fun t x ->
    let imp = 
# 161 "src/frontend/parser.mly"
            ( true )
# 3013 "src/frontend/parser.ml"
     in
    (
# 154 "src/frontend/parser.mly"
   ( Hashtbl.clear hash_size_tvar;
     (x, (t,(false,get_arity t,imp))) )
# 3019 "src/frontend/parser.ml"
     : (Ast.x * (Types.ty * (bool * int * bool))))

let _menhir_action_137 =
  fun t x ->
    let imp = 
# 162 "src/frontend/parser.mly"
  (false)
# 3027 "src/frontend/parser.ml"
     in
    (
# 154 "src/frontend/parser.mly"
   ( Hashtbl.clear hash_size_tvar;
     (x, (t,(false,get_arity t,imp))) )
# 3033 "src/frontend/parser.ml"
     : (Ast.x * (Types.ty * (bool * int * bool))))

let _menhir_action_138 =
  fun t x ->
    let imp = 
# 161 "src/frontend/parser.mly"
            ( true )
# 3041 "src/frontend/parser.ml"
     in
    (
# 157 "src/frontend/parser.mly"
   ( Hashtbl.clear hash_size_tvar;
     (x, (t,(true,get_arity t,imp))) )
# 3047 "src/frontend/parser.ml"
     : (Ast.x * (Types.ty * (bool * int * bool))))

let _menhir_action_139 =
  fun t x ->
    let imp = 
# 162 "src/frontend/parser.mly"
  (false)
# 3055 "src/frontend/parser.ml"
     in
    (
# 157 "src/frontend/parser.mly"
   ( Hashtbl.clear hash_size_tvar;
     (x, (t,(true,get_arity t,imp))) )
# 3061 "src/frontend/parser.ml"
     : (Ast.x * (Types.ty * (bool * int * bool))))

let _menhir_action_140 =
  fun _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt_ret ->
    (
# 256 "src/frontend/parser.mly"
        (
            let ef = mk_let_fun ~loc:(with_file (_startpos_f_,_endpos_e1_))
                                ~p_ty_opt
                                ~ty_opt_ret
                        e1
            in
            (P_var f,ef)
        )
# 3076 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_141 =
  fun _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt_ret ->
    (
# 256 "src/frontend/parser.mly"
        (
            let ef = mk_let_fun ~loc:(with_file (_startpos_f_,_endpos_e1_))
                                ~p_ty_opt
                                ~ty_opt_ret
                        e1
            in
            (P_var f,ef)
        )
# 3091 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_142 =
  fun _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt ->
    (
# 280 "src/frontend/parser.mly"
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
# 3114 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_143 =
  fun _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt ->
    (
# 280 "src/frontend/parser.mly"
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
# 3137 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_144 =
  fun e2 ->
    (
# 481 "src/frontend/parser.mly"
          ( e2,E_const Unit )
# 3145 "src/frontend/parser.ml"
     : (Ast.e_static * Ast.e_static))

let _menhir_action_145 =
  fun e2 e3 ->
    (
# 482 "src/frontend/parser.mly"
                       ( e2, e3 )
# 3153 "src/frontend/parser.ml"
     : (Ast.e_static * Ast.e_static))

let _menhir_action_146 =
  fun _endpos_e_ _startpos_e_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    let _loc = (_startpos, _endpos) in
    (
# 452 "src/frontend/parser.mly"
              ( mk_loc (with_file _loc) e )
# 3164 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_147 =
  fun e ->
    (
# 455 "src/frontend/parser.mly"
            ( e )
# 3172 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_148 =
  fun _endpos_e_ _startpos__1_ e f ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 457 "src/frontend/parser.mly"
        ( mk_fix f e (with_file _loc) )
# 3183 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_149 =
  fun e p_ty_opt ->
    (
# 459 "src/frontend/parser.mly"
        ( let (p,ty_p_opt) = p_ty_opt in
          mk_fun_ty_annot_p p ty_p_opt e )
# 3192 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_150 =
  fun e1 e2_e3 ->
    (
# 462 "src/frontend/parser.mly"
        ( let (e2,e3) = e2_e3 in E_if(e1,e2,e3) )
# 3200 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_151 =
  fun b e2 ->
    (
# 464 "src/frontend/parser.mly"
        ( let (p,e1) = b in
          E_letIn(p,Types.new_ty_unknown(),e1,e2) )
# 3209 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_152 =
  fun e1 es ->
    (
# 476 "src/frontend/parser.mly"
        (
            E_par(e1::es)
        )
# 3219 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_153 =
  fun e1 es ->
    (
# 476 "src/frontend/parser.mly"
        (
            E_par(e1::es)
        )
# 3229 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_154 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 3237 "src/frontend/parser.ml"
     : ((Ast.c list * Ast.e_static) list))

let _menhir_action_155 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 3245 "src/frontend/parser.ml"
     : ((Ast.c list * Ast.e_static) list))

let _menhir_action_156 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 3253 "src/frontend/parser.ml"
     : (Ast.e list))

let _menhir_action_157 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 3261 "src/frontend/parser.ml"
     : (Ast.e list))

let _menhir_action_158 =
  fun v ->
    (
# 407 "src/frontend/parser.mly"
           ( v )
# 3269 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_159 =
  fun e p_ty_opt ->
    (
# 409 "src/frontend/parser.mly"
      (
        let p,ty_opt = p_ty_opt in
        mk_fun_ty_annot p ty_opt e
    )
# 3280 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_160 =
  fun e p x ->
    (
# 712 "src/frontend/parser.mly"
                                      ( (x,(p,e)) )
# 3288 "src/frontend/parser.ml"
     : (Ast.x * (Ast.p * Ast.e_static)))

let _menhir_action_161 =
  fun cs e ->
    (
# 704 "src/frontend/parser.mly"
                                                                ( (cs,e) )
# 3296 "src/frontend/parser.ml"
     : (Ast.c list * Ast.e_static))

let _menhir_action_162 =
  fun e ->
    (
# 707 "src/frontend/parser.mly"
                                          ( [],Some e )
# 3304 "src/frontend/parser.ml"
     : ((Ast.x * (Ast.p * Ast.e_static)) list * Ast.e_static option))

let _menhir_action_163 =
  fun h ->
    (
# 708 "src/frontend/parser.mly"
                                          ( [h],None )
# 3312 "src/frontend/parser.ml"
     : ((Ast.x * (Ast.p * Ast.e_static)) list * Ast.e_static option))

let _menhir_action_164 =
  fun h rev_cases ->
    (
# 709 "src/frontend/parser.mly"
                                          ( let (hs,eo) = rev_cases in h::hs,eo )
# 3320 "src/frontend/parser.ml"
     : ((Ast.x * (Ast.p * Ast.e_static)) list * Ast.e_static option))

let _menhir_action_165 =
  fun x ->
    (
# 228 "<standard.mly>"
    ( [ x ] )
# 3328 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_166 =
  fun x xs ->
    (
# 231 "<standard.mly>"
    ( x :: xs )
# 3336 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_167 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 3344 "src/frontend/parser.ml"
     : (unit option))

let _menhir_action_168 =
  fun x ->
    (
# 114 "<standard.mly>"
    ( Some x )
# 3352 "src/frontend/parser.ml"
     : (unit option))

let _menhir_action_169 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 3360 "src/frontend/parser.ml"
     : (unit option))

let _menhir_action_170 =
  fun x ->
    (
# 114 "<standard.mly>"
    ( Some x )
# 3368 "src/frontend/parser.ml"
     : (unit option))

let _menhir_action_171 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 3376 "src/frontend/parser.ml"
     : (unit option))

let _menhir_action_172 =
  fun x ->
    (
# 114 "<standard.mly>"
    ( Some x )
# 3384 "src/frontend/parser.ml"
     : (unit option))

let _menhir_action_173 =
  fun p ->
    (
# 723 "src/frontend/parser.mly"
         ( p )
# 3392 "src/frontend/parser.ml"
     : (Ast.p))

let _menhir_action_174 =
  fun p ps ->
    (
# 725 "src/frontend/parser.mly"
  ( P_tuple (p::ps) )
# 3400 "src/frontend/parser.ml"
     : (Ast.p))

let _menhir_action_175 =
  fun _endpos_pi_ _startpos_ec_ ec pi ->
    let _endpos = _endpos_pi_ in
    let _startpos = _startpos_ec_ in
    let _loc = (_startpos, _endpos) in
    (
# 134 "src/frontend/parser.mly"
                      ( let (ecs,efs),gs,ts,ds = pi in
                        let (x,t) = ec in
                        let f = String.capitalize_ascii x in
                        let arg = gensym ~prefix:"arg" () in
                        let v = E_fun(P_var arg,(Types.new_ty_unknown(),Types.new_tyB_unknown()), 
                             E_run(f,E_var arg)) in
                        (((f,t)::ecs,efs), gs,    ts,    (((P_var x,v),with_file _loc)::ds)) )
# 3417 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_176 =
  fun ef pi ->
    (
# 141 "src/frontend/parser.mly"
                      ( let (ecs,efs),gs,ts,ds = pi in ((ecs,ef::efs), gs,    ts,    ds   ) )
# 3428 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_177 =
  fun g pi ->
    (
# 142 "src/frontend/parser.mly"
                      ( let (ecs,efs),gs,ts,ds = pi in ((ecs,    efs), g::gs, ts,    ds   ) )
# 3439 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_178 =
  fun d pi ->
    (
# 143 "src/frontend/parser.mly"
                      ( let (ecs,efs),gs,ts,ds = pi in ((ecs,    efs), gs,    d::ts, ds   ) )
# 3450 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_179 =
  fun d pi ->
    (
# 144 "src/frontend/parser.mly"
                      ( let (ecs,efs),gs,ts,ds = pi in ((ecs,    efs), gs,    ts,    d::ds) )
# 3461 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_180 =
  fun pi ->
    (
# 145 "src/frontend/parser.mly"
                      ( pi )
# 3472 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_181 =
  fun () ->
    (
# 146 "src/frontend/parser.mly"
                      ( ([],[]),[],[],[] )
# 3483 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_182 =
  fun () ->
    (
# 485 "src/frontend/parser.mly"
     ( None )
# 3494 "src/frontend/parser.ml"
     : (Types.ty option))

let _menhir_action_183 =
  fun ty ->
    (
# 486 "src/frontend/parser.mly"
               ( Some ty )
# 3502 "src/frontend/parser.ml"
     : (Types.ty option))

let _menhir_action_184 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3510 "src/frontend/parser.ml"
     : (int list))

let _menhir_action_185 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3518 "src/frontend/parser.ml"
     : (int list))

let _menhir_action_186 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3526 "src/frontend/parser.ml"
     : (Ast.p list))

let _menhir_action_187 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3534 "src/frontend/parser.ml"
     : (Ast.p list))

let _menhir_action_188 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3542 "src/frontend/parser.ml"
     : (Ast.e list))

let _menhir_action_189 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3550 "src/frontend/parser.ml"
     : (Ast.e list))

let _menhir_action_190 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3558 "src/frontend/parser.ml"
     : (Ast.c list))

let _menhir_action_191 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3566 "src/frontend/parser.ml"
     : (Ast.c list))

let _menhir_action_192 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3574 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_193 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3582 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_194 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3590 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_195 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3598 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_196 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3606 "src/frontend/parser.ml"
     : (Types.size list))

let _menhir_action_197 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3614 "src/frontend/parser.ml"
     : (Types.size list))

let _menhir_action_198 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3622 "src/frontend/parser.ml"
     : (Types.tyB list))

let _menhir_action_199 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3630 "src/frontend/parser.ml"
     : (Types.tyB list))

let _menhir_action_200 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3638 "src/frontend/parser.ml"
     : (Ast.c list))

let _menhir_action_201 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3646 "src/frontend/parser.ml"
     : (Ast.c list))

let _menhir_action_202 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3654 "src/frontend/parser.ml"
     : ((Ast.x * Types.tyB) list))

let _menhir_action_203 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3662 "src/frontend/parser.ml"
     : ((Ast.x * Types.tyB) list))

let _menhir_action_204 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3670 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_205 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3678 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_206 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3686 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_207 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3694 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_208 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3702 "src/frontend/parser.ml"
     : (Types.tyB list))

let _menhir_action_209 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3710 "src/frontend/parser.ml"
     : (Types.tyB list))

let _menhir_action_210 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3718 "src/frontend/parser.ml"
     : (Types.ty list))

let _menhir_action_211 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3726 "src/frontend/parser.ml"
     : (Types.ty list))

let _menhir_action_212 =
  fun n ->
    (
# 390 "src/frontend/parser.mly"
            ( Sz_lit n )
# 3734 "src/frontend/parser.ml"
     : (Types.size))

let _menhir_action_213 =
  fun x ->
    (
# 391 "src/frontend/parser.mly"
               (
    decl_size_var x
  )
# 3744 "src/frontend/parser.ml"
     : (Types.size))

let _menhir_action_214 =
  fun sz ->
    (
# 359 "src/frontend/parser.mly"
          ( [sz] )
# 3752 "src/frontend/parser.ml"
     : (Types.size list))

let _menhir_action_215 =
  fun szs ->
    (
# 360 "src/frontend/parser.mly"
                                                ( szs )
# 3760 "src/frontend/parser.ml"
     : (Types.size list))

let _menhir_action_216 =
  fun _endpos__7_ _startpos__1_ e ec x ->
    let _endpos = _endpos__7_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 171 "src/frontend/parser.mly"
    ( let to_int e =
        match un_annot e with
        | E_const (Int(n,_)) -> n
        | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                          ~msg:("dimension for "^x^" should be an integer") ()
      in
      x,Static_array(e2c ec,to_int e)
  )
# 3778 "src/frontend/parser.ml"
     : (Ast.x * Ast.static))

let _menhir_action_217 =
  fun _endpos__6_ _startpos__1_ ty x ->
    let _endpos = _endpos__6_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 181 "src/frontend/parser.mly"
    (
      let loc = with_file _loc in
      if Types.no_unknown_in_ty ty then (
         Prelude.Errors.raise_error ~loc
           ~msg:"this type annotation should not contain type unknowns"
      ) ();
      x,Static_array_of (ty,loc)
    )
# 3796 "src/frontend/parser.ml"
     : (Ast.x * Ast.static))

let _menhir_action_218 =
  fun e ->
    (
# 194 "src/frontend/parser.mly"
                           ( e )
# 3804 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_219 =
  fun ty tys ->
    (
# 369 "src/frontend/parser.mly"
    ( Ty_tuple (ty::tys) )
# 3812 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_220 =
  fun ty ->
    (
# 370 "src/frontend/parser.mly"
             (ty)
# 3820 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_221 =
  fun sz ->
    (
# 325 "src/frontend/parser.mly"
                ( TyB_size(sz) )
# 3828 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_222 =
  fun tyB tyBs ->
    (
# 327 "src/frontend/parser.mly"
    ( TyB_tuple (tyB::tyBs) )
# 3836 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_223 =
  fun tyB ->
    (
# 328 "src/frontend/parser.mly"
               (tyB)
# 3844 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_224 =
  fun x ->
    (
# 309 "src/frontend/parser.mly"
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
# 3861 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_225 =
  fun x ->
    (
# 332 "src/frontend/parser.mly"
                  ( decl_tyB_var x )
# 3869 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_226 =
  fun x ->
    (
# 332 "src/frontend/parser.mly"
                  ( decl_tyB_var x )
# 3877 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_227 =
  fun tyB ->
    (
# 333 "src/frontend/parser.mly"
                ( tyB )
# 3885 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_228 =
  fun tyB ->
    (
# 334 "src/frontend/parser.mly"
                        ( tyB )
# 3893 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_229 =
  fun _endpos_szs_ _startpos_x_ szs x ->
    let _endpos = _endpos_szs_ in
    let _startpos = _startpos_x_ in
    let _loc = (_startpos, _endpos) in
    (
# 336 "src/frontend/parser.mly"
     ( match x with
       | "_" -> (new_tyB_unknown ())
       | "int" -> (match szs with 
                   | sz::[] -> 
                      TyB_int(sz)
                   | _ -> assert false) 
       | ("array" (* | "vect"*)) -> 
            Prelude.Errors.raise_error ~loc:(with_file _loc)
                 ~msg:("type parameter expected for type constructor "^x) ()
       | _ -> TyB_abstract(x,szs,[]) )
# 3913 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_230 =
  fun _endpos_szs_ _startpos_tyB_ szs tyB x ->
    let tyBs = 
# 363 "src/frontend/parser.mly"
               ( [tyB] )
# 3921 "src/frontend/parser.ml"
     in
    let _startpos_tyBs_ = _startpos_tyB_ in
    let _endpos = _endpos_szs_ in
    let _startpos = _startpos_tyBs_ in
    let _loc = (_startpos, _endpos) in
    (
# 347 "src/frontend/parser.mly"
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
# 3939 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_231 =
  fun _endpos_szs_ _startpos__1_ szs tyBs x ->
    let tyBs = 
# 364 "src/frontend/parser.mly"
                                                        ( tyBs )
# 3947 "src/frontend/parser.ml"
     in
    let _startpos_tyBs_ = _startpos__1_ in
    let _endpos = _endpos_szs_ in
    let _startpos = _startpos_tyBs_ in
    let _loc = (_startpos, _endpos) in
    (
# 347 "src/frontend/parser.mly"
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
# 3965 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_232 =
  fun x ->
    (
# 302 "src/frontend/parser.mly"
        ( x,None )
# 3973 "src/frontend/parser.ml"
     : (Types.x * Types.ty option))

let _menhir_action_233 =
  fun ty x ->
    (
# 304 "src/frontend/parser.mly"
        ( x,Some ty )
# 3981 "src/frontend/parser.ml"
     : (Types.x * Types.ty option))

let _menhir_action_234 =
  fun x_ty_opt ->
    (
# 306 "src/frontend/parser.mly"
        ( x_ty_opt )
# 3989 "src/frontend/parser.ml"
     : (Types.x * Types.ty option))

let _menhir_action_235 =
  fun x ->
    (
# 302 "src/frontend/parser.mly"
        ( x,None )
# 3997 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_236 =
  fun ty x ->
    (
# 304 "src/frontend/parser.mly"
        ( x,Some ty )
# 4005 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_237 =
  fun x_ty_opt ->
    (
# 306 "src/frontend/parser.mly"
        ( x_ty_opt )
# 4013 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_238 =
  fun tyB x ->
    (
# 250 "src/frontend/parser.mly"
                        ( x,tyB )
# 4021 "src/frontend/parser.ml"
     : (Ast.x * Types.tyB))

let _menhir_action_239 =
  fun tyB ->
    (
# 322 "src/frontend/parser.mly"
                ( Ty_base tyB )
# 4029 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_240 =
  fun x ->
    (
# 374 "src/frontend/parser.mly"
               ( decl_ty_var x )
# 4037 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_241 =
  fun sz tyB ->
    (
# 375 "src/frontend/parser.mly"
                                   ( Ty_array(sz,tyB) )
# 4045 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_242 =
  fun ty ->
    (
# 376 "src/frontend/parser.mly"
              ( ty )
# 4053 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_243 =
  fun ty tyB ->
    (
# 383 "src/frontend/parser.mly"
                                 ( Ty_fun(ty,Dur_one,tyB) )
# 4061 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_244 =
  fun ty tyB ->
    (
# 384 "src/frontend/parser.mly"
                                               ( Ty_fun(ty,new_dur_unknown(),tyB) )
# 4069 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_245 =
  fun ty tyB ->
    (
# 385 "src/frontend/parser.mly"
                           ( Ty_fun(ty,Dur_zero,tyB) )
# 4077 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_246 =
  fun tyB ->
    (
# 386 "src/frontend/parser.mly"
               ( Ty_base tyB )
# 4085 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_247 =
  fun ty ->
    (
# 387 "src/frontend/parser.mly"
                      ( ty )
# 4093 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_248 =
  fun _endpos__5_ _startpos__1_ ts x ->
    let _endpos = _endpos__5_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 245 "src/frontend/parser.mly"
   ( add_alias x (TyB_sum ts) (with_file _loc);
     clear_tyvar_constraints();
     x,ts )
# 4106 "src/frontend/parser.ml"
     : (Ast.x * (Ast.x * Types.tyB) list))

let _menhir_action_249 =
  fun _endpos__5_ _startpos__1_ tyB x ->
    let _endpos = _endpos__5_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 220 "src/frontend/parser.mly"
                                     ( add_alias x tyB (with_file _loc) )
# 4117 "src/frontend/parser.ml"
     : (unit))

let _menhir_action_250 =
  fun _endpos__1_inlined1_ _startpos__1_ tyB ->
    let r = 
# 237 "src/frontend/parser.mly"
             ( None )
# 4125 "src/frontend/parser.ml"
     in
    let _endpos_r_ = _endpos__1_inlined1_ in
    let _endpos = _endpos_r_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 222 "src/frontend/parser.mly"
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
# 4147 "src/frontend/parser.ml"
     : (unit))

let _menhir_action_251 =
  fun _endpos__3_ _startpos__1_ op tyB ->
    let r = 
# 238 "src/frontend/parser.mly"
                         ( Some (op,[]) )
# 4155 "src/frontend/parser.ml"
     in
    let _endpos_r_ = _endpos__3_ in
    let _endpos = _endpos_r_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 222 "src/frontend/parser.mly"
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
# 4177 "src/frontend/parser.ml"
     : (unit))

let _menhir_action_252 =
  fun _endpos__5_ _startpos__1_ intl op tyB ->
    let r = 
# 240 "src/frontend/parser.mly"
   ( Some (op,intl) )
# 4185 "src/frontend/parser.ml"
     in
    let _endpos_r_ = _endpos__5_ in
    let _endpos = _endpos_r_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 222 "src/frontend/parser.mly"
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
# 4207 "src/frontend/parser.ml"
     : (unit))

let _menhir_action_253 =
  fun _endpos_v_ _startpos_v_ v ->
    let _endpos = _endpos_v_ in
    let _startpos = _startpos_v_ in
    let _loc = (_startpos, _endpos) in
    (
# 399 "src/frontend/parser.mly"
               ( mk_loc (with_file _loc) v )
# 4218 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_254 =
  fun e es ->
    (
# 403 "src/frontend/parser.mly"
    ( E_tuple (e::es) )
# 4226 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_255 =
  fun e ->
    (
# 404 "src/frontend/parser.mly"
           ( e )
# 4234 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_256 =
  fun _endpos_e_ _startpos_x_ e x ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos_x_ in
    let _loc = (_startpos, _endpos) in
    (
# 716 "src/frontend/parser.mly"
        ( match x with
          | "_" -> e
          | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                  ~msg:"the wildcard_case should be named \"_\"" () )
# 4248 "src/frontend/parser.ml"
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
    | PAUSE ->
        "PAUSE"
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
  
  let _menhir_run_567 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_pi =
    fun _menhir_stack _v ->
      MenhirBox_pi _v
  
  let rec _menhir_goto_pi : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _endpos _v _menhir_s ->
      match _menhir_s with
      | MenhirState464 ->
          _menhir_run_567 _menhir_stack _v
      | MenhirState552 ->
          _menhir_run_566 _menhir_stack _endpos _v
      | MenhirState553 ->
          _menhir_run_565 _menhir_stack _endpos _v
      | MenhirState563 ->
          _menhir_run_564 _menhir_stack _endpos _v
      | MenhirState558 ->
          _menhir_run_559 _menhir_stack _endpos _v
      | MenhirState556 ->
          _menhir_run_557 _menhir_stack _endpos _v
      | MenhirState554 ->
          _menhir_run_555 _menhir_stack _endpos _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_566 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_type_alias -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _endpos _v ->
      let MenhirCell1_type_alias (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let (_endpos_pi_, pi) = (_endpos, _v) in
      let _v = _menhir_action_180 pi in
      _menhir_goto_pi _menhir_stack _endpos_pi_ _v _menhir_s
  
  and _menhir_run_565 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_typ_sum -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _endpos _v ->
      let MenhirCell1_typ_sum (_menhir_stack, _menhir_s, d) = _menhir_stack in
      let (_endpos_pi_, pi) = (_endpos, _v) in
      let _v = _menhir_action_178 d pi in
      _menhir_goto_pi _menhir_stack _endpos_pi_ _v _menhir_s
  
  and _menhir_run_564 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_decl -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _endpos _v ->
      let MenhirCell1_decl (_menhir_stack, _menhir_s, d) = _menhir_stack in
      let (_endpos_pi_, pi) = (_endpos, _v) in
      let _v = _menhir_action_179 d pi in
      _menhir_goto_pi _menhir_stack _endpos_pi_ _v _menhir_s
  
  and _menhir_run_559 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_ext_circ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _endpos _v ->
      let MenhirCell1_ext_circ (_menhir_stack, _menhir_s, ec, _startpos_ec_) = _menhir_stack in
      let (_endpos_pi_, pi) = (_endpos, _v) in
      let _v = _menhir_action_175 _endpos_pi_ _startpos_ec_ ec pi in
      _menhir_goto_pi _menhir_stack _endpos_pi_ _v _menhir_s
  
  and _menhir_run_557 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_ext_fun -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _endpos _v ->
      let MenhirCell1_ext_fun (_menhir_stack, _menhir_s, ef) = _menhir_stack in
      let (_endpos_pi_, pi) = (_endpos, _v) in
      let _v = _menhir_action_176 ef pi in
      _menhir_goto_pi _menhir_stack _endpos_pi_ _v _menhir_s
  
  and _menhir_run_555 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_static -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _endpos _v ->
      let MenhirCell1_static (_menhir_stack, _menhir_s, g) = _menhir_stack in
      let (_endpos_pi_, pi) = (_endpos, _v) in
      let _v = _menhir_action_177 g pi in
      _menhir_goto_pi _menhir_stack _endpos_pi_ _v _menhir_s
  
  let _menhir_run_551 : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
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
          let _v = _menhir_action_133 e in
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
      | MenhirState137 ->
          _menhir_run_138 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState466 ->
          _menhir_run_127 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_127 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState134 ->
          _menhir_run_127 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_127 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
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
      let _v = _menhir_action_030 _endpos_e_ _startpos_e_ e in
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
      | MenhirState552 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState553 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState554 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState556 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState558 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState563 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState538 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState533 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState529 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState020 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState447 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState449 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState030 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState436 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState419 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState415 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState412 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState400 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState402 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState107 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState388 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState392 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState382 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState386 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState379 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState377 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState366 ->
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
      | MenhirState409 ->
          _menhir_run_427 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState428 ->
          _menhir_run_427 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState425 ->
          _menhir_run_427 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
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
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState552 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState553 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState554 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState556 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState558 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState563 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState538 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState533 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState529 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState519 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState517 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState007 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState458 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState017 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState020 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState022 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState447 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState449 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState030 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState444 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState441 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState031 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState436 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState419 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState415 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState412 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState400 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState402 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState107 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState388 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState392 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState382 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState386 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState379 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState377 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState366 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState362 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState351 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState198 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState201 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState342 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState344 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState339 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState335 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState203 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState331 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState216 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState218 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState219 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState320 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState316 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState294 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState299 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState248 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState289 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState287 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState285 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState283 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState281 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState279 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState277 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState275 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState273 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState271 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState269 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState267 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState265 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState263 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState261 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState259 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState257 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState255 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState253 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState251 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState246 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState241 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState231 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState229 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState199 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState104 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState087 ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_427 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PIPE ->
          let _menhir_stack = MenhirCell1_const (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState428 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
  
  and _menhir_run_032 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_x_, _startpos_x_, x) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_102 x in
      _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _startpos_x_ _v _menhir_s _tok
  
  and _menhir_run_050 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState050 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LXOR ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_036 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_038 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_040 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__2_ = _endpos in
      let _v = _menhir_action_097 () in
      _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_041 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_043 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_045 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_051 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_048 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_MINUS -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_MINUS (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__3_ = _endpos in
      let _v = _menhir_action_105 () in
      _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_052 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_054 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_056 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_058 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_060 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_062 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_064 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_066 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_068 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_070 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_072 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_074 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_076 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_078 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_049 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LCUR (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState049 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_080 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
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
      | AMP | AMP_AMP | AND | ASR | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LAND | LCUR | LE | LOR | LPAREN | LSL | LSR | LT | LXOR | MATCH | MINUS | MOD | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RCUR | RESET | RESIZE_INT | RIGHT_ARROW | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TIMES | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH | XOR ->
          let (_endpos_n_, _startpos_n_, n) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_099 n in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_n_ _startpos_n_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_083 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
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
      | MenhirState428 ->
          _menhir_run_429 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState425 ->
          _menhir_run_418 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState409 ->
          _menhir_run_418 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_429 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_const -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_const (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_201 x xs in
      _menhir_goto_separated_nonempty_list_PIPE_const_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_418 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_separated_nonempty_list_PIPE_const_ (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState419 in
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
              let _v = _menhir_action_011 k x in
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
      | MenhirState519 ->
          _menhir_run_520 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState517 ->
          _menhir_run_518 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState007 ->
          _menhir_run_460 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState022 ->
          _menhir_run_452 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState444 ->
          _menhir_run_445 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState441 ->
          _menhir_run_442 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState031 ->
          _menhir_run_439 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState034 ->
          _menhir_run_405 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState316 ->
          _menhir_run_316 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState294 ->
          _menhir_run_316 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState248 ->
          _menhir_run_294 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState464 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState552 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState553 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState554 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState556 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState558 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState563 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState538 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState533 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState529 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState017 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState458 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState020 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState447 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState449 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState030 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState436 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState419 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState415 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState412 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState400 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState402 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState107 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState388 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState392 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState382 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState386 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState379 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState377 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState366 ->
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
      | MenhirState105 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState104 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState097 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState098 ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_520 : type  ttv_stack. (((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_STATIC _menhir_cell0_IDENT, _menhir_box_pi) _menhir_cell1_aexp -> _ -> _ -> _ -> _ -> _menhir_box_pi =
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
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState554
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | TYPE ->
          _menhir_run_465 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState554
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | SHARED ->
          _menhir_run_492 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState554
      | OPERATOR ->
          _menhir_run_498 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | LET ->
          _menhir_run_514 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState554
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState554
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | EXTERNAL ->
          _menhir_run_546 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | EOF ->
          _menhir_run_551 _menhir_stack _menhir_lexbuf MenhirState554
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState554
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState554
      | _ ->
          _eRR ()
  
  and _menhir_run_465 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_TYPE (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState465
      | TVAR_IDENT _v ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState465
      | LT ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState465
      | LPAREN ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState465
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
              _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState466
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
                  _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TVAR_IDENT _v ->
                  _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | LT ->
                  _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | AT | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | PAUSE | REF | REGISTER | RESIZE_INT | RUN | SEMI_SEMI | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TIMES | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
              let _v =
                let x = _v in
                _menhir_action_224 x
              in
              _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_0 _v MenhirState465 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_113 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_startpos_x_, x) = (_startpos, _v) in
      let _v = _menhir_action_226 x in
      _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_x_ _v _menhir_s _tok
  
  and _menhir_goto_tyB_next : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState115 ->
          _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState148 ->
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState465 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState467 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState469 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState165 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState163 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState154 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState548 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState523 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState508 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState501 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState495 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState396 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState358 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState210 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState195 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState129 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState131 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_168 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ARRAY ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | RPAREN ->
          let tyB = _v in
          let _v = _menhir_action_223 tyB in
          _menhir_goto_tyB _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | IMPLY | QUESTION_MARK | RIGHT_ARROW ->
          let tyB = _v in
          let _v = _menhir_action_246 tyB in
          _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_146 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB_next -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState146 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TVAR_IDENT _v ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_143 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_startpos_x_, x) = (_startpos, _v) in
      let _v = _menhir_action_225 x in
      _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_x_ _v _menhir_s _tok
  
  and _menhir_run_144 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState144 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TVAR_IDENT _v ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LT ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_116 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState116 in
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
  
  and _menhir_run_119 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TVAR_IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState119
      | LT ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState119
      | INT_LIT _v_1 ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState119
      | ARRAY | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | AT | BANG | BOOL_LIT _ | COMMA | CREATE | EOF | EQ | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | IMPLY | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | PAUSE | PIPE | QUESTION_MARK | REF | REGISTER | RESIZE_INT | RIGHT_ARROW | RPAREN | RUN | SEMI_SEMI | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TIMES | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let (_startpos_x_, x) = (_startpos, _v) in
          let _v = _menhir_action_224 x in
          _menhir_goto_tyB_ident _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_x_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_120 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState120 in
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
      match _menhir_s with
      | MenhirState465 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState467 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState469 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState165 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState163 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState154 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState148 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState548 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState523 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState508 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState501 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState495 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState396 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState358 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState210 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState195 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState115 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState129 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState131 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_149 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_startpos_tyB_, tyB) = (_startpos, _v) in
      let _v = _menhir_action_227 tyB in
      _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_tyB_ _v _menhir_s _tok
  
  and _menhir_run_140 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ARRAY | COMMA | IDENT _ ->
          let (_startpos_tyB_, tyB) = (_startpos, _v) in
          let _v = _menhir_action_227 tyB in
          _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_tyB_ _v _menhir_s _tok
      | AT | EQ | IMPLY | QUESTION_MARK | RIGHT_ARROW | RPAREN | SEMI_SEMI | TIMES ->
          let tyB = _v in
          let _v = _menhir_action_239 tyB in
          let ty = _v in
          let _v = _menhir_action_242 ty in
          _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_ty_next : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState131 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState129 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState548 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState523 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState508 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState501 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState495 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState396 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState358 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState210 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState195 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState115 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_130 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_ty_next as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState131 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVAR_IDENT _v ->
              _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RIGHT_ARROW ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v) in
          _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer
      | QUESTION_MARK ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v) in
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IMPLY ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v) in
          _menhir_run_165 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AT | EQ | RPAREN | SEMI_SEMI ->
          let x = _v in
          let _v = _menhir_action_210 x in
          _menhir_goto_separated_nonempty_list_TIMES_ty_next_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_114 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_startpos_x_, x) = (_startpos, _v) in
      let _v = _menhir_action_225 x in
      _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_x_ _v _menhir_s _tok
  
  and _menhir_run_115 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState115 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TVAR_IDENT _v ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LT ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_142 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState142 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TVAR_IDENT _v ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LT ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_162 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW ->
          let _menhir_s = MenhirState163 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVAR_IDENT _v ->
              _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LT ->
              _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_165 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState165 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TVAR_IDENT _v ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LT ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_TIMES_ty_next_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_ty_next as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState129 ->
          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState131 ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_167 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty_next (_menhir_stack, _menhir_s, ty) = _menhir_stack in
      let tys = _v in
      let _v = _menhir_action_219 ty tys in
      _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_ty : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState548 ->
          _menhir_run_549 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState523 ->
          _menhir_run_524 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState508 ->
          _menhir_run_509 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState501 ->
          _menhir_run_502 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState495 ->
          _menhir_run_496 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState396 ->
          _menhir_run_397 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState358 ->
          _menhir_run_359 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState210 ->
          _menhir_run_211 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState195 ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState190 ->
          _menhir_run_191 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState112 ->
          _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState115 ->
          _menhir_run_169 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_549 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_EXTERNAL _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
          let MenhirCell1_EXTERNAL (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_134 t x in
          _menhir_goto_ext_circ _menhir_stack _menhir_lexbuf _menhir_lexer _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_ext_circ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ext_circ (_menhir_stack, _menhir_s, _v, _startpos) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState558
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | TYPE ->
          _menhir_run_465 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState558
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | SHARED ->
          _menhir_run_492 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState558
      | OPERATOR ->
          _menhir_run_498 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | LET ->
          _menhir_run_514 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState558
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState558
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | EXTERNAL ->
          _menhir_run_546 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | EOF ->
          _menhir_run_551 _menhir_stack _menhir_lexbuf MenhirState558
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState558
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState558
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PIPE_RBRACKET ->
          let _v = _menhir_action_156 () in
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
  
  and _menhir_run_027 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_PARFOR (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | EQ ->
              let _menhir_s = MenhirState029 in
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
                  _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | REF ->
                  _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PAUSE ->
                  _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PARFOR ->
                  _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPERATOR_IDENT _v ->
                  _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | MINUS ->
                  _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MATCH ->
                  _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_GENERATE ->
                  _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LET ->
                  _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LENGTH ->
                  _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LCUR ->
                  _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_OF_TUPLE ->
                  _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_LIT _v ->
                  _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IF ->
                  _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FUN ->
                  _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FOR ->
                  _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FIX ->
                  _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | EXEC ->
                  _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CREATE ->
                  _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL_LIT _v ->
                  _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
  
  and _menhir_run_030 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_REGISTER (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState030
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState030
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState030
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | LET ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState030
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | IDENT _v ->
          let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACKET ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState030, _v, _startpos_0, _endpos) in
              _menhir_run_203 _menhir_stack _menhir_lexbuf _menhir_lexer
          | INIT ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState030, _v, _startpos_0, _endpos) in
              let _menhir_s = MenhirState441 in
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
              | PARFOR ->
                  _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPERATOR_IDENT _v ->
                  _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | MATCH ->
                  _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LCUR ->
                  _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_OF_TUPLE ->
                  _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_LIT _v ->
                  _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IDENT _v ->
                  _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FOR ->
                  _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL_LIT _v ->
                  _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | DOT_LENGTH ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState030, _v, _startpos_0, _endpos) in
              _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer
          | DOT ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState030, _v, _startpos_0, _endpos) in
              _menhir_run_334 _menhir_stack _menhir_lexbuf _menhir_lexer
          | AMP | AMP_AMP | ASR | BANG | BOOL_LIT _ | COL_EQ | COMMA | DIV | EQ | EQ_EQ | FOR | GE | GT | IDENT _ | INT_LIT _ | INT_OF_TUPLE | LAND | LCUR | LE | LOR | LPAREN | LSL | LSR | LT | LXOR | MATCH | MINUS | MOD | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PLUS | RESIZE_INT | SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | TIMES | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | XOR ->
              let (_menhir_s, _endpos_x_, _startpos_x_, x) = (MenhirState030, _endpos, _startpos_0, _v) in
              let _v = _menhir_action_013 _endpos_x_ _startpos_x_ x in
              _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _startpos_x_ _v _menhir_s _tok
          | _ ->
              _eRR ())
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState030
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | _ ->
          _eRR ()
  
  and _menhir_run_031 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_REF (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState031 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_033 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_MATCH (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState033 in
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_034 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_PAUSE (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState034 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_035 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState035 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer
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
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer
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
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NEQ ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LXOR ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LOR ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LET ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LE ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAND ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | GT ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EQ_EQ ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASR ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_047 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState047 in
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
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_089 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
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
  
  and _menhir_run_093 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_x_, _startpos_x_, x) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_013 _endpos_x_ _startpos_x_ x in
      _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _startpos_x_ _v _menhir_s _tok
  
  and _menhir_run_094 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
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
              let _menhir_s = MenhirState096 in
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
                  _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | REF ->
                  _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PAUSE ->
                  _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PARFOR ->
                  _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPERATOR_IDENT _v ->
                  _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | MINUS ->
                  _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MATCH ->
                  _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_GENERATE ->
                  _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LET ->
                  _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LENGTH ->
                  _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LCUR ->
                  _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_OF_TUPLE ->
                  _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_LIT _v ->
                  _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IF ->
                  _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FUN ->
                  _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FOR ->
                  _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FIX ->
                  _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | EXEC ->
                  _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CREATE ->
                  _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL_LIT _v ->
                  _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
  
  and _menhir_run_097 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState097 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_098 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_BANG (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState098 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_103 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_MACRO_GENERATE (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState103 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_107 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState107 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer
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
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer
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
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NEQ ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LXOR ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LOR ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LET ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LE ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAND ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | GT ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EQ_EQ ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASR ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_108 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | REC ->
          let _menhir_stack = MenhirCell1_REC (_menhir_stack, MenhirState108) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
          | IDENT _v ->
              let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LPAREN ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState109, _v, _startpos_0, _endpos) in
                  _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState174
              | IDENT _v_1 ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState109, _v, _startpos_0, _endpos) in
                  _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState174
              | COL ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState109, _v, _startpos_0, _endpos) in
                  _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState174
              | EQ ->
                  let _v =
                    let x = _v in
                    _menhir_action_232 x
                  in
                  _menhir_run_350 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_0 _v MenhirState109 _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | LPAREN ->
          _menhir_run_354 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | IDENT _v ->
          let _startpos_2 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState108, _v, _startpos_2, _endpos) in
              _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState360
          | IDENT _v_3 ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState108, _v, _startpos_2, _endpos) in
              _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState360
          | COL | EQ ->
              let _v =
                let x = _v in
                _menhir_action_029 x
              in
              _menhir_run_376 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState108 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_110 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | IDENT _v ->
          let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COL ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState110, _v, _startpos_0, _endpos) in
              _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState111
          | RPAREN ->
              let _v =
                let x = _v in
                _menhir_action_232 x
              in
              _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_112 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_COL (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState112 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TVAR_IDENT _v ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_172 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let x_ty_opt = _v in
          let _v = _menhir_action_234 x_ty_opt in
          _menhir_goto_ty_annot_IDENT_ _menhir_stack _menhir_lexbuf _menhir_lexer _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_ty_annot_IDENT_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState526 ->
          _menhir_run_532 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_350 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_532 : type  ttv_stack. (((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty_annot_IDENT_ (_menhir_stack, _menhir_s, _v, _startpos) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState533 in
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
      | AMP | AMP_AMP | AND | ASR | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LAND | LCUR | LE | LOR | LPAREN | LSL | LSR | LT | LXOR | MATCH | MINUS | MOD | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TIMES | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH | XOR ->
          let (_endpos_x_, _startpos_x_, x) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_013 _endpos_x_ _startpos_x_ x in
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_176 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _v = _menhir_action_027 () in
      _menhir_goto_apat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_apat : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState432 ->
          _menhir_run_411 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState410 ->
          _menhir_run_411 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState514 ->
          _menhir_run_376 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_376 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState374 ->
          _menhir_run_376 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState354 ->
          _menhir_run_357 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState536 ->
          _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState527 ->
          _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState360 ->
          _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState204 ->
          _menhir_run_209 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState297 ->
          _menhir_run_209 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState205 ->
          _menhir_run_209 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState175 ->
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState185 ->
          _menhir_run_184 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState182 ->
          _menhir_run_184 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState177 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_411 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_UP_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW ->
          let _menhir_s = MenhirState412 in
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
  
  and _menhir_run_376 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState377 in
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TVAR_IDENT _v ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_357 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COL ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_358 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let x = _v in
          let _v = _menhir_action_235 x in
          _menhir_goto_ty_annot_apat_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_182 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_apat -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState182 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_177 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState177 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_178 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let x = _v in
      let _v = _menhir_action_029 x in
      _menhir_goto_apat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_ty_annot_apat_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState514 ->
          _menhir_run_365 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState374 ->
          _menhir_run_365 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_365 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState354 ->
          _menhir_run_355 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_365 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty_annot_apat_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState366 in
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          let _v = _menhir_action_237 x_ty_opt in
          _menhir_goto_ty_annot_apat_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_349 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let p = _v in
      let _v = _menhir_action_086 p in
      _menhir_goto_arg_ty_atomic _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_arg_ty_atomic : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState536 ->
          _menhir_run_537 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState527 ->
          _menhir_run_528 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState360 ->
          _menhir_run_361 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_537 : type  ttv_stack. (((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_arg_ty_atomic (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState537
      | COL ->
          _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState537
      | _ ->
          _eRR ()
  
  and _menhir_run_194 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_182 () in
      _menhir_goto_ret_ty_annot_eq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_ret_ty_annot_eq : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState537 ->
          _menhir_run_538 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState528 ->
          _menhir_run_529 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState361 ->
          _menhir_run_362 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState193 ->
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_538 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ret_ty_annot_eq (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState538
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState538
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState538
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | LET ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState538
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState538
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState538
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState538
      | _ ->
          _eRR ()
  
  and _menhir_run_529 : type  ttv_stack. (((((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ret_ty_annot_eq (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState529
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState529
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState529
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | LET ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState529
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState529
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState529
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState529
      | _ ->
          _eRR ()
  
  and _menhir_run_362 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ret_ty_annot_eq (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState362
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState362
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState362
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | LET ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState362
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState362
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState362
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState362
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState198
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | LET ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState198
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState198
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState198
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState198
      | _ ->
          _eRR ()
  
  and _menhir_run_195 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_COL (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState195 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TVAR_IDENT _v ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_528 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_arg_ty_atomic (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState528
      | COL ->
          _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState528
      | _ ->
          _eRR ()
  
  and _menhir_run_361 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_arg_ty_atomic (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState361
      | COL ->
          _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState361
      | _ ->
          _eRR ()
  
  and _menhir_run_193 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_REC, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_arg_ty_atomic (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | COL ->
          _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | _ ->
          _eRR ()
  
  and _menhir_run_209 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COL ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState210 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVAR_IDENT _v ->
              _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
      | MenhirState175 ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState354 ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState177 ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_213 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let p = _v in
      let _v = _menhir_action_087 p in
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
  
  and _menhir_run_206 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_pat (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_180 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_pat -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_pat (_menhir_stack, _, p) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _v = _menhir_action_028 p in
      _menhir_goto_apat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_187 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_179 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_pat (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_189 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COL ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState190 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVAR_IDENT _v ->
              _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let p = _v in
          let _v = _menhir_action_173 p in
          _menhir_goto_pat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_184 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_apat as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState185 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
      | MenhirState185 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState182 ->
          _menhir_run_183 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_186 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_apat, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_apat (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_187 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_apat_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_183 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_apat (_menhir_stack, _menhir_s, p) = _menhir_stack in
      let ps = _v in
      let _v = _menhir_action_174 p ps in
      _menhir_goto_pat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_181 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let p = _v in
          let _v = _menhir_action_173 p in
          _menhir_goto_pat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
  
  and _menhir_run_175 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState175 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_354 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_loption_separated_nonempty_list_COMMA_app_exp__ : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_SHARP_PIPE_LBRACKET -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_SHARP_PIPE_LBRACKET (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__3_, xs) = (_endpos, _v) in
      let _v = _menhir_action_014 xs in
      _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
  
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
                      _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | TVAR_IDENT _v ->
                      _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | LPAREN ->
                      _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IDENT _v ->
                      _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
                      _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | TVAR_IDENT _v ->
                      _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | LPAREN ->
                      _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IDENT _v ->
                      _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
                  _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TVAR_IDENT _v ->
                  _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | LPAREN ->
                  _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_514 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STATIC ->
          let _menhir_stack = MenhirCell1_STATIC (_menhir_stack, MenhirState514) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v, _startpos, _endpos) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | EQ ->
                  let _menhir_s = MenhirState517 in
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
                  | PARFOR ->
                      _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | OPERATOR_IDENT _v ->
                      _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | MATCH ->
                      _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LPAREN ->
                      _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LCUR ->
                      _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | INT_OF_TUPLE ->
                      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | INT_LIT _v ->
                      _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | IDENT _v ->
                      _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | FOR ->
                      _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | BOOL_LIT _v ->
                      _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | BANG ->
                      _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | _ ->
                      _eRR ())
              | COL ->
                  let _menhir_s = MenhirState523 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | TYB_VAR_IDENT _v ->
                      _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | TVAR_IDENT _v ->
                      _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | LPAREN ->
                      _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IDENT _v ->
                      _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | REC ->
          let _menhir_stack = MenhirCell1_REC (_menhir_stack, MenhirState514) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState526
          | IDENT _v ->
              let _startpos_10 = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LPAREN ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState526, _v, _startpos_10, _endpos) in
                  _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState527
              | IDENT _v_11 ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState526, _v, _startpos_10, _endpos) in
                  _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v_11 MenhirState527
              | COL ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState526, _v, _startpos_10, _endpos) in
                  _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState527
              | EQ ->
                  let _v =
                    let x = _v in
                    _menhir_action_232 x
                  in
                  _menhir_run_532 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_10 _v MenhirState526 _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | LPAREN ->
          _menhir_run_354 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState514
      | IDENT _v ->
          let _startpos_12 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState514, _v, _startpos_12, _endpos) in
              _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState536
          | IDENT _v_13 ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState514, _v, _startpos_12, _endpos) in
              _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v_13 MenhirState536
          | COL | EQ ->
              let _v =
                let x = _v in
                _menhir_action_029 x
              in
              _menhir_run_376 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState514 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_546 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
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
              let _menhir_s = MenhirState548 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TYB_VAR_IDENT _v ->
                  _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TVAR_IDENT _v ->
                  _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | LPAREN ->
                  _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_524 : type  ttv_stack. ((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_STATIC _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_pi =
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
          let _v = _menhir_action_137 t x in
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
                  let _v = _menhir_action_136 t x in
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState556
      | OPERATOR ->
          _menhir_run_498 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | LET ->
          _menhir_run_514 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState556
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState556
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | EXTERNAL ->
          _menhir_run_546 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | EOF ->
          _menhir_run_551 _menhir_stack _menhir_lexbuf MenhirState556
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState556
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState556
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
          let _v = _menhir_action_139 t x in
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
                  let _v = _menhir_action_138 t x in
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
          let _v = _menhir_action_135 t x in
          _menhir_goto_ext_circ _menhir_stack _menhir_lexbuf _menhir_lexer _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_397 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_exp -> _ -> _ -> _ -> _ -> ttv_result =
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
      let _v = _menhir_action_236 ty x in
      _menhir_goto_ty_annot_apat_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_211 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let ty = _v in
      let _v = _menhir_action_089 ty in
      let MenhirCell1_apat (_menhir_stack, _menhir_s, p) = _menhir_stack in
      let ty = _v in
      let _v = _menhir_action_088 p ty in
      _menhir_goto_arg_ty_unparen _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_196 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic, ttv_result) _menhir_cell1_COL -> _ -> _ -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_191 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_171 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_COL -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_COL (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
      let ty = _v in
      let _v = _menhir_action_233 ty x in
      _menhir_goto_ty_annot_IDENT_ _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_x_ _v _menhir_s _tok
  
  and _menhir_run_169 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let ty = _v in
          let _v = _menhir_action_247 ty in
          _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_141 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_ty_next, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty_next (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_211 x xs in
      _menhir_goto_separated_nonempty_list_TIMES_ty_next_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_128 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState129 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVAR_IDENT _v ->
              _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RIGHT_ARROW ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v) in
          _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer
      | QUESTION_MARK ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v) in
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IMPLY ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v) in
          _menhir_run_165 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AT | EQ | RPAREN | SEMI_SEMI ->
          let ty = _v in
          let _v = _menhir_action_220 ty in
          _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_134 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB_next -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v, _startpos, _endpos) in
      let _menhir_s = MenhirState134 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TVAR_IDENT _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LT ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_136 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB_next -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LT ->
          let _menhir_s = MenhirState137 in
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
  
  and _menhir_goto_tyB : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState465 ->
          _menhir_run_479 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState467 ->
          _menhir_run_474 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState469 ->
          _menhir_run_470 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState165 ->
          _menhir_run_166 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState163 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState142 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState154 ->
          _menhir_run_155 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState115 ->
          _menhir_run_152 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_152 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
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
              | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | INT_LIT _ | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | PAUSE | REF | REGISTER | RESIZE_INT | RUN | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
                  let _ = _menhir_action_171 () in
                  _menhir_run_490 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IF | INT_LIT _ | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | PAUSE | REF | REGISTER | RESIZE_INT | RUN | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
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
      let _v = _menhir_action_250 _endpos__1_inlined1_ _startpos__1_ tyB in
      _menhir_goto_type_alias _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_type_alias : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_alias (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState552
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | TYPE ->
          _menhir_run_465 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState552
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | SHARED ->
          _menhir_run_492 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState552
      | OPERATOR ->
          _menhir_run_498 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | LET ->
          _menhir_run_514 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState552
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState552
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | EXTERNAL ->
          _menhir_run_546 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | EOF ->
          _menhir_run_551 _menhir_stack _menhir_lexbuf MenhirState552
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState552
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | _ ->
          _eRR ()
  
  and _menhir_run_490 : type  ttv_stack. (((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_tyB, _menhir_box_pi) _menhir_cell1_AT _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok ->
      let MenhirCell0_IDENT (_menhir_stack, op, _, _) = _menhir_stack in
      let MenhirCell1_AT (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_tyB (_menhir_stack, _, tyB) = _menhir_stack in
      let MenhirCell1_TYPE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__3_ = _endpos in
      let _v = _menhir_action_251 _endpos__3_ _startpos__1_ op tyB in
      _menhir_goto_type_alias _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_487 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_tyB, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_separated_nonempty_list_COMMA_INT_LIT_ _menhir_cell0_RBRACKET -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok ->
      let MenhirCell0_RBRACKET (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_separated_nonempty_list_COMMA_INT_LIT_ (_menhir_stack, _, intl) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _, op, _, _) = _menhir_stack in
      let MenhirCell1_tyB (_menhir_stack, _, tyB) = _menhir_stack in
      let MenhirCell1_TYPE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__5_ = _endpos in
      let _v = _menhir_action_252 _endpos__5_ _startpos__1_ intl op tyB in
      _menhir_goto_type_alias _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_478 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_EQ, _menhir_box_pi) _menhir_cell1_separated_nonempty_list_PIPE_ty_case_ -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok ->
      let MenhirCell1_separated_nonempty_list_PIPE_ty_case_ (_menhir_stack, _, ts) = _menhir_stack in
      let MenhirCell1_EQ (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _, x, _, _) = _menhir_stack in
      let MenhirCell1_TYPE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__5_ = _endpos in
      let _v = _menhir_action_248 _endpos__5_ _startpos__1_ ts x in
      let _menhir_stack = MenhirCell1_typ_sum (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState553
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | TYPE ->
          _menhir_run_465 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState553
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | SHARED ->
          _menhir_run_492 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState553
      | OPERATOR ->
          _menhir_run_498 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | LET ->
          _menhir_run_514 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState553
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState553
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | EXTERNAL ->
          _menhir_run_546 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | EOF ->
          _menhir_run_551 _menhir_stack _menhir_lexbuf MenhirState553
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState553
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState553
      | _ ->
          _eRR ()
  
  and _menhir_run_476 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_EQ, _menhir_box_pi) _menhir_cell1_tyB -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok ->
      let MenhirCell1_tyB (_menhir_stack, _, tyB) = _menhir_stack in
      let MenhirCell1_EQ (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _, x, _, _) = _menhir_stack in
      let MenhirCell1_TYPE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__5_ = _endpos in
      let _v = _menhir_action_249 _endpos__5_ _startpos__1_ tyB x in
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
      | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | INT_LIT _ | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | PAUSE | REF | REGISTER | RESIZE_INT | RUN | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
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
      | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | INT_LIT _ | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | PAUSE | REF | REGISTER | RESIZE_INT | RUN | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let _ = _menhir_action_171 () in
          _menhir_run_476 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_470 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_UP_IDENT -> _ -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_UP_IDENT (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let tyB = _v in
      let _v = _menhir_action_238 tyB x in
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
      | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | INT_LIT _ | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | PAUSE | REF | REGISTER | RESIZE_INT | RUN | SEMI_SEMI | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
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
              _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVAR_IDENT _v ->
              _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LT ->
              _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
      | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | INT_LIT _ | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | PAUSE | REF | REGISTER | RESIZE_INT | RUN | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
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
  
  and _menhir_run_166 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty_next (_menhir_stack, _menhir_s, ty) = _menhir_stack in
      let tyB = _v in
      let _v = _menhir_action_245 ty tyB in
      _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_164 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty_next (_menhir_stack, _menhir_s, ty) = _menhir_stack in
      let tyB = _v in
      let _v = _menhir_action_244 ty tyB in
      _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_161 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty_next (_menhir_stack, _menhir_s, ty) = _menhir_stack in
      let tyB = _v in
      let _v = _menhir_action_243 ty tyB in
      _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_155 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_tyB as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_tyB (_menhir_stack, _menhir_s, _v) in
          _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let x = _v in
          let _v = _menhir_action_198 x in
          _menhir_goto_separated_nonempty_list_COMMA_tyB_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_154 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState154 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TVAR_IDENT _v ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LT ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_tyB_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState115 ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState144 ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState154 ->
          _menhir_run_156 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_157 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
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
          let _menhir_s = MenhirState159 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TVAR_IDENT _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LT ->
              _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_156 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_tyB (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_199 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_tyB_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_152 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let tyB = _v in
          let _v = _menhir_action_228 tyB in
          _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos__1_ _v _menhir_s _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_tyB (_menhir_stack, _menhir_s, _v) in
          _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_147 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_tyB_next as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          let _menhir_s = MenhirState148 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVAR_IDENT _v ->
              _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | IDENT _v_3 ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | AT | BANG | BOOL_LIT _ | COMMA | CREATE | EOF | EQ | EXEC | EXTERNAL | FIX | FOR | FUN | IF | IMPLY | INT_LIT _ | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | PAUSE | PIPE | QUESTION_MARK | REF | REGISTER | RESIZE_INT | RIGHT_ARROW | RPAREN | RUN | SEMI_SEMI | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let x = _v in
          let _v = _menhir_action_208 x in
          _menhir_goto_separated_nonempty_list_TIMES_tyB_next_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_TIMES_tyB_next_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_tyB_next as 'stack) -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState146 ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState148 ->
          _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_151 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB_next -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_tyB_next (_menhir_stack, _menhir_s, tyB, _) = _menhir_stack in
      let tyBs = _v in
      let _v = _menhir_action_222 tyB tyBs in
      _menhir_goto_tyB _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
  
  and _menhir_run_150 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_tyB_next, ttv_result) _menhir_cell1_tyB_next -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_tyB_next (_menhir_stack, _menhir_s, x, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_209 x xs in
      _menhir_goto_separated_nonempty_list_TIMES_tyB_next_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
  
  and _menhir_run_145 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | AT | BANG | BOOL_LIT _ | COMMA | CREATE | EOF | EQ | EXEC | EXTERNAL | FIX | FOR | FUN | IF | IMPLY | INT_LIT _ | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | PAUSE | PIPE | QUESTION_MARK | REF | REGISTER | RESIZE_INT | RIGHT_ARROW | RPAREN | RUN | SEMI_SEMI | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let tyB = _v in
          let _v = _menhir_action_223 tyB in
          _menhir_goto_tyB _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_133 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ARRAY ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AT | EQ | IMPLY | QUESTION_MARK | RIGHT_ARROW | RPAREN | SEMI_SEMI | TIMES ->
          let tyB = _v in
          let _v = _menhir_action_246 tyB in
          _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_518 : type  ttv_stack. (((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_STATIC _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | HAT ->
          let _menhir_s = MenhirState519 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
  
  and _menhir_run_445 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_REGISTER, ttv_result) _menhir_cell1_exp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_exp (_menhir_stack, _, ev, _, _) = _menhir_stack in
      let MenhirCell1_REGISTER (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e0_, e0) = (_endpos, _v) in
      let _v = _menhir_action_073 _endpos_e0_ _startpos__1_ e0 ev in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e0_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_442 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_REGISTER, ttv_result) _menhir_cell1_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_IDENT (_menhir_stack, _, f, _, _) = _menhir_stack in
      let MenhirCell1_REGISTER (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e0_, e0) = (_endpos, _v) in
      let _v = _menhir_action_074 e0 f in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e0_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_439 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_REF -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_REF (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_038 e in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_405 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_PAUSE -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_PAUSE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_031 _endpos_e_ _startpos__1_ e in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
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
      | PARFOR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | OPERATOR_IDENT _v_2 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState316
      | MATCH ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | LPAREN ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | LCUR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | INT_OF_TUPLE ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | INT_LIT _v_3 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState316
      | IDENT _v_4 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState316
      | FOR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | BOOL_LIT _v_5 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState316
      | BANG ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState316
      | AMP | AMP_AMP | AND | ASR | AT | AT_AT | COL | COL_EQ | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | HAT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
          let (_endpos_x_, x) = (_endpos, _v) in
          let _v = _menhir_action_165 x in
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
      let _v = _menhir_action_166 x xs in
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
      | PARFOR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | OPERATOR_IDENT _v_2 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState294
      | MATCH ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | LPAREN ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | LCUR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | INT_OF_TUPLE ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | INT_LIT _v_3 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState294
      | IDENT _v_4 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState294
      | FOR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | BOOL_LIT _v_5 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState294
      | BANG ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
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
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_296 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_296 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | AMP | AMP_AMP | AND | ASR | COL | COL_EQ | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | HAT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
          let (_endpos_x_, x) = (_endpos, _v) in
          let _v = _menhir_action_165 x in
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
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer
      | UP_IDENT _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TIMES ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | RPAREN ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NEQ ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LXOR ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_296 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LOR ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAND ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | GT ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FUN ->
          _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EQ_EQ ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ASR ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer
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
          _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
      | PARFOR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | OPERATOR_IDENT _v_2 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState248
      | MATCH ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | LPAREN ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | LCUR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | INT_OF_TUPLE ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | INT_LIT _v_3 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState248
      | IDENT _v_4 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState248
      | FOR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_11 MenhirState248
      | BANG ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState248
      | AMP | AMP_AMP | AND | ASR | AT | AT_AT | COL | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | HAT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
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
  
  and _menhir_run_106 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_MACRO_GENERATE, ttv_result) _menhir_cell1_aexp, ttv_result) _menhir_cell1_aexp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_aexp (_menhir_stack, _, e_init2, _, _) = _menhir_stack in
      let MenhirCell1_aexp (_menhir_stack, _, ef1, _, _) = _menhir_stack in
      let MenhirCell1_MACRO_GENERATE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_st3_, e_st3) = (_endpos, _v) in
      let _v = _menhir_action_077 _endpos_e_st3_ _startpos__1_ e_init2 e_st3 ef1 in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_st3_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_105 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_MACRO_GENERATE, ttv_result) _menhir_cell1_aexp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState105
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState105
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState105
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState105
      | IDENT _v_4 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState105
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState105
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | _ ->
          _eRR ()
  
  and _menhir_run_104 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_MACRO_GENERATE as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState104
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState104
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState104
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState104
      | IDENT _v_4 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState104
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState104
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | _ ->
          _eRR ()
  
  and _menhir_run_102 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_MINUS -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_MINUS (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e1_, e1) = (_endpos, _v) in
      let _v = _menhir_action_050 e1 in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_101 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_BANG -> _ -> _ -> _ -> _ -> _ -> ttv_result =
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
      let _v = _menhir_action_158 v in
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
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_296 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
      let _v = _menhir_action_254 e es in
      _menhir_goto_value_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_es_ _startpos_e_ _v
  
  and _menhir_goto_value_desc : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v ->
      let (_endpos_v_, _startpos_v_, v) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_253 _endpos_v_ _startpos_v_ v in
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
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_296 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_255 e in
          _menhir_goto_value_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v
      | _ ->
          _eRR ()
  
  and _menhir_run_099 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_c_, _startpos_c_, c) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_006 c in
      _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_c_ _startpos_c_ _v _menhir_s _tok
  
  and _menhir_run_086 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_const (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState087 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
      | MenhirState087 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState049 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_088 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_const -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_const (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_191 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_const_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_084 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LCUR -> _ -> _ -> _ -> ttv_result =
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
      let _v = _menhir_action_157 x in
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
      | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | FOR | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LPAREN | MATCH | OPERATOR_IDENT _ | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | FOR | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LPAREN | MATCH | OPERATOR_IDENT _ | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | FOR | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LPAREN | MATCH | OPERATOR_IDENT _ | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | FOR | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LPAREN | MATCH | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | FOR | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LPAREN | MATCH | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AMP | AMP_AMP | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LE | LPAREN | LT | MATCH | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AMP | AMP_AMP | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LE | LPAREN | LT | MATCH | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AMP | AMP_AMP | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LE | LPAREN | LT | MATCH | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AMP | AMP_AMP | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LE | LPAREN | LT | MATCH | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AMP | AMP_AMP | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LE | LPAREN | LT | MATCH | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AMP | AMP_AMP | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LE | LPAREN | LT | MATCH | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AMP | AMP_AMP | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LE | LPAREN | LT | MATCH | MINUS | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AMP | AMP_AMP | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LE | LPAREN | LT | MATCH | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | FOR | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LPAREN | MATCH | OPERATOR_IDENT _ | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AMP | AMP_AMP | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LE | LPAREN | LT | MATCH | MINUS | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AMP | AMP_AMP | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LE | LPAREN | LT | MATCH | MINUS | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AMP | AMP_AMP | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LE | LPAREN | LT | MATCH | MINUS | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AMP | AMP_AMP | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LE | LPAREN | LT | MATCH | MINUS | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AMP | AMP_AMP | AND | ASR | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LE | LPAREN | LSL | LSR | LT | MATCH | MINUS | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TIMES | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | FOR | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LPAREN | MATCH | OPERATOR_IDENT _ | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
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
      | AND | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | FOR | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LCUR | LPAREN | MATCH | OPERATOR_IDENT _ | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH ->
          let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_147 e in
          _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_lexp_desc : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_146 _endpos_e_ _startpos_e_ e in
      _menhir_goto_lexp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v _menhir_s _tok
  
  and _menhir_goto_lexp : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState020 ->
          _menhir_run_453 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState392 ->
          _menhir_run_391 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState388 ->
          _menhir_run_391 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState386 ->
          _menhir_run_385 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState382 ->
          _menhir_run_385 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState107 ->
          _menhir_run_381 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
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
      | MenhirState552 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState553 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState563 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState558 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState556 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState554 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState538 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState533 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState529 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState449 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState447 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState030 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState436 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState419 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState415 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState412 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState402 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState400 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState379 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState377 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState366 ->
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
  
  and _menhir_run_391 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PIPE_COMMA_PIPE ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState392 in
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
      | MenhirState392 ->
          _menhir_run_393 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState388 ->
          _menhir_run_389 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_393 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_lexp (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_205 x xs in
      _menhir_goto_separated_nonempty_list_PIPE_COMMA_PIPE_lexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_389 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_lexp (_menhir_stack, _, e1, _, _) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__5_, es) = (_endpos, _v) in
      let _v = _menhir_action_153 e1 es in
      _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_385 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PIPE_PIPE ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState386 in
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
      | MenhirState386 ->
          _menhir_run_387 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState382 ->
          _menhir_run_383 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_387 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_lexp (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_207 x xs in
      _menhir_goto_separated_nonempty_list_PIPE_PIPE_lexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_383 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_lexp (_menhir_stack, _, e1, _, _) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__5_, es) = (_endpos, _v) in
      let _v = _menhir_action_152 e1 es in
      _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_381 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          let _v = _menhir_action_132 e in
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
      let _v = _menhir_action_129 _endpos_e_ _startpos_e_ e in
      _menhir_goto_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v _menhir_s _tok
  
  and _menhir_goto_exp : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState464 ->
          _menhir_run_560 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState552 ->
          _menhir_run_560 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState553 ->
          _menhir_run_560 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState554 ->
          _menhir_run_560 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState556 ->
          _menhir_run_560 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState563 ->
          _menhir_run_560 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState558 ->
          _menhir_run_560 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState538 ->
          _menhir_run_539 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState533 ->
          _menhir_run_534 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState529 ->
          _menhir_run_530 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_462 _menhir_stack _v _tok
      | MenhirState449 ->
          _menhir_run_450 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState447 ->
          _menhir_run_448 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_446 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState030 ->
          _menhir_run_443 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState436 ->
          _menhir_run_437 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState419 ->
          _menhir_run_420 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState415 ->
          _menhir_run_416 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState412 ->
          _menhir_run_413 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState033 ->
          _menhir_run_406 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState402 ->
          _menhir_run_403 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState400 ->
          _menhir_run_401 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_399 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_394 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState107 ->
          _menhir_run_394 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState379 ->
          _menhir_run_380 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState377 ->
          _menhir_run_378 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState366 ->
          _menhir_run_367 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState362 ->
          _menhir_run_363 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
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
  
  and _menhir_run_560 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_endpos__2_, _startpos_e_, e) = (_endpos_0, _startpos, _v) in
          let _v = _menhir_action_127 _endpos__2_ _startpos_e_ e in
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
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState563
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | TYPE ->
          _menhir_run_465 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState563
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | SHARED ->
          _menhir_run_492 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState563
      | OPERATOR ->
          _menhir_run_498 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | LET ->
          _menhir_run_514 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState563
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState563
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | EXTERNAL ->
          _menhir_run_546 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | EOF ->
          _menhir_run_551 _menhir_stack _menhir_lexbuf MenhirState563
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState563
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState563
      | _ ->
          _eRR ()
  
  and _menhir_run_539 : type  ttv_stack. (((((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic, _menhir_box_pi) _menhir_cell1_ret_ty_annot_eq as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ret_ty_annot_eq (_menhir_stack, _, ty_opt_ret) = _menhir_stack in
          let MenhirCell1_arg_ty_atomic (_menhir_stack, _, p_ty_opt) = _menhir_stack in
          let MenhirCell1_IDENT (_menhir_stack, _, f, _startpos_f_, _) = _menhir_stack in
          let (_endpos__5_, _endpos_e1_, e1) = (_endpos_0, _endpos, _v) in
          let _v = _menhir_action_141 _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt_ret in
          let _endpos = _endpos__5_ in
          let (_endpos_b_, b) = (_endpos, _v) in
          let _v = _menhir_action_024 b in
          _menhir_goto_after_let_SEMI_SEMI_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_b_ _v _tok
      | IN ->
          let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_364 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_goto_after_let_SEMI_SEMI_ : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_LET -> _ -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_b_, b) = (_endpos, _v) in
      let _v = _menhir_action_126 _endpos_b_ _startpos__1_ b in
      _menhir_goto_decl_all _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_364 : type  ttv_stack ttv_result. (((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic, ttv_result) _menhir_cell1_ret_ty_annot_eq, ttv_result) _menhir_cell1_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_exp (_menhir_stack, _, e1, _, _endpos_e1_) = _menhir_stack in
      let MenhirCell1_ret_ty_annot_eq (_menhir_stack, _, ty_opt_ret) = _menhir_stack in
      let MenhirCell1_arg_ty_atomic (_menhir_stack, _, p_ty_opt) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, f, _startpos_f_, _) = _menhir_stack in
      let _v = _menhir_action_140 _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt_ret in
      let b = _v in
      let _v = _menhir_action_020 b in
      _menhir_goto_after_let_IN_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_after_let_IN_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_after_let_IN_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState379
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState379
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState379
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | LET ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState379
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | IDENT _v_4 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState379
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState379
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | ARRAY_MAKE ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | ARRAY_LENGTH ->
          _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | ARRAY_CREATE ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState379
      | _ ->
          _eRR ()
  
  and _menhir_run_534 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_ty_annot_IDENT_ as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ty_annot_IDENT_ (_menhir_stack, _, f_ty_opt, _startpos_f_ty_opt_) = _menhir_stack in
          let MenhirCell1_REC (_menhir_stack, _) = _menhir_stack in
          let (_endpos__5_, _endpos_e1_, e1) = (_endpos_0, _endpos, _v) in
          let _v = _menhir_action_026 _endpos_e1_ _startpos_f_ty_opt_ e1 f_ty_opt in
          _menhir_goto_after_let_SEMI_SEMI_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _v _tok
      | IN ->
          let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_353 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_353 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_REC, ttv_result) _menhir_cell1_ty_annot_IDENT_, ttv_result) _menhir_cell1_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_exp (_menhir_stack, _, e1, _, _endpos_e1_) = _menhir_stack in
      let MenhirCell1_ty_annot_IDENT_ (_menhir_stack, _, f_ty_opt, _startpos_f_ty_opt_) = _menhir_stack in
      let MenhirCell1_REC (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_022 _endpos_e1_ _startpos_f_ty_opt_ e1 f_ty_opt in
      _menhir_goto_after_let_IN_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_530 : type  ttv_stack. ((((((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic, _menhir_box_pi) _menhir_cell1_ret_ty_annot_eq as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
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
          let _v = _menhir_action_143 _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt in
          let _endpos = _endpos__6_ in
          let (_endpos_e_, e) = (_endpos, _v) in
          let _v = _menhir_action_025 e in
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
      let _v = _menhir_action_142 _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt in
      let e = _v in
      let _v = _menhir_action_021 e in
      _menhir_goto_after_let_IN_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_450 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_PARFOR _menhir_cell0_IDENT, ttv_result) _menhir_cell1_exp, ttv_result) _menhir_cell1_exp -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | DONE ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_exp (_menhir_stack, _, e_st2, _, _) = _menhir_stack in
          let MenhirCell1_exp (_menhir_stack, _, e_st1, _, _) = _menhir_stack in
          let MenhirCell0_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
          let MenhirCell1_PARFOR (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (e, _endpos__9_) = (_v, _endpos_0) in
          let _v = _menhir_action_018 _endpos__9_ _startpos__1_ e e_st1 e_st2 x in
          _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__9_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_448 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_PARFOR _menhir_cell0_IDENT, ttv_result) _menhir_cell1_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | DO ->
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
  
  and _menhir_run_446 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_PARFOR _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | TO ->
          let _menhir_s = MenhirState447 in
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
  
  and _menhir_run_443 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_REGISTER as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | INIT ->
          let _menhir_s = MenhirState444 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_437 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_MATCH, ttv_result) _menhir_cell1_exp _menhir_cell0_option_PIPE_, ttv_result) _menhir_cell1_list_match_case_const_ _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | END ->
          _menhir_run_423 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState437
      | AMP | AMP_AMP | AND | ASR | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DIV | DO | DONE | ELSE | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LAND | LCUR | LE | LOR | LPAREN | LSL | LSR | LT | LXOR | MATCH | MINUS | MOD | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TIMES | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH | XOR ->
          let _ = _menhir_action_167 () in
          _menhir_run_438 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_423 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_x_, x) = (_endpos, ()) in
      let _ = _menhir_action_168 x in
      _menhir_goto_option_END_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _menhir_s _tok
  
  and _menhir_goto_option_END_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _menhir_s _tok ->
      match _menhir_s with
      | MenhirState437 ->
          _menhir_run_438 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | MenhirState422 ->
          _menhir_run_424 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_438 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_MATCH, ttv_result) _menhir_cell1_exp _menhir_cell0_option_PIPE_, ttv_result) _menhir_cell1_list_match_case_const_ _menhir_cell0_IDENT, ttv_result) _menhir_cell1_exp -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok ->
      let MenhirCell1_exp (_menhir_stack, _, otherwise, _, _) = _menhir_stack in
      let MenhirCell0_IDENT (_menhir_stack, _, _, _) = _menhir_stack in
      let MenhirCell1_list_match_case_const_ (_menhir_stack, _, cases) = _menhir_stack in
      let MenhirCell0_option_PIPE_ (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_exp (_menhir_stack, _, e, _, _) = _menhir_stack in
      let MenhirCell1_MATCH (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__9_ = _endpos in
      let _v = _menhir_action_015 cases e otherwise in
      _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__9_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_424 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_MATCH, ttv_result) _menhir_cell1_exp _menhir_cell0_option_PIPE_, ttv_result) _menhir_cell1_match_cases -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok ->
      let MenhirCell1_match_cases (_menhir_stack, _, rev_cases) = _menhir_stack in
      let MenhirCell0_option_PIPE_ (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_exp (_menhir_stack, _, e, _, _) = _menhir_stack in
      let MenhirCell1_MATCH (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__6_ = _endpos in
      let _v = _menhir_action_016 e rev_cases in
      _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__6_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_420 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_separated_nonempty_list_PIPE_const_ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | PIPE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_separated_nonempty_list_PIPE_const_ (_menhir_stack, _menhir_s, cs) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_161 cs e in
          let _menhir_stack = MenhirCell1_match_case_const (_menhir_stack, _menhir_s, _v) in
          (match (_tok : MenhirBasics.token) with
          | UP_IDENT _v_0 ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState425
          | STRING_LIT _v_1 ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState425
          | OPERATOR_IDENT _v_2 ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState425
          | LPAREN ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState425
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState425
          | INT_LIT _v_3 ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState425
          | BOOL_LIT _v_4 ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState425
          | IDENT _ ->
              let _v_5 = _menhir_action_154 () in
              _menhir_run_426 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_426 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_match_case_const -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_match_case_const (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_155 x xs in
      _menhir_goto_list_match_case_const_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_list_match_case_const_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState409 ->
          _menhir_run_434 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState425 ->
          _menhir_run_426 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_434 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_MATCH, ttv_result) _menhir_cell1_exp _menhir_cell0_option_PIPE_ as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
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
              let _menhir_s = MenhirState436 in
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
                  _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | REF ->
                  _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PAUSE ->
                  _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PARFOR ->
                  _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPERATOR_IDENT _v ->
                  _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | MINUS ->
                  _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MATCH ->
                  _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_GENERATE ->
                  _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LET ->
                  _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LENGTH ->
                  _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LCUR ->
                  _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_OF_TUPLE ->
                  _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_LIT _v ->
                  _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IF ->
                  _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FUN ->
                  _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FOR ->
                  _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FIX ->
                  _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | EXEC ->
                  _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CREATE ->
                  _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL_LIT _v ->
                  _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
  
  and _menhir_run_416 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_256 _endpos_e_ _startpos_x_ e x in
      let e = _v in
      let _v = _menhir_action_162 e in
      _menhir_goto_match_cases _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
  
  and _menhir_goto_match_cases : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState431 ->
          _menhir_run_433 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState409 ->
          _menhir_run_422 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_433 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_match_case -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_match_case (_menhir_stack, _menhir_s, h) = _menhir_stack in
      let rev_cases = _v in
      let _v = _menhir_action_164 h rev_cases in
      _menhir_goto_match_cases _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
  
  and _menhir_run_422 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_MATCH, ttv_result) _menhir_cell1_exp _menhir_cell0_option_PIPE_ as 'stack) -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_match_cases (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | END ->
          _menhir_run_423 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState422
      | AMP | AMP_AMP | AND | ASR | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DIV | DO | DONE | ELSE | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LAND | LCUR | LE | LOR | LPAREN | LSL | LSR | LT | LXOR | MATCH | MINUS | MOD | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TIMES | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH | XOR ->
          let _ = _menhir_action_167 () in
          _menhir_run_424 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_413 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_UP_IDENT, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_apat (_menhir_stack, _, p) = _menhir_stack in
      let MenhirCell1_UP_IDENT (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_160 e p x in
      match (_tok : MenhirBasics.token) with
      | PIPE ->
          let _menhir_stack = MenhirCell1_match_case (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState431 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UP_IDENT _v ->
              let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _menhir_stack = MenhirCell1_UP_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_s = MenhirState432 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LPAREN ->
                  _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | IDENT _v ->
              _menhir_run_414 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | AMP | AMP_AMP | AND | ASR | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LAND | LCUR | LE | LOR | LPAREN | LSL | LSR | LT | LXOR | MATCH | MINUS | MOD | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TIMES | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH | XOR ->
          let h = _v in
          let _v = _menhir_action_163 h in
          _menhir_goto_match_cases _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_414 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW ->
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
  
  and _menhir_run_406 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_MATCH as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
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
              let _menhir_stack = MenhirCell1_UP_IDENT (_menhir_stack, MenhirState409, _v_0, _startpos, _endpos) in
              _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState410
          | IDENT _v_1 ->
              let _menhir_stack = MenhirCell1_UP_IDENT (_menhir_stack, MenhirState409, _v_0, _startpos, _endpos) in
              _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState410
          | PIPE | RIGHT_ARROW ->
              let _v_2 =
                let x = _v_0 in
                _menhir_action_103 x
              in
              _menhir_run_427 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v_2 MenhirState409 _tok
          | _ ->
              _eRR ())
      | STRING_LIT _v_3 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState409
      | OPERATOR_IDENT _v_4 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState409
      | LPAREN ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState409
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState409
      | INT_LIT _v_5 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState409
      | IDENT _v_6 ->
          _menhir_run_414 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState409
      | BOOL_LIT _v_7 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState409
      | _ ->
          _eRR ()
  
  and _menhir_run_403 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_FOR _menhir_cell0_IDENT, ttv_result) _menhir_cell1_exp, ttv_result) _menhir_cell1_exp -> _ -> _ -> _ -> _ -> ttv_result =
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
          let _v = _menhir_action_017 e e1 e2 i in
          _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__9_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_401 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_FOR _menhir_cell0_IDENT, ttv_result) _menhir_cell1_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | DO ->
          let _menhir_s = MenhirState402 in
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
  
  and _menhir_run_399 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FOR _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | TO ->
          let _menhir_s = MenhirState400 in
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
  
  and _menhir_run_394 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
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
          let _menhir_s = MenhirState396 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVAR_IDENT _v ->
              _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_380 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_after_let_IN_ -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_after_let_IN_ (_menhir_stack, _, b) = _menhir_stack in
      let MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_151 b e2 in
      _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_378 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> ttv_result =
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
          let _menhir_s = MenhirState374 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_354 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
      | MenhirState374 ->
          _menhir_run_375 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState514 ->
          _menhir_run_370 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_370 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_375 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_binding_apat_exp_ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_binding_apat_exp_ (_menhir_stack, _menhir_s, b1) = _menhir_stack in
      let bs = _v in
      let _v = _menhir_action_096 b1 bs in
      _menhir_goto_bindings_and_apat_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_370 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let w = _v in
      let _v = _menhir_action_094 w in
      _menhir_goto_bindings_apat_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_bindings_apat_exp_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState514 ->
          _menhir_run_543 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_371 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_543 : type  ttv_stack. ((ttv_stack, _menhir_box_pi) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (b, _endpos__2_) = (_v, _endpos) in
          let _v = _menhir_action_023 b in
          _menhir_goto_after_let_SEMI_SEMI_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _v _tok
      | IN ->
          let _menhir_stack = MenhirCell1_bindings_apat_exp_ (_menhir_stack, _menhir_s, _v) in
          _menhir_run_372 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_372 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_bindings_apat_exp_ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_bindings_apat_exp_ (_menhir_stack, _menhir_s, b) = _menhir_stack in
      let _v = _menhir_action_019 b in
      _menhir_goto_after_let_IN_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_371 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_bindings_apat_exp_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | IN ->
          _menhir_run_372 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_367 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_annot_apat_ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty_annot_apat_ (_menhir_stack, _menhir_s, p_ty_opt) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_092 e p_ty_opt in
      _menhir_goto_binding_apat_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_363 : type  ttv_stack ttv_result. (((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic, ttv_result) _menhir_cell1_ret_ty_annot_eq as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | IN ->
          _menhir_run_364 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          | AMP | AMP_AMP | AND | ASR | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LAND | LCUR | LE | LOR | LPAREN | LSL | LSR | LT | LXOR | MATCH | MINUS | MOD | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TIMES | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH | XOR ->
              let MenhirCell0_LPAREN (_menhir_stack, _) = _menhir_stack in
              let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
              let (_endpos__5_, e1) = (_endpos_0, _v) in
              let _v = _menhir_action_041 e1 x in
              _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _startpos_x_ _v _menhir_s _tok
          | LEFT_ARROW ->
              let MenhirCell0_LPAREN (_menhir_stack, _) = _menhir_stack in
              let (_endpos__4_, e) = (_endpos_0, _v) in
              let _v = _menhir_action_128 e in
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
                  _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | REF ->
                  _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PAUSE ->
                  _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PARFOR ->
                  _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPERATOR_IDENT _v ->
                  _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | MINUS ->
                  _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MATCH ->
                  _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_GENERATE ->
                  _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LENGTH ->
                  _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LCUR ->
                  _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_OF_TUPLE ->
                  _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_LIT _v ->
                  _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IDENT _v ->
                  _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FOR ->
                  _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | EXEC ->
                  _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CREATE ->
                  _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL_LIT _v ->
                  _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
                  _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | REF ->
                  _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PAUSE ->
                  _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PARFOR ->
                  _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPERATOR_IDENT _v ->
                  _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | MINUS ->
                  _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MATCH ->
                  _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_GENERATE ->
                  _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LENGTH ->
                  _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LCUR ->
                  _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_OF_TUPLE ->
                  _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_LIT _v ->
                  _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IDENT _v ->
                  _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FOR ->
                  _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | EXEC ->
                  _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CREATE ->
                  _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL_LIT _v ->
                  _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_MAKE ->
                  _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_LENGTH ->
                  _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_CREATE ->
                  _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | AMP | AMP_AMP | AND | ASR | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LAND | LCUR | LE | LOR | LPAREN | LSL | LSR | LT | LXOR | MATCH | MINUS | MOD | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TIMES | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH | XOR ->
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
      let _v = _menhir_action_149 e p_ty_opt in
      _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_327 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_FIX _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell0_IDENT (_menhir_stack, f, _, _) = _menhir_stack in
      let MenhirCell1_FIX (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_148 _endpos_e_ _startpos__1_ e f in
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | REF ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | PAUSE ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | PARFOR ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | OPERATOR_IDENT _v_3 ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState323
          | MINUS ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | MATCH ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | MACRO_GENERATE ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | LPAREN ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | LET ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | LENGTH ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | LCUR ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | INT_OF_TUPLE ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | INT_LIT _v_4 ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState323
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
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
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
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState323
          | BANG ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
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
          | AMP | AMP_AMP | AND | ASR | AT | AT_AT | COL | COL_EQ | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | HAT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
              let MenhirCell1_EXEC (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
              let (_endpos__3_, e1) = (_endpos_0, _v) in
              let _v = _menhir_action_079 _endpos__3_ _startpos__1_ e1 in
              _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
          | _ ->
              _eRR ())
      | AMP | AMP_AMP | AND | ASR | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LAND | LCUR | LE | LOR | LPAREN | LSL | LSR | LT | LXOR | MATCH | MINUS | MOD | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TIMES | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH | XOR ->
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
      let _v = _menhir_action_159 e p_ty_opt in
      _menhir_goto_lvalue _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_243 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_lexp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_130 e1 e2 in
      _menhir_goto_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_345 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IF, ttv_result) _menhir_cell1_exp, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_lexp (_menhir_stack, _, e2, _, _) = _menhir_stack in
      let (_endpos_e3_, e3) = (_endpos, _v) in
      let _v = _menhir_action_145 e2 e3 in
      _menhir_goto_if_end _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e3_ _v _tok
  
  and _menhir_goto_if_end : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IF, ttv_result) _menhir_cell1_exp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_exp (_menhir_stack, _, e1, _, _) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e2_e3_, e2_e3) = (_endpos, _v) in
      let _v = _menhir_action_150 e1 e2_e3 in
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AMP | AMP_AMP | AND | ASR | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DIV | DO | DONE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LAND | LCUR | LE | LOR | LPAREN | LSL | LSR | LT | LXOR | MATCH | MINUS | MOD | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TIMES | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH | XOR ->
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_144 e2 in
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AMP | AMP_AMP | AND | ASR | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LAND | LCUR | LE | LOR | LPAREN | LSL | LSR | LT | LXOR | MATCH | MINUS | MOD | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TIMES | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH | XOR ->
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PAUSE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AMP | AMP_AMP | AND | ASR | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LAND | LCUR | LE | LOR | LPAREN | LSL | LSR | LT | LXOR | MATCH | MINUS | MOD | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TIMES | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH | XOR ->
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
      let _v = _menhir_action_131 e es in
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
      | AMP | AMP_AMP | AND | ASR | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LAND | LCUR | LE | LOR | LPAREN | LSL | LSR | LT | LXOR | MATCH | MINUS | MOD | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TIMES | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH | XOR ->
          let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_132 e in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
  
  and _menhir_run_138 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB_next -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | GT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_tyB_next (_menhir_stack, _menhir_s, tyB, _) = _menhir_stack in
          let sz = _v in
          let _v = _menhir_action_241 sz tyB in
          _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_127 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let (_endpos_sz_, sz) = (_endpos, _v) in
      let _v = _menhir_action_214 sz in
      _menhir_goto_size_list _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_sz_ _v _menhir_s _tok
  
  and _menhir_goto_size_list : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState159 ->
          _menhir_run_160 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState134 ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState466 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState119 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_160 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_separated_nonempty_list_COMMA_tyB_ _menhir_cell0_RPAREN _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell0_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
      let MenhirCell0_RPAREN (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_separated_nonempty_list_COMMA_tyB_ (_menhir_stack, _, tyBs) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_szs_, szs) = (_endpos, _v) in
      let _v = _menhir_action_231 _endpos_szs_ _startpos__1_ szs tyBs x in
      _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_135 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB_next _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell0_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
      let MenhirCell1_tyB_next (_menhir_stack, _menhir_s, tyB, _startpos_tyB_) = _menhir_stack in
      let (_endpos_szs_, szs) = (_endpos, _v) in
      let _v = _menhir_action_230 _endpos_szs_ _startpos_tyB_ szs tyB x in
      _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_tyB_ _v _menhir_s _tok
  
  and _menhir_run_126 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
      let (_endpos_szs_, szs) = (_endpos, _v) in
      let _v = _menhir_action_229 _endpos_szs_ _startpos_x_ szs x in
      _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_x_ _v _menhir_s _tok
  
  and _menhir_run_121 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_size (_menhir_stack, _menhir_s, _v, _endpos) in
          let _menhir_s = MenhirState122 in
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
      | MenhirState120 ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState122 ->
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_124 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LT -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LT (_menhir_stack, _menhir_s) = _menhir_stack in
      let (_endpos__3_, szs) = (_endpos, _v) in
      let _v = _menhir_action_215 szs in
      _menhir_goto_size_list _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _v _menhir_s _tok
  
  and _menhir_run_123 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_size -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_size (_menhir_stack, _menhir_s, x, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_197 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_size_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_117 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LT -> _ -> _ -> _ -> _ -> ttv_result =
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
          let _v = _menhir_action_012 l in
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PAUSE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | OPERATOR ->
          _menhir_run_498 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_514 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXTERNAL ->
          _menhir_run_546 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EOF ->
          _menhir_run_551 _menhir_stack _menhir_lexbuf _menhir_s
      | CREATE ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
