
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

  | MenhirState034 : (('s, 'r) _menhir_cell1_MINUS, 'r) _menhir_state
    (** State 034.
        Stack shape : MINUS.
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

  | MenhirState097 : (('s, 'r) _menhir_cell1_MACRO_GENERATE, 'r) _menhir_state
    (** State 097.
        Stack shape : MACRO_GENERATE.
        Start symbol: <undetermined>. *)

  | MenhirState098 : (('s, 'r) _menhir_cell1_BANG, 'r) _menhir_state
    (** State 098.
        Stack shape : BANG.
        Start symbol: <undetermined>. *)

  | MenhirState102 : ((('s, 'r) _menhir_cell1_MACRO_GENERATE, 'r) _menhir_cell1_aexp, 'r) _menhir_state
    (** State 102.
        Stack shape : MACRO_GENERATE aexp.
        Start symbol: <undetermined>. *)

  | MenhirState103 : (((('s, 'r) _menhir_cell1_MACRO_GENERATE, 'r) _menhir_cell1_aexp, 'r) _menhir_cell1_aexp, 'r) _menhir_state
    (** State 103.
        Stack shape : MACRO_GENERATE aexp aexp.
        Start symbol: <undetermined>. *)

  | MenhirState105 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 105.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState106 : (('s, 'r) _menhir_cell1_LET, 'r) _menhir_state
    (** State 106.
        Stack shape : LET.
        Start symbol: <undetermined>. *)

  | MenhirState107 : ((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_REC, 'r) _menhir_state
    (** State 107.
        Stack shape : LET REC.
        Start symbol: <undetermined>. *)

  | MenhirState108 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 108.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState109 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 109.
        Stack shape : LPAREN IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState110 : ((('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_COL, 'r) _menhir_state
    (** State 110.
        Stack shape : IDENT COL.
        Start symbol: <undetermined>. *)

  | MenhirState113 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 113.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState114 : (('s, 'r) _menhir_cell1_LT, 'r) _menhir_state
    (** State 114.
        Stack shape : LT.
        Start symbol: <undetermined>. *)

  | MenhirState117 : (('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 117.
        Stack shape : IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState118 : (('s, 'r) _menhir_cell1_LT, 'r) _menhir_state
    (** State 118.
        Stack shape : LT.
        Start symbol: <undetermined>. *)

  | MenhirState120 : (('s, 'r) _menhir_cell1_size, 'r) _menhir_state
    (** State 120.
        Stack shape : size.
        Start symbol: <undetermined>. *)

  | MenhirState127 : (('s, 'r) _menhir_cell1_ty_next, 'r) _menhir_state
    (** State 127.
        Stack shape : ty_next.
        Start symbol: <undetermined>. *)

  | MenhirState129 : ((('s, 'r) _menhir_cell1_ty_next, 'r) _menhir_cell1_ty_next, 'r) _menhir_state
    (** State 129.
        Stack shape : ty_next ty_next.
        Start symbol: <undetermined>. *)

  | MenhirState131 : (('s, 'r) _menhir_cell1_tyB_next _menhir_cell0_IDENT, 'r) _menhir_state
    (** State 131.
        Stack shape : tyB_next IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState135 : (('s, 'r) _menhir_cell1_ty_next, 'r) _menhir_state
    (** State 135.
        Stack shape : ty_next.
        Start symbol: <undetermined>. *)

  | MenhirState136 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 136.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState138 : (('s, 'r) _menhir_cell1_tyB_next, 'r) _menhir_state
    (** State 138.
        Stack shape : tyB_next.
        Start symbol: <undetermined>. *)

  | MenhirState140 : ((('s, 'r) _menhir_cell1_tyB_next, 'r) _menhir_cell1_tyB_next, 'r) _menhir_state
    (** State 140.
        Stack shape : tyB_next tyB_next.
        Start symbol: <undetermined>. *)

  | MenhirState145 : (('s, 'r) _menhir_cell1_tyB, 'r) _menhir_state
    (** State 145.
        Stack shape : tyB.
        Start symbol: <undetermined>. *)

  | MenhirState150 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_separated_nonempty_list_COMMA_tyB_ _menhir_cell0_RPAREN _menhir_cell0_IDENT, 'r) _menhir_state
    (** State 150.
        Stack shape : LPAREN separated_nonempty_list(COMMA,tyB) RPAREN IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState154 : (('s, 'r) _menhir_cell1_ty_next, 'r) _menhir_state
    (** State 154.
        Stack shape : ty_next.
        Start symbol: <undetermined>. *)

  | MenhirState156 : (('s, 'r) _menhir_cell1_ty_next, 'r) _menhir_state
    (** State 156.
        Stack shape : ty_next.
        Start symbol: <undetermined>. *)

  | MenhirState159 : (('s, 'r) _menhir_cell1_ty_next, 'r) _menhir_state
    (** State 159.
        Stack shape : ty_next.
        Start symbol: <undetermined>. *)

  | MenhirState169 : (((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_REC, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 169.
        Stack shape : LET REC IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState170 : ((('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 170.
        Stack shape : IDENT LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState172 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 172.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState177 : (('s, 'r) _menhir_cell1_apat, 'r) _menhir_state
    (** State 177.
        Stack shape : apat.
        Start symbol: <undetermined>. *)

  | MenhirState180 : ((('s, 'r) _menhir_cell1_apat, 'r) _menhir_cell1_apat, 'r) _menhir_state
    (** State 180.
        Stack shape : apat apat.
        Start symbol: <undetermined>. *)

  | MenhirState185 : (((('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_apat, 'r) _menhir_state
    (** State 185.
        Stack shape : IDENT LPAREN apat.
        Start symbol: <undetermined>. *)

  | MenhirState188 : ((((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_REC, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_arg_ty_atomic, 'r) _menhir_state
    (** State 188.
        Stack shape : LET REC IDENT arg_ty_atomic.
        Start symbol: <undetermined>. *)

  | MenhirState190 : (((('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_arg_ty_atomic, 'r) _menhir_cell1_COL, 'r) _menhir_state
    (** State 190.
        Stack shape : IDENT arg_ty_atomic COL.
        Start symbol: <undetermined>. *)

  | MenhirState193 : (((((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_REC, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_arg_ty_atomic, 'r) _menhir_cell1_ret_ty_annot_eq, 'r) _menhir_state
    (** State 193.
        Stack shape : LET REC IDENT arg_ty_atomic ret_ty_annot_eq.
        Start symbol: <undetermined>. *)

  | MenhirState194 : (('s, 'r) _menhir_cell1_LENGTH, 'r) _menhir_state
    (** State 194.
        Stack shape : LENGTH.
        Start symbol: <undetermined>. *)

  | MenhirState196 : (('s, 'r) _menhir_cell1_IF, 'r) _menhir_state
    (** State 196.
        Stack shape : IF.
        Start symbol: <undetermined>. *)

  | MenhirState198 : (('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 198.
        Stack shape : IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState199 : (('s, 'r) _menhir_cell1_FUN, 'r) _menhir_state
    (** State 199.
        Stack shape : FUN.
        Start symbol: <undetermined>. *)

  | MenhirState200 : ((('s, 'r) _menhir_cell1_FUN, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 200.
        Stack shape : FUN LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState205 : (('s, 'r) _menhir_cell1_apat, 'r) _menhir_state
    (** State 205.
        Stack shape : apat.
        Start symbol: <undetermined>. *)

  | MenhirState211 : ((('s, 'r) _menhir_cell1_FUN, 'r) _menhir_cell1_arg_ty, 'r) _menhir_state
    (** State 211.
        Stack shape : FUN arg_ty.
        Start symbol: <undetermined>. *)

  | MenhirState213 : (('s, 'r) _menhir_cell1_FIX _menhir_cell0_IDENT, 'r) _menhir_state
    (** State 213.
        Stack shape : FIX IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState214 : (('s, 'r) _menhir_cell1_EXEC, 'r) _menhir_state
    (** State 214.
        Stack shape : EXEC.
        Start symbol: <undetermined>. *)

  | MenhirState216 : (('s, 'r) _menhir_cell1_CREATE, 'r) _menhir_state
    (** State 216.
        Stack shape : CREATE.
        Start symbol: <undetermined>. *)

  | MenhirState222 : (('s, 'r) _menhir_cell1_ARRAY_MAKE, 'r) _menhir_state
    (** State 222.
        Stack shape : ARRAY_MAKE.
        Start symbol: <undetermined>. *)

  | MenhirState224 : ((('s, 'r) _menhir_cell1_ARRAY_MAKE, 'r) _menhir_cell1_size _menhir_cell0_GT, 'r) _menhir_state
    (** State 224.
        Stack shape : ARRAY_MAKE size GT.
        Start symbol: <undetermined>. *)

  | MenhirState226 : (('s, 'r) _menhir_cell1_ARRAY_LENGTH, 'r) _menhir_state
    (** State 226.
        Stack shape : ARRAY_LENGTH.
        Start symbol: <undetermined>. *)

  | MenhirState229 : (('s, 'r) _menhir_cell1_ARRAY_CREATE, 'r) _menhir_state
    (** State 229.
        Stack shape : ARRAY_CREATE.
        Start symbol: <undetermined>. *)

  | MenhirState236 : (('s, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 236.
        Stack shape : lexp.
        Start symbol: <undetermined>. *)

  | MenhirState241 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 241.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState243 : (('s, 'r) _menhir_cell1_aexp, 'r) _menhir_state
    (** State 243.
        Stack shape : aexp.
        Start symbol: <undetermined>. *)

  | MenhirState244 : ((('s, 'r) _menhir_cell1_aexp, 'r) _menhir_cell1_COL_EQ, 'r) _menhir_state
    (** State 244.
        Stack shape : aexp COL_EQ.
        Start symbol: <undetermined>. *)

  | MenhirState246 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 246.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState248 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 248.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState250 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 250.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState252 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 252.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState254 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 254.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState256 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 256.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState258 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 258.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState260 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 260.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState262 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 262.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState264 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 264.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState266 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 266.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState268 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 268.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState270 : (('s, 'r) _menhir_cell1_app_exp _menhir_cell0_MINUS, 'r) _menhir_state
    (** State 270.
        Stack shape : app_exp MINUS.
        Start symbol: <undetermined>. *)

  | MenhirState272 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 272.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState274 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 274.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState276 : (('s, 'r) _menhir_cell1_app_exp _menhir_cell0_GT, 'r) _menhir_state
    (** State 276.
        Stack shape : app_exp GT.
        Start symbol: <undetermined>. *)

  | MenhirState278 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 278.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState280 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 280.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState282 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 282.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState284 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 284.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState286 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 286.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState289 : ((('s, 'r) _menhir_cell1_aexp, 'r) _menhir_cell1_aexp, 'r) _menhir_state
    (** State 289.
        Stack shape : aexp aexp.
        Start symbol: <undetermined>. *)

  | MenhirState290 : (((('s, 'r) _menhir_cell1_aexp, 'r) _menhir_cell1_aexp, 'r) _menhir_cell1_AT_AT, 'r) _menhir_state
    (** State 290.
        Stack shape : aexp aexp AT_AT.
        Start symbol: <undetermined>. *)

  | MenhirState291 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 291.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState292 : (('s, 'r) _menhir_cell1_FUN, 'r) _menhir_state
    (** State 292.
        Stack shape : FUN.
        Start symbol: <undetermined>. *)

  | MenhirState294 : ((('s, 'r) _menhir_cell1_FUN, 'r) _menhir_cell1_arg_ty, 'r) _menhir_state
    (** State 294.
        Stack shape : FUN arg_ty.
        Start symbol: <undetermined>. *)

  | MenhirState300 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_lvalue, 'r) _menhir_state
    (** State 300.
        Stack shape : LPAREN lvalue.
        Start symbol: <undetermined>. *)

  | MenhirState303 : ((('s, 'r) _menhir_cell1_lvalue, 'r) _menhir_cell1_lvalue, 'r) _menhir_state
    (** State 303.
        Stack shape : lvalue lvalue.
        Start symbol: <undetermined>. *)

  | MenhirState308 : (((('s, 'r) _menhir_cell1_aexp, 'r) _menhir_cell1_aexp, 'r) _menhir_cell1_AT, 'r) _menhir_state
    (** State 308.
        Stack shape : aexp aexp AT.
        Start symbol: <undetermined>. *)

  | MenhirState311 : (((('s, 'r) _menhir_cell1_aexp, 'r) _menhir_cell1_aexp, 'r) _menhir_cell1_aexp, 'r) _menhir_state
    (** State 311.
        Stack shape : aexp aexp aexp.
        Start symbol: <undetermined>. *)

  | MenhirState312 : (('s, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 312.
        Stack shape : lexp.
        Start symbol: <undetermined>. *)

  | MenhirState315 : ((('s, 'r) _menhir_cell1_lexp, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 315.
        Stack shape : lexp lexp.
        Start symbol: <undetermined>. *)

  | MenhirState318 : ((('s, 'r) _menhir_cell1_EXEC, 'r) _menhir_cell1_exp _menhir_cell0_DEFAULT, 'r) _menhir_state
    (** State 318.
        Stack shape : EXEC exp DEFAULT.
        Start symbol: <undetermined>. *)

  | MenhirState320 : (((('s, 'r) _menhir_cell1_EXEC, 'r) _menhir_cell1_exp _menhir_cell0_DEFAULT, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 320.
        Stack shape : EXEC exp DEFAULT lexp.
        Start symbol: <undetermined>. *)

  | MenhirState326 : ((('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_exp _menhir_cell0_RBRACKET, 'r) _menhir_state
    (** State 326.
        Stack shape : IDENT exp RBRACKET.
        Start symbol: <undetermined>. *)

  | MenhirState330 : (('s, 'r) _menhir_cell1_IDENT _menhir_cell0_LPAREN, 'r) _menhir_state
    (** State 330.
        Stack shape : IDENT LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState334 : (('s, 'r) _menhir_cell1_IDENT _menhir_cell0_dot_get, 'r) _menhir_state
    (** State 334.
        Stack shape : IDENT dot_get.
        Start symbol: <undetermined>. *)

  | MenhirState337 : ((('s, 'r) _menhir_cell1_IF, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 337.
        Stack shape : IF exp.
        Start symbol: <undetermined>. *)

  | MenhirState339 : (((('s, 'r) _menhir_cell1_IF, 'r) _menhir_cell1_exp, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 339.
        Stack shape : IF exp lexp.
        Start symbol: <undetermined>. *)

  | MenhirState346 : (((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_REC, 'r) _menhir_cell1_ty_annot_IDENT_, 'r) _menhir_state
    (** State 346.
        Stack shape : LET REC ty_annot(IDENT).
        Start symbol: <undetermined>. *)

  | MenhirState349 : (('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_state
    (** State 349.
        Stack shape : LPAREN.
        Start symbol: <undetermined>. *)

  | MenhirState353 : (('s, 'r) _menhir_cell1_apat, 'r) _menhir_state
    (** State 353.
        Stack shape : apat.
        Start symbol: <undetermined>. *)

  | MenhirState355 : ((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 355.
        Stack shape : LET IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState356 : (((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_arg_ty_atomic, 'r) _menhir_state
    (** State 356.
        Stack shape : LET IDENT arg_ty_atomic.
        Start symbol: <undetermined>. *)

  | MenhirState357 : ((((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_IDENT, 'r) _menhir_cell1_arg_ty_atomic, 'r) _menhir_cell1_ret_ty_annot_eq, 'r) _menhir_state
    (** State 357.
        Stack shape : LET IDENT arg_ty_atomic ret_ty_annot_eq.
        Start symbol: <undetermined>. *)

  | MenhirState361 : (('s, 'r) _menhir_cell1_ty_annot_apat_, 'r) _menhir_state
    (** State 361.
        Stack shape : ty_annot(apat).
        Start symbol: <undetermined>. *)

  | MenhirState369 : (('s, 'r) _menhir_cell1_binding_apat_exp_, 'r) _menhir_state
    (** State 369.
        Stack shape : binding(apat,exp).
        Start symbol: <undetermined>. *)

  | MenhirState372 : (('s, 'r) _menhir_cell1_apat, 'r) _menhir_state
    (** State 372.
        Stack shape : apat.
        Start symbol: <undetermined>. *)

  | MenhirState374 : ((('s, 'r) _menhir_cell1_LET, 'r) _menhir_cell1_after_let_IN_, 'r) _menhir_state
    (** State 374.
        Stack shape : LET after_let(IN).
        Start symbol: <undetermined>. *)

  | MenhirState377 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 377.
        Stack shape : LPAREN lexp.
        Start symbol: <undetermined>. *)

  | MenhirState381 : ((('s, 'r) _menhir_cell1_lexp, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 381.
        Stack shape : lexp lexp.
        Start symbol: <undetermined>. *)

  | MenhirState383 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 383.
        Stack shape : LPAREN lexp.
        Start symbol: <undetermined>. *)

  | MenhirState387 : ((('s, 'r) _menhir_cell1_lexp, 'r) _menhir_cell1_lexp, 'r) _menhir_state
    (** State 387.
        Stack shape : lexp lexp.
        Start symbol: <undetermined>. *)

  | MenhirState391 : ((('s, 'r) _menhir_cell1_LPAREN, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 391.
        Stack shape : LPAREN exp.
        Start symbol: <undetermined>. *)

  | MenhirState395 : ((('s, 'r) _menhir_cell1_FOR _menhir_cell0_IDENT, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 395.
        Stack shape : FOR IDENT exp.
        Start symbol: <undetermined>. *)

  | MenhirState397 : (((('s, 'r) _menhir_cell1_FOR _menhir_cell0_IDENT, 'r) _menhir_cell1_exp, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 397.
        Stack shape : FOR IDENT exp exp.
        Start symbol: <undetermined>. *)

  | MenhirState404 : ((('s, 'r) _menhir_cell1_MATCH, 'r) _menhir_cell1_exp _menhir_cell0_option_PIPE_, 'r) _menhir_state
    (** State 404.
        Stack shape : MATCH exp option(PIPE).
        Start symbol: <undetermined>. *)

  | MenhirState405 : (((('s, 'r) _menhir_cell1_MATCH, 'r) _menhir_cell1_exp _menhir_cell0_option_PIPE_, 'r) _menhir_cell1_UP_IDENT, 'r) _menhir_state
    (** State 405.
        Stack shape : MATCH exp option(PIPE) UP_IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState407 : ((('s, 'r) _menhir_cell1_UP_IDENT, 'r) _menhir_cell1_apat, 'r) _menhir_state
    (** State 407.
        Stack shape : UP_IDENT apat.
        Start symbol: <undetermined>. *)

  | MenhirState410 : (('s, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 410.
        Stack shape : IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState414 : (('s, 'r) _menhir_cell1_separated_nonempty_list_PIPE_const_, 'r) _menhir_state
    (** State 414.
        Stack shape : separated_nonempty_list(PIPE,const).
        Start symbol: <undetermined>. *)

  | MenhirState419 : (('s, 'r) _menhir_cell1_match_case_const, 'r) _menhir_state
    (** State 419.
        Stack shape : match_case_const.
        Start symbol: <undetermined>. *)

  | MenhirState422 : (('s, 'r) _menhir_cell1_const, 'r) _menhir_state
    (** State 422.
        Stack shape : const.
        Start symbol: <undetermined>. *)

  | MenhirState425 : (('s, 'r) _menhir_cell1_match_case, 'r) _menhir_state
    (** State 425.
        Stack shape : match_case.
        Start symbol: <undetermined>. *)

  | MenhirState426 : ((('s, 'r) _menhir_cell1_match_case, 'r) _menhir_cell1_UP_IDENT, 'r) _menhir_state
    (** State 426.
        Stack shape : match_case UP_IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState430 : (((('s, 'r) _menhir_cell1_MATCH, 'r) _menhir_cell1_exp _menhir_cell0_option_PIPE_, 'r) _menhir_cell1_list_match_case_const_ _menhir_cell0_IDENT, 'r) _menhir_state
    (** State 430.
        Stack shape : MATCH exp option(PIPE) list(match_case_const) IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState435 : ((('s, 'r) _menhir_cell1_REGISTER, 'r) _menhir_cell1_IDENT, 'r) _menhir_state
    (** State 435.
        Stack shape : REGISTER IDENT.
        Start symbol: <undetermined>. *)

  | MenhirState438 : ((('s, 'r) _menhir_cell1_REGISTER, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 438.
        Stack shape : REGISTER exp.
        Start symbol: <undetermined>. *)

  | MenhirState441 : ((('s, 'r) _menhir_cell1_PARFOR _menhir_cell0_IDENT, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 441.
        Stack shape : PARFOR IDENT exp.
        Start symbol: <undetermined>. *)

  | MenhirState443 : (((('s, 'r) _menhir_cell1_PARFOR _menhir_cell0_IDENT, 'r) _menhir_cell1_exp, 'r) _menhir_cell1_exp, 'r) _menhir_state
    (** State 443.
        Stack shape : PARFOR IDENT exp exp.
        Start symbol: <undetermined>. *)

  | MenhirState452 : (('s, 'r) _menhir_cell1_app_exp, 'r) _menhir_state
    (** State 452.
        Stack shape : app_exp.
        Start symbol: <undetermined>. *)

  | MenhirState458 : ('s, _menhir_box_pi) _menhir_state
    (** State 458.
        Stack shape : .
        Start symbol: pi. *)

  | MenhirState459 : (('s, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_state
    (** State 459.
        Stack shape : TYPE.
        Start symbol: pi. *)

  | MenhirState460 : ((('s, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_state
    (** State 460.
        Stack shape : TYPE IDENT.
        Start symbol: pi. *)

  | MenhirState461 : (((('s, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_EQ, _menhir_box_pi) _menhir_state
    (** State 461.
        Stack shape : TYPE IDENT EQ.
        Start symbol: pi. *)

  | MenhirState463 : (('s, _menhir_box_pi) _menhir_cell1_UP_IDENT, _menhir_box_pi) _menhir_state
    (** State 463.
        Stack shape : UP_IDENT.
        Start symbol: pi. *)

  | MenhirState466 : (('s, _menhir_box_pi) _menhir_cell1_ty_case, _menhir_box_pi) _menhir_state
    (** State 466.
        Stack shape : ty_case.
        Start symbol: pi. *)

  | MenhirState468 : ((((('s, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_EQ, _menhir_box_pi) _menhir_cell1_tyB, _menhir_box_pi) _menhir_state
    (** State 468.
        Stack shape : TYPE IDENT EQ tyB.
        Start symbol: pi. *)

  | MenhirState471 : ((((('s, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_EQ, _menhir_box_pi) _menhir_cell1_separated_nonempty_list_PIPE_ty_case_, _menhir_box_pi) _menhir_state
    (** State 471.
        Stack shape : TYPE IDENT EQ separated_nonempty_list(PIPE,ty_case).
        Start symbol: pi. *)

  | MenhirState473 : ((('s, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_tyB, _menhir_box_pi) _menhir_state
    (** State 473.
        Stack shape : TYPE tyB.
        Start symbol: pi. *)

  | MenhirState475 : (((('s, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_tyB, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_state
    (** State 475.
        Stack shape : TYPE tyB IDENT.
        Start symbol: pi. *)

  | MenhirState477 : (('s, _menhir_box_pi) _menhir_cell1_INT_LIT, _menhir_box_pi) _menhir_state
    (** State 477.
        Stack shape : INT_LIT.
        Start symbol: pi. *)

  | MenhirState480 : ((((('s, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_tyB, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_separated_nonempty_list_COMMA_INT_LIT_ _menhir_cell0_RBRACKET, _menhir_box_pi) _menhir_state
    (** State 480.
        Stack shape : TYPE tyB IDENT separated_nonempty_list(COMMA,INT_LIT) RBRACKET.
        Start symbol: pi. *)

  | MenhirState483 : (((('s, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_tyB, _menhir_box_pi) _menhir_cell1_AT _menhir_cell0_IDENT, _menhir_box_pi) _menhir_state
    (** State 483.
        Stack shape : TYPE tyB AT IDENT.
        Start symbol: pi. *)

  | MenhirState489 : (('s, _menhir_box_pi) _menhir_cell1_SHARED _menhir_cell0_EXTERNAL _menhir_cell0_IDENT, _menhir_box_pi) _menhir_state
    (** State 489.
        Stack shape : SHARED EXTERNAL IDENT.
        Start symbol: pi. *)

  | MenhirState495 : (('s, _menhir_box_pi) _menhir_cell1_OPERATOR _menhir_cell0_OPERATOR_IDENT, _menhir_box_pi) _menhir_state
    (** State 495.
        Stack shape : OPERATOR OPERATOR_IDENT.
        Start symbol: pi. *)

  | MenhirState502 : (('s, _menhir_box_pi) _menhir_cell1_OPERATOR _menhir_cell0_OPERATOR_IDENT, _menhir_box_pi) _menhir_state
    (** State 502.
        Stack shape : OPERATOR OPERATOR_IDENT.
        Start symbol: pi. *)

  | MenhirState508 : (('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_state
    (** State 508.
        Stack shape : LET.
        Start symbol: pi. *)

  | MenhirState511 : ((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_STATIC _menhir_cell0_IDENT, _menhir_box_pi) _menhir_state
    (** State 511.
        Stack shape : LET STATIC IDENT.
        Start symbol: pi. *)

  | MenhirState513 : (((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_STATIC _menhir_cell0_IDENT, _menhir_box_pi) _menhir_cell1_aexp, _menhir_box_pi) _menhir_state
    (** State 513.
        Stack shape : LET STATIC IDENT aexp.
        Start symbol: pi. *)

  | MenhirState517 : ((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_STATIC _menhir_cell0_IDENT, _menhir_box_pi) _menhir_state
    (** State 517.
        Stack shape : LET STATIC IDENT.
        Start symbol: pi. *)

  | MenhirState520 : ((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_state
    (** State 520.
        Stack shape : LET REC.
        Start symbol: pi. *)

  | MenhirState521 : (((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_state
    (** State 521.
        Stack shape : LET REC IDENT.
        Start symbol: pi. *)

  | MenhirState522 : ((((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic, _menhir_box_pi) _menhir_state
    (** State 522.
        Stack shape : LET REC IDENT arg_ty_atomic.
        Start symbol: pi. *)

  | MenhirState523 : (((((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic, _menhir_box_pi) _menhir_cell1_ret_ty_annot_eq, _menhir_box_pi) _menhir_state
    (** State 523.
        Stack shape : LET REC IDENT arg_ty_atomic ret_ty_annot_eq.
        Start symbol: pi. *)

  | MenhirState527 : (((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_ty_annot_IDENT_, _menhir_box_pi) _menhir_state
    (** State 527.
        Stack shape : LET REC ty_annot(IDENT).
        Start symbol: pi. *)

  | MenhirState530 : ((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_state
    (** State 530.
        Stack shape : LET IDENT.
        Start symbol: pi. *)

  | MenhirState531 : (((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic, _menhir_box_pi) _menhir_state
    (** State 531.
        Stack shape : LET IDENT arg_ty_atomic.
        Start symbol: pi. *)

  | MenhirState532 : ((((('s, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic, _menhir_box_pi) _menhir_cell1_ret_ty_annot_eq, _menhir_box_pi) _menhir_state
    (** State 532.
        Stack shape : LET IDENT arg_ty_atomic ret_ty_annot_eq.
        Start symbol: pi. *)

  | MenhirState542 : (('s, _menhir_box_pi) _menhir_cell1_EXTERNAL _menhir_cell0_IDENT, _menhir_box_pi) _menhir_state
    (** State 542.
        Stack shape : EXTERNAL IDENT.
        Start symbol: pi. *)

  | MenhirState546 : (('s, _menhir_box_pi) _menhir_cell1_type_alias, _menhir_box_pi) _menhir_state
    (** State 546.
        Stack shape : type_alias.
        Start symbol: pi. *)

  | MenhirState547 : (('s, _menhir_box_pi) _menhir_cell1_typ_sum, _menhir_box_pi) _menhir_state
    (** State 547.
        Stack shape : typ_sum.
        Start symbol: pi. *)

  | MenhirState548 : (('s, _menhir_box_pi) _menhir_cell1_static, _menhir_box_pi) _menhir_state
    (** State 548.
        Stack shape : static.
        Start symbol: pi. *)

  | MenhirState550 : (('s, _menhir_box_pi) _menhir_cell1_ext_fun, _menhir_box_pi) _menhir_state
    (** State 550.
        Stack shape : ext_fun.
        Start symbol: pi. *)

  | MenhirState552 : (('s, _menhir_box_pi) _menhir_cell1_ext_circ, _menhir_box_pi) _menhir_state
    (** State 552.
        Stack shape : ext_circ.
        Start symbol: pi. *)

  | MenhirState557 : (('s, _menhir_box_pi) _menhir_cell1_decl, _menhir_box_pi) _menhir_state
    (** State 557.
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
# 1307 "src/frontend/parser.ml"
) * Lexing.position * Lexing.position

and 's _menhir_cell0_IDENT = 
  | MenhirCell0_IDENT of 's * (
# 79 "src/frontend/parser.mly"
       (string)
# 1314 "src/frontend/parser.ml"
) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_INT_LIT = 
  | MenhirCell1_INT_LIT of 's * ('s, 'r) _menhir_state * (
# 81 "src/frontend/parser.mly"
       (int)
# 1324 "src/frontend/parser.ml"
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
# 1364 "src/frontend/parser.ml"
) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_PARFOR = 
  | MenhirCell1_PARFOR of 's * ('s, 'r) _menhir_state * Lexing.position

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
# 1410 "src/frontend/parser.ml"
) * Lexing.position * Lexing.position

and 's _menhir_cell0_UP_IDENT = 
  | MenhirCell0_UP_IDENT of 's * (
# 79 "src/frontend/parser.mly"
       (string)
# 1417 "src/frontend/parser.ml"
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
# 630 "src/frontend/parser.mly"
              ( mk_loc (with_file _loc) e )
# 1443 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_003 =
  fun ex ->
    (
# 633 "src/frontend/parser.mly"
               ( E_get(ex) )
# 1451 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_004 =
  fun e ->
    (
# 634 "src/frontend/parser.mly"
                      ( e )
# 1459 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_005 =
  fun e ty ->
    (
# 635 "src/frontend/parser.mly"
                                ( ty_annot ~ty e )
# 1467 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_006 =
  fun c ->
    (
# 636 "src/frontend/parser.mly"
          ( E_const c )
# 1475 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_007 =
  fun k ->
    (
# 638 "src/frontend/parser.mly"
                          ( E_const (Op(Runtime(Resize_int k))) )
# 1483 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_008 =
  fun k ->
    (
# 639 "src/frontend/parser.mly"
                               ( E_const (Op(Runtime(Tuple_of_int k))) )
# 1491 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_009 =
  fun k ->
    (
# 640 "src/frontend/parser.mly"
                               ( E_const (Op(Runtime(Int_of_tuple k))) )
# 1499 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_010 =
  fun e ->
    (
# 641 "src/frontend/parser.mly"
                     (
                let loc = loc_of e in
                match Ast_undecorated.remove_deco e with
                | E_tuple[v;e] -> assert (is_variable v || evaluated v); (* todo: error *)
                                  let x = Ast.gensym () in
                                  E_vector_mapi(false,(P_var x,(Types.new_tyB_unknown(),Types.new_tyB_unknown()),E_app(v,E_var x)), 
                                     mk_loc loc e, new_size_unknown ())
                | _ -> assert false (* todo error *) )
# 1514 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_011 =
  fun k x ->
    (
# 650 "src/frontend/parser.mly"
                        ( E_const (Op(Runtime(Unroll k))) )
# 1522 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_012 =
  fun l ->
    (
# 651 "src/frontend/parser.mly"
                           ( E_const (Op(Runtime(Vector_create l))) )
# 1530 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_013 =
  fun _endpos_x_ _startpos_x_ x ->
    let _endpos = _endpos_x_ in
    let _startpos = _startpos_x_ in
    let _loc = (_startpos, _endpos) in
    (
# 652 "src/frontend/parser.mly"
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
# 1560 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_014 =
  fun xs ->
    let _2 = 
# 241 "<standard.mly>"
    ( xs )
# 1568 "src/frontend/parser.ml"
     in
    (
# 673 "src/frontend/parser.mly"
    ( (* Buffer n *) assert false (*todo*)  )
# 1573 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_015 =
  fun cases e otherwise ->
    (
# 678 "src/frontend/parser.mly"
      ( E_case(e,cases,otherwise) )
# 1581 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_016 =
  fun e rev_cases ->
    (
# 682 "src/frontend/parser.mly"
      ( let (hs,eo) = rev_cases in
        E_match(e,List.rev hs,eo) )
# 1590 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_017 =
  fun e e1 e2 i ->
    (
# 685 "src/frontend/parser.mly"
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
# 1611 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_018 =
  fun _endpos__9_ _startpos__1_ e e_st1 e_st2 x ->
    let _endpos = _endpos__9_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 701 "src/frontend/parser.mly"
      ( E_for(x,e_st1,e_st2,e,with_file _loc) )
# 1622 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_019 =
  fun b ->
    (
# 265 "src/frontend/parser.mly"
                             ( b )
# 1630 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_020 =
  fun b ->
    (
# 267 "src/frontend/parser.mly"
        ( b )
# 1638 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_021 =
  fun e ->
    (
# 269 "src/frontend/parser.mly"
        ( e )
# 1646 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_022 =
  fun _endpos_e1_ _startpos_f_ty_opt_ e1 f_ty_opt ->
    (
# 271 "src/frontend/parser.mly"
        (
            let f,ty_opt = f_ty_opt in
            let loc_fun = with_file (_startpos_f_ty_opt_,_endpos_e1_) in
            let v = mk_fix f (ty_annot_opt ~ty:ty_opt e1) loc_fun in
            P_var f, v
        )
# 1659 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_023 =
  fun b ->
    (
# 265 "src/frontend/parser.mly"
                             ( b )
# 1667 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_024 =
  fun b ->
    (
# 267 "src/frontend/parser.mly"
        ( b )
# 1675 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_025 =
  fun e ->
    (
# 269 "src/frontend/parser.mly"
        ( e )
# 1683 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_026 =
  fun _endpos_e1_ _startpos_f_ty_opt_ e1 f_ty_opt ->
    (
# 271 "src/frontend/parser.mly"
        (
            let f,ty_opt = f_ty_opt in
            let loc_fun = with_file (_startpos_f_ty_opt_,_endpos_e1_) in
            let v = mk_fix f (ty_annot_opt ~ty:ty_opt e1) loc_fun in
            P_var f, v
        )
# 1696 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_027 =
  fun () ->
    (
# 729 "src/frontend/parser.mly"
                ( P_unit )
# 1704 "src/frontend/parser.ml"
     : (Ast.p))

let _menhir_action_028 =
  fun p ->
    (
# 730 "src/frontend/parser.mly"
                      ( p )
# 1712 "src/frontend/parser.ml"
     : (Ast.p))

let _menhir_action_029 =
  fun x ->
    (
# 731 "src/frontend/parser.mly"
          ( P_var x )
# 1720 "src/frontend/parser.ml"
     : (Ast.p))

let _menhir_action_030 =
  fun _endpos_e_ _startpos_e_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    let _loc = (_startpos, _endpos) in
    (
# 516 "src/frontend/parser.mly"
                 ( mk_loc (with_file _loc) e )
# 1731 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_031 =
  fun _endpos_e_ _startpos__1_ e sz ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 519 "src/frontend/parser.mly"
                                  ( E_array_make(sz, e, with_file _loc) )
# 1742 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_032 =
  fun _endpos__6_ _startpos__1_ sz ->
    let _endpos = _endpos__6_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 521 "src/frontend/parser.mly"
                                     ( E_array_create(sz, with_file _loc) )
# 1753 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_033 =
  fun _endpos__6_ _startpos__1_ sz ->
    let _endpos = _endpos__6_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 521 "src/frontend/parser.mly"
                                     ( E_array_create(sz, with_file _loc) )
# 1764 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_034 =
  fun e1 e2 v ->
    (
# 524 "src/frontend/parser.mly"
    ( 
        E_app(e1,E_tuple[e2;v]) 
    )
# 1774 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_035 =
  fun e1 e2 v ->
    (
# 524 "src/frontend/parser.mly"
    ( 
        E_app(e1,E_tuple[e2;v]) 
    )
# 1784 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_036 =
  fun e ex ->
    (
# 527 "src/frontend/parser.mly"
                           ( E_set(ex,e) )
# 1792 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_037 =
  fun e ->
    (
# 528 "src/frontend/parser.mly"
                        ( E_ref e )
# 1800 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_038 =
  fun _endpos_e_ _startpos__1_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 533 "src/frontend/parser.mly"
             ( match Ast_undecorated.remove_deco e with 
               | E_tuple[E_var x;e1;e2] -> E_array_set(x,e1,e2) 
               | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                   ~msg:"... array set" () )
# 1814 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_039 =
  fun e1 x ->
    (
# 539 "src/frontend/parser.mly"
   ( E_array_get(x,e1) )
# 1822 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_040 =
  fun e1 x ->
    (
# 539 "src/frontend/parser.mly"
   ( E_array_get(x,e1) )
# 1830 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_041 =
  fun e1 x ->
    (
# 540 "src/frontend/parser.mly"
                     ( E_array_get(x,e1) )
# 1838 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_042 =
  fun x ->
    (
# 541 "src/frontend/parser.mly"
                     ( E_array_length x )
# 1846 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_043 =
  fun _endpos_a_ _startpos__1_ a ->
    let _endpos = _endpos_a_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 543 "src/frontend/parser.mly"
                ( match un_annot a with
                  | E_var x -> E_array_length x 
                  | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                          ~msg:"array length: should be a variable......" () )
# 1860 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_044 =
  fun _endpos_a_ _startpos__1_ a ->
    let _endpos = _endpos_a_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 543 "src/frontend/parser.mly"
                ( match un_annot a with
                  | E_var x -> E_array_length x 
                  | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                          ~msg:"array length: should be a variable......" () )
# 1874 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_045 =
  fun e1 e2 x ->
    (
# 549 "src/frontend/parser.mly"
  ( E_array_set(x,e1,e2) )
# 1882 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_046 =
  fun e1 e2 x ->
    (
# 549 "src/frontend/parser.mly"
  ( E_array_set(x,e1,e2) )
# 1890 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_047 =
  fun n ->
    (
# 550 "src/frontend/parser.mly"
                        ( E_const(C_size n) )
# 1898 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_048 =
  fun _endpos_es_ _startpos_e_ e es ->
    let _endpos = _endpos_es_ in
    let _startpos = _startpos_e_ in
    let _loc = (_startpos, _endpos) in
    (
# 552 "src/frontend/parser.mly"
      ( match e::es with
        | [e1;e2] -> (match un_annot e1 with
                      | E_var _ | E_const _ | E_fun _ -> 
                        E_app(e1,e2)
                      | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                   ~msg:"expression in functional position should be a variable or a constante" ())
        | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                   ~msg:"All functions and primitives should be unary. Hints: use a tuple as argument" () )
# 1916 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_049 =
  fun e1 ->
    (
# 560 "src/frontend/parser.mly"
                                       ( E_app(E_const(Op(Runtime(External_fun("Int.neg",new_ty_unknown ())))),e1) )
# 1924 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_050 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 766 "src/frontend/parser.mly"
             ( External_fun("Int.add",new_ty_unknown ()) )
# 1932 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 1942 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_051 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 767 "src/frontend/parser.mly"
             ( External_fun("Int.sub",new_ty_unknown ()) )
# 1950 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 1960 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_052 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 768 "src/frontend/parser.mly"
             ( External_fun("Int.mul",new_ty_unknown ()) )
# 1968 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 1978 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_053 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 769 "src/frontend/parser.mly"
             ( External_fun("Int.div",new_ty_unknown ()) )
# 1986 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 1996 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_054 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 770 "src/frontend/parser.mly"
             ( External_fun("Int.modulo",new_ty_unknown ()) )
# 2004 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2014 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_055 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 771 "src/frontend/parser.mly"
             ( External_fun("Int.lt",new_ty_unknown ()) )
# 2022 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2032 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_056 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 772 "src/frontend/parser.mly"
             ( External_fun("Int.gt",new_ty_unknown ()) )
# 2040 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2050 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_057 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 773 "src/frontend/parser.mly"
             ( External_fun("Int.le",new_ty_unknown ()) )
# 2058 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2068 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_058 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 774 "src/frontend/parser.mly"
             ( External_fun("Int.ge",new_ty_unknown ()) )
# 2076 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2086 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_059 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 775 "src/frontend/parser.mly"
             ( External_fun("Int.eq",new_ty_unknown ()) )
# 2094 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2104 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_060 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 775 "src/frontend/parser.mly"
             ( External_fun("Int.eq",new_ty_unknown ()) )
# 2112 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2122 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_061 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 776 "src/frontend/parser.mly"
             ( External_fun("Int.neq",new_ty_unknown ()) )
# 2130 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2140 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_062 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 777 "src/frontend/parser.mly"
             ( External_fun("Bool.land",new_ty_unknown ()) )
# 2148 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2158 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_063 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 778 "src/frontend/parser.mly"
             ( External_fun("Bool.lxor",new_ty_unknown ()) )
# 2166 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2176 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_064 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 779 "src/frontend/parser.mly"
             ( External_fun("Int.lxor",new_ty_unknown ()) )
# 2184 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2194 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_065 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 780 "src/frontend/parser.mly"
             ( External_fun("Int.land",new_ty_unknown ()) )
# 2202 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2212 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_066 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 781 "src/frontend/parser.mly"
             ( External_fun("Int.lor",new_ty_unknown ()) )
# 2220 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2230 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_067 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 782 "src/frontend/parser.mly"
             ( External_fun("Int.lsl",new_ty_unknown ()) )
# 2238 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2248 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_068 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 783 "src/frontend/parser.mly"
             ( External_fun("Int.lsr",new_ty_unknown ()) )
# 2256 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2266 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_069 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let op = 
# 784 "src/frontend/parser.mly"
             ( External_fun("Int.asr",new_ty_unknown ()) )
# 2274 "src/frontend/parser.ml"
     in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 562 "src/frontend/parser.mly"
        ( E_app (mk_loc (with_file _loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file _loc) @@ E_tuple [e1;e2])
        )
# 2284 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_070 =
  fun _endpos_e2_ _startpos_e1_ e1 e2 ->
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 566 "src/frontend/parser.mly"
        ( let e3 = mk_loc (with_file _loc) @@ E_const (Bool false) in
          E_if(e1,e2,e3)
        )
# 2297 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_071 =
  fun _endpos_e3_ _startpos_e1_ e1 e3 ->
    let _endpos = _endpos_e3_ in
    let _startpos = _startpos_e1_ in
    let _loc = (_startpos, _endpos) in
    (
# 570 "src/frontend/parser.mly"
        ( let e2 = mk_loc (with_file _loc) @@ E_const (Bool true) in
          E_if(e1,e2,e3)
        )
# 2310 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_072 =
  fun _endpos_e0_ _startpos__1_ e0 ev ->
    let _endpos = _endpos_e0_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 574 "src/frontend/parser.mly"
       ( match un_annot ev with
         | E_fun(p,(_,tyB),e1) -> E_reg((p,tyB,e1),e0,Ast.gensym ())
         | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                               ~msg:"This expression should be a function" ()
       )
# 2325 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_073 =
  fun e0 f ->
    (
# 580 "src/frontend/parser.mly"
       (
        let y = gensym () in
         E_reg((P_var y,Types.new_tyB_unknown(),E_app(E_var f,E_var y)),e0,Ast.gensym ())
       )
# 2336 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_074 =
  fun e1 e2 ->
    (
# 585 "src/frontend/parser.mly"
       ( E_exec(e1,e2,None,"") )
# 2344 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_075 =
  fun e1 e2 e3 ->
    (
# 587 "src/frontend/parser.mly"
       ( E_exec(e1,e2,Some e3,"") )
# 2352 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_076 =
  fun _endpos_e_st3_ _startpos__1_ e_init2 e_st3 ef1 ->
    let _endpos = _endpos_e_st3_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 589 "src/frontend/parser.mly"
  ( let z = Ast.gensym () in
    E_generate((P_var z,(Types.new_ty_unknown(),Types.new_tyB_unknown()),  E_app(ef1,E_var z)),e_init2,e_st3,with_file _loc) )
# 2364 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_077 =
  fun _endpos_e1_ _startpos__1_ e1 ->
    let _endpos = _endpos_e1_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 593 "src/frontend/parser.mly"
    (
        Prelude.Errors.raise_error ~loc:(with_file _loc)
            ~msg:"missing ``default'' close; `exec e default e` expected" ()
    )
# 2378 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_078 =
  fun _endpos__3_ _startpos__1_ e1 ->
    let _endpos = _endpos__3_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 598 "src/frontend/parser.mly"
    (
        Prelude.Errors.raise_error ~loc:(with_file _loc)
            ~msg:"missing expression after keyword ``default''; `exec e default e` expected" ()
    )
# 2392 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_079 =
  fun e i ->
    (
# 618 "src/frontend/parser.mly"
     ( E_run(i, e) )
# 2400 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_080 =
  fun e ->
    (
# 622 "src/frontend/parser.mly"
         ( e )
# 2408 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_081 =
  fun a ->
    (
# 445 "src/frontend/parser.mly"
                                 ( a )
# 2416 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_082 =
  fun a ->
    (
# 445 "src/frontend/parser.mly"
                                 ( a )
# 2424 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_083 =
  fun p ->
    (
# 448 "src/frontend/parser.mly"
                      ( p, None )
# 2432 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_084 =
  fun p ty ->
    (
# 449 "src/frontend/parser.mly"
                                 (p, Some ty)
# 2440 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_085 =
  fun p ->
    (
# 450 "src/frontend/parser.mly"
         ( p, None )
# 2448 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_086 =
  fun p ->
    (
# 440 "src/frontend/parser.mly"
        ( p, None )
# 2456 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_087 =
  fun p ty ->
    (
# 441 "src/frontend/parser.mly"
                     (p, Some ty)
# 2464 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_088 =
  fun ty ->
    (
# 398 "src/frontend/parser.mly"
      ( ty )
# 2472 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_089 =
  fun e ->
    (
# 417 "src/frontend/parser.mly"
                        ( e )
# 2480 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_090 =
  fun c ->
    (
# 418 "src/frontend/parser.mly"
          ( E_const c )
# 2488 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_091 =
  fun e p_ty_opt ->
    (
# 508 "src/frontend/parser.mly"
        (
            let p,ty_opt = p_ty_opt in
            p,ty_annot_opt ~ty:ty_opt e
        )
# 2499 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_092 =
  fun e p ->
    (
# 513 "src/frontend/parser.mly"
    ( p,e )
# 2507 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_093 =
  fun w ->
    (
# 492 "src/frontend/parser.mly"
  ( match w with
    | [],_ | _,[] -> assert false
    | [p],[e] -> (p,e)
    | ps,es -> (P_tuple ps, E_par es) )
# 2518 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_094 =
  fun b ->
    (
# 498 "src/frontend/parser.mly"
                 ( let (p,e) = b in ([p],[e]) )
# 2526 "src/frontend/parser.ml"
     : (Ast.p list * Ast.e_static list))

let _menhir_action_095 =
  fun b1 bs ->
    (
# 500 "src/frontend/parser.mly"
   ( let (p1,e1) = b1 in
     let (ps,es) = bs in
     (p1::ps,e1::es) )
# 2536 "src/frontend/parser.ml"
     : (Ast.p list * Ast.e_static list))

let _menhir_action_096 =
  fun () ->
    (
# 740 "src/frontend/parser.mly"
                ( Unit )
# 2544 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_097 =
  fun b ->
    (
# 741 "src/frontend/parser.mly"
                ( Bool b )
# 2552 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_098 =
  fun n ->
    (
# 742 "src/frontend/parser.mly"
            (
    Int (n,new_size_unknown()) )
# 2561 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_099 =
  fun _endpos_k_ _startpos_n_ k n ->
    let _endpos = _endpos_k_ in
    let _startpos = _startpos_n_ in
    let _loc = (_startpos, _endpos) in
    (
# 745 "src/frontend/parser.mly"
    ( if Float.log2 (float n) >= float (k-1) then
       Prelude.Errors.raise_error ~loc:(with_file _loc)
          ~msg:("Integer literal "^
                string_of_int n^
                " exceeds the range of representable integers of type int<"^
                string_of_int k ^">") ()
      else Int (n,Sz_lit k) )
# 2578 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_100 =
  fun s ->
    (
# 752 "src/frontend/parser.mly"
                         ( String s )
# 2586 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_101 =
  fun x ->
    (
# 753 "src/frontend/parser.mly"
                   ( Op(Runtime(External_fun(x,new_ty_unknown()))) )
# 2594 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_102 =
  fun x ->
    (
# 754 "src/frontend/parser.mly"
                         ( Inj x )
# 2602 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_103 =
  fun () ->
    let op = 
# 766 "src/frontend/parser.mly"
             ( External_fun("Int.add",new_ty_unknown ()) )
# 2610 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2615 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_104 =
  fun () ->
    let op = 
# 767 "src/frontend/parser.mly"
             ( External_fun("Int.sub",new_ty_unknown ()) )
# 2623 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2628 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_105 =
  fun () ->
    let op = 
# 768 "src/frontend/parser.mly"
             ( External_fun("Int.mul",new_ty_unknown ()) )
# 2636 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2641 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_106 =
  fun () ->
    let op = 
# 769 "src/frontend/parser.mly"
             ( External_fun("Int.div",new_ty_unknown ()) )
# 2649 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2654 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_107 =
  fun () ->
    let op = 
# 770 "src/frontend/parser.mly"
             ( External_fun("Int.modulo",new_ty_unknown ()) )
# 2662 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2667 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_108 =
  fun () ->
    let op = 
# 771 "src/frontend/parser.mly"
             ( External_fun("Int.lt",new_ty_unknown ()) )
# 2675 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2680 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_109 =
  fun () ->
    let op = 
# 772 "src/frontend/parser.mly"
             ( External_fun("Int.gt",new_ty_unknown ()) )
# 2688 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2693 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_110 =
  fun () ->
    let op = 
# 773 "src/frontend/parser.mly"
             ( External_fun("Int.le",new_ty_unknown ()) )
# 2701 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2706 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_111 =
  fun () ->
    let op = 
# 774 "src/frontend/parser.mly"
             ( External_fun("Int.ge",new_ty_unknown ()) )
# 2714 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2719 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_112 =
  fun () ->
    let op = 
# 775 "src/frontend/parser.mly"
             ( External_fun("Int.eq",new_ty_unknown ()) )
# 2727 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2732 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_113 =
  fun () ->
    let op = 
# 775 "src/frontend/parser.mly"
             ( External_fun("Int.eq",new_ty_unknown ()) )
# 2740 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2745 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_114 =
  fun () ->
    let op = 
# 776 "src/frontend/parser.mly"
             ( External_fun("Int.neq",new_ty_unknown ()) )
# 2753 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2758 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_115 =
  fun () ->
    let op = 
# 777 "src/frontend/parser.mly"
             ( External_fun("Bool.land",new_ty_unknown ()) )
# 2766 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2771 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_116 =
  fun () ->
    let op = 
# 778 "src/frontend/parser.mly"
             ( External_fun("Bool.lxor",new_ty_unknown ()) )
# 2779 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2784 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_117 =
  fun () ->
    let op = 
# 779 "src/frontend/parser.mly"
             ( External_fun("Int.lxor",new_ty_unknown ()) )
# 2792 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2797 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_118 =
  fun () ->
    let op = 
# 780 "src/frontend/parser.mly"
             ( External_fun("Int.land",new_ty_unknown ()) )
# 2805 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2810 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_119 =
  fun () ->
    let op = 
# 781 "src/frontend/parser.mly"
             ( External_fun("Int.lor",new_ty_unknown ()) )
# 2818 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2823 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_120 =
  fun () ->
    let op = 
# 782 "src/frontend/parser.mly"
             ( External_fun("Int.lsl",new_ty_unknown ()) )
# 2831 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2836 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_121 =
  fun () ->
    let op = 
# 783 "src/frontend/parser.mly"
             ( External_fun("Int.lsr",new_ty_unknown ()) )
# 2844 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2849 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_122 =
  fun () ->
    let op = 
# 784 "src/frontend/parser.mly"
             ( External_fun("Int.asr",new_ty_unknown ()) )
# 2857 "src/frontend/parser.ml"
     in
    (
# 755 "src/frontend/parser.mly"
                         ( Op(Runtime(op)) )
# 2862 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_123 =
  fun cs ->
    (
# 757 "src/frontend/parser.mly"
    ( C_vector cs )
# 2870 "src/frontend/parser.ml"
     : (Ast.c))

let _menhir_action_124 =
  fun d ->
    (
# 204 "src/frontend/parser.mly"
  ( clear_tyvar_constraints();
    d )
# 2879 "src/frontend/parser.ml"
     : ((Ast.p * Ast.e_static) * Prelude.loc))

let _menhir_action_125 =
  fun _endpos_b_ _startpos__1_ b ->
    let _endpos = _endpos_b_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 209 "src/frontend/parser.mly"
        ( b,(with_file _loc) )
# 2890 "src/frontend/parser.ml"
     : ((Ast.p * Ast.e_static) * Prelude.loc))

let _menhir_action_126 =
  fun _endpos__2_ _startpos_e_ e ->
    let _endpos = _endpos__2_ in
    let _startpos = _startpos_e_ in
    let _loc = (_startpos, _endpos) in
    (
# 213 "src/frontend/parser.mly"
                  ( ((P_var "_", e),(with_file _loc))  )
# 2901 "src/frontend/parser.ml"
     : ((Ast.p * Ast.e_static) * Prelude.loc))

let _menhir_action_127 =
  fun e ->
    (
# 626 "src/frontend/parser.mly"
                        ( e )
# 2909 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_128 =
  fun _endpos_e_ _startpos_e_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    let _loc = (_startpos, _endpos) in
    (
# 423 "src/frontend/parser.mly"
             ( mk_loc (with_file _loc) e )
# 2920 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_129 =
  fun e1 e2 ->
    (
# 429 "src/frontend/parser.mly"
        (
            E_letIn(P_unit,Ty_base TyB_unit, e1,e2)
        )
# 2930 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_130 =
  fun e es ->
    (
# 433 "src/frontend/parser.mly"
        (
            E_tuple (e::es)
        )
# 2940 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_131 =
  fun e ->
    (
# 437 "src/frontend/parser.mly"
         ( e )
# 2948 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_132 =
  fun e ->
    (
# 200 "src/frontend/parser.mly"
            (e)
# 2956 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_133 =
  fun t x ->
    (
# 164 "src/frontend/parser.mly"
                                      ( (x, (t, false)) )
# 2964 "src/frontend/parser.ml"
     : (Ast.x * (Types.ty * bool)))

let _menhir_action_134 =
  fun t x ->
    (
# 165 "src/frontend/parser.mly"
                                             ( (x, (t, true)) )
# 2972 "src/frontend/parser.ml"
     : (Ast.x * (Types.ty * bool)))

let _menhir_action_135 =
  fun t x ->
    let imp = 
# 160 "src/frontend/parser.mly"
            ( true )
# 2980 "src/frontend/parser.ml"
     in
    (
# 153 "src/frontend/parser.mly"
   ( Hashtbl.clear hash_size_tvar;
     (x, (t,(false,get_arity t,imp))) )
# 2986 "src/frontend/parser.ml"
     : (Ast.x * (Types.ty * (bool * int * bool))))

let _menhir_action_136 =
  fun t x ->
    let imp = 
# 161 "src/frontend/parser.mly"
  (false)
# 2994 "src/frontend/parser.ml"
     in
    (
# 153 "src/frontend/parser.mly"
   ( Hashtbl.clear hash_size_tvar;
     (x, (t,(false,get_arity t,imp))) )
# 3000 "src/frontend/parser.ml"
     : (Ast.x * (Types.ty * (bool * int * bool))))

let _menhir_action_137 =
  fun t x ->
    let imp = 
# 160 "src/frontend/parser.mly"
            ( true )
# 3008 "src/frontend/parser.ml"
     in
    (
# 156 "src/frontend/parser.mly"
   ( Hashtbl.clear hash_size_tvar;
     (x, (t,(true,get_arity t,imp))) )
# 3014 "src/frontend/parser.ml"
     : (Ast.x * (Types.ty * (bool * int * bool))))

let _menhir_action_138 =
  fun t x ->
    let imp = 
# 161 "src/frontend/parser.mly"
  (false)
# 3022 "src/frontend/parser.ml"
     in
    (
# 156 "src/frontend/parser.mly"
   ( Hashtbl.clear hash_size_tvar;
     (x, (t,(true,get_arity t,imp))) )
# 3028 "src/frontend/parser.ml"
     : (Ast.x * (Types.ty * (bool * int * bool))))

let _menhir_action_139 =
  fun _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt_ret ->
    (
# 255 "src/frontend/parser.mly"
        (
            let ef = mk_let_fun ~loc:(with_file (_startpos_f_,_endpos_e1_))
                                ~p_ty_opt
                                ~ty_opt_ret
                        e1
            in
            (P_var f,ef)
        )
# 3043 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_140 =
  fun _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt_ret ->
    (
# 255 "src/frontend/parser.mly"
        (
            let ef = mk_let_fun ~loc:(with_file (_startpos_f_,_endpos_e1_))
                                ~p_ty_opt
                                ~ty_opt_ret
                        e1
            in
            (P_var f,ef)
        )
# 3058 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_141 =
  fun _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt ->
    (
# 279 "src/frontend/parser.mly"
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
# 3081 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_142 =
  fun _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt ->
    (
# 279 "src/frontend/parser.mly"
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
# 3104 "src/frontend/parser.ml"
     : (Ast.p * Ast.e_static))

let _menhir_action_143 =
  fun e2 ->
    (
# 483 "src/frontend/parser.mly"
          ( e2,E_const Unit )
# 3112 "src/frontend/parser.ml"
     : (Ast.e_static * Ast.e_static))

let _menhir_action_144 =
  fun e2 e3 ->
    (
# 484 "src/frontend/parser.mly"
                       ( e2, e3 )
# 3120 "src/frontend/parser.ml"
     : (Ast.e_static * Ast.e_static))

let _menhir_action_145 =
  fun _endpos_e_ _startpos_e_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    let _loc = (_startpos, _endpos) in
    (
# 454 "src/frontend/parser.mly"
              ( mk_loc (with_file _loc) e )
# 3131 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_146 =
  fun e ->
    (
# 457 "src/frontend/parser.mly"
            ( e )
# 3139 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_147 =
  fun _endpos_e_ _startpos__1_ e f ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 459 "src/frontend/parser.mly"
        ( mk_fix f e (with_file _loc) )
# 3150 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_148 =
  fun e p_ty_opt ->
    (
# 461 "src/frontend/parser.mly"
        ( let (p,ty_p_opt) = p_ty_opt in
          mk_fun_ty_annot_p p ty_p_opt e )
# 3159 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_149 =
  fun e1 e2_e3 ->
    (
# 464 "src/frontend/parser.mly"
        ( let (e2,e3) = e2_e3 in E_if(e1,e2,e3) )
# 3167 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_150 =
  fun b e2 ->
    (
# 466 "src/frontend/parser.mly"
        ( let (p,e1) = b in
          E_letIn(p,Types.new_ty_unknown(),e1,e2) )
# 3176 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_151 =
  fun e1 es ->
    (
# 478 "src/frontend/parser.mly"
        (
            E_par(e1::es)
        )
# 3186 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_152 =
  fun e1 es ->
    (
# 478 "src/frontend/parser.mly"
        (
            E_par(e1::es)
        )
# 3196 "src/frontend/parser.ml"
     : (Ast.e))

let _menhir_action_153 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 3204 "src/frontend/parser.ml"
     : ((Ast.c list * Ast.e_static) list))

let _menhir_action_154 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 3212 "src/frontend/parser.ml"
     : ((Ast.c list * Ast.e_static) list))

let _menhir_action_155 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 3220 "src/frontend/parser.ml"
     : (Ast.e list))

let _menhir_action_156 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 3228 "src/frontend/parser.ml"
     : (Ast.e list))

let _menhir_action_157 =
  fun v ->
    (
# 409 "src/frontend/parser.mly"
           ( v )
# 3236 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_158 =
  fun e p_ty_opt ->
    (
# 411 "src/frontend/parser.mly"
      (
        let p,ty_opt = p_ty_opt in
        mk_fun_ty_annot p ty_opt e
    )
# 3247 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_159 =
  fun e p x ->
    (
# 713 "src/frontend/parser.mly"
                                      ( (x,(p,e)) )
# 3255 "src/frontend/parser.ml"
     : (Ast.x * (Ast.p * Ast.e_static)))

let _menhir_action_160 =
  fun cs e ->
    (
# 705 "src/frontend/parser.mly"
                                                                ( (cs,e) )
# 3263 "src/frontend/parser.ml"
     : (Ast.c list * Ast.e_static))

let _menhir_action_161 =
  fun e ->
    (
# 708 "src/frontend/parser.mly"
                                          ( [],Some e )
# 3271 "src/frontend/parser.ml"
     : ((Ast.x * (Ast.p * Ast.e_static)) list * Ast.e_static option))

let _menhir_action_162 =
  fun h ->
    (
# 709 "src/frontend/parser.mly"
                                          ( [h],None )
# 3279 "src/frontend/parser.ml"
     : ((Ast.x * (Ast.p * Ast.e_static)) list * Ast.e_static option))

let _menhir_action_163 =
  fun h rev_cases ->
    (
# 710 "src/frontend/parser.mly"
                                          ( let (hs,eo) = rev_cases in h::hs,eo )
# 3287 "src/frontend/parser.ml"
     : ((Ast.x * (Ast.p * Ast.e_static)) list * Ast.e_static option))

let _menhir_action_164 =
  fun x ->
    (
# 228 "<standard.mly>"
    ( [ x ] )
# 3295 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_165 =
  fun x xs ->
    (
# 231 "<standard.mly>"
    ( x :: xs )
# 3303 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_166 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 3311 "src/frontend/parser.ml"
     : (unit option))

let _menhir_action_167 =
  fun x ->
    (
# 114 "<standard.mly>"
    ( Some x )
# 3319 "src/frontend/parser.ml"
     : (unit option))

let _menhir_action_168 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 3327 "src/frontend/parser.ml"
     : (unit option))

let _menhir_action_169 =
  fun x ->
    (
# 114 "<standard.mly>"
    ( Some x )
# 3335 "src/frontend/parser.ml"
     : (unit option))

let _menhir_action_170 =
  fun p ->
    (
# 724 "src/frontend/parser.mly"
         ( p )
# 3343 "src/frontend/parser.ml"
     : (Ast.p))

let _menhir_action_171 =
  fun p ps ->
    (
# 726 "src/frontend/parser.mly"
  ( P_tuple (p::ps) )
# 3351 "src/frontend/parser.ml"
     : (Ast.p))

let _menhir_action_172 =
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
                        let v = E_fun(P_var arg,(Types.new_ty_unknown(),Types.new_tyB_unknown()), 
                             E_run(f,E_var arg)) in
                        (((f,t)::ecs,efs), gs,    ts,    (((P_var x,v),with_file _loc)::ds)) )
# 3368 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_173 =
  fun ef pi ->
    (
# 140 "src/frontend/parser.mly"
                      ( let (ecs,efs),gs,ts,ds = pi in ((ecs,ef::efs), gs,    ts,    ds   ) )
# 3379 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_174 =
  fun g pi ->
    (
# 141 "src/frontend/parser.mly"
                      ( let (ecs,efs),gs,ts,ds = pi in ((ecs,    efs), g::gs, ts,    ds   ) )
# 3390 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_175 =
  fun d pi ->
    (
# 142 "src/frontend/parser.mly"
                      ( let (ecs,efs),gs,ts,ds = pi in ((ecs,    efs), gs,    d::ts, ds   ) )
# 3401 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_176 =
  fun d pi ->
    (
# 143 "src/frontend/parser.mly"
                      ( let (ecs,efs),gs,ts,ds = pi in ((ecs,    efs), gs,    ts,    d::ds) )
# 3412 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_177 =
  fun pi ->
    (
# 144 "src/frontend/parser.mly"
                      ( pi )
# 3423 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_178 =
  fun () ->
    (
# 145 "src/frontend/parser.mly"
                      ( ([],[]),[],[],[] )
# 3434 "src/frontend/parser.ml"
     : (((Ast.x * (Types.ty * bool)) list *
   (Ast.x * (Types.ty * (bool * int * bool))) list) *
  (Ast.x * Ast.static) list * (Ast.x * (Ast.x * Types.tyB) list) list *
  ((Ast.p * Ast.e_static) * Prelude.loc) list))

let _menhir_action_179 =
  fun () ->
    (
# 487 "src/frontend/parser.mly"
     ( None )
# 3445 "src/frontend/parser.ml"
     : (Types.ty option))

let _menhir_action_180 =
  fun ty ->
    (
# 488 "src/frontend/parser.mly"
               ( Some ty )
# 3453 "src/frontend/parser.ml"
     : (Types.ty option))

let _menhir_action_181 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3461 "src/frontend/parser.ml"
     : (int list))

let _menhir_action_182 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3469 "src/frontend/parser.ml"
     : (int list))

let _menhir_action_183 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3477 "src/frontend/parser.ml"
     : (Ast.p list))

let _menhir_action_184 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3485 "src/frontend/parser.ml"
     : (Ast.p list))

let _menhir_action_185 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3493 "src/frontend/parser.ml"
     : (Ast.e list))

let _menhir_action_186 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3501 "src/frontend/parser.ml"
     : (Ast.e list))

let _menhir_action_187 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3509 "src/frontend/parser.ml"
     : (Ast.c list))

let _menhir_action_188 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3517 "src/frontend/parser.ml"
     : (Ast.c list))

let _menhir_action_189 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3525 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_190 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3533 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_191 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3541 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_192 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3549 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_193 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3557 "src/frontend/parser.ml"
     : (Types.size list))

let _menhir_action_194 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3565 "src/frontend/parser.ml"
     : (Types.size list))

let _menhir_action_195 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3573 "src/frontend/parser.ml"
     : (Types.tyB list))

let _menhir_action_196 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3581 "src/frontend/parser.ml"
     : (Types.tyB list))

let _menhir_action_197 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3589 "src/frontend/parser.ml"
     : (Ast.c list))

let _menhir_action_198 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3597 "src/frontend/parser.ml"
     : (Ast.c list))

let _menhir_action_199 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3605 "src/frontend/parser.ml"
     : ((Ast.x * Types.tyB) list))

let _menhir_action_200 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3613 "src/frontend/parser.ml"
     : ((Ast.x * Types.tyB) list))

let _menhir_action_201 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3621 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_202 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3629 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_203 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3637 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_204 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3645 "src/frontend/parser.ml"
     : (Ast.e_static list))

let _menhir_action_205 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3653 "src/frontend/parser.ml"
     : (Types.tyB list))

let _menhir_action_206 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3661 "src/frontend/parser.ml"
     : (Types.tyB list))

let _menhir_action_207 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 3669 "src/frontend/parser.ml"
     : (Types.ty list))

let _menhir_action_208 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 3677 "src/frontend/parser.ml"
     : (Types.ty list))

let _menhir_action_209 =
  fun n ->
    (
# 392 "src/frontend/parser.mly"
            ( Sz_lit n )
# 3685 "src/frontend/parser.ml"
     : (Types.size))

let _menhir_action_210 =
  fun x ->
    (
# 393 "src/frontend/parser.mly"
               (
    decl_size_var x
  )
# 3695 "src/frontend/parser.ml"
     : (Types.size))

let _menhir_action_211 =
  fun sz ->
    (
# 357 "src/frontend/parser.mly"
          ( [sz] )
# 3703 "src/frontend/parser.ml"
     : (Types.size list))

let _menhir_action_212 =
  fun szs ->
    (
# 358 "src/frontend/parser.mly"
                                                ( szs )
# 3711 "src/frontend/parser.ml"
     : (Types.size list))

let _menhir_action_213 =
  fun _endpos__7_ _startpos__1_ e ec x ->
    let _endpos = _endpos__7_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 170 "src/frontend/parser.mly"
    ( let to_int e =
        match un_annot e with
        | E_const (Int(n,_)) -> n
        | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                          ~msg:("dimension for "^x^" should be an integer") ()
      in
      x,Static_array(e2c ec,to_int e)
  )
# 3729 "src/frontend/parser.ml"
     : (Ast.x * Ast.static))

let _menhir_action_214 =
  fun _endpos__6_ _startpos__1_ ty x ->
    let _endpos = _endpos__6_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 180 "src/frontend/parser.mly"
    (
      let loc = with_file _loc in
      if Types.no_unknown_in_ty ty then (
         Prelude.Errors.raise_error ~loc
           ~msg:"this type annotation should not contain type unknowns"
      ) ();
      x,Static_array_of (ty,loc)
    )
# 3747 "src/frontend/parser.ml"
     : (Ast.x * Ast.static))

let _menhir_action_215 =
  fun e ->
    (
# 193 "src/frontend/parser.mly"
                           ( e )
# 3755 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_216 =
  fun ty tys ->
    (
# 367 "src/frontend/parser.mly"
    ( Ty_tuple (ty::tys) )
# 3763 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_217 =
  fun ty ->
    (
# 368 "src/frontend/parser.mly"
             (ty)
# 3771 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_218 =
  fun sz ->
    (
# 324 "src/frontend/parser.mly"
                ( TyB_size(sz) )
# 3779 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_219 =
  fun tyB tyBs ->
    (
# 326 "src/frontend/parser.mly"
    ( TyB_tuple (tyB::tyBs) )
# 3787 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_220 =
  fun tyB ->
    (
# 327 "src/frontend/parser.mly"
               (tyB)
# 3795 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_221 =
  fun x ->
    (
# 308 "src/frontend/parser.mly"
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
# 3812 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_222 =
  fun x ->
    (
# 330 "src/frontend/parser.mly"
                  ( decl_tyB_var x )
# 3820 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_223 =
  fun tyB ->
    (
# 331 "src/frontend/parser.mly"
                ( tyB )
# 3828 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_224 =
  fun tyB ->
    (
# 332 "src/frontend/parser.mly"
                        ( tyB )
# 3836 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_225 =
  fun _endpos_szs_ _startpos_x_ szs x ->
    let _endpos = _endpos_szs_ in
    let _startpos = _startpos_x_ in
    let _loc = (_startpos, _endpos) in
    (
# 334 "src/frontend/parser.mly"
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
# 3856 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_226 =
  fun _endpos_szs_ _startpos_tyB_ szs tyB x ->
    let tyBs = 
# 361 "src/frontend/parser.mly"
               ( [tyB] )
# 3864 "src/frontend/parser.ml"
     in
    let _startpos_tyBs_ = _startpos_tyB_ in
    let _endpos = _endpos_szs_ in
    let _startpos = _startpos_tyBs_ in
    let _loc = (_startpos, _endpos) in
    (
# 345 "src/frontend/parser.mly"
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
# 3882 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_227 =
  fun _endpos_szs_ _startpos__1_ szs tyBs x ->
    let tyBs = 
# 362 "src/frontend/parser.mly"
                                                        ( tyBs )
# 3890 "src/frontend/parser.ml"
     in
    let _startpos_tyBs_ = _startpos__1_ in
    let _endpos = _endpos_szs_ in
    let _startpos = _startpos_tyBs_ in
    let _loc = (_startpos, _endpos) in
    (
# 345 "src/frontend/parser.mly"
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
# 3908 "src/frontend/parser.ml"
     : (Types.tyB))

let _menhir_action_228 =
  fun x ->
    (
# 301 "src/frontend/parser.mly"
        ( x,None )
# 3916 "src/frontend/parser.ml"
     : (Types.x * Types.ty option))

let _menhir_action_229 =
  fun ty x ->
    (
# 303 "src/frontend/parser.mly"
        ( x,Some ty )
# 3924 "src/frontend/parser.ml"
     : (Types.x * Types.ty option))

let _menhir_action_230 =
  fun x_ty_opt ->
    (
# 305 "src/frontend/parser.mly"
        ( x_ty_opt )
# 3932 "src/frontend/parser.ml"
     : (Types.x * Types.ty option))

let _menhir_action_231 =
  fun x ->
    (
# 301 "src/frontend/parser.mly"
        ( x,None )
# 3940 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_232 =
  fun ty x ->
    (
# 303 "src/frontend/parser.mly"
        ( x,Some ty )
# 3948 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_233 =
  fun x_ty_opt ->
    (
# 305 "src/frontend/parser.mly"
        ( x_ty_opt )
# 3956 "src/frontend/parser.ml"
     : (Ast.p * Types.ty option))

let _menhir_action_234 =
  fun tyB x ->
    (
# 249 "src/frontend/parser.mly"
                        ( x,tyB )
# 3964 "src/frontend/parser.ml"
     : (Ast.x * Types.tyB))

let _menhir_action_235 =
  fun x ->
    (
# 372 "src/frontend/parser.mly"
               ( decl_ty_var x )
# 3972 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_236 =
  fun _endpos__5_ _startpos_ty_tyB_ sz ty_tyB ->
    let _endpos = _endpos__5_ in
    let _startpos = _startpos_ty_tyB_ in
    let _loc = (_startpos, _endpos) in
    (
# 374 "src/frontend/parser.mly"
    ( let tyB = as_tyB ~loc:(with_file _loc) ty_tyB in
      Ty_array(sz,tyB) )
# 3984 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_237 =
  fun ty tyB ->
    (
# 385 "src/frontend/parser.mly"
                                 ( Ty_fun(ty,Dur_one,tyB) )
# 3992 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_238 =
  fun ty tyB ->
    (
# 386 "src/frontend/parser.mly"
                                               ( Ty_fun(ty,new_dur_unknown(),tyB) )
# 4000 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_239 =
  fun ty tyB ->
    (
# 387 "src/frontend/parser.mly"
                           ( Ty_fun(ty,Dur_zero,tyB) )
# 4008 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_240 =
  fun tyB ->
    (
# 388 "src/frontend/parser.mly"
               ( Ty_base tyB )
# 4016 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_241 =
  fun ty ->
    (
# 389 "src/frontend/parser.mly"
                      ( ty )
# 4024 "src/frontend/parser.ml"
     : (Types.ty))

let _menhir_action_242 =
  fun _endpos__5_ _startpos__1_ ts x ->
    let _endpos = _endpos__5_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 244 "src/frontend/parser.mly"
   ( add_alias x (TyB_sum ts) (with_file _loc);
     clear_tyvar_constraints();
     x,ts )
# 4037 "src/frontend/parser.ml"
     : (Ast.x * (Ast.x * Types.tyB) list))

let _menhir_action_243 =
  fun _endpos__5_ _startpos__1_ tyB x ->
    let _endpos = _endpos__5_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 219 "src/frontend/parser.mly"
                                     ( add_alias x tyB (with_file _loc) )
# 4048 "src/frontend/parser.ml"
     : (unit))

let _menhir_action_244 =
  fun _endpos__1_inlined1_ _startpos__1_ tyB ->
    let r = 
# 236 "src/frontend/parser.mly"
             ( None )
# 4056 "src/frontend/parser.ml"
     in
    let _endpos_r_ = _endpos__1_inlined1_ in
    let _endpos = _endpos_r_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 221 "src/frontend/parser.mly"
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
# 4078 "src/frontend/parser.ml"
     : (unit))

let _menhir_action_245 =
  fun _endpos__3_ _startpos__1_ op tyB ->
    let r = 
# 237 "src/frontend/parser.mly"
                         ( Some (op,[]) )
# 4086 "src/frontend/parser.ml"
     in
    let _endpos_r_ = _endpos__3_ in
    let _endpos = _endpos_r_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 221 "src/frontend/parser.mly"
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
# 4108 "src/frontend/parser.ml"
     : (unit))

let _menhir_action_246 =
  fun _endpos__5_ _startpos__1_ intl op tyB ->
    let r = 
# 239 "src/frontend/parser.mly"
   ( Some (op,intl) )
# 4116 "src/frontend/parser.ml"
     in
    let _endpos_r_ = _endpos__5_ in
    let _endpos = _endpos_r_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    (
# 221 "src/frontend/parser.mly"
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
# 4138 "src/frontend/parser.ml"
     : (unit))

let _menhir_action_247 =
  fun _endpos_v_ _startpos_v_ v ->
    let _endpos = _endpos_v_ in
    let _startpos = _startpos_v_ in
    let _loc = (_startpos, _endpos) in
    (
# 401 "src/frontend/parser.mly"
               ( mk_loc (with_file _loc) v )
# 4149 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_248 =
  fun e es ->
    (
# 405 "src/frontend/parser.mly"
    ( E_tuple (e::es) )
# 4157 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_249 =
  fun e ->
    (
# 406 "src/frontend/parser.mly"
           ( e )
# 4165 "src/frontend/parser.ml"
     : (Ast.e_static))

let _menhir_action_250 =
  fun _endpos_e_ _startpos_x_ e x ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos_x_ in
    let _loc = (_startpos, _endpos) in
    (
# 717 "src/frontend/parser.mly"
        ( match x with
          | "_" -> e
          | _ -> Prelude.Errors.raise_error ~loc:(with_file _loc)
                  ~msg:"the wildcard_case should be named \"_\"" () )
# 4179 "src/frontend/parser.ml"
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
  
  let _menhir_run_561 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_pi =
    fun _menhir_stack _v ->
      MenhirBox_pi _v
  
  let rec _menhir_goto_pi : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _endpos _v _menhir_s ->
      match _menhir_s with
      | MenhirState458 ->
          _menhir_run_561 _menhir_stack _v
      | MenhirState546 ->
          _menhir_run_560 _menhir_stack _endpos _v
      | MenhirState547 ->
          _menhir_run_559 _menhir_stack _endpos _v
      | MenhirState557 ->
          _menhir_run_558 _menhir_stack _endpos _v
      | MenhirState552 ->
          _menhir_run_553 _menhir_stack _endpos _v
      | MenhirState550 ->
          _menhir_run_551 _menhir_stack _endpos _v
      | MenhirState548 ->
          _menhir_run_549 _menhir_stack _endpos _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_560 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_type_alias -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _endpos _v ->
      let MenhirCell1_type_alias (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let (_endpos_pi_, pi) = (_endpos, _v) in
      let _v = _menhir_action_177 pi in
      _menhir_goto_pi _menhir_stack _endpos_pi_ _v _menhir_s
  
  and _menhir_run_559 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_typ_sum -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _endpos _v ->
      let MenhirCell1_typ_sum (_menhir_stack, _menhir_s, d) = _menhir_stack in
      let (_endpos_pi_, pi) = (_endpos, _v) in
      let _v = _menhir_action_175 d pi in
      _menhir_goto_pi _menhir_stack _endpos_pi_ _v _menhir_s
  
  and _menhir_run_558 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_decl -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _endpos _v ->
      let MenhirCell1_decl (_menhir_stack, _menhir_s, d) = _menhir_stack in
      let (_endpos_pi_, pi) = (_endpos, _v) in
      let _v = _menhir_action_176 d pi in
      _menhir_goto_pi _menhir_stack _endpos_pi_ _v _menhir_s
  
  and _menhir_run_553 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_ext_circ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _endpos _v ->
      let MenhirCell1_ext_circ (_menhir_stack, _menhir_s, ec, _startpos_ec_) = _menhir_stack in
      let (_endpos_pi_, pi) = (_endpos, _v) in
      let _v = _menhir_action_172 _endpos_pi_ _startpos_ec_ ec pi in
      _menhir_goto_pi _menhir_stack _endpos_pi_ _v _menhir_s
  
  and _menhir_run_551 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_ext_fun -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _endpos _v ->
      let MenhirCell1_ext_fun (_menhir_stack, _menhir_s, ef) = _menhir_stack in
      let (_endpos_pi_, pi) = (_endpos, _v) in
      let _v = _menhir_action_173 ef pi in
      _menhir_goto_pi _menhir_stack _endpos_pi_ _v _menhir_s
  
  and _menhir_run_549 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_static -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _endpos _v ->
      let MenhirCell1_static (_menhir_stack, _menhir_s, g) = _menhir_stack in
      let (_endpos_pi_, pi) = (_endpos, _v) in
      let _v = _menhir_action_174 g pi in
      _menhir_goto_pi _menhir_stack _endpos_pi_ _v _menhir_s
  
  let _menhir_run_545 : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_s ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _endpos__1_ = _endpos in
      let _v = _menhir_action_178 () in
      _menhir_goto_pi _menhir_stack _endpos__1_ _v _menhir_s
  
  let _menhir_run_456 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_exp_eof =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let e = _v in
          let _v = _menhir_action_132 e in
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
      let _v = _menhir_action_210 x in
      _menhir_goto_size _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _v _menhir_s _tok
  
  and _menhir_goto_size : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState229 ->
          _menhir_run_230 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState222 ->
          _menhir_run_223 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState216 ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState159 ->
          _menhir_run_160 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState460 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState150 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState131 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState117 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState024 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState002 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_230 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ARRAY_CREATE -> _ -> _ -> _ -> _ -> ttv_result =
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
                  let _v = _menhir_action_032 _endpos__6_ _startpos__1_ sz in
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
      | MenhirState452 ->
          _menhir_run_451 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState017 ->
          _menhir_run_451 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState334 ->
          _menhir_run_335 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState326 ->
          _menhir_run_327 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState320 ->
          _menhir_run_321 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState286 ->
          _menhir_run_287 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState284 ->
          _menhir_run_285 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState282 ->
          _menhir_run_283 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState280 ->
          _menhir_run_281 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState278 ->
          _menhir_run_279 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState276 ->
          _menhir_run_277 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState274 ->
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState272 ->
          _menhir_run_273 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState270 ->
          _menhir_run_271 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState268 ->
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState266 ->
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState264 ->
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState262 ->
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState260 ->
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState258 ->
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState256 ->
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState254 ->
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState252 ->
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState250 ->
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState248 ->
          _menhir_run_249 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState246 ->
          _menhir_run_247 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState244 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState241 ->
          _menhir_run_242 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState458 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState546 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState547 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState548 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState550 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState552 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState557 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState532 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState527 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState523 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState020 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState441 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState443 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState030 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState430 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState414 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState410 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState407 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState395 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState397 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState383 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState387 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState377 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState381 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState374 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState372 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState361 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState357 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState346 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState193 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState196 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState337 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState339 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState198 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState211 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState213 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState214 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState318 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState312 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState315 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState294 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState236 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_451 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_266 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_268 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_272 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_274 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_276 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_278 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_280 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_282 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState452 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP_AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_284 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_286 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PIPE_RBRACKET ->
          let x = _v in
          let _v = _menhir_action_185 x in
          _menhir_goto_separated_nonempty_list_COMMA_app_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_241 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
      let _v = _menhir_action_102 x in
      _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _startpos_x_ _v _menhir_s _tok
  
  and _menhir_goto_const : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState404 ->
          _menhir_run_421 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState422 ->
          _menhir_run_421 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState419 ->
          _menhir_run_421 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_305 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState290 ->
          _menhir_run_305 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_305 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_305 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState303 ->
          _menhir_run_305 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState458 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState546 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState547 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState548 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState550 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState552 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState557 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState532 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState527 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState523 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState513 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState511 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState007 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState452 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState017 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState020 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState022 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState441 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState443 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState030 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState438 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState435 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState031 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState430 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState414 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState410 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState407 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState395 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState397 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState383 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState387 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState377 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState381 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState374 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState372 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState361 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState357 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState346 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState193 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState196 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState337 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState339 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState334 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState198 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState326 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState211 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState213 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState214 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState318 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState320 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState312 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState315 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState311 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState289 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState294 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState243 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState286 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState284 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState282 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState280 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState278 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState276 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState274 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState272 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState270 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState268 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState266 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState264 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState262 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState260 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState258 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState256 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState254 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState252 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState250 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState248 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState246 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState244 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState241 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState236 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState226 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState224 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState194 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState102 ->
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
  
  and _menhir_run_421 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PIPE ->
          let _menhir_stack = MenhirCell1_const (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState422 in
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
          let _v = _menhir_action_197 x in
          _menhir_goto_separated_nonempty_list_PIPE_const_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_016 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_s_, _startpos_s_, s) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_100 s in
      _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_s_ _startpos_s_ _v _menhir_s _tok
  
  and _menhir_run_032 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_x_, _startpos_x_, x) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_101 x in
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
          let _v = _menhir_action_116 () in
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
          let _v = _menhir_action_105 () in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_040 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__2_ = _endpos in
      let _v = _menhir_action_096 () in
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
          let _v = _menhir_action_103 () in
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
          let _v = _menhir_action_114 () in
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
          let _v = _menhir_action_107 () in
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
      let _v = _menhir_action_104 () in
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
          let _v = _menhir_action_117 () in
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
          let _v = _menhir_action_108 () in
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
          let _v = _menhir_action_121 () in
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
          let _v = _menhir_action_120 () in
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
          let _v = _menhir_action_119 () in
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
          let _v = _menhir_action_110 () in
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
          let _v = _menhir_action_118 () in
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
          let _v = _menhir_action_109 () in
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
          let _v = _menhir_action_111 () in
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
          let _v = _menhir_action_113 () in
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
          let _v = _menhir_action_112 () in
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
          let _v = _menhir_action_106 () in
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
          let _v = _menhir_action_122 () in
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
          let _v = _menhir_action_115 () in
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
              let _v = _menhir_action_099 _endpos_k_ _startpos_n_ k n in
              _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_k_ _startpos_n_ _v _menhir_s _tok
          | _ ->
              _eRR ())
      | AMP | AMP_AMP | AND | ASR | AT | AT_AT | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | HAT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LAND | LCUR | LE | LOR | LPAREN | LSL | LSR | LT | LXOR | MATCH | MINUS | MOD | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RCUR | RESET | RESIZE_INT | RIGHT_ARROW | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TIMES | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH | XOR ->
          let (_endpos_n_, _startpos_n_, n) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_098 n in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_n_ _startpos_n_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_083 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_b_, _startpos_b_, b) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_097 b in
      _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_b_ _startpos_b_ _v _menhir_s _tok
  
  and _menhir_goto_separated_nonempty_list_PIPE_const_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState422 ->
          _menhir_run_423 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState419 ->
          _menhir_run_413 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState404 ->
          _menhir_run_413 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_423 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_const -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_const (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_198 x xs in
      _menhir_goto_separated_nonempty_list_PIPE_const_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_413 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_separated_nonempty_list_PIPE_const_ (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState414 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
      | MenhirState513 ->
          _menhir_run_514 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState511 ->
          _menhir_run_512 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState007 ->
          _menhir_run_454 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState022 ->
          _menhir_run_446 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState438 ->
          _menhir_run_439 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState435 ->
          _menhir_run_436 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState031 ->
          _menhir_run_433 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState034 ->
          _menhir_run_400 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState047 ->
          _menhir_run_400 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState311 ->
          _menhir_run_311 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState289 ->
          _menhir_run_311 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState243 ->
          _menhir_run_289 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState458 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState546 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState547 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState548 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState550 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState552 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState557 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState532 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState527 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState523 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState017 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState452 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState020 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState441 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState443 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState030 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState430 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState414 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState410 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState407 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState395 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState397 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState383 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState387 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState377 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState381 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState374 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState372 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState361 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState357 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState346 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState193 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState196 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState337 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState339 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState334 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState198 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState326 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState211 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState213 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState214 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState318 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState320 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState312 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState315 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState236 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState294 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState244 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState266 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState284 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState286 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState282 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState280 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState278 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState276 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState274 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState272 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState268 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState270 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState258 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState260 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState262 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState264 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState246 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState256 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState254 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState252 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState250 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState248 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState241 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState226 ->
          _menhir_run_227 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState224 ->
          _menhir_run_225 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState194 ->
          _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState103 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState102 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_514 : type  ttv_stack. (((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_STATIC _menhir_cell0_IDENT, _menhir_box_pi) _menhir_cell1_aexp -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_215 e in
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_aexp (_menhir_stack, _, ec, _, _) = _menhir_stack in
          let MenhirCell0_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
          let MenhirCell1_STATIC (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (e, _endpos__7_) = (_v, _endpos) in
          let _v = _menhir_action_213 _endpos__7_ _startpos__1_ e ec x in
          _menhir_goto_static _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_static : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_static (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState548
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | TYPE ->
          _menhir_run_459 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState548
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | SHARED ->
          _menhir_run_486 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState548
      | OPERATOR ->
          _menhir_run_492 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | LET ->
          _menhir_run_508 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState548
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | IDENT _v_4 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState548
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | EXTERNAL ->
          _menhir_run_540 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | EOF ->
          _menhir_run_545 _menhir_stack _menhir_lexbuf MenhirState548
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState548
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState548
      | _ ->
          _eRR ()
  
  and _menhir_run_459 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_TYPE (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState459
      | LT ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState459
      | LPAREN ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState459
      | IDENT _v ->
          let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TVAR_IDENT _v_1 ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState459, _v, _startpos_0, _endpos) in
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState460
          | LT ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState459, _v, _startpos_0, _endpos) in
              _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState460
          | INT_LIT _v_2 ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState459, _v, _startpos_0, _endpos) in
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState460
          | EQ ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState459, _v, _startpos_0, _endpos) in
              let _menhir_stack = MenhirCell1_EQ (_menhir_stack, MenhirState460) in
              let _menhir_s = MenhirState461 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | UP_IDENT _v ->
                  _menhir_run_462 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TYB_VAR_IDENT _v ->
                  _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | LT ->
                  _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | AT | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | REF | REGISTER | RESIZE_INT | RUN | SEMI_SEMI | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TIMES | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
              let (_menhir_s, _startpos_x_, x) = (MenhirState459, _startpos_0, _v) in
              let _v = _menhir_action_221 x in
              _menhir_goto_tyB_ident _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_x_ _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_111 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_startpos_x_, x) = (_startpos, _v) in
      let _v = _menhir_action_222 x in
      _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_x_ _v _menhir_s _tok
  
  and _menhir_goto_tyB_next : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState113 ->
          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState459 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState461 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState463 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState154 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState135 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState145 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState542 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState517 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState502 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState495 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState489 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState391 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState353 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState205 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState185 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState127 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState129 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_163 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_138 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | COMMA | RPAREN ->
          let tyB = _v in
          let _v = _menhir_action_220 tyB in
          _menhir_goto_tyB _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | ARRAY | IMPLY | QUESTION_MARK | RIGHT_ARROW ->
          let (_startpos_tyB_, tyB) = (_startpos, _v) in
          let _v = _menhir_action_240 tyB in
          _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_tyB_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_138 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB_next -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState138 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_136 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState136 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LT ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_114 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState114 in
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
      let _v = _menhir_action_209 n in
      _menhir_goto_size _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_n_ _v _menhir_s _tok
  
  and _menhir_run_117 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TVAR_IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState117
      | LT ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState117
      | INT_LIT _v_1 ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState117
      | ARRAY | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | AT | BANG | BOOL_LIT _ | COMMA | CREATE | EOF | EQ | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | IMPLY | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | PIPE | QUESTION_MARK | REF | REGISTER | RESIZE_INT | RIGHT_ARROW | RPAREN | RUN | SEMI_SEMI | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TIMES | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let (_startpos_x_, x) = (_startpos, _v) in
          let _v = _menhir_action_221 x in
          _menhir_goto_tyB_ident _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_x_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_118 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState118 in
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
      let _v = _menhir_action_223 tyB in
      _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_tyB_ _v _menhir_s _tok
  
  and _menhir_run_131 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB_next -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v, _startpos, _endpos) in
      let _menhir_s = MenhirState131 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TVAR_IDENT _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LT ->
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_tyB : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState459 ->
          _menhir_run_473 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState461 ->
          _menhir_run_468 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState463 ->
          _menhir_run_464 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState156 ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState154 ->
          _menhir_run_155 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState135 ->
          _menhir_run_152 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState145 ->
          _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState113 ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_473 : type  ttv_stack. ((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_tyB (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          _menhir_run_469 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState473
      | IDENT _v_0 ->
          let (_v, _menhir_s) = (_v_0, MenhirState473) in
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACKET ->
              let _menhir_s = MenhirState475 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | INT_LIT _v ->
                  _menhir_run_476 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | AT ->
          let _menhir_stack = MenhirCell1_AT (_menhir_stack, MenhirState473) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v_2 ->
              let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v_2, _startpos, _endpos) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | SEMI_SEMI ->
                  _menhir_run_469 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState483
              | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | INT_LIT _ | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | REF | REGISTER | RESIZE_INT | RUN | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
                  let _ = _menhir_action_168 () in
                  _menhir_run_484 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IF | INT_LIT _ | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | REF | REGISTER | RESIZE_INT | RUN | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let _ = _menhir_action_168 () in
          _menhir_run_485 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_469 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_x_, x) = (_endpos, ()) in
      let _ = _menhir_action_169 x in
      _menhir_goto_option_SEMI_SEMI_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _menhir_s _tok
  
  and _menhir_goto_option_SEMI_SEMI_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _menhir_s _tok ->
      match _menhir_s with
      | MenhirState473 ->
          _menhir_run_485 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | MenhirState483 ->
          _menhir_run_484 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | MenhirState480 ->
          _menhir_run_481 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | MenhirState471 ->
          _menhir_run_472 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | MenhirState468 ->
          _menhir_run_470 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_485 : type  ttv_stack. ((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_tyB -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok ->
      let MenhirCell1_tyB (_menhir_stack, _, tyB) = _menhir_stack in
      let MenhirCell1_TYPE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__1_inlined1_ = _endpos in
      let _v = _menhir_action_244 _endpos__1_inlined1_ _startpos__1_ tyB in
      _menhir_goto_type_alias _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_type_alias : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_alias (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState546
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | TYPE ->
          _menhir_run_459 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState546
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | SHARED ->
          _menhir_run_486 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState546
      | OPERATOR ->
          _menhir_run_492 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | LET ->
          _menhir_run_508 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState546
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | IDENT _v_4 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState546
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | EXTERNAL ->
          _menhir_run_540 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | EOF ->
          _menhir_run_545 _menhir_stack _menhir_lexbuf MenhirState546
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState546
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState546
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
          let _v = _menhir_action_047 n in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PIPE_RBRACKET ->
          let _v = _menhir_action_155 () in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
              | PARFOR ->
                  _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPERATOR_IDENT _v ->
                  _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | MINUS ->
                  _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MATCH ->
                  _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_GENERATE ->
                  _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LET ->
                  _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LENGTH ->
                  _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LCUR ->
                  _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_OF_TUPLE ->
                  _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_LIT _v ->
                  _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IF ->
                  _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FUN ->
                  _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FOR ->
                  _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FIX ->
                  _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | EXEC ->
                  _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CREATE ->
                  _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL_LIT _v ->
                  _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_MAKE ->
                  _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_LENGTH ->
                  _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_CREATE ->
                  _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState030
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | LET ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState030
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | IDENT _v ->
          let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACKET ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState030, _v, _startpos_0, _endpos) in
              _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer
          | INIT ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState030, _v, _startpos_0, _endpos) in
              let _menhir_s = MenhirState435 in
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
              _menhir_run_328 _menhir_stack _menhir_lexbuf _menhir_lexer
          | DOT ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState030, _v, _startpos_0, _endpos) in
              _menhir_run_329 _menhir_stack _menhir_lexbuf _menhir_lexer
          | AMP | AMP_AMP | ASR | BANG | BOOL_LIT _ | COL_EQ | COMMA | DIV | EQ | EQ_EQ | FOR | GE | GT | IDENT _ | INT_LIT _ | INT_OF_TUPLE | LAND | LCUR | LE | LOR | LPAREN | LSL | LSR | LT | LXOR | MATCH | MINUS | MOD | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PLUS | RESIZE_INT | SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | TIMES | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | XOR ->
              let (_menhir_s, _endpos_x_, _startpos_x_, x) = (MenhirState030, _endpos, _startpos_0, _v) in
              let _v = _menhir_action_013 _endpos_x_ _startpos_x_ x in
              _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _startpos_x_ _v _menhir_s _tok
          | _ ->
              _eRR ())
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState030
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_034 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s, _startpos) in
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
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LXOR ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LOR ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LET ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | GT ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EQ_EQ ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASR ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
              | PARFOR ->
                  _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPERATOR_IDENT _v ->
                  _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | MINUS ->
                  _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MATCH ->
                  _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_GENERATE ->
                  _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LET ->
                  _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LENGTH ->
                  _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LCUR ->
                  _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_OF_TUPLE ->
                  _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_LIT _v ->
                  _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IF ->
                  _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FUN ->
                  _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FOR ->
                  _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FIX ->
                  _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | EXEC ->
                  _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CREATE ->
                  _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL_LIT _v ->
                  _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_MAKE ->
                  _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_LENGTH ->
                  _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_CREATE ->
                  _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_097 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_MACRO_GENERATE (_menhir_stack, _menhir_s, _startpos) in
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
  
  and _menhir_run_105 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState105 in
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
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LXOR ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LOR ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LET ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | GT ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EQ_EQ ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ASR ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_106 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | REC ->
          let _menhir_stack = MenhirCell1_REC (_menhir_stack, MenhirState106) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState107
          | IDENT _v ->
              let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LPAREN ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState107, _v, _startpos_0, _endpos) in
                  _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState169
              | IDENT _v_1 ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState107, _v, _startpos_0, _endpos) in
                  _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState169
              | COL ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState107, _v, _startpos_0, _endpos) in
                  _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState169
              | EQ ->
                  let _v =
                    let x = _v in
                    _menhir_action_228 x
                  in
                  _menhir_run_345 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_0 _v MenhirState107 _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | LPAREN ->
          _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState106
      | IDENT _v ->
          let _startpos_2 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState106, _v, _startpos_2, _endpos) in
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState355
          | IDENT _v_3 ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState106, _v, _startpos_2, _endpos) in
              _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState355
          | COL | EQ ->
              let _v =
                let x = _v in
                _menhir_action_029 x
              in
              _menhir_run_371 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState106 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_108 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | IDENT _v ->
          let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COL ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState108, _v, _startpos_0, _endpos) in
              _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
          | RPAREN ->
              let _v =
                let x = _v in
                _menhir_action_228 x
              in
              _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_110 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_COL (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState110 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TVAR_IDENT _v ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_112 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_startpos_x_, x) = (_startpos, _v) in
      let _v = _menhir_action_235 x in
      _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_x_ _v _menhir_s _tok
  
  and _menhir_goto_ty_next : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState129 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState127 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState542 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState517 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState502 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState495 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState489 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState391 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState353 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState205 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState185 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState113 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_128 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_ty_next as 'stack) -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          let _menhir_s = MenhirState129 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVAR_IDENT _v ->
              _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RIGHT_ARROW ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer
      | QUESTION_MARK ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IMPLY ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_156 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARRAY ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_158 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AT | EQ | RPAREN | SEMI_SEMI ->
          let x = _v in
          let _v = _menhir_action_207 x in
          _menhir_goto_separated_nonempty_list_TIMES_ty_next_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_113 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState113 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TVAR_IDENT _v ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LT ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_135 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState135 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LT ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_153 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW ->
          let _menhir_s = MenhirState154 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LT ->
              _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_156 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState156 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LT ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_158 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LT ->
          let _menhir_s = MenhirState159 in
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
      | MenhirState127 ->
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState129 ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_162 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty_next (_menhir_stack, _menhir_s, ty, _) = _menhir_stack in
      let tys = _v in
      let _v = _menhir_action_216 ty tys in
      _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_ty : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState542 ->
          _menhir_run_543 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState517 ->
          _menhir_run_518 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState502 ->
          _menhir_run_503 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState495 ->
          _menhir_run_496 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState489 ->
          _menhir_run_490 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState391 ->
          _menhir_run_392 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState353 ->
          _menhir_run_354 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState205 ->
          _menhir_run_206 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState190 ->
          _menhir_run_191 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState185 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState110 ->
          _menhir_run_166 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState113 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_543 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_EXTERNAL _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
          let MenhirCell1_EXTERNAL (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_133 t x in
          _menhir_goto_ext_circ _menhir_stack _menhir_lexbuf _menhir_lexer _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_ext_circ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ext_circ (_menhir_stack, _menhir_s, _v, _startpos) in
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
          _menhir_run_459 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState552
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | SHARED ->
          _menhir_run_486 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState552
      | OPERATOR ->
          _menhir_run_492 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | LET ->
          _menhir_run_508 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState552
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | IDENT _v_4 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState552
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | EXTERNAL ->
          _menhir_run_540 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | EOF ->
          _menhir_run_545 _menhir_stack _menhir_lexbuf MenhirState552
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState552
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState552
      | _ ->
          _eRR ()
  
  and _menhir_run_486 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
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
                  let _menhir_s = MenhirState489 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | TYB_VAR_IDENT _v ->
                      _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | TVAR_IDENT _v ->
                      _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | LPAREN ->
                      _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IDENT _v ->
                      _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_492 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
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
                  let _menhir_s = MenhirState495 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | TYB_VAR_IDENT _v ->
                      _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | TVAR_IDENT _v ->
                      _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | LPAREN ->
                      _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IDENT _v ->
                      _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
              let _menhir_s = MenhirState502 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TYB_VAR_IDENT _v ->
                  _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TVAR_IDENT _v ->
                  _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | LPAREN ->
                  _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_508 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STATIC ->
          let _menhir_stack = MenhirCell1_STATIC (_menhir_stack, MenhirState508) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v, _startpos, _endpos) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | EQ ->
                  let _menhir_s = MenhirState511 in
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
                  let _menhir_s = MenhirState517 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | TYB_VAR_IDENT _v ->
                      _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | TVAR_IDENT _v ->
                      _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | LPAREN ->
                      _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IDENT _v ->
                      _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | REC ->
          let _menhir_stack = MenhirCell1_REC (_menhir_stack, MenhirState508) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState520
          | IDENT _v ->
              let _startpos_10 = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LPAREN ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState520, _v, _startpos_10, _endpos) in
                  _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState521
              | IDENT _v_11 ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState520, _v, _startpos_10, _endpos) in
                  _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v_11 MenhirState521
              | COL ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState520, _v, _startpos_10, _endpos) in
                  _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState521
              | EQ ->
                  let _v =
                    let x = _v in
                    _menhir_action_228 x
                  in
                  _menhir_run_526 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_10 _v MenhirState520 _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | LPAREN ->
          _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState508
      | IDENT _v ->
          let _startpos_12 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState508, _v, _startpos_12, _endpos) in
              _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState530
          | IDENT _v_13 ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState508, _v, _startpos_12, _endpos) in
              _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v_13 MenhirState530
          | COL | EQ ->
              let _v =
                let x = _v in
                _menhir_action_029 x
              in
              _menhir_run_371 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState508 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_170 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState170 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_171 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _v = _menhir_action_027 () in
      _menhir_goto_apat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_apat : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState426 ->
          _menhir_run_406 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState405 ->
          _menhir_run_406 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState508 ->
          _menhir_run_371 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_371 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState369 ->
          _menhir_run_371 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState349 ->
          _menhir_run_352 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState530 ->
          _menhir_run_344 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState521 ->
          _menhir_run_344 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState355 ->
          _menhir_run_344 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState169 ->
          _menhir_run_344 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState199 ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState292 ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState200 ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState170 ->
          _menhir_run_184 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState180 ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState177 ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState172 ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_406 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_UP_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW ->
          let _menhir_s = MenhirState407 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_194 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LENGTH (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState194 in
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
  
  and _menhir_run_196 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState196 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_197 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT_LENGTH ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_328 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_329 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | ASR | BANG | BOOL_LIT _ | COL | COL_EQ | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | FOR | GE | GT | IDENT _ | IN | INIT | INT_LIT _ | INT_OF_TUPLE | LAND | LCUR | LE | LOR | LPAREN | LSL | LSR | LT | LXOR | MATCH | MINUS | MOD | NEQ | OPERATOR_IDENT _ | OR | PARFOR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RESIZE_INT | RPAREN | SEMI | SEMI_SEMI | SHARP_PIPE_LBRACKET | STRING_LIT _ | THEN | TIMES | TO | TUPLE_OF_INT | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE | WITH | XOR ->
          let (_endpos_x_, _startpos_x_, x) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_013 _endpos_x_ _startpos_x_ x in
          _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _startpos_x_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_198 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_IDENT -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState198 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_199 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_FUN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState199 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_200 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_200 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState200 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_172 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState172 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_173 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let x = _v in
      let _v = _menhir_action_029 x in
      _menhir_goto_apat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_212 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_FIX (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState213 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_214 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_EXEC (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState214 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_215 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_CREATE (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LT ->
          let _menhir_s = MenhirState216 in
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
  
  and _menhir_run_221 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_ARRAY_MAKE (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LT ->
          let _menhir_s = MenhirState222 in
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
      let _menhir_stack = MenhirCell1_ARRAY_LENGTH (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState226 in
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
  
  and _menhir_run_228 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_ARRAY_CREATE (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LT ->
          let _menhir_s = MenhirState229 in
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
  
  and _menhir_run_328 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_IDENT -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
      let _endpos__2_ = _endpos in
      let _v = _menhir_action_042 x in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos_x_ _v _menhir_s _tok
  
  and _menhir_run_329 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_IDENT -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _menhir_stack = MenhirCell0_LPAREN (_menhir_stack, _startpos) in
          let _menhir_s = MenhirState330 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_371 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState372 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | COL ->
          _menhir_run_353 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_353 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_apat -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState353 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TVAR_IDENT _v ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_352 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COL ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_353 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let x = _v in
          let _v = _menhir_action_231 x in
          _menhir_goto_ty_annot_apat_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_177 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_apat -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState177 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_ty_annot_apat_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState508 ->
          _menhir_run_360 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState369 ->
          _menhir_run_360 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_360 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState349 ->
          _menhir_run_350 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_360 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty_annot_apat_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState361 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_350 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let x_ty_opt = _v in
          let _v = _menhir_action_233 x_ty_opt in
          _menhir_goto_ty_annot_apat_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_344 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let p = _v in
      let _v = _menhir_action_085 p in
      _menhir_goto_arg_ty_atomic _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_arg_ty_atomic : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState530 ->
          _menhir_run_531 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState521 ->
          _menhir_run_522 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState355 ->
          _menhir_run_356 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState169 ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_531 : type  ttv_stack. (((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_arg_ty_atomic (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState531
      | COL ->
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState531
      | _ ->
          _eRR ()
  
  and _menhir_run_189 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_179 () in
      _menhir_goto_ret_ty_annot_eq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_ret_ty_annot_eq : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState531 ->
          _menhir_run_532 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState522 ->
          _menhir_run_523 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState356 ->
          _menhir_run_357 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState188 ->
          _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_532 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ret_ty_annot_eq (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState532
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState532
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState532
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | LET ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState532
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | IDENT _v_4 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState532
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState532
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState532
      | _ ->
          _eRR ()
  
  and _menhir_run_523 : type  ttv_stack. (((((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ret_ty_annot_eq (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState523
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState523
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState523
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | LET ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState523
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | IDENT _v_4 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState523
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState523
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState523
      | _ ->
          _eRR ()
  
  and _menhir_run_357 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ret_ty_annot_eq (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState357
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState357
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState357
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | LET ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState357
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | IDENT _v_4 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState357
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState357
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState357
      | _ ->
          _eRR ()
  
  and _menhir_run_193 : type  ttv_stack ttv_result. (((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_REC, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ret_ty_annot_eq (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState193
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState193
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState193
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | LET ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState193
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | IDENT _v_4 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState193
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState193
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | _ ->
          _eRR ()
  
  and _menhir_run_190 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic as 'stack) -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_COL (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState190 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TVAR_IDENT _v ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_522 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_arg_ty_atomic (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState522
      | COL ->
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState522
      | _ ->
          _eRR ()
  
  and _menhir_run_356 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_arg_ty_atomic (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
      | COL ->
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
      | _ ->
          _eRR ()
  
  and _menhir_run_188 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_REC, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_arg_ty_atomic (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState188
      | COL ->
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState188
      | _ ->
          _eRR ()
  
  and _menhir_run_204 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COL ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState205 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVAR_IDENT _v ->
              _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RIGHT_ARROW | RPAREN ->
          let p = _v in
          let _v = _menhir_action_170 p in
          _menhir_goto_pat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_pat : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState292 ->
          _menhir_run_208 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState199 ->
          _menhir_run_208 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState200 ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState170 ->
          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState349 ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState172 ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_208 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let p = _v in
      let _v = _menhir_action_086 p in
      _menhir_goto_arg_ty_unparen _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_arg_ty_unparen : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState292 ->
          _menhir_run_209 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState199 ->
          _menhir_run_209 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState200 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_209 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let a = _v in
      let _v = _menhir_action_081 a in
      _menhir_goto_arg_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_arg_ty : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState292 ->
          _menhir_run_293 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState199 ->
          _menhir_run_210 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_293 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_arg_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW ->
          let _menhir_s = MenhirState294 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_210 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_arg_ty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW ->
          let _menhir_s = MenhirState211 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_202 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let a = _v in
          let _v = _menhir_action_082 a in
          _menhir_goto_arg_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_201 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_pat (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_175 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_pat -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_pat (_menhir_stack, _, p) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _v = _menhir_action_028 p in
      _menhir_goto_apat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_182 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let p = _v in
          let _v = _menhir_action_083 p in
          _menhir_goto_arg_ty_atomic _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_174 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_pat (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_184 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COL ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState185 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVAR_IDENT _v ->
              _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let p = _v in
          let _v = _menhir_action_170 p in
          _menhir_goto_pat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_179 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_apat as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState180 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RIGHT_ARROW | RPAREN ->
          let x = _v in
          let _v = _menhir_action_183 x in
          _menhir_goto_separated_nonempty_list_COMMA_apat_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_apat_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_apat as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState180 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState177 ->
          _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_181 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_apat, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_apat (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_184 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_apat_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_178 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_apat (_menhir_stack, _menhir_s, p) = _menhir_stack in
      let ps = _v in
      let _v = _menhir_action_171 p ps in
      _menhir_goto_pat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_176 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_apat (_menhir_stack, _menhir_s, _v) in
          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let p = _v in
          let _v = _menhir_action_170 p in
          _menhir_goto_pat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_526 : type  ttv_stack. (((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty_annot_IDENT_ (_menhir_stack, _menhir_s, _v, _startpos) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState527 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_349 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState349 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_540 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
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
              let _menhir_s = MenhirState542 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TYB_VAR_IDENT _v ->
                  _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TVAR_IDENT _v ->
                  _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | LPAREN ->
                  _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_518 : type  ttv_stack. ((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_STATIC _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
          let MenhirCell1_STATIC (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (ty, _endpos__6_) = (_v, _endpos) in
          let _v = _menhir_action_214 _endpos__6_ _startpos__1_ ty x in
          _menhir_goto_static _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_503 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_OPERATOR _menhir_cell0_OPERATOR_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_OPERATOR_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
          let MenhirCell1_OPERATOR (_menhir_stack, _menhir_s) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_136 t x in
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
                  let _v = _menhir_action_135 t x in
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
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState550
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | TYPE ->
          _menhir_run_459 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState550
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | SHARED ->
          _menhir_run_486 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState550
      | OPERATOR ->
          _menhir_run_492 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | LET ->
          _menhir_run_508 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState550
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | IDENT _v_4 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState550
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | EXTERNAL ->
          _menhir_run_540 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | EOF ->
          _menhir_run_545 _menhir_stack _menhir_lexbuf MenhirState550
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState550
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState550
      | _ ->
          _eRR ()
  
  and _menhir_run_496 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_OPERATOR _menhir_cell0_OPERATOR_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_pi =
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
  
  and _menhir_run_490 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_SHARED _menhir_cell0_EXTERNAL _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
          let MenhirCell0_EXTERNAL (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_SHARED (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_134 t x in
          _menhir_goto_ext_circ _menhir_stack _menhir_lexbuf _menhir_lexer _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_392 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_exp -> _ -> _ -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_354 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_apat (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let ty = _v in
      let _v = _menhir_action_232 ty x in
      _menhir_goto_ty_annot_apat_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_206 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let ty = _v in
      let _v = _menhir_action_088 ty in
      let MenhirCell1_apat (_menhir_stack, _menhir_s, p) = _menhir_stack in
      let ty = _v in
      let _v = _menhir_action_087 p ty in
      _menhir_goto_arg_ty_unparen _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_191 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic, ttv_result) _menhir_cell1_COL -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_COL (_menhir_stack, _menhir_s) = _menhir_stack in
          let ty = _v in
          let _v = _menhir_action_180 ty in
          _menhir_goto_ret_ty_annot_eq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_186 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_apat (_menhir_stack, _, p) = _menhir_stack in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let ty = _v in
          let _v = _menhir_action_084 p ty in
          _menhir_goto_arg_ty_atomic _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_166 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_COL -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_COL (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
      let ty = _v in
      let _v = _menhir_action_229 ty x in
      _menhir_goto_ty_annot_IDENT_ _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_x_ _v _menhir_s _tok
  
  and _menhir_goto_ty_annot_IDENT_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState520 ->
          _menhir_run_526 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState107 ->
          _menhir_run_345 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_345 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_REC as 'stack) -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ty_annot_IDENT_ (_menhir_stack, _menhir_s, _v, _startpos) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState346 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_167 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let x_ty_opt = _v in
          let _v = _menhir_action_230 x_ty_opt in
          _menhir_goto_ty_annot_IDENT_ _menhir_stack _menhir_lexbuf _menhir_lexer _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_164 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let ty = _v in
          let _v = _menhir_action_241 ty in
          _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_134 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_ty_next, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty_next (_menhir_stack, _menhir_s, x, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_208 x xs in
      _menhir_goto_separated_nonempty_list_TIMES_ty_next_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_126 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          let _menhir_s = MenhirState127 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVAR_IDENT _v ->
              _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RIGHT_ARROW ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer
      | QUESTION_MARK ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IMPLY ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_156 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARRAY ->
          let _menhir_stack = MenhirCell1_ty_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_158 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AT | EQ | RPAREN | SEMI_SEMI ->
          let ty = _v in
          let _v = _menhir_action_217 ty in
          _menhir_goto_ty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
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
  
  and _menhir_run_484 : type  ttv_stack. (((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_tyB, _menhir_box_pi) _menhir_cell1_AT _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok ->
      let MenhirCell0_IDENT (_menhir_stack, op, _, _) = _menhir_stack in
      let MenhirCell1_AT (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_tyB (_menhir_stack, _, tyB) = _menhir_stack in
      let MenhirCell1_TYPE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__3_ = _endpos in
      let _v = _menhir_action_245 _endpos__3_ _startpos__1_ op tyB in
      _menhir_goto_type_alias _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_481 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_tyB, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_separated_nonempty_list_COMMA_INT_LIT_ _menhir_cell0_RBRACKET -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok ->
      let MenhirCell0_RBRACKET (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_separated_nonempty_list_COMMA_INT_LIT_ (_menhir_stack, _, intl) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _, op, _, _) = _menhir_stack in
      let MenhirCell1_tyB (_menhir_stack, _, tyB) = _menhir_stack in
      let MenhirCell1_TYPE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__5_ = _endpos in
      let _v = _menhir_action_246 _endpos__5_ _startpos__1_ intl op tyB in
      _menhir_goto_type_alias _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_472 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_EQ, _menhir_box_pi) _menhir_cell1_separated_nonempty_list_PIPE_ty_case_ -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok ->
      let MenhirCell1_separated_nonempty_list_PIPE_ty_case_ (_menhir_stack, _, ts) = _menhir_stack in
      let MenhirCell1_EQ (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _, x, _, _) = _menhir_stack in
      let MenhirCell1_TYPE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__5_ = _endpos in
      let _v = _menhir_action_242 _endpos__5_ _startpos__1_ ts x in
      let _menhir_stack = MenhirCell1_typ_sum (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState547
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | TYPE ->
          _menhir_run_459 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState547
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | SHARED ->
          _menhir_run_486 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState547
      | OPERATOR ->
          _menhir_run_492 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | LET ->
          _menhir_run_508 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState547
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | IDENT _v_4 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState547
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | EXTERNAL ->
          _menhir_run_540 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | EOF ->
          _menhir_run_545 _menhir_stack _menhir_lexbuf MenhirState547
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState547
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState547
      | _ ->
          _eRR ()
  
  and _menhir_run_470 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_EQ, _menhir_box_pi) _menhir_cell1_tyB -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok ->
      let MenhirCell1_tyB (_menhir_stack, _, tyB) = _menhir_stack in
      let MenhirCell1_EQ (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _, x, _, _) = _menhir_stack in
      let MenhirCell1_TYPE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__5_ = _endpos in
      let _v = _menhir_action_243 _endpos__5_ _startpos__1_ tyB x in
      _menhir_goto_type_alias _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_476 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_INT_LIT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState477 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | INT_LIT _v ->
              _menhir_run_476 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RBRACKET ->
          let x = _v in
          let _v = _menhir_action_181 x in
          _menhir_goto_separated_nonempty_list_COMMA_INT_LIT_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_INT_LIT_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState475 ->
          _menhir_run_479 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState477 ->
          _menhir_run_478 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_479 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_tyB, _menhir_box_pi) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_separated_nonempty_list_COMMA_INT_LIT_ (_menhir_stack, _menhir_s, _v) in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell0_RBRACKET (_menhir_stack, _endpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          _menhir_run_469 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState480
      | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | INT_LIT _ | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | REF | REGISTER | RESIZE_INT | RUN | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let _ = _menhir_action_168 () in
          _menhir_run_481 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_478 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_INT_LIT -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_INT_LIT (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_182 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_INT_LIT_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_468 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_EQ as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_tyB (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          _menhir_run_469 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState468
      | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | INT_LIT _ | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | REF | REGISTER | RESIZE_INT | RUN | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let _ = _menhir_action_168 () in
          _menhir_run_470 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_464 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_UP_IDENT -> _ -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_UP_IDENT (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let tyB = _v in
      let _v = _menhir_action_234 tyB x in
      match (_tok : MenhirBasics.token) with
      | PIPE ->
          let _menhir_stack = MenhirCell1_ty_case (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState466 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UP_IDENT _v ->
              _menhir_run_462 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | INT_LIT _ | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | REF | REGISTER | RESIZE_INT | RUN | SEMI_SEMI | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let x = _v in
          let _v = _menhir_action_199 x in
          _menhir_goto_separated_nonempty_list_PIPE_ty_case_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_462 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_UP_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | OF ->
          let _menhir_s = MenhirState463 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LT ->
              _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_PIPE_ty_case_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState461 ->
          _menhir_run_471 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState466 ->
          _menhir_run_467 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_471 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_TYPE, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_EQ as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_separated_nonempty_list_PIPE_ty_case_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          _menhir_run_469 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState471
      | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | BANG | BOOL_LIT _ | CREATE | EOF | EXEC | EXTERNAL | FIX | FOR | FUN | IDENT _ | IF | INT_LIT _ | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | REF | REGISTER | RESIZE_INT | RUN | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let _ = _menhir_action_168 () in
          _menhir_run_472 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_467 : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_ty_case -> _ -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_ty_case (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_200 x xs in
      _menhir_goto_separated_nonempty_list_PIPE_ty_case_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
  
  and _menhir_run_157 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty_next (_menhir_stack, _menhir_s, ty, _startpos_ty_) = _menhir_stack in
      let tyB = _v in
      let _v = _menhir_action_239 ty tyB in
      _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_ty_ _v _menhir_s _tok
  
  and _menhir_run_155 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty_next (_menhir_stack, _menhir_s, ty, _startpos_ty_) = _menhir_stack in
      let tyB = _v in
      let _v = _menhir_action_238 ty tyB in
      _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_ty_ _v _menhir_s _tok
  
  and _menhir_run_152 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty_next (_menhir_stack, _menhir_s, ty, _startpos_ty_) = _menhir_stack in
      let tyB = _v in
      let _v = _menhir_action_237 ty tyB in
      _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_ty_ _v _menhir_s _tok
  
  and _menhir_run_146 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_tyB as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_tyB (_menhir_stack, _menhir_s, _v) in
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let x = _v in
          let _v = _menhir_action_195 x in
          _menhir_goto_separated_nonempty_list_COMMA_tyB_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_145 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState145 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYB_VAR_IDENT _v ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LT ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_tyB_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState113 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState136 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState145 ->
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_148 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
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
          let _menhir_s = MenhirState150 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TVAR_IDENT _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LT ->
              _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_147 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_tyB (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_196 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_tyB_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_143 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let tyB = _v in
          let _v = _menhir_action_224 tyB in
          _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos__1_ _v _menhir_s _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_tyB (_menhir_stack, _menhir_s, _v) in
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_139 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_tyB_next as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          let _menhir_s = MenhirState140 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | IDENT _v_2 ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | ARRAY | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | AT | BANG | BOOL_LIT _ | COMMA | CREATE | EOF | EQ | EXEC | EXTERNAL | FIX | FOR | FUN | IF | IMPLY | INT_LIT _ | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | PIPE | QUESTION_MARK | REF | REGISTER | RESIZE_INT | RIGHT_ARROW | RPAREN | RUN | SEMI_SEMI | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let x = _v in
          let _v = _menhir_action_205 x in
          _menhir_goto_separated_nonempty_list_TIMES_tyB_next_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_TIMES_tyB_next_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_tyB_next as 'stack) -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState138 ->
          _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState140 ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_142 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB_next -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_tyB_next (_menhir_stack, _menhir_s, tyB, _) = _menhir_stack in
      let tyBs = _v in
      let _v = _menhir_action_219 tyB tyBs in
      _menhir_goto_tyB _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
  
  and _menhir_run_141 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_tyB_next, ttv_result) _menhir_cell1_tyB_next -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_tyB_next (_menhir_stack, _menhir_s, x, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_206 x xs in
      _menhir_goto_separated_nonempty_list_TIMES_tyB_next_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
  
  and _menhir_run_137 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_138 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ARRAY | ARRAY_CREATE | ARRAY_LENGTH | ARRAY_MAKE | AT | BANG | BOOL_LIT _ | COMMA | CREATE | EOF | EQ | EXEC | EXTERNAL | FIX | FOR | FUN | IF | IMPLY | INT_LIT _ | INT_OF_TUPLE | LCUR | LENGTH | LET | LPAREN | MACRO_GENERATE | MATCH | MINUS | OPERATOR | OPERATOR_IDENT _ | PARFOR | PIPE | QUESTION_MARK | REF | REGISTER | RESIZE_INT | RIGHT_ARROW | RPAREN | RUN | SEMI_SEMI | SET | SHARED | SHARP_PIPE_LBRACKET | SIZE_CREATE | STRING_LIT _ | TUPLE_OF_INT | TYPE | UNROLL | UP_IDENT _ | VECTOR_MAPI | VECT_CREATE ->
          let tyB = _v in
          let _v = _menhir_action_220 tyB in
          _menhir_goto_tyB _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_130 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_tyB_next (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ARRAY | AT | EQ | IMPLY | QUESTION_MARK | RIGHT_ARROW | RPAREN | SEMI_SEMI | TIMES ->
          let (_startpos_tyB_, tyB) = (_startpos, _v) in
          let _v = _menhir_action_240 tyB in
          _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_tyB_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_512 : type  ttv_stack. (((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_STATIC _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | HAT ->
          let _menhir_s = MenhirState513 in
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
  
  and _menhir_run_454 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_VECTOR_MAPI -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_VECTOR_MAPI (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_010 e in
      _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_446 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_RUN _menhir_cell0_UP_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell0_UP_IDENT (_menhir_stack, i, _, _) = _menhir_stack in
      let MenhirCell1_RUN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_079 e i in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_439 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_REGISTER, ttv_result) _menhir_cell1_exp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_exp (_menhir_stack, _, ev, _, _) = _menhir_stack in
      let MenhirCell1_REGISTER (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e0_, e0) = (_endpos, _v) in
      let _v = _menhir_action_072 _endpos_e0_ _startpos__1_ e0 ev in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e0_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_436 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_REGISTER, ttv_result) _menhir_cell1_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_IDENT (_menhir_stack, _, f, _, _) = _menhir_stack in
      let MenhirCell1_REGISTER (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e0_, e0) = (_endpos, _v) in
      let _v = _menhir_action_073 e0 f in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e0_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_433 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_REF -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_REF (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_037 e in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_400 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_MINUS -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_MINUS (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e1_, e1) = (_endpos, _v) in
      let _v = _menhir_action_049 e1 in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_311 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_aexp, ttv_result) _menhir_cell1_aexp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState311
      | VECTOR_MAPI ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState311
      | UP_IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState311
      | UNROLL ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState311
      | TUPLE_OF_INT ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState311
      | STRING_LIT _v_1 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState311
      | SHARP_PIPE_LBRACKET ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState311
      | RESIZE_INT ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState311
      | PARFOR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState311
      | OPERATOR_IDENT _v_2 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState311
      | MATCH ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState311
      | LPAREN ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState311
      | LCUR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState311
      | INT_OF_TUPLE ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState311
      | INT_LIT _v_3 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState311
      | IDENT _v_4 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState311
      | FOR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState311
      | BOOL_LIT _v_5 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState311
      | BANG ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState311
      | AMP | AMP_AMP | AND | ASR | COL | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
          let (_endpos_x_, x) = (_endpos, _v) in
          let _v = _menhir_action_164 x in
          _menhir_goto_nonempty_list_aexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_nonempty_list_aexp_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_aexp as 'stack) -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState311 ->
          _menhir_run_310 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState289 ->
          _menhir_run_310 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState243 ->
          _menhir_run_288 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_310 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_aexp, ttv_result) _menhir_cell1_aexp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_aexp (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let (_endpos_xs_, xs) = (_endpos, _v) in
      let _v = _menhir_action_165 x xs in
      _menhir_goto_nonempty_list_aexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_xs_ _v _menhir_s _tok
  
  and _menhir_run_288 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_aexp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_aexp (_menhir_stack, _menhir_s, e, _startpos_e_, _) = _menhir_stack in
      let (_endpos_es_, es) = (_endpos, _v) in
      let _v = _menhir_action_048 _endpos_es_ _startpos_e_ e es in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_es_ _startpos_e_ _v _menhir_s _tok
  
  and _menhir_run_289 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_aexp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState289
      | VECTOR_MAPI ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState289
      | UP_IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState289
      | UNROLL ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState289
      | TUPLE_OF_INT ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState289
      | STRING_LIT _v_1 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState289
      | SHARP_PIPE_LBRACKET ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState289
      | RESIZE_INT ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState289
      | PARFOR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState289
      | OPERATOR_IDENT _v_2 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState289
      | MATCH ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState289
      | LPAREN ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState289
      | LCUR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState289
      | INT_OF_TUPLE ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState289
      | INT_LIT _v_3 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState289
      | IDENT _v_4 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState289
      | FOR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState289
      | BOOL_LIT _v_5 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState289
      | BANG ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState289
      | AT_AT ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_stack = MenhirCell1_AT_AT (_menhir_stack, MenhirState289) in
          let _menhir_s = MenhirState290 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_292 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | AT ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_stack = MenhirCell1_AT (_menhir_stack, MenhirState289) in
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
              _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_292 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | AMP | AMP_AMP | AND | ASR | COL | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
          let (_endpos_x_, x) = (_endpos, _v) in
          let _v = _menhir_action_164 x in
          _menhir_goto_nonempty_list_aexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_291 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState291 in
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
          _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          _menhir_run_292 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
  
  and _menhir_run_292 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_FUN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState292 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_200 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_243 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState243
      | VECTOR_MAPI ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState243
      | UP_IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState243
      | UNROLL ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState243
      | TUPLE_OF_INT ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState243
      | STRING_LIT _v_1 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState243
      | SHARP_PIPE_LBRACKET ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState243
      | RESIZE_INT ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState243
      | PARFOR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState243
      | OPERATOR_IDENT _v_2 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState243
      | MATCH ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState243
      | LPAREN ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState243
      | LCUR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState243
      | INT_OF_TUPLE ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState243
      | INT_LIT _v_3 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState243
      | IDENT _v_4 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState243
      | FOR ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState243
      | COL_EQ ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_stack = MenhirCell1_COL_EQ (_menhir_stack, MenhirState243) in
          let _menhir_s = MenhirState244 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | BOOL_LIT _v_11 ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_11 MenhirState243
      | BANG ->
          let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState243
      | AMP | AMP_AMP | AND | ASR | COL | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
          let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_080 e in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_227 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ARRAY_LENGTH -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_ARRAY_LENGTH (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_a_, a) = (_endpos, _v) in
      let _v = _menhir_action_043 _endpos_a_ _startpos__1_ a in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_a_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_225 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_ARRAY_MAKE, ttv_result) _menhir_cell1_size _menhir_cell0_GT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell0_GT (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_size (_menhir_stack, _, sz, _) = _menhir_stack in
      let MenhirCell1_ARRAY_MAKE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_031 _endpos_e_ _startpos__1_ e sz in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_195 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LENGTH -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_LENGTH (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_a_, a) = (_endpos, _v) in
      let _v = _menhir_action_044 _endpos_a_ _startpos__1_ a in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_a_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_104 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_MACRO_GENERATE, ttv_result) _menhir_cell1_aexp, ttv_result) _menhir_cell1_aexp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_aexp (_menhir_stack, _, e_init2, _, _) = _menhir_stack in
      let MenhirCell1_aexp (_menhir_stack, _, ef1, _, _) = _menhir_stack in
      let MenhirCell1_MACRO_GENERATE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_st3_, e_st3) = (_endpos, _v) in
      let _v = _menhir_action_076 _endpos_e_st3_ _startpos__1_ e_init2 e_st3 ef1 in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_st3_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_103 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_MACRO_GENERATE, ttv_result) _menhir_cell1_aexp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState103
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState103
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState103
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState103
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState103
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState103
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState103
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState103
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState103
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState103
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState103
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState103
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState103
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState103
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState103
      | IDENT _v_4 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState103
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState103
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState103
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState103
      | _ ->
          _eRR ()
  
  and _menhir_run_102 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_MACRO_GENERATE as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_aexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState102
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState102
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState102
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState102
      | IDENT _v_4 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState102
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState102
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | _ ->
          _eRR ()
  
  and _menhir_run_101 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_BANG -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_BANG (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_ex_, ex) = (_endpos, _v) in
      let _v = _menhir_action_003 ex in
      _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_ex_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_305 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_c_, _startpos_c_, c) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_090 c in
      _menhir_goto_avalue _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_c_ _startpos_c_ _v _menhir_s _tok
  
  and _menhir_goto_avalue : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_v_, _startpos_v_, v) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_157 v in
      _menhir_goto_lvalue _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_v_ _startpos_v_ _v _menhir_s _tok
  
  and _menhir_goto_lvalue : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState308 ->
          _menhir_run_309 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState290 ->
          _menhir_run_307 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState303 ->
          _menhir_run_302 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_302 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_299 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_309 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_aexp, ttv_result) _menhir_cell1_aexp, ttv_result) _menhir_cell1_AT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_AT (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_aexp (_menhir_stack, _, e2, _, _) = _menhir_stack in
      let MenhirCell1_aexp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_v_, v) = (_endpos, _v) in
      let _v = _menhir_action_034 e1 e2 v in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_v_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_307 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_aexp, ttv_result) _menhir_cell1_aexp, ttv_result) _menhir_cell1_AT_AT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_AT_AT (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_aexp (_menhir_stack, _, e2, _, _) = _menhir_stack in
      let MenhirCell1_aexp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_v_, v) = (_endpos, _v) in
      let _v = _menhir_action_035 e1 e2 v in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_v_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_302 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lvalue as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_lvalue (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState303 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_292 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let (_endpos_x_, x) = (_endpos, _v) in
          let _v = _menhir_action_191 x in
          _menhir_goto_separated_nonempty_list_COMMA_lvalue_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_lvalue_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lvalue as 'stack) -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s ->
      match _menhir_s with
      | MenhirState303 ->
          _menhir_run_304 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v
      | MenhirState300 ->
          _menhir_run_301 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_304 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lvalue, ttv_result) _menhir_cell1_lvalue -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v ->
      let MenhirCell1_lvalue (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let (_endpos_xs_, xs) = (_endpos, _v) in
      let _v = _menhir_action_192 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_lvalue_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_xs_ _v _menhir_s
  
  and _menhir_run_301 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_lvalue -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v ->
      let MenhirCell1_lvalue (_menhir_stack, _, e, _startpos_e_, _) = _menhir_stack in
      let (_endpos_es_, es) = (_endpos, _v) in
      let _v = _menhir_action_248 e es in
      _menhir_goto_value_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_es_ _startpos_e_ _v
  
  and _menhir_goto_value_desc : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v ->
      let (_endpos_v_, _startpos_v_, v) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_247 _endpos_v_ _startpos_v_ v in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__3_, e) = (_endpos, _v) in
      let _v = _menhir_action_089 e in
      _menhir_goto_avalue _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_299 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_lvalue (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState300 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UP_IDENT _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STRING_LIT _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_292 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_249 e in
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
          let _v = _menhir_action_187 x in
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
      let _v = _menhir_action_188 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_const_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_084 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LCUR -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LCUR (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__3_, cs) = (_endpos, _v) in
      let _v = _menhir_action_123 cs in
      _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
  
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_258 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState258 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_266 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState266 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_268 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState268 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_248 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState248 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_270 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell0_MINUS (_menhir_stack, _startpos) in
      let _menhir_s = MenhirState270 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_250 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState250 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_272 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState272 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_260 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState260 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_262 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState262 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_252 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState252 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_274 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState274 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_254 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState254 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_276 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell0_GT (_menhir_stack, _endpos) in
      let _menhir_s = MenhirState276 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_278 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState278 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_280 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState280 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_282 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState282 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_256 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState256 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_264 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState264 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_284 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState284 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_286 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState286 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_app_exp_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState452 ->
          _menhir_run_453 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState017 ->
          _menhir_run_448 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_453 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_app_exp (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_186 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_app_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_448 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_SHARP_PIPE_LBRACKET -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let x = _v in
      let _v = _menhir_action_156 x in
      _menhir_goto_loption_separated_nonempty_list_COMMA_app_exp__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
  
  and _menhir_run_335 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT _menhir_cell0_dot_get as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_266 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_268 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_272 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_274 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_276 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_278 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_280 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_282 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP_AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_284 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_286 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | IN | INIT | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell0_dot_get (_menhir_stack, e1, _) = _menhir_stack in
          let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_046 e1 e2 x in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_x_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_327 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_exp _menhir_cell0_RBRACKET as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_266 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_268 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_272 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_274 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_276 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_278 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_280 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_282 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP_AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_284 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_286 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | IN | INIT | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell0_RBRACKET (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_exp (_menhir_stack, _, e1, _, _) = _menhir_stack in
          let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_045 e1 e2 x in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_x_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_321 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_EXEC, ttv_result) _menhir_cell1_exp _menhir_cell0_DEFAULT, ttv_result) _menhir_cell1_lexp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_266 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_268 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_272 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_274 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_276 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_278 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_280 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_282 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP_AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_284 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_286 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | IN | INIT | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_lexp (_menhir_stack, _, e2, _, _) = _menhir_stack in
          let MenhirCell0_DEFAULT (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_exp (_menhir_stack, _, e1, _, _) = _menhir_stack in
          let MenhirCell1_EXEC (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos_e3_, e3) = (_endpos, _v) in
          let _v = _menhir_action_075 e1 e2 e3 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_287 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_268 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_272 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_274 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_276 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_278 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_280 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_282 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP_AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_284 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_286 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | IN | INIT | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_062 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_285 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_268 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_272 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_274 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_276 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_278 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_280 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_282 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP_AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_284 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_286 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | IN | INIT | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_070 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_283 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_059 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_281 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_060 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_279 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_058 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_277 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp _menhir_cell0_GT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell0_GT (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_056 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_275 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_057 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_273 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_055 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_271 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp _menhir_cell0_MINUS as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | MINUS | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell0_MINUS (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_051 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_269 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_061 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_267 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_266 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_268 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_272 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_274 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_276 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_278 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_280 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_282 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP_AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_284 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_286 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | IN | INIT | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e3_, e3) = (_endpos, _v) in
          let _v = _menhir_action_071 _endpos_e3_ _startpos_e1_ e1 e3 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e3_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_265 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | MINUS | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_069 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_263 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | MINUS | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_067 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_261 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | MINUS | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_068 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_259 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LT | MINUS | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_050 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_257 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_053 _endpos_e2_ _startpos_e1_ e1 e2 in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_255 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_065 _endpos_e2_ _startpos_e1_ e1 e2 in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_253 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_066 _endpos_e2_ _startpos_e1_ e1 e2 in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_251 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_064 _endpos_e2_ _startpos_e1_ e1 e2 in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_249 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_054 _endpos_e2_ _startpos_e1_ e1 e2 in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_247 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_app_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | ASR | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LE | LSL | LSR | LT | MINUS | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH ->
          let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_052 _endpos_e2_ _startpos_e1_ e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_245 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_aexp, ttv_result) _menhir_cell1_COL_EQ as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_266 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_268 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_272 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_274 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_276 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_278 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_280 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_282 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP_AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_284 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_286 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | IN | INIT | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let MenhirCell1_COL_EQ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_aexp (_menhir_stack, _menhir_s, ex, _startpos_ex_, _) = _menhir_stack in
          let (_endpos_e_, e) = (_endpos, _v) in
          let _v = _menhir_action_036 e ex in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_ex_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_242 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_app_exp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_app_exp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_063 _endpos_e2_ _startpos_e1_ e1 e2 in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_240 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TIMES ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_266 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NEQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_268 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LXOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_272 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSL ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LOR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_274 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LAND ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_276 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_278 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ_EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_280 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_282 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASR ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_264 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP_AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_284 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP ->
          let _menhir_stack = MenhirCell1_app_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_286 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | COL | COMMA | DEFAULT | DO | DONE | ELSE | END | EOF | IN | INIT | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TO | WITH ->
          let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_146 e in
          _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_lexp_desc : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_145 _endpos_e_ _startpos_e_ e in
      _menhir_goto_lexp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v _menhir_s _tok
  
  and _menhir_goto_lexp : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState020 ->
          _menhir_run_447 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState387 ->
          _menhir_run_386 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState383 ->
          _menhir_run_386 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState381 ->
          _menhir_run_380 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState377 ->
          _menhir_run_380 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_376 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState339 ->
          _menhir_run_340 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState337 ->
          _menhir_run_338 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState318 ->
          _menhir_run_319 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState315 ->
          _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState312 ->
          _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState458 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState546 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState547 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState557 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState552 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState550 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState548 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState532 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState527 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState523 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState443 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState441 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState030 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState430 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState414 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState410 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState407 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState397 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState395 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState374 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState372 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState361 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState357 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState346 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState193 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState196 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState198 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState211 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState213 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState294 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState236 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState214 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_447 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_SET -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_SET (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_038 _endpos_e_ _startpos__1_ e in
      _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_386 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PIPE_COMMA_PIPE ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState387 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let x = _v in
          let _v = _menhir_action_201 x in
          _menhir_goto_separated_nonempty_list_PIPE_COMMA_PIPE_lexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_PIPE_COMMA_PIPE_lexp_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState387 ->
          _menhir_run_388 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState383 ->
          _menhir_run_384 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_388 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_lexp (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_202 x xs in
      _menhir_goto_separated_nonempty_list_PIPE_COMMA_PIPE_lexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_384 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_lexp (_menhir_stack, _, e1, _, _) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__5_, es) = (_endpos, _v) in
      let _v = _menhir_action_152 e1 es in
      _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_380 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PIPE_PIPE ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState381 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let x = _v in
          let _v = _menhir_action_203 x in
          _menhir_goto_separated_nonempty_list_PIPE_PIPE_lexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_PIPE_PIPE_lexp_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState381 ->
          _menhir_run_382 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState377 ->
          _menhir_run_378 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_382 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_lexp (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_204 x xs in
      _menhir_goto_separated_nonempty_list_PIPE_PIPE_lexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_378 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_lexp (_menhir_stack, _, e1, _, _) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__5_, es) = (_endpos, _v) in
      let _v = _menhir_action_151 e1 es in
      _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_376 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_236 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PIPE_PIPE ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | PIPE_COMMA_PIPE ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState383 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | COMMA ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_312 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COL | RPAREN ->
          let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_131 e in
          _menhir_goto_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_236 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_lexp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState236 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_312 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_lexp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState312 in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_exp_desc : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_128 _endpos_e_ _startpos_e_ e in
      _menhir_goto_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v _menhir_s _tok
  
  and _menhir_goto_exp : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState458 ->
          _menhir_run_554 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState546 ->
          _menhir_run_554 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState547 ->
          _menhir_run_554 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState548 ->
          _menhir_run_554 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState550 ->
          _menhir_run_554 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState557 ->
          _menhir_run_554 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState552 ->
          _menhir_run_554 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState532 ->
          _menhir_run_533 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState527 ->
          _menhir_run_528 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState523 ->
          _menhir_run_524 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_456 _menhir_stack _v _tok
      | MenhirState443 ->
          _menhir_run_444 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState441 ->
          _menhir_run_442 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_440 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState030 ->
          _menhir_run_437 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState430 ->
          _menhir_run_431 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState414 ->
          _menhir_run_415 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState410 ->
          _menhir_run_411 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState407 ->
          _menhir_run_408 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState033 ->
          _menhir_run_401 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState397 ->
          _menhir_run_398 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState395 ->
          _menhir_run_396 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_394 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_389 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_389 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState374 ->
          _menhir_run_375 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState372 ->
          _menhir_run_373 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState361 ->
          _menhir_run_362 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState357 ->
          _menhir_run_358 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState346 ->
          _menhir_run_347 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState193 ->
          _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState196 ->
          _menhir_run_336 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_331 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState198 ->
          _menhir_run_324 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState211 ->
          _menhir_run_323 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState213 ->
          _menhir_run_322 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState214 ->
          _menhir_run_317 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState294 ->
          _menhir_run_295 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState236 ->
          _menhir_run_238 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_554 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_endpos__2_, _startpos_e_, e) = (_endpos_0, _startpos, _v) in
          let _v = _menhir_action_126 _endpos__2_ _startpos_e_ e in
          _menhir_goto_decl_all _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_decl_all : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let d = _v in
      let _v = _menhir_action_124 d in
      let _menhir_stack = MenhirCell1_decl (_menhir_stack, _menhir_s, _v) in
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
          _menhir_run_459 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState557
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | SHARED ->
          _menhir_run_486 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState557
      | OPERATOR ->
          _menhir_run_492 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | LET ->
          _menhir_run_508 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState557
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | IDENT _v_4 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState557
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | EXTERNAL ->
          _menhir_run_540 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | EOF ->
          _menhir_run_545 _menhir_stack _menhir_lexbuf MenhirState557
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState557
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState557
      | _ ->
          _eRR ()
  
  and _menhir_run_533 : type  ttv_stack. (((((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic, _menhir_box_pi) _menhir_cell1_ret_ty_annot_eq as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_SEMI ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ret_ty_annot_eq (_menhir_stack, _, ty_opt_ret) = _menhir_stack in
          let MenhirCell1_arg_ty_atomic (_menhir_stack, _, p_ty_opt) = _menhir_stack in
          let MenhirCell1_IDENT (_menhir_stack, _, f, _startpos_f_, _) = _menhir_stack in
          let (_endpos__5_, _endpos_e1_, e1) = (_endpos_0, _endpos, _v) in
          let _v = _menhir_action_140 _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt_ret in
          let _endpos = _endpos__5_ in
          let (_endpos_b_, b) = (_endpos, _v) in
          let _v = _menhir_action_024 b in
          _menhir_goto_after_let_SEMI_SEMI_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_b_ _v _tok
      | IN ->
          let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_359 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_goto_after_let_SEMI_SEMI_ : type  ttv_stack. (ttv_stack, _menhir_box_pi) _menhir_cell1_LET -> _ -> _ -> _ -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_b_, b) = (_endpos, _v) in
      let _v = _menhir_action_125 _endpos_b_ _startpos__1_ b in
      _menhir_goto_decl_all _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_359 : type  ttv_stack ttv_result. (((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic, ttv_result) _menhir_cell1_ret_ty_annot_eq, ttv_result) _menhir_cell1_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_exp (_menhir_stack, _, e1, _, _endpos_e1_) = _menhir_stack in
      let MenhirCell1_ret_ty_annot_eq (_menhir_stack, _, ty_opt_ret) = _menhir_stack in
      let MenhirCell1_arg_ty_atomic (_menhir_stack, _, p_ty_opt) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, f, _startpos_f_, _) = _menhir_stack in
      let _v = _menhir_action_139 _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt_ret in
      let b = _v in
      let _v = _menhir_action_020 b in
      _menhir_goto_after_let_IN_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_after_let_IN_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_after_let_IN_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VECT_CREATE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | VECTOR_MAPI ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | UP_IDENT _v_0 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState374
      | UNROLL ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | STRING_LIT _v_1 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState374
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | SET ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | RUN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | RESIZE_INT ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | REGISTER ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | REF ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | OPERATOR_IDENT _v_2 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState374
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | LET ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | INT_LIT _v_3 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState374
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | IDENT _v_4 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState374
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | BOOL_LIT _v_5 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState374
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState374
      | _ ->
          _eRR ()
  
  and _menhir_run_528 : type  ttv_stack. ((((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_ty_annot_IDENT_ as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
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
          _menhir_run_348 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_348 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_REC, ttv_result) _menhir_cell1_ty_annot_IDENT_, ttv_result) _menhir_cell1_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_exp (_menhir_stack, _, e1, _, _endpos_e1_) = _menhir_stack in
      let MenhirCell1_ty_annot_IDENT_ (_menhir_stack, _, f_ty_opt, _startpos_f_ty_opt_) = _menhir_stack in
      let MenhirCell1_REC (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_022 _endpos_e1_ _startpos_f_ty_opt_ e1 f_ty_opt in
      _menhir_goto_after_let_IN_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_524 : type  ttv_stack. ((((((ttv_stack, _menhir_box_pi) _menhir_cell1_LET, _menhir_box_pi) _menhir_cell1_REC, _menhir_box_pi) _menhir_cell1_IDENT, _menhir_box_pi) _menhir_cell1_arg_ty_atomic, _menhir_box_pi) _menhir_cell1_ret_ty_annot_eq as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
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
          let _v = _menhir_action_142 _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt in
          let _endpos = _endpos__6_ in
          let (_endpos_e_, e) = (_endpos, _v) in
          let _v = _menhir_action_025 e in
          _menhir_goto_after_let_SEMI_SEMI_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _v _tok
      | IN ->
          let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_343 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_343 : type  ttv_stack ttv_result. ((((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_REC, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic, ttv_result) _menhir_cell1_ret_ty_annot_eq, ttv_result) _menhir_cell1_exp -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_exp (_menhir_stack, _, e1, _, _endpos_e1_) = _menhir_stack in
      let MenhirCell1_ret_ty_annot_eq (_menhir_stack, _, ty_opt) = _menhir_stack in
      let MenhirCell1_arg_ty_atomic (_menhir_stack, _, p_ty_opt) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _, f, _startpos_f_, _) = _menhir_stack in
      let MenhirCell1_REC (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_141 _endpos_e1_ _startpos_f_ e1 f p_ty_opt ty_opt in
      let e = _v in
      let _v = _menhir_action_021 e in
      _menhir_goto_after_let_IN_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_444 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_PARFOR _menhir_cell0_IDENT, ttv_result) _menhir_cell1_exp, ttv_result) _menhir_cell1_exp -> _ -> _ -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_442 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_PARFOR _menhir_cell0_IDENT, ttv_result) _menhir_cell1_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | DO ->
          let _menhir_s = MenhirState443 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_440 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_PARFOR _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | TO ->
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_437 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_REGISTER as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | INIT ->
          let _menhir_s = MenhirState438 in
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
  
  and _menhir_run_431 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_MATCH, ttv_result) _menhir_cell1_exp _menhir_cell0_option_PIPE_, ttv_result) _menhir_cell1_list_match_case_const_ _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> ttv_result =
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
          let _v = _menhir_action_015 cases e otherwise in
          _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__9_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_415 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_separated_nonempty_list_PIPE_const_ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | PIPE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_separated_nonempty_list_PIPE_const_ (_menhir_stack, _menhir_s, cs) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_160 cs e in
          let _menhir_stack = MenhirCell1_match_case_const (_menhir_stack, _menhir_s, _v) in
          (match (_tok : MenhirBasics.token) with
          | UP_IDENT _v_0 ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState419
          | STRING_LIT _v_1 ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState419
          | OPERATOR_IDENT _v_2 ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState419
          | LPAREN ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState419
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState419
          | INT_LIT _v_3 ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState419
          | BOOL_LIT _v_4 ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState419
          | IDENT _ ->
              let _v_5 = _menhir_action_153 () in
              _menhir_run_420 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_420 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_match_case_const -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_match_case_const (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_154 x xs in
      _menhir_goto_list_match_case_const_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_list_match_case_const_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState404 ->
          _menhir_run_428 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState419 ->
          _menhir_run_420 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_428 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_MATCH, ttv_result) _menhir_cell1_exp _menhir_cell0_option_PIPE_ as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
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
              let _menhir_s = MenhirState430 in
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
              | PARFOR ->
                  _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPERATOR_IDENT _v ->
                  _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | MINUS ->
                  _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MATCH ->
                  _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_GENERATE ->
                  _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LET ->
                  _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LENGTH ->
                  _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LCUR ->
                  _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_OF_TUPLE ->
                  _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_LIT _v ->
                  _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IF ->
                  _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FUN ->
                  _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FOR ->
                  _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | FIX ->
                  _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | EXEC ->
                  _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CREATE ->
                  _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL_LIT _v ->
                  _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_MAKE ->
                  _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_LENGTH ->
                  _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_CREATE ->
                  _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_411 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_250 _endpos_e_ _startpos_x_ e x in
      let e = _v in
      let _v = _menhir_action_161 e in
      _menhir_goto_match_cases _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_match_cases : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState425 ->
          _menhir_run_427 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState404 ->
          _menhir_run_417 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_427 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_match_case -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_match_case (_menhir_stack, _menhir_s, h) = _menhir_stack in
      let rev_cases = _v in
      let _v = _menhir_action_163 h rev_cases in
      _menhir_goto_match_cases _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_417 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_MATCH, ttv_result) _menhir_cell1_exp _menhir_cell0_option_PIPE_ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | END ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_option_PIPE_ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_exp (_menhir_stack, _, e, _, _) = _menhir_stack in
          let MenhirCell1_MATCH (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__6_, rev_cases) = (_endpos, _v) in
          let _v = _menhir_action_016 e rev_cases in
          _menhir_goto_aexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__6_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_408 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_UP_IDENT, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_apat (_menhir_stack, _, p) = _menhir_stack in
      let MenhirCell1_UP_IDENT (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_159 e p x in
      match (_tok : MenhirBasics.token) with
      | PIPE ->
          let _menhir_stack = MenhirCell1_match_case (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState425 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UP_IDENT _v ->
              let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _menhir_stack = MenhirCell1_UP_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_s = MenhirState426 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LPAREN ->
                  _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | IDENT _v ->
              _menhir_run_409 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | END ->
          let h = _v in
          let _v = _menhir_action_162 h in
          _menhir_goto_match_cases _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_409 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RIGHT_ARROW ->
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
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REF ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_401 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_MATCH as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | PIPE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let x = () in
              let _v = _menhir_action_167 x in
              _menhir_goto_option_PIPE_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BOOL_LIT _ | IDENT _ | INT_LIT _ | LCUR | LPAREN | OPERATOR_IDENT _ | STRING_LIT _ | UP_IDENT _ ->
              let _v = _menhir_action_166 () in
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
              let _menhir_stack = MenhirCell1_UP_IDENT (_menhir_stack, MenhirState404, _v_0, _startpos, _endpos) in
              _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState405
          | IDENT _v_1 ->
              let _menhir_stack = MenhirCell1_UP_IDENT (_menhir_stack, MenhirState404, _v_0, _startpos, _endpos) in
              _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState405
          | PIPE | RIGHT_ARROW ->
              let _v_2 =
                let x = _v_0 in
                _menhir_action_102 x
              in
              _menhir_run_421 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v_2 MenhirState404 _tok
          | _ ->
              _eRR ())
      | STRING_LIT _v_3 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState404
      | OPERATOR_IDENT _v_4 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState404
      | LPAREN ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState404
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState404
      | INT_LIT _v_5 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState404
      | IDENT _v_6 ->
          _menhir_run_409 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState404
      | BOOL_LIT _v_7 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState404
      | _ ->
          _eRR ()
  
  and _menhir_run_398 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_FOR _menhir_cell0_IDENT, ttv_result) _menhir_cell1_exp, ttv_result) _menhir_cell1_exp -> _ -> _ -> _ -> _ -> ttv_result =
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
  
  and _menhir_run_396 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_FOR _menhir_cell0_IDENT, ttv_result) _menhir_cell1_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | DO ->
          let _menhir_s = MenhirState397 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_394 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FOR _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | TO ->
          let _menhir_s = MenhirState395 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_389 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
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
          let _menhir_s = MenhirState391 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYB_VAR_IDENT _v ->
              _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVAR_IDENT _v ->
              _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_375 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_after_let_IN_ -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_after_let_IN_ (_menhir_stack, _, b) = _menhir_stack in
      let MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_150 b e2 in
      _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_373 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_apat -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_apat (_menhir_stack, _menhir_s, p) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_092 e p in
      _menhir_goto_binding_apat_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_binding_apat_exp_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | AND ->
          let _menhir_stack = MenhirCell1_binding_apat_exp_ (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState369 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | IN | SEMI_SEMI ->
          let b = _v in
          let _v = _menhir_action_094 b in
          _menhir_goto_bindings_and_apat_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_bindings_and_apat_exp_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState369 ->
          _menhir_run_370 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState508 ->
          _menhir_run_365 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_365 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_370 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_binding_apat_exp_ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_binding_apat_exp_ (_menhir_stack, _menhir_s, b1) = _menhir_stack in
      let bs = _v in
      let _v = _menhir_action_095 b1 bs in
      _menhir_goto_bindings_and_apat_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_365 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let w = _v in
      let _v = _menhir_action_093 w in
      _menhir_goto_bindings_apat_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_bindings_apat_exp_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState508 ->
          _menhir_run_537 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_366 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_537 : type  ttv_stack. ((ttv_stack, _menhir_box_pi) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_pi) _menhir_state -> _ -> _menhir_box_pi =
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
          _menhir_run_367 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_367 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_bindings_apat_exp_ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_bindings_apat_exp_ (_menhir_stack, _menhir_s, b) = _menhir_stack in
      let _v = _menhir_action_019 b in
      _menhir_goto_after_let_IN_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_366 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_bindings_apat_exp_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | IN ->
          _menhir_run_367 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_362 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_annot_apat_ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ty_annot_apat_ (_menhir_stack, _menhir_s, p_ty_opt) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_091 e p_ty_opt in
      _menhir_goto_binding_apat_exp_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_358 : type  ttv_stack ttv_result. (((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic, ttv_result) _menhir_cell1_ret_ty_annot_eq as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | IN ->
          _menhir_run_359 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_347 : type  ttv_stack ttv_result. ((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_REC, ttv_result) _menhir_cell1_ty_annot_IDENT_ as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | IN ->
          _menhir_run_348 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_342 : type  ttv_stack ttv_result. ((((((ttv_stack, ttv_result) _menhir_cell1_LET, ttv_result) _menhir_cell1_REC, ttv_result) _menhir_cell1_IDENT, ttv_result) _menhir_cell1_arg_ty_atomic, ttv_result) _menhir_cell1_ret_ty_annot_eq as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | IN ->
          _menhir_run_343 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_336 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | THEN ->
          let _menhir_s = MenhirState337 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_331 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_IDENT _menhir_cell0_LPAREN -> _ -> _ -> _ -> _ -> ttv_result =
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
              let _v = _menhir_action_040 e1 x in
              _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _startpos_x_ _v _menhir_s _tok
          | LEFT_ARROW ->
              let MenhirCell0_LPAREN (_menhir_stack, _) = _menhir_stack in
              let (_endpos__4_, e) = (_endpos_0, _v) in
              let _v = _menhir_action_127 e in
              let _endpos = _endpos__4_ in
              let _menhir_stack = MenhirCell0_dot_get (_menhir_stack, _v, _endpos) in
              let _menhir_s = MenhirState334 in
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
              | PARFOR ->
                  _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPERATOR_IDENT _v ->
                  _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | MINUS ->
                  _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MATCH ->
                  _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_GENERATE ->
                  _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LENGTH ->
                  _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LCUR ->
                  _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_OF_TUPLE ->
                  _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_LIT _v ->
                  _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IDENT _v ->
                  _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FOR ->
                  _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | EXEC ->
                  _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CREATE ->
                  _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL_LIT _v ->
                  _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_MAKE ->
                  _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_LENGTH ->
                  _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_CREATE ->
                  _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_324 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RBRACKET ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LEFT_ARROW ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_RBRACKET (_menhir_stack, _endpos_0) in
              let _menhir_s = MenhirState326 in
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
              | PARFOR ->
                  _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPERATOR_IDENT _v ->
                  _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | MINUS ->
                  _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MATCH ->
                  _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MACRO_GENERATE ->
                  _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LENGTH ->
                  _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LCUR ->
                  _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_OF_TUPLE ->
                  _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT_LIT _v ->
                  _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IDENT _v ->
                  _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FOR ->
                  _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | EXEC ->
                  _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CREATE ->
                  _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL_LIT _v ->
                  _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_MAKE ->
                  _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_LENGTH ->
                  _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ARRAY_CREATE ->
                  _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | AMP | AMP_AMP | AND | ASR | COL | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
              let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
              let (_endpos__4_, e1) = (_endpos_0, _v) in
              let _v = _menhir_action_039 e1 x in
              _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__4_ _startpos_x_ _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_323 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_arg_ty -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_arg_ty (_menhir_stack, _, p_ty_opt) = _menhir_stack in
      let MenhirCell1_FUN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_148 e p_ty_opt in
      _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_322 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_FIX _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell0_IDENT (_menhir_stack, f, _, _) = _menhir_stack in
      let MenhirCell1_FIX (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_147 _endpos_e_ _startpos__1_ e f in
      _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_317 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_EXEC as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | DEFAULT ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VECT_CREATE ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | VECTOR_MAPI ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | UP_IDENT _v_1 ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState318
          | UNROLL ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | TUPLE_OF_INT ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | STRING_LIT _v_2 ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState318
          | SIZE_CREATE ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | SHARP_PIPE_LBRACKET ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | SET ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | RUN ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | RESIZE_INT ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | REGISTER ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | REF ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | PARFOR ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | OPERATOR_IDENT _v_3 ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState318
          | MINUS ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | MATCH ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | MACRO_GENERATE ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | LPAREN ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | LET ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | LENGTH ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | LCUR ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | INT_OF_TUPLE ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | INT_LIT _v_4 ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState318
          | IF ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | IDENT _v_5 ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState318
          | FUN ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | FOR ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | FIX ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | EXEC ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | CREATE ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | BOOL_LIT _v_6 ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState318
          | BANG ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | ARRAY_MAKE ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | ARRAY_LENGTH ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | ARRAY_CREATE ->
              let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_DEFAULT (_menhir_stack, _endpos_0) in
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
          | AMP | AMP_AMP | AND | ASR | COL | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
              let MenhirCell1_EXEC (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
              let (_endpos__3_, e1) = (_endpos_0, _v) in
              let _v = _menhir_action_078 _endpos__3_ _startpos__1_ e1 in
              _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
          | _ ->
              _eRR ())
      | AMP | AMP_AMP | AND | ASR | COL | COMMA | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
          let MenhirCell1_EXEC (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos_e1_, e1) = (_endpos, _v) in
          let _v = _menhir_action_077 _endpos_e1_ _startpos__1_ e1 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e1_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_295 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_FUN, ttv_result) _menhir_cell1_arg_ty -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_arg_ty (_menhir_stack, _, p_ty_opt) = _menhir_stack in
      let MenhirCell1_FUN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_158 e p_ty_opt in
      _menhir_goto_lvalue _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_238 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_lexp (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_129 e1 e2 in
      _menhir_goto_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_340 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IF, ttv_result) _menhir_cell1_exp, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_lexp (_menhir_stack, _, e2, _, _) = _menhir_stack in
      let (_endpos_e3_, e3) = (_endpos, _v) in
      let _v = _menhir_action_144 e2 e3 in
      _menhir_goto_if_end _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e3_ _v _tok
  
  and _menhir_goto_if_end : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_IF, ttv_result) _menhir_cell1_exp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_exp (_menhir_stack, _, e1, _, _) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e2_e3_, e2_e3) = (_endpos, _v) in
      let _v = _menhir_action_149 e1 e2_e3 in
      _menhir_goto_lexp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_e3_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_338 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_IF, ttv_result) _menhir_cell1_exp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AMP | AMP_AMP | AND | ASR | COL | COMMA | DEFAULT | DIV | DO | DONE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_143 e2 in
          _menhir_goto_if_end _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_319 : type  ttv_stack ttv_result. (((ttv_stack, ttv_result) _menhir_cell1_EXEC, ttv_result) _menhir_cell1_exp _menhir_cell0_DEFAULT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RESET ->
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AMP | AMP_AMP | AND | ASR | COL | COMMA | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
          let MenhirCell0_DEFAULT (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_exp (_menhir_stack, _, e1, _, _) = _menhir_stack in
          let MenhirCell1_EXEC (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_074 e1 e2 in
          _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_314 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState315 in
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
          | PARFOR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPERATOR_IDENT _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MACRO_GENERATE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LENGTH ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCUR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_OF_TUPLE ->
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_LIT _v ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IF ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXEC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CREATE ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_LIT _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_MAKE ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_LENGTH ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ARRAY_CREATE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AMP | AMP_AMP | AND | ASR | COL | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
          let (_endpos_x_, x) = (_endpos, _v) in
          let _v = _menhir_action_189 x in
          _menhir_goto_separated_nonempty_list_COMMA_lexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_lexp_ : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp as 'stack) -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState315 ->
          _menhir_run_316 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState312 ->
          _menhir_run_313 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_316 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_lexp, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_lexp (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let (_endpos_xs_, xs) = (_endpos, _v) in
      let _v = _menhir_action_190 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_lexp_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_xs_ _v _menhir_s _tok
  
  and _menhir_run_313 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_lexp -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_lexp (_menhir_stack, _menhir_s, e, _startpos_e_, _) = _menhir_stack in
      let (_endpos_es_, es) = (_endpos, _v) in
      let _v = _menhir_action_130 e es in
      _menhir_goto_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_es_ _startpos_e_ _v _menhir_s _tok
  
  and _menhir_run_235 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_236 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA ->
          let _menhir_stack = MenhirCell1_lexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_312 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMP | AMP_AMP | AND | ASR | COL | DEFAULT | DIV | DO | DONE | ELSE | END | EOF | EQ | EQ_EQ | GE | GT | IN | INIT | LAND | LE | LOR | LSL | LSR | LT | LXOR | MINUS | MOD | NEQ | OR | PIPE | PIPE_COMMA_PIPE | PIPE_PIPE | PIPE_RBRACKET | PLUS | RBRACKET | RESET | RPAREN | SEMI_SEMI | THEN | TIMES | TO | WITH | XOR ->
          let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_131 e in
          _menhir_goto_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_223 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_ARRAY_MAKE as 'stack) -> _ -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_size (_menhir_stack, _menhir_s, _v, _endpos) in
      match (_tok : MenhirBasics.token) with
      | GT ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_GT (_menhir_stack, _endpos) in
          let _menhir_s = MenhirState224 in
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
  
  and _menhir_run_217 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_CREATE -> _ -> _ -> _ -> _ -> ttv_result =
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
                  let _v = _menhir_action_033 _endpos__6_ _startpos__1_ sz in
                  _menhir_goto_app_exp_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__6_ _startpos__1_ _v _menhir_s _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_160 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_ty_next -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | GT ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ty_next (_menhir_stack, _menhir_s, ty_tyB, _startpos_ty_tyB_) = _menhir_stack in
          let (sz, _endpos__5_) = (_v, _endpos_0) in
          let _v = _menhir_action_236 _endpos__5_ _startpos_ty_tyB_ sz ty_tyB in
          _menhir_goto_ty_next _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_ty_tyB_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_125 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let (_endpos_sz_, sz) = (_endpos, _v) in
      let _v = _menhir_action_211 sz in
      _menhir_goto_size_list _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_sz_ _v _menhir_s _tok
  
  and _menhir_goto_size_list : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState150 ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState131 ->
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState460 ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState117 ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_151 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_LPAREN, ttv_result) _menhir_cell1_separated_nonempty_list_COMMA_tyB_ _menhir_cell0_RPAREN _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell0_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
      let MenhirCell0_RPAREN (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_separated_nonempty_list_COMMA_tyB_ (_menhir_stack, _, tyBs) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_szs_, szs) = (_endpos, _v) in
      let _v = _menhir_action_227 _endpos_szs_ _startpos__1_ szs tyBs x in
      _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_132 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_tyB_next _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell0_IDENT (_menhir_stack, x, _, _) = _menhir_stack in
      let MenhirCell1_tyB_next (_menhir_stack, _menhir_s, tyB, _startpos_tyB_) = _menhir_stack in
      let (_endpos_szs_, szs) = (_endpos, _v) in
      let _v = _menhir_action_226 _endpos_szs_ _startpos_tyB_ szs tyB x in
      _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_tyB_ _v _menhir_s _tok
  
  and _menhir_run_124 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_IDENT -> _ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
      let (_endpos_szs_, szs) = (_endpos, _v) in
      let _v = _menhir_action_225 _endpos_szs_ _startpos_x_ szs x in
      _menhir_goto_tyB_next _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos_x_ _v _menhir_s _tok
  
  and _menhir_run_119 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_size (_menhir_stack, _menhir_s, _v, _endpos) in
          let _menhir_s = MenhirState120 in
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
          let _v = _menhir_action_193 x in
          _menhir_goto_separated_nonempty_list_COMMA_size_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_size_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState118 ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState120 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_122 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LT -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LT (_menhir_stack, _menhir_s) = _menhir_stack in
      let (_endpos__3_, szs) = (_endpos, _v) in
      let _v = _menhir_action_212 szs in
      _menhir_goto_size_list _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _v _menhir_s _tok
  
  and _menhir_run_121 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_size -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_size (_menhir_stack, _menhir_s, x, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_194 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_size_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_115 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LT -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | GT ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LT (_menhir_stack, _menhir_s) = _menhir_stack in
          let (_endpos, sz) = (_endpos_0, _v) in
          let _v = _menhir_action_218 sz in
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  let _menhir_run_458 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_pi =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState458 in
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
          _menhir_run_459 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUPLE_OF_INT ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LIT _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SIZE_CREATE ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARP_PIPE_LBRACKET ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SHARED ->
          _menhir_run_486 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
      | PARFOR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPERATOR_IDENT _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | OPERATOR ->
          _menhir_run_492 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MACRO_GENERATE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_508 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LENGTH ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCUR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_OF_TUPLE ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_LIT _v ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IF ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXTERNAL ->
          _menhir_run_540 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXEC ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EOF ->
          _menhir_run_545 _menhir_stack _menhir_lexbuf _menhir_s
      | CREATE ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_LIT _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_MAKE ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_LENGTH ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ARRAY_CREATE ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
end

let pi =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_pi v = _menhir_run_458 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

let exp_eof =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_exp_eof v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
