%{

  open Prelude
  open Operators
  open Types
  open Ast
  open Ast_mk

  (* location augmented we a file name *)
  let with_file loc =
    (!Current_filename.current_file_name, loc)


  let alias_types = Hashtbl.create 10

  let rec as_const loc e =
    match un_annot e with
    | E_const c -> c
    | E_tuple es -> C_tuple(List.map (as_const loc) es)
    | _ -> Prelude.Errors.raise_error ~loc:(with_file loc)
              ~msg:"this expression should be a constant" ()

%}

%token LPAREN RPAREN LBRACKET RBRACKET COMMA PIPE_PIPE PIPE_COMMA_PIPE EQ EQ_EQ COL SEMI HAT STATIC DOT_LENGTH
%token SHARP_PIPE_LBRACKET PIPE_RBRACKET
%token FUN AMP DOT REGISTER EXEC LAST DEFAULT
%token NODE IMPLY
%token MATCH WITH PIPE END
%token OF
%token LET REC AND IN IF THEN ELSE FIX VAR
%token <string> IDENT UP_IDENT TVAR_IDENT
%token <bool> BOOL_LIT
%token <int> INT_LIT
%token PLUS MINUS TIMES LT LE GT GE NEQ NOT MOD DIV AMP_AMP OR
%token XOR LAND LOR LXOR LSL LSR ASR RESIZE_INT TUPLE_OF_INT
%token EOF
%token SEMI_SEMI
%token LEFT_ARROW RIGHT_ARROW
%token EXIT_REPL
%token <string> STRING_LIT
%token QUOTE TYPE
%token TILDE PAR GENERATE
%token FOR TO DO DONE
%token BIG_LAMBDA 
/* The precedences must be listed from low to high. */

%right    PIPE_PIPE PIPE_COMMA_PIPE /* parallel construct */
%nonassoc IN
%nonassoc SEMI
%nonassoc LET
%nonassoc IF THEN ELSE
%right    LEFT_ARROW
%left     COMMA
%right    AMP AMP_AMP OR
%left     LT LE GT GE NEQ EQ EQ_EQ
%left     PLUS MINUS
%right    LSL LSR ASR
%left     TIMES
%left     DIV MOD LAND LOR LXOR XOR
%nonassoc prec_unary_minus
%nonassoc DOT
%nonassoc BOOL_LIT IDENT LPAREN

%start <(x * static) list * (x * (x * ty) list) list * ((p*e)*Prelude.loc) list> pi
%start <((p*e)*Prelude.loc) option> decl_opt
%start <e> exp_eof

%%

pi:
| g=static pi=pi { let gs,ts,ds= pi in (g::gs,ts,ds) }
| d=typ_sum pi=pi { let gs,ts,ds= pi in (gs,d::ts,ds) }
| d=decl pi=pi { let gs,ts,ds= pi in (gs,ts,d::ds) }
| type_alias pi=pi { pi }
| EOF { [],[],[] }

static:
| LET STATIC x=IDENT EQ w=const_init_static HAT n=INT_LIT SEMI_SEMI {
      let (ce,tyopt) = w in
      let c = as_const $loc ce in
      (* todo: add loc and type annotation [tyopt] *)
      (x,Static_array(c,n)) 
  }
| LET STATIC x=IDENT EQ ec=aexp SEMI_SEMI {
      let c = as_const $loc ec in
      (x,Static_const c)

}


const_init_static:
| ce=aexp { (ce,None) }
| LPAREN ce=exp COL ty=ty RPAREN { (ce,Some ty) }

exp_eof:
| e=exp EOF {e}

decl_opt:
| d=decl { Some d }
| EXIT_REPL { None }

decl:
| LET b=after_let(SEMI_SEMI)
        { b,(with_file $loc) }
| NODE b=fun_decl(SEMI_SEMI)
        { enforce_node b,(with_file $loc) }

| e=exp SEMI_SEMI { ((P_var "_", e),(with_file $loc))  }
/*
| EOF { E_var (!Ast_mk.main_symbol) }*/

type_alias: /* todo: avoid side effect, which depends on the left-to-right evaluation order */
| TYPE x=IDENT EQ ty=ty SEMI_SEMI { Hashtbl.add alias_types x ty }

typ_sum:
| TYPE x=IDENT EQ ts=separated_nonempty_list(PIPE,ty_case) { x,ts }

ty_case:
| x=UP_IDENT OF ty=ty { x,ty }

fun_decl(In_kw):
| f=IDENT p_ty_opt=arg_ty_atomic ls_opt=labels_fun_decl
                  ty_opt_ret=ret_ty_annot_eq
    e1=exp In_kw
        {
            let ef = mk_let_fun ~loc:(with_file ($startpos(f),$endpos(e1)))
                                ~p_ty_opt
                                ~ty_opt_ret
                (match ls_opt with
                 | None -> e1
                 | Some ls -> List.fold_right (fun l e -> E_absLabel(l,e)) ls e1)
            in
            (P_var f,ef)
        }

labels_fun_decl:
| { None }
| ls=nonempty_list(lbl) { Some (ls) }

lbl:
TILDE x=IDENT { x }

lblapp:
| TILDE x=IDENT COL y=IDENT { (x,St_var y) }
| TILDE x=IDENT COL c=const { (x,St_const c) }

after_let(In_kw):
| b=bindings(apat,exp) In_kw { b }
| b=fun_decl(In_kw)
        { b }
| e=fun_rec_decl(In_kw)
        { e }
| REC f_ty_opt=ty_annot(IDENT) EQ e1=exp In_kw
        {
            let f,ty_opt = f_ty_opt in
            let loc_fun = with_file ($startpos(f_ty_opt),$endpos(e1)) in
            P_var f, mk_fix f (ty_annot_opt ~ty:ty_opt e1) loc_fun
        }
fun_rec_decl(In_kw):
| REC f=IDENT p_ty_opt=arg_ty_atomic ty_opt=ret_ty_annot_eq e1=exp In_kw
        {
            let p_ty_opt_f =
              let open Types in
              match p_ty_opt with
              | p,None -> p,None
              | p,Some t -> p,Some (fun_ty t (unknown()) (unknown()))
            in
            let loc_fun = with_file ($startpos(f),$endpos(e1)) in
            let (p,ty_f_opt) = p_ty_opt_f in
            let ef = mk_fun_ty_annot p ty_f_opt (ty_annot_opt ~ty:ty_opt e1)
                   |> mk_loc loc_fun in
            P_var f, mk_fix f ef loc_fun
        }




ty_annot(X) :
| x=X
        { x,None }
| x=X COL ty=ty
        { x,Some ty }
| LPAREN x_ty_opt=ty_annot(X) RPAREN
        { x_ty_opt }

ty:
| arg=oty IMPLY ret=oty { T_fun{arg;dur=T_size 0;ret} }
| arg=oty RIGHT_ARROW ret=oty { T_fun{arg;dur=(unknown());ret} }
| arg=oty MINUS LBRACKET ty=ty RBRACKET RIGHT_ARROW ret=oty { T_fun{arg;dur=ty;ret} }
| t=oty { t }

oty:
| tys=separated_nonempty_list(TIMES,aty) { group_ts tys }

aty:
| x=IDENT { match x with
            | "unit" -> T_const TUnit
            | "bool" -> T_const TBool
            | "int" -> T_const (TInt (T_size 32))
            | s -> (match Hashtbl.find_opt alias_types s with
                    | Some t -> t
                    | None -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                              ~msg:("unbound type constructor "^s) ()) }
| x=IDENT LT tz=ty GT { match x with
                        | "string" -> T_string tz
                        | "int" -> T_const (TInt tz)
                        | s -> Prelude.Errors.raise_error ~loc:(with_file $loc) ~msg:("unbound type constructor "^s) () }
| at=aty STATIC LT tz=ty GT { T_array{elem=at;size=tz} }
| n=INT_LIT           { T_size n }
| x=TVAR_IDENT { unknown () } /* TODO: hashmap to constrain occurrences */
| LPAREN ty=ty RPAREN { ty }


value:
  v=value_desc { mk_loc (with_file $loc) v }

value_desc:
| e=lvalue COMMA es=separated_nonempty_list(COMMA,lvalue)
    { E_tuple (e::es) }
| e=lvalue { e }

lvalue:
| v=avalue { v }
| FUN p_ty_opt=arg_ty RIGHT_ARROW e=exp
      {
        let p,ty_opt = p_ty_opt in
        mk_fun_ty_annot p ty_opt e
    }

avalue:
| LPAREN e=value RPAREN { e }
| c=const { E_const c }


/* decorated expression */
exp:
  e=exp_desc { mk_loc (with_file $loc) e }

/* expression description
   with decorated sub-expression */
exp_desc:
| e1=app_exp SEMI e2=exp
        {
            E_letIn(P_unit,e1,e2)
        }
| e=lexp COMMA es=separated_nonempty_list(COMMA,lexp)
        {
            E_tuple (e::es)
        }
| e1=lexp PIPE_PIPE e2=lexp
| e1=lexp PIPE_COMMA_PIPE e2=lexp
        {
            E_par[e1;e2]
        }
| e=lexp {e}


arg_ty_unparen:
| p=pat { p, None }
| p=apat COL ty=aty  {p, Some ty}

arg_ty:
| a=arg_ty_unparen
| LPAREN a=arg_ty_unparen RPAREN { a }

arg_ty_atomic:
| LPAREN p=pat RPAREN { p, None }
| LPAREN p=apat COL ty=ty RPAREN {p, Some ty}
| p=apat { p, None }


lexp:
  e=lexp_desc { mk_loc (with_file $loc) e }

lexp_desc:
| e=app_exp { e }
| FIX f=IDENT e=exp
        { mk_fix f e (with_file $loc) }
| FUN p_ty_opt=arg_ty RIGHT_ARROW e=exp
        { let (p,ty_p_opt) = p_ty_opt in
          mk_fun_ty_annot_p p ty_p_opt e }
| IF e1=exp THEN e2_e3=if_end
        { let (e2,e3) = e2_e3 in E_if(e1,e2,e3) }
| LET b=after_let(IN) e2=exp
        { let (p,e1) = b in
          E_letIn(p,e1,e2) }
| VAR x=IDENT EQ e1=exp IN e2=exp
        { E_lastIn(x,e1,e2) }
| NODE b=fun_decl(IN) e2=exp
        { let (p,e1) = enforce_node b in
          E_letIn(p,e1,e2) }
| BIG_LAMBDA TILDE x=IDENT /* COL ty */
DOT e=exp { E_absLabel(x,e) }

if_end:
| e2=lexp { e2,E_const Unit }
| e2=lexp ELSE e3=lexp { e2, e3 }

ret_ty_annot_eq:
| EQ { None }
| COL ty=ty EQ { Some ty }

bindings(P,E):
| w=bindings_and(P,E)
  { match w with
    | [],_ | _,[] -> assert false
    | [p],[e] -> (p,e)
    | ps,es -> (P_tuple ps, E_par es) }

bindings_and(P,E):
| b=binding(P,E) { let (p,e) = b in ([p],[e]) }
| b1=binding(P,E) AND bs=bindings_and(P,E)
   { let (p1,e1) = b1 in
     let (ps,es) = bs in
     (p1::ps,e1::es) }



binding(P,E):
| p_ty_opt=ty_annot(P) EQ e=E
        {
            let p,ty_opt = p_ty_opt in
            p,ty_annot_opt ~ty:ty_opt e
        }
| p=P EQ e=E
    { p,e }

app_exp:
  e=app_exp_desc { mk_loc (with_file $loc) e }

app_exp_desc:
| x=IDENT LEFT_ARROW e=aexp { E_set(x,e) }
| x=IDENT LBRACKET e1=exp RBRACKET
| x=IDENT DOT LPAREN e1=exp RPAREN { E_static_array_get(x,e1) }
| x=IDENT DOT_LENGTH { E_static_array_length x }
| x=IDENT LBRACKET e1=exp RBRACKET LEFT_ARROW e2=app_exp
| x=IDENT DOT LPAREN e1=exp RPAREN LEFT_ARROW e2=app_exp 
  { E_static_array_set(x,e1,e2) }
| e=aexp  es=aexp+ ls=list(lblapp)
      { match e::es with
        | [e1;e2] -> (match un_annot e1 with
                      | E_var _ | E_const _ | E_appLabel _ -> 
                          let e = E_app(e1,e2) in
                          List.fold_left (fun e (l,lc) -> 
                            E_appLabel(e,l,lc)) e ls
                      | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                   ~msg:"expression in functional position should be a variable or a constante" ())
        | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                   ~msg:"All functions and primitives should be unary. Hints: use a tuple as argument" () }
| e=aexp ll=lblapp { let (l,lc) = ll in E_appLabel(e,l,lc) } 
| MINUS e1=aexp %prec prec_unary_minus { E_app(E_const(Op(Runtime(Sub))),E_tuple[E_const(Int(0,unknown()));e1]) }
| e1=app_exp op=binop e2=app_exp
        { E_app (mk_loc (with_file $loc) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file $loc) @@ E_tuple [e1;e2])
        }
| e1=app_exp AMP_AMP e2=app_exp
        { let e3 = mk_loc (with_file $loc) @@ E_const (Bool false) in
          E_if(e1,e2,e3)
        }
| e1=app_exp OR e3=app_exp
        { let e2 = mk_loc (with_file $loc) @@ E_const (Bool true) in
          E_if(e1,e2,e3)
        }
| REGISTER ev=avalue LAST e0=aexp
       { match un_annot ev with
         | E_fun(p,e1) -> E_reg((p,e1),e0,Ast.gensym ())
         | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                               ~msg:"This expression should be a function" ()
       }
| REGISTER f=IDENT LAST e0=aexp
       {
        let y = gensym () in
         E_reg((P_var y,E_app(E_var f,E_var y)),e0,Ast.gensym ())
       }
| EXEC e1=exp DEFAULT e2=lexp
       { E_exec(e1,e2,"") }
| FUN ls=nonempty_list(lbl) RIGHT_ARROW e=exp
    { List.fold_right (fun l e -> E_absLabel(l,e)) ls e }
| GENERATE ef1=aexp e_init2=aexp lbl=lbl COL e_st3=aexp
  { let z = Ast.gensym () in
    (if lbl = "depth" then () 
     else Prelude.Errors.raise_error ~loc:(with_file $loc)
             ~msg:"keyword ~depth expected." ());
    E_generate((P_var z,E_app(ef1,E_var z)),e_init2,e_st3,with_file $loc) }

| e=aexp { e }

lc:
| x=IDENT { St_var x }
| c=const { St_const c }

aexp:
  e=aexp_desc { mk_loc (with_file $loc) e }

aexp_desc:
| LPAREN e=exp RPAREN { e }
| LPAREN e=exp COL ty=ty RPAREN { ty_annot ~ty e }
| c=const { E_const c }

| x=RESIZE_INT LT k=INT_LIT GT { E_const (Op(Runtime(Resize_int k))) }
| x=TUPLE_OF_INT LT k=INT_LIT GT { E_const (Op(Runtime(Tuple_of_int k))) }
| x=IDENT { match x with
            | "abs" -> E_const (Op(Runtime(Abs)))
            | "print" -> E_const (Op(Runtime(Print)))
            | "print_string" -> E_const (Op(Runtime(Print_string)))
            | "print_int" -> E_const (Op(Runtime(Print_int)))
            | "print_newline" -> E_const (Op(Runtime(Print_newline)))
            | "string_length" -> E_const (Op(Runtime(String_length)))
            | "assert" -> E_const (Op(Runtime(Assert)))
            | "_" -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                         ~msg:"wildcard \"_\" not expected." ()
            | _ -> E_var x }
| SHARP_PIPE_LBRACKET separated_list(COMMA,app_exp) PIPE_RBRACKET
    { (* Buffer n *) assert false (*todo*)  }

| MATCH e=exp WITH
    PIPE? cases=match_case_const*
    IDENT RIGHT_ARROW otherwise=exp END
      { E_case(e,cases,otherwise) }

| MATCH e=exp WITH
    PIPE? rev_cases=match_cases END
      { let (hs,eo) = rev_cases in
        E_match(e,List.rev hs,eo) }
| FOR x=IDENT EQ e_st1=exp TO e_st2=exp DO e=exp DONE 
      { E_for(x,e_st1,e_st2,e,with_file $loc) }

match_case_const:
| c=const RIGHT_ARROW e=exp PIPE { (c,e) }

match_cases:
| e=wildcard_case                         { [],Some e }
| h=match_case                            { [h],None }
| h=match_case PIPE rev_cases=match_cases { let (hs,eo) = rev_cases in h::hs,eo }

match_case:
| x=UP_IDENT p=apat RIGHT_ARROW e=exp { (x,(p,e)) }

wildcard_case:
| x=IDENT RIGHT_ARROW e=exp
        { match x with
          | "_" -> e
          | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                  ~msg:"the wildcard_case should be named \"_\"" () }


pat:
| p=apat { p }
| p=apat COMMA ps=separated_nonempty_list(COMMA,apat)
  { P_tuple (p::ps) }

apat:
| LPAREN RPAREN { P_unit }
| LPAREN p=pat RPAREN { p }
| x=IDENT { P_var x }


(* const_with_neg_int:
| c=const | LPAREN c=const_with_neg_int RPAREN  { c }
| MINUS n=INT_LIT { Int(- n,unknown()) }
*)

const:
| LPAREN RPAREN { Unit }
| b=BOOL_LIT    { Bool b }
| n=INT_LIT {
    Int (n,unknown()) }
| n=INT_LIT QUOTE k=INT_LIT
    { if Float.log2 (float n) >= float (k-1) then
       Prelude.Errors.raise_error ~loc:(with_file $loc)
          ~msg:("Integer literal "^
                string_of_int n^
                " exceeds the range of representable integers of type int<"^
                string_of_int k ^">") ()
      else Int (n,T_size k) }
| s=STRING_LIT           { String s }
| NOT                    { Op(Runtime(Not)) }
| x=UP_IDENT             { Inj x }
| LPAREN op=binop RPAREN { Op(Runtime(op)) }

%inline binop:
| PLUS       { Add }
| MINUS      { Sub }
| TIMES      { Mult }
| DIV        { Div }
| MOD        { Mod }
| LT         { Lt }
| GT         { Gt }
| LE         { Le }
| GE         { Ge }
| EQ | EQ_EQ { Eq }
| NEQ        { Neq }
| AMP        { And }
| XOR        { Xor }
| LXOR       { Lxor }
| LAND       { Land }
| LOR        { Lor }
| LSL        { Lsl }
| LSR        { Lsr }
| ASR        { Asr }
