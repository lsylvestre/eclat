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
%}

%token LPAREN RPAREN LBRACKET RBRACKET COMMA PIPE_PIPE PIPE_COMMA_PIPE EQ EQ_EQ COL SEMI HAT STATIC DOT_LENGTH ARRAY_LENGTH
%token SHARP_PIPE_LBRACKET LBRACKET_PIPE PIPE_RBRACKET
%token FUN AMP DOT REGISTER EXEC LAST DEFAULT
%token NODE IMPLY
%token MATCH WITH PIPE END
%token OF
%token LET REC AND IN IF THEN ELSE FIX
%token <string> IDENT UP_IDENT TVAR_IDENT
%token <bool> BOOL_LIT
%token <int> INT_LIT
%token PLUS MINUS TIMES LT LE GT GE NEQ NOT MOD DIV AMP_AMP OR
%token XOR LAND LOR LXOR LSL LSR ASR RESIZE_INT TUPLE_OF_INT INT_OF_TUPLE
%token TUPLE_GET TUPLE_UPDATE
%token UNROLL AT AT_AT
%token GET SET LENGTH CREATE
%token SIZE_CREATE VECTOR_CREATE

%token EOF
%token SEMI_SEMI
%token LEFT_ARROW RIGHT_ARROW
%token EXIT_REPL
%token <string> STRING_LIT
%token QUOTE TYPE
%token MACRO_GENERATE
%token MACRO_FOR PARFOR FOR TO DO DONE
%token REF COL_EQ BANG
%token IMMEDIATE
%token ARRAY_CREATE
%token INIT_TUPLE INIT_INT
%token VECTOR_MAPI INT_MAPI
/* The precedences must be listed from low to high. */

%right    PIPE_PIPE PIPE_COMMA_PIPE /* parallel construct */
%nonassoc IN
%nonassoc SEMI
%nonassoc LET
%nonassoc IF THEN ELSE
%right    LEFT_ARROW
%left     COMMA
%right    OR
%right    AMP AMP_AMP
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

static: /* todo: add loc and type annotation [tyopt] */
| LET STATIC x=IDENT EQ ec=aexp es=static_dim_exp+ SEMI_SEMI
    { let to_int e =
        match un_annot e with
        | E_const (Int(n,_)) -> n
        | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                          ~msg:("dimension for "^x^" should be an integer") ()
      in
      match List.map to_int es with
      | [n] -> x,Static_array(e2c ec,n)
      | ns -> x,Static_matrix(e2c ec,ns) 
  }

| LET STATIC x=IDENT COL ty=ty SEMI_SEMI
    {
      let loc = with_file $loc in
      if Types.no_unknown_in_ty ty then (
         Prelude.Errors.raise_error ~loc
           ~msg:"this type annotation should not contain type unknowns"
      ) ();
      x,Static_array_of (ty,loc)
    }





static_dim_exp: HAT e=aexp { e }

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
| TYPE x=IDENT EQ ty=ty SEMI_SEMI? { add_alias x ty (with_file $loc) }

typ_sum:
| TYPE x=IDENT EQ ts=separated_nonempty_list(PIPE,ty_case) SEMI_SEMI? 
   { add_alias x (T_sum ts) (with_file $loc);
     x,ts }

ty_case:
| x=UP_IDENT OF ty=ty { x,ty }

fun_decl(In_kw):
| f=IDENT p_ty_opt=arg_ty_atomic
                  ty_opt_ret=ret_ty_annot_eq
    e1=exp In_kw
        {
            let ef = mk_let_fun ~loc:(with_file ($startpos(f),$endpos(e1)))
                                ~p_ty_opt
                                ~ty_opt_ret
                        e1
            in
            (P_var f,ef)
        }

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
            let v = mk_fix f (ty_annot_opt ~ty:ty_opt e1) loc_fun in
            P_var f, v
        }
fun_rec_decl(In_kw):
| REC (* o=IMMEDIATE?*) f=IDENT p_ty_opt=arg_ty_atomic ty_opt=ret_ty_annot_eq e1=exp In_kw
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
            let v = mk_fix f ef loc_fun in
            (*match o with 
            | None -> *)P_var f, v
            (* | Some _ -> P_var f, E_letIn(P_var f,v,e1)*)
        }




ty_annot(X) :
| x=X
        { x,None }
| x=X COL ty=ty
        { x,Some ty }
| LPAREN x_ty_opt=ty_annot(X) RPAREN
        { x_ty_opt }

ty:
| arg=oty IMPLY ret=oty { T_fun{arg;dur=T_response_time 0;ret} }
| arg=oty RIGHT_ARROW ret=oty { T_fun{arg;dur=(unknown());ret} }
| arg=oty MINUS LBRACKET ty=ty RBRACKET RIGHT_ARROW ret=oty { T_fun{arg;dur=ty;ret} }
| t=oty { t }

oty:
| tys=separated_nonempty_list(TIMES,aty) { group_ts tys }

aty:
| x=IDENT { match x with
            | "unit" -> T_const TUnit
            | "bool" -> T_const TBool
            | "int" -> Prelude.Errors.warning ~loc:(with_file $loc) (fun fmt ->
                         Format.fprintf fmt "unspecified integer size; replaced by 32\n");
                       T_const (TInt (T_size 32))
            | "string" -> T_string (unknown())
            | s -> (match Hashtbl.find_opt alias_types s with
                    | Some (t,_) -> t
                    | None -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                              ~msg:("unbound type constructor "^s) ()) }
| x=IDENT LT tz=ty GT { match x with
                        | "string" -> T_string tz
                        | "int" -> T_const (TInt tz)
                        | s -> (match Hashtbl.find_opt alias_types s with
                                | Some (t,_) -> t
                                | None -> Prelude.Errors.raise_error ~loc:(with_file $loc) ~msg:("unbound unary type constructor "^s) ()) }
| at=aty x=IDENT LT tz=ty GT 
    { match x with
      | "array" -> T_array{elem=at;size=tz}
      | "vector" -> T_vector{elem=at;size=tz}
      | _ -> (match Hashtbl.find_opt alias_types x with
              | Some (t,_) -> t
              | None -> Prelude.Errors.raise_error ~loc:(with_file $loc) ~msg:("unbound binary type constructor "^x) ()) }
     
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
| e1=lexp SEMI e2=exp
        {
            E_letIn(P_unit,e1,e2)
        }
| e=lexp COMMA es=separated_nonempty_list(COMMA,lexp)
        {
            E_tuple (e::es)
        }

| e=lexp { e }

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
| NODE b=fun_decl(IN) e2=exp 
    /* variant of LET enforcing the defined function to be instantaneous */
        { let (p,e1) = enforce_node b in
          E_letIn(p,e1,e2) }
| LPAREN e1=lexp PIPE_PIPE 
         es=separated_nonempty_list(PIPE_PIPE,lexp) 
  RPAREN
| LPAREN e1=lexp PIPE_COMMA_PIPE 
         es=separated_nonempty_list(PIPE_COMMA_PIPE,lexp)
  RPAREN
        {
            E_par(e1::es)
        }

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
| ARRAY_CREATE e1=aexp
| CREATE e1=aexp { E_local_static_array(e1,with_file $loc) }
| e1=aexp es=static_dim_exp+
   { match es with
     | [] -> assert false
     (* | [e2] -> E_local_static_array(e1,e2, with_file $loc) *)
     | es' -> E_local_static_matrix(e1,es', with_file $loc) }
| e1=aexp e2=aexp AT v=lvalue
| e1=aexp e2=aexp AT_AT v=lvalue 
    { 
        E_app(e1,E_tuple[e2;v]) 
    }
| ex=aexp COL_EQ e=app_exp { E_set(ex,e) }
| REF e=aexp            { E_ref e }
/* | GET e=lexp { match Ast_undecorated.remove_deco e with 
               | E_tuple[E_var x;e1] -> E_array_get(x,e1) 
               | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                   ~msg:"array get" () } */
| SET e=lexp { match Ast_undecorated.remove_deco e with 
               | E_tuple[E_var x;e1;e2] -> E_array_set(x,e1,e2) 
               | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                   ~msg:"... array set" () }
| x=IDENT LBRACKET e1=exp RBRACKET
| x=IDENT DOT LPAREN e1=exp RPAREN
   { E_array_get(x,e1) }
| x=IDENT LBRACKET n=INT_LIT RBRACKET DOT_LENGTH 
  { E_matrix_size(x,n) }
| x=IDENT e1=dot_get { E_array_get(x,e1) }
| x=IDENT e1=dot_get es=dot_get+ { E_matrix_get(x,e1::es) }
/* | x=IDENT e1=dot_get es=dot_get+ DOT_LENGTH 
  { E_matrix_size(x,List.length es - 1) }*/
| x=IDENT DOT_LENGTH { E_array_length x }
| ARRAY_LENGTH a=aexp
| LENGTH a=aexp { match un_annot a with
                  | E_var x -> E_array_length x 
                  | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                          ~msg:"array length: should be a variable......" () }
| x=IDENT LBRACKET e1=exp RBRACKET LEFT_ARROW e2=app_exp
| x=IDENT e1=dot_get LEFT_ARROW e2=app_exp 
  { E_array_set(x,e1,e2) }
| x=IDENT e1=dot_get es=dot_get+ LEFT_ARROW e2=app_exp 
  { E_matrix_set(x,e1::es,e2) }
| SIZE_CREATE n=INT_LIT { E_const(C_size n) }
| e=aexp  es=aexp+
      { match e::es with
        | [e1;e2] -> (match un_annot e1 with
                      | E_var _ | E_const _ | E_fun _ -> 
                        E_app(e1,e2)
                      | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                   ~msg:"expression in functional position should be a variable or a constante" ())
        | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                   ~msg:"All functions and primitives should be unary. Hints: use a tuple as argument" () }
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
| REGISTER ev=exp LAST e0=aexp
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
| MACRO_GENERATE ef1=aexp e_init2=aexp e_st3=aexp
  { let z = Ast.gensym () in
    E_generate((P_var z,E_app(ef1,E_var z)),e_init2,e_st3,with_file $loc) }

| EXEC e1=exp 
    {
        Prelude.Errors.raise_error ~loc:(with_file $loc)
            ~msg:"missing ``default'' close; `exec e default e` expected" ()
    }
| EXEC e1=exp DEFAULT
    {
        Prelude.Errors.raise_error ~loc:(with_file $loc)
            ~msg:"missing expression after keyword ``default''; `exec e default e` expected" ()
    }
| REGISTER e1=exp LAST
    {
        Prelude.Errors.raise_error ~loc:(with_file $loc)
            ~msg:"missing expression after keyword ``last''; `reg e last e` expected" ()
    }
| INIT_TUPLE LT k=INT_LIT GT e=exp 
    { 
        E_tuple (List.init k (fun i ->  E_app(e,E_const(Int(i,unknown()))))) 
    }
| INIT_INT LT k=INT_LIT GT e=exp 
    { 
        E_app(E_const (Op(Runtime(Int_of_tuple k))),
              E_tuple (List.init k (fun i -> E_app(e,E_const(Int(i,unknown()))))))
    }


| e=aexp { e }




dot_get:
DOT LPAREN e=exp RPAREN { e }


aexp:
  e=aexp_desc { mk_loc (with_file $loc) e }

aexp_desc:
| BANG ex=aexp { E_get(ex) }
| LPAREN e=exp RPAREN { e }
| LPAREN e=exp COL ty=ty RPAREN { ty_annot ~ty e }
| c=const { E_const c }

| RESIZE_INT LT k=INT_LIT GT { E_const (Op(Runtime(Resize_int k))) }
| TUPLE_OF_INT LT k=INT_LIT GT { E_const (Op(Runtime(Tuple_of_int k))) }
| INT_OF_TUPLE LT k=INT_LIT GT { E_const (Op(Runtime(Int_of_tuple k))) }
| VECTOR_MAPI e=aexp {
                match Ast_undecorated.remove_deco e with
                | E_tuple[v;e] -> assert (is_variable v || evaluated v); (* todo: error *)
                                  let x = Ast.gensym () in
                                  E_vector_mapi(false,(P_var x,E_app(v,E_var x)), e, unknown ())
                | _ -> assert false (* todo error *) }
| INT_MAPI e=aexp {
                match Ast_undecorated.remove_deco e with
                | E_tuple[v;e] -> assert (is_variable v || evaluated v); (* todo: error *)
                                  let x = Ast.gensym () in
                                  E_int_mapi(false,(P_var x,E_app(v,E_var x)), e, unknown ())
                | _ -> assert false (* todo error *) }
| VECTOR_CREATE e=exp {
                match Ast_undecorated.remove_deco e with
                | E_tuple[v;e] -> (match Ast_undecorated.remove_deco v with
                                   | E_const(Int(n,_)) ->
                                      E_app(E_const(Op(Runtime(Vector_make))),
                                            E_tuple[E_const(C_size n);e])
                                   | _ -> assert false)
                | _ -> assert false (* todo error *) 
            }
| x=UNROLL AT k=INT_LIT { E_const (Op(Runtime(Unroll k))) }
| x=IDENT { match x with
            | "vect_make" -> E_const (Op(Runtime(Vector_make)))
            | "vect_size" | "vector_length" -> E_const (Op(Runtime(Vector_length(unknown(),unknown()))))
            | "vect_nth"| "vector_get" -> E_const (Op(Runtime(Vector_get(unknown()))))
            | "vect_copy_with" -> E_const (Op(Runtime(Vector_update (unknown()))))
            | "abs" -> E_const (Op(Runtime(Abs)))
            | "print" -> E_const (Op(Runtime(Print)))
            | "print_string" -> E_const (Op(Runtime(Print_string)))
            | "print_int" -> E_const (Op(Runtime(Print_int)))
            | "print_newline" -> E_const (Op(Runtime(Print_newline)))
            | "string_length" -> E_const (Op(Runtime(String_length)))
            | "assert" -> E_const (Op(Runtime(Assert)))
            | "get_bit"| "nth_bit" -> E_const (Op(Runtime(GetBit)))
            | "update_bit" -> E_const (Op(Runtime(UpdateBit)))
            | "size_of_val" -> E_const (Op(Runtime(Size_of_val (unknown(),unknown()))))
            | "get" -> let x = gensym () in let y = gensym () in
                       E_fun(P_tuple[P_var x;P_var y], E_array_get(x,E_var y))
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
| FOR i=IDENT EQ e1=exp TO e2=exp DO e=exp DONE
      { let loop = gensym ~prefix:"loop" () in
        let n0 = gensym ~prefix:"n0" () in
        let n = gensym ~prefix:"n" () in
        E_letIn(P_var n0, e1,
        E_letIn(P_var n, e2,
        E_letIn(P_var loop,E_fix(loop,(P_var i,
                              E_if(E_app(E_const(Op(Runtime(Gt))),E_tuple[E_var i;E_var n]),
                                   E_const(Unit),
                                   E_letIn(P_unit,e,E_app(E_var loop,E_app(E_const(Op(Runtime(Add))),E_tuple[E_var i;E_const (Int (1,unknown()))])))))), 
                 E_app(E_var loop,E_var n0))))
                }

| MACRO_FOR x=IDENT EQ e_st1=exp TO e_st2=exp DO e=exp DONE 
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
| LBRACKET_PIPE cs=separated_nonempty_list(SEMI,const) PIPE_RBRACKET
    { C_vector cs }

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
