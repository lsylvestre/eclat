%{

  open Prelude
  open Operators
  open Types
  open Ast
  open Ast_mk

  (* location augmented we a file name *)
  let with_file loc =
    (!Current_filename.current_file_name, loc)

  let already_defined_types = Hashtbl.create 100 ;;
  
  let rename_new_type_ident x =
    match Hashtbl.find_all already_defined_types x with
    | [] -> Hashtbl.add already_defined_types x x; x
    | l -> let y = x ^"/"^string_of_int (List.length l+1) in
           Hashtbl.add already_defined_types x y;
           y

  let rename_from_defined_type x =
    match Hashtbl.find_opt already_defined_types x with
    | None -> x
    | Some y -> y

  let note_no_recursive_type ~loc x ty =
    (* if identifier x occurs in tyB, emit a note 
     saying that, in Eclat, types are not recursive *)
    if Types.alias_find_ty x ty then
      Prelude.Errors.note ~loc (fun fmt -> 
        Format.fprintf fmt "Types are not recursive@,")
  
  let check_no_free_type_variable_decl ~loc x' ty tyargs szs =
    let vs = Types.vars_of_ty ty in
    Vs.iter (fun u _ ->
              match u.name with
              | None -> () (* type variable without name are simply ignored *)
              | Some name ->
                 if (List.mem name tyargs || List.mem name szs)
                 then () else
                   Prelude.Errors.syntax_error
                           ~msg:("The type variable `"^name^"` is unbound in this type declaration ("^
                            x'^").")
                           loc;
                 ) vs

  let add_alias x (ty,szs,args) loc =
    Hashtbl.add Types.global_type_declarations x (Alias ((ty,szs,args),loc))


  let rec as_const loc e =
    match un_annot e with
    | E_const c -> c
    | E_tuple es -> C_tuple(List.map (as_const loc) es)
    | _ -> Format.fprintf Format.std_formatter "--->%a\n" Ast_pprint.pp_exp e;
    Prelude.Errors.raise_error ~loc:(with_file loc)
              ~msg:"this expression should be a constant" ()

  let hash_dur_tvar = Hashtbl.create 10 ;;
  let decl_dur_var x =
    if Hashtbl.mem hash_dur_tvar x then
    Hashtbl.find hash_dur_tvar x else
    (let v = (new_dur_unknown ~name:x ()) in
     Hashtbl.add hash_dur_tvar x v; v)

  let hash_size_tvar = Hashtbl.create 10 ;;
  let decl_size_var x =
    if Hashtbl.mem hash_size_tvar x then
    Hashtbl.find hash_size_tvar x else
    (let v = (new_size_unknown ~name:x ()) in
     Hashtbl.add hash_size_tvar x v; v)

  let hash_ty_tvar = Hashtbl.create 10 ;;
  let decl_ty_var x =
    if Hashtbl.mem hash_ty_tvar x then
    Hashtbl.find hash_ty_tvar x else
    (let v = (new_ty_unknown ~name:x ()) in
     Hashtbl.add hash_ty_tvar x v; v)

  let hash_tyB_tvar = Hashtbl.create 10 ;;
  let decl_tyB_var x =
    if Hashtbl.mem hash_tyB_tvar x then
    Hashtbl.find hash_tyB_tvar x else
    (let v = (new_tyB_unknown ~name:x ()) in
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
    | _ -> 0 (* todo *)


let check_abstract_type ~loc x szs tyB_list =
  let check_when_present szs' tyB_list' =
    (if List.compare_lengths szs szs' = 0 then ()
    else let szs_length = List.length szs' in
           Prelude.Errors.error ~loc 
             (fun fmt -> Format.fprintf fmt 
                            "type %s expects %d size parameter%s" 
                            x szs_length 
                            (if szs_length <= 1 then "" else "s")));
    if List.compare_lengths tyB_list tyB_list' = 0 then ()
    else let tyB_list_length = List.length tyB_list' in
         Prelude.Errors.error ~loc 
             (fun fmt -> Format.fprintf fmt 
                            "type %s expects %d type parameter%s" 
                            x tyB_list_length 
                            (if tyB_list_length <= 1 then "" else "s"))
  in
  match Hashtbl.find_opt Types.global_type_declarations x with
  | None ->
     if Hashtbl.mem Types.global_type_declarations x then () 
     else Prelude.Errors.error ~loc (fun fmt -> Format.fprintf fmt "Unbound type constructor %s" x)
  | Some(Alias((_,szs',ty_list'),_)) ->
      check_when_present szs' ty_list'
  | Some(Abstract((_,szs',tyB_list',_),_)) ->
      check_when_present szs' tyB_list'


  let special_operator_table = Prelude.hashtbl_of_list @@
     [ "let"; "node"; "rec" ; "ref" ; "pause"; "abs"; "not"; "print";
       "print_ascii"; "print_string" ; "print_newline"; "print_int";
       "string_length"; "array_length"; "get"; "set"; "int"; 
       "array_from_file" ]

  

%}

%token LPAREN RPAREN LCUR RCUR LBRACKET RBRACKET COMMA PIPE_PIPE PIPE_COMMA_PIPE EQ EQ_EQ COL SEMI HAT STATIC
%token LBRACKET_PIPE PIPE_RBRACKET
%token FUN AMP DOT REGISTER EXEC INIT DEFAULT RESET WHERE RETURNS PERCENT
%token NODE IMPLY MINUS_LCUR RCUR_MINUS_GT
%token MATCH WITH PIPE END
%token OF
%token LET REC AND IN IF THEN ELSE FIX
%token IMPURE
%token <string> IDENT UP_IDENT TVAR_IDENT TYB_VAR_IDENT DUR_VAR_IDENT SIZE_VAR_IDENT
%token <bool> BOOL_LIT
%token <int> INT_LIT
%token <char> CHAR_LIT
%token PLUS MINUS TIMES LT LE GT GE NEQ NOT MOD DIV AMP_AMP OR WHEN FBY MERGE
%token DOLLARD
%token XOR LAND LOR LXOR LSL LSR ASR RESIZE_INT VECT_CREATE TUPLE_OF_INT INT_OF_TUPLE
%token TUPLE_GET TUPLE_UPDATE
%token UNROLL AT AT_AT
%token CREATE GET_START GET_END
%token SIZE_CREATE
%token WITH_SIZES
%token EXTERNAL OPERATOR SHARED RUN
%token <string> SYM
%token <string> OPERATOR_IDENT
%token ASSERT
%token BY WCET

%token EOF
%token SEMI_SEMI
%token LEFT_ARROW RIGHT_ARROW
%token <string> STRING_LIT
%token QUOTE TYPE
%token MACRO_GENERATE
%token MACRO_FOR PARFOR FOR TO DO DONE
%token COL_EQ BANG QUESTION_MARK
%token IMMEDIATE
%token ARRAY_CREATE ARRAY_MAKE
%token INIT_TUPLE INIT_INT
%token VECTOR_MAPI INT_MAPI
%token EMIT SIGNAL LOOP TRAP EXIT SUSPEND
%token LT_LT GT_GT
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
%nonassoc LT_LT GT_GT
%nonassoc BOOL_LIT IDENT LPAREN

%start <((x * (ty * bool * Prelude.loc)) list * (x * (ty * (bool * int * bool * Prelude.loc))) list) 
          * (x * static) list * (x * (x * tyB) list) list * ((p*e) * Prelude.loc) list> pi
%start <e list> arguments_eof
%%

pi:
| ec=decl_external pi=pi 
  { let (ecs,efs),gs,ts,ds = pi in
    let (x,(t,shared,loc_x)) = ec in
    let check_no_ty_var t =
       let open Types in
       let vs = free_vars_of_type (Vs.empty,t) in
       if Vs.is_empty vs then () else
       Prelude.Errors.syntax_error
           ~msg:("type signature of external function "^x^" should not contain type unknowns")
           (with_file $loc(ec))
    in
     check_no_ty_var t;
    let f = String.capitalize_ascii x in
    let arg = gensym ~prefix:"arg" () in
    let mk e = mk_loc (with_file $loc(ec)) e in
    let v = mk @@ E_fun(P_var arg,(Types.new_ty_unknown(),Types.new_tyB_unknown()), 
         mk @@ E_run(f,(mk @@ E_var arg),gensym())) in
    (((f,(t,shared,loc_x))::ecs,efs), gs,    ts,    (((P_var x,v),with_file $loc)::ds))}
| ef=ext_operator pi=pi { let (ecs,efs),gs,ts,ds = pi in ((ecs,ef::efs), gs,    ts,    ds   ) }
| g=static pi=pi        { let (ecs,efs),gs,ts,ds = pi in ((ecs,    efs), g::gs, ts,    ds   ) }
| d=type_decl pi=pi     { match d with 
                          | None -> pi 
                          | Some (x,l) ->
                             let (ecs,efs),gs,ts,ds = pi in
                             ((ecs,    efs), gs,    (x,l)::ts, ds   )
                        }
| d=decl pi=pi          { let (ecs,efs),gs,ts,ds = pi in ((ecs,    efs), gs,    ts,    d::ds) }
| EOF                   { ([],[]),[],[],[] }

ext_operator:
| OPERATOR ws=WITH_SIZES? x=OPERATOR_IDENT COL t=ty imp=is_impure SEMI_SEMI 
   { Hashtbl.clear hash_size_tvar;
     let copy_t = Types.copy_ty t in
     (* copying [t] is very important for let-polymorphism *)
     let loc_x = with_file $loc(x) in
     let n = get_arity t in
     if n = 0 then Prelude.Errors.raise_error ~loc:(with_file $loc)
                     ~msg:("operator "^x^" should have a functional type") ();
     (x, (copy_t,(ws <> None,n,imp,loc_x))) }
%inline is_impure:
| AT IMPURE { true }
| {false}

decl_external:
| EXTERNAL x=IDENT COL t=ty SEMI_SEMI { (x, (t, false, (with_file $loc(x)))) }
| SHARED EXTERNAL x=IDENT COL t=ty SEMI_SEMI { (x, (t, true, (with_file $loc(x)))) }


static: /* todo: add loc and type annotation [tyopt] */
| LET STATIC x=IDENT EQ ec=aexp e=static_dim_exp SEMI_SEMI
    { let to_int e =
        match un_annot e with
        | E_const (Int(n,_)) -> n
        | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                          ~msg:("dimension for "^x^" should be an integer") ()
      in
      x,Static_array(e2c ec,to_int e)
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

decl:
| d=decl_all 
  { clear_tyvar_constraints();
    d }

decl_all:
| exp after_missing_semi_semi
/* | LET after_let(after_missing_semi_semi)*/
        { Prelude.Errors.syntax_error 
            ~msg:"';;' expected at the end of this declaration"
            (with_file ($loc)) }
| LET b=after_let(SEMI_SEMI?)
        { b,(with_file $loc) }
| NODE f=IDENT p=apat RETURNS p2=apat EQ eqs=equations SEMI_SEMI { 
    (let p',e = eqs in
        P_var f, E_fun(p,(Types.new_ty_unknown(),Types.new_tyB_unknown()),
     E_letIn(p',Types.new_ty_unknown(),e,Pattern.pat2exp p2))),(with_file $loc) }
| e=exp SEMI_SEMI { ((P_var "_", e),(with_file $loc))  }

after_missing_semi_semi:
LET | NODE | EOF | TYPE {}

type_decl_sizes2:
| { [] }
| LT szs=separated_nonempty_list(COMMA,size_unknown) GT { szs }

type_decl_ty_param_then_ident:
| x=IDENT
    { [],x }
| u=ty_or_TyB_unknown x=IDENT 
    { [u],x }
| LPAREN uu=separated_nonempty_list(COMMA,ty_or_TyB_unknown) RPAREN x=IDENT 
    { uu,x }


type_decl: 
| kw=TYPE t_with_params=type_decl_ty_param_then_ident
           szs=type_decl_sizes2 a=type_decl_end {
    let tyargs_with_annot,x = t_with_params in
    
    let check_all_tyB_vars ~msg_basic_type () =
      List.iter (fun (x,(locx,b)) -> if b then
          let open Prelude.Errors in
          warning ~loc:(with_file locx) (fun fmt -> 
          Format.fprintf fmt "basic type variable '%s should be prefixed with `~`.@," x);
          note ~loc:(with_file $loc) (fun fmt ->
            Format.fprintf fmt "%s\n" msg_basic_type)
        ) tyargs_with_annot
    in

    let tyargs = List.map fst tyargs_with_annot in

    (match Hashtbl.find_opt Types.global_type_declarations x with
    | None -> ()
    | Some ( Alias(_,loc') | Abstract(_,loc')) -> 
        let open Errors in
        note ~loc:(with_file $loc)
          (fun fmt ->
            Format.fprintf fmt "type %s overides a previous declaration of %s (%a)\n"
              x x (fun fmt -> emph_pp bold pp_loc fmt) loc'));
        let x' = rename_new_type_ident x in
        match a with
        | `Ty ty ->   
            check_no_free_type_variable_decl ~loc:(with_file $loc(kw)) x' ty tyargs szs;
            note_no_recursive_type ~loc:(with_file $loc) x ty; (* x is the previous name *)
            add_alias x' (ty,szs,tyargs) (with_file $loc);
            None
        | `R r ->
            check_all_tyB_vars ~msg_basic_type:"abstract types are basic types" ();
            let tyargs = List.map (fun x -> new_tyB_unknown ~name:x ()) tyargs in
            let szs = List.map (fun x -> new_size_unknown ~name:x ()) szs in
            let (op,intl) = r in
            Hashtbl.add Types.global_type_declarations x' (Abstract ((op,szs,tyargs,intl),with_file $loc));
            clear_tyvar_constraints ();
            None
        | `Sum tyBs ->
            check_all_tyB_vars ~msg_basic_type:"sum types are basic types" ();
            let tyB = TyB_sum tyBs in
            check_no_free_type_variable_decl ~loc:(with_file $loc(kw)) x' (Ty_base tyB) tyargs szs;
            note_no_recursive_type ~loc:(with_file $loc) x (Ty_base tyB); (* x is the previous name *)
            add_alias x' (Ty_base tyB,szs,tyargs) (with_file $loc);
            clear_tyvar_constraints();
             Some (x',tyBs) 
  }

type_decl_end:
| EQ ty=ty SEMI_SEMI? {`Ty ty }
| r=rest_abstract { `R r }
| EQ tyBs=separated_nonempty_list(PIPE,ty_case) SEMI_SEMI?  {`Sum tyBs }


%inline rest_abstract:
| SEMI_SEMI ?  { ("mul",[]) }
| AT op=IDENT SEMI_SEMI? { (op,[]) }
| AT op=INT_LIT SEMI_SEMI?  { (string_of_int op,[]) }
| op=IDENT LBRACKET intl=separated_nonempty_list(COMMA,INT_LIT) RBRACKET SEMI_SEMI?
   { (op,intl) }

ty_case:
| x=UP_IDENT OF ty=ty { x,Types.as_tyB ~loc:(with_file $loc) ty }
| x=UP_IDENT          { x,TyB_unit }

wcet_opt(In_kw):
| WCET d=dur In_kw { Some d }
| In_kw { None }


fun_decl(In_kw):
| f=IDENT p_ty_opt_list=arg_ty_atomic+ 
      ty_opt_ret=ret_ty_annot_eq
      e1=exp wo=wcet_opt(In_kw) { 
    let ps, ts = List.split @@ 
                    List.map (fun (p,ty_opt) -> 
                                match ty_opt with
                                | None -> p, new_ty_unknown()
                                | Some ty -> P_tyConstr(p,ty),ty
                      ) p_ty_opt_list in
    let p_ty_opt = group_ps ps, Some (group_ts ts) in
    let ef = mk_let_fun ~loc:(with_file ($startpos(f),$endpos(e1)))
                               ~p_ty_opt ~ty_opt_ret e1 in
    (P_var f, match wo with 
              | None -> ef
              | Some dur -> 
                 ty_annot ~ty:(Ty_fun(new_ty_unknown(),dur,new_tyB_unknown())) ef)
  }

size_param_fun_decl:
| LT_LT sz=size GT_GT { sz }

rec_kw: REC | SHARED {}

after_let(In_kw):
| b=bindings(apat,exp) In_kw { b }
| b=fun_decl(In_kw)          { b }
| e=fun_rec_decl(In_kw)      { e }
| rec_kw f_ty_opt=ty_annot(IDENT) EQ e1=exp In_kw {
    let f,ty_opt = f_ty_opt in
    let loc_fun = with_file ($startpos(f_ty_opt),$endpos(e1)) in
    let v = mk_fix f (ty_annot_opt ~ty:ty_opt e1) loc_fun in
    P_var f, v
  }
fun_rec_decl(In_kw):
| rec_kw f=IDENT p_ty_opt=arg_ty_atomic 
      ty_opt=ret_ty_annot_eq e1=exp In_kw {
    let p_ty_opt_f =
      let open Types in
      match p_ty_opt with
      | p,None -> p,None
      | p,Some t -> p,Some (Ty_fun(t,new_dur_unknown(),new_tyB_unknown()))
    in
    let loc_fun = with_file ($startpos(f),$endpos(e1)) in
    let (p,ty_f_opt) = p_ty_opt_f in
    let ef = mk_fun_ty_annot p ty_f_opt (ty_annot_opt ~ty:ty_opt e1)
           |> mk_loc loc_fun in
    let v = mk_fix f ef loc_fun in
    P_var f, v
  }

ty_annot(X) :
| x=X                                { x,None }
| x=X COL ty=ty                      { x,Some ty }
| LPAREN x_ty_opt=ty_annot(X) RPAREN { x_ty_opt }

size_list:
| {[]}
| LT sz=size GT { [sz] }
| LT szs=separated_nonempty_list(COMMA,size) GT { szs }

ty:
| ty=ty_tuple_next RIGHT_ARROW ty2=ty_tuple_next {
    Ty_fun(ty,Dur_int 1,Types.as_tyB ~loc:(with_file $loc) ty2) 
  } 
| ty=ty_tuple_next MINUS_LCUR d=dur RCUR_MINUS_GT ty2=ty_tuple_next {
    Ty_fun(ty,d,Types.as_tyB ~loc:(with_file $loc) ty2)
  } 
| ty=ty_tuple_next IMPLY ty2=ty_tuple_next {
    Ty_fun(ty,Dur_int 0,Types.as_tyB ~loc:(with_file $loc) ty2) 
  } 
| ty=ty_tuple_next { ty }

dur:
| n=INT_LIT {
    if n >= 0 then Dur_int n else
           Prelude.Errors.raise_error ~loc:(with_file $loc)
             ~msg:("duration literal should be postive") ()
  }

| x=IDENT LPAREN d1=dur COMMA d2=dur RPAREN {
    match x with
    | "max" -> Dur_max(d1,d2)
    | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc(x))
              ~msg:("unexpected duration operator "^x) ()
  }
| x=DUR_VAR_IDENT { decl_dur_var x }
| x=SIZE_VAR_IDENT { let sz = decl_size_var x in Dur_mulDiv(sz,Dur_int 1,Sz_lit 1) } 
| d1=dur PLUS d2=dur { Dur_add(d1,d2) }
| d1=dur DIV sz=size { Dur_mulDiv(Sz_lit 1,d1,sz) }
| sz=asize TIMES d2=dur { Dur_mulDiv(sz,d2,Sz_lit 1) }
| sz=asize {Dur_mulDiv(sz,Dur_int 1,Sz_lit 1) }
| LPAREN d=dur RPAREN {d}

asize:
| n=INT_LIT { assert (n >= 0); Sz_lit n }
| x=size_unknown { decl_size_var x }
| LPAREN sz=size RPAREN { sz }


ty_tuple_next:
| t1=apty0 TIMES ts=separated_nonempty_list(TIMES,apty0) {
    Ty_tuple(t1::ts)
  }
| ty=apty0 { ty }

apty0:
| tys=ty_list {
    match tys with
    | [t] -> t
    | _ -> assert false (* todo: err *) 
  }
| tys=ty_list SIGNAL {
    match tys with
    | [ty] -> Ty_signal (Types.as_tyB ~loc:(with_file $loc) ty) 
    | _ ->
        Prelude.Errors.error ~loc:(with_file $loc) (fun fmt ->
          Format.fprintf fmt "type constructor signal expect one basic type parameter\n")
  }
| tys=ty_list x=IDENT szs=size_list {
    let x = rename_from_defined_type x in
    let make_x_ty x =
      match Hashtbl.find_opt Types.global_type_declarations x with
      | Some (Alias ((_,args,szs0),_)) -> 
        Ty_alias(x,szs,tys)
      | _ -> 
        let tyBs = List.map (Types.as_tyB ~loc:(with_file $loc)) tys in
        check_abstract_type ~loc:(with_file $loc(x)) x szs tyBs;
        Ty_base (TyB_abstract(x,szs,tyBs))
    in
    if Hashtbl.mem Types.global_type_declarations x then make_x_ty x
    else
    match x with
    | "int" ->
        let open Prelude.Errors in 
        error ~loc:(with_file $loc) (fun fmt ->
          let open Format in
          fprintf fmt "type `int` does not expect type parameters\n"
        )
    | "ref" ->
       (let open Prelude.Errors in
        match tys,szs with
        | [ty],[] -> Ty_ref(Types.as_tyB ~loc:(with_file $loc) ty)
        | _,[] ->
            Prelude.Errors.raise_error ~loc:(with_file $loc)
              ~msg:"type `ref` expects one type parameter" ();
        | _ ->
            Prelude.Errors.raise_error ~loc:(with_file $loc)
              ~msg:"type `ref` does not expect size parameters" ())
    | "array" ->
       (let open Prelude.Errors in
        match tys,szs with
        | [ty],[sz] ->
            Ty_array(sz,Types.as_tyB ~loc:(with_file $loc) ty, new_label_unknown())
        | _,[_] ->
            Prelude.Errors.raise_error ~loc:(with_file $loc)
              ~msg:"type `array` expects one type parameter" ();
        | _ ->
            Prelude.Errors.raise_error ~loc:(with_file $loc)
              ~msg:"type `array` expects one size parameter" ())
    | _ ->
        make_x_ty x 
  }
apty:
| x=IDENT szs=size_list {
    let x = rename_from_defined_type x in
    let make_x_ty x =
      match Hashtbl.find_opt Types.global_type_declarations x with
      | Some (Alias ((_,args,szs0),_)) -> 
          Ty_alias(x,szs,[])
      | _ ->
          let tyBs = [] in
          check_abstract_type ~loc:(with_file $loc(x)) x szs tyBs;
          Ty_base (TyB_abstract(x,szs,tyBs))
    in
    if Hashtbl.mem Types.global_type_declarations x
    then make_x_ty x else 
    match x with
    | "_" -> (new_ty_unknown ())
    | "unit" ->
        if szs = [] then Ty_base TyB_unit else
        Prelude.Errors.raise_error ~loc:(with_file $loc)
          ~msg:"type unit expects no size parameter" ()
    | "bool" ->
        if szs = [] then Ty_base TyB_bool else
        Prelude.Errors.raise_error ~loc:(with_file $loc)
          ~msg:"type bool expects no size parameter" ()
    | "int" ->
        if (szs = []) then Ty_base (TyB_int(Sz_lit 32)) else
        (match szs with
        | [sz] -> Ty_base (TyB_int sz)
        | _ -> assert false)
    | "string" ->
        Ty_base (TyB_string (new_size_unknown()))
    | _ ->
      make_x_ty x }
| t=aty {t}

aty:
| tyB=ty_or_TyB_unknown_decl { tyB }
| LCUR v=record_ext { 
    let (bs,row) = v in
    Ty_base (TyB_record{fields=smap_of_list bs;row})
  }
| LT_LT sz=size GT_GT { Ty_size sz }
| LPAREN ty=ty RPAREN { ty }

record_ext:
| name=TYB_VAR_IDENT RCUR { [],new_tyB_unknown ~name () }
| RCUR         { [],TyB_unit }
| b=record_field_tyB v=record_ext_next { 
    let (l,row) = v in (b::l,row) 
  }

record_ext_next:
| SEMI name=TYB_VAR_IDENT RCUR { [],new_tyB_unknown ~name () }
| SEMI? RCUR        { [],TyB_unit }
| SEMI b=record_field_tyB  v=record_ext_next {
    let (l,row) = v in (b::l,row)
  }

record_field_tyB:
| x=IDENT COL ty=ty { x, Types.as_tyB ~loc:(with_file $loc(ty)) ty }

ty_list:
| LPAREN ts=ty_list RPAREN {ts}
| t=apty {[t]}
| LPAREN t1=ty COMMA ts=separated_nonempty_list(COMMA,ty) RPAREN
   { t1::ts }


size:
| LPAREN sz=size RPAREN { sz }
| n=INT_LIT { Sz_lit n }
| x=size_unknown { decl_size_var x }
| n=INT_LIT PLUS sz=size 
| sz=size PLUS n=INT_LIT { Sz_add(sz,n) }
| n=INT_LIT TIMES sz=size
| sz=size TIMES n=INT_LIT { 
     if n = 2 then Sz_twice(sz) 
     else Prelude.Errors.raise_error ~loc:(with_file $loc)
            ~msg:"unsupported size multiplication: only multiplication by two is currently supported.@," ()
  }
| n=INT_LIT HAT sz=size { 
     if n = 2 then Sz_pow2(sz) 
     else Prelude.Errors.raise_error ~loc:(with_file $loc)
            ~msg:"unsupported size exponentiation: only `2 to the power of a size` is currently supported.@," ()
  }

size_unknown:
| x=SIZE_VAR_IDENT { x }
| x=TVAR_IDENT {
    Prelude.Errors.warning ~loc:(with_file $loc(x)) (fun fmt -> 
          Format.fprintf fmt "size variable '%s should be prefixed with `?`.@," x);
    x
  }
| TYB_VAR_IDENT x=IDENT {
    Prelude.Errors.warning ~loc:(with_file $loc(x)) (fun fmt -> 
          Format.fprintf fmt "size variable ~%s should be prefixed with `?`.@," x);
    x
  }

ty_or_TyB_unknown:
| x=TYB_VAR_IDENT { (x,($loc(x),false)) }
| x=TVAR_IDENT    { (x,($loc(x),true)) }

ty_or_TyB_unknown_decl:
| x=TVAR_IDENT    { decl_ty_var x }
| x=TYB_VAR_IDENT { Ty_base (decl_tyB_var x) }


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
            E_letIn(P_unit,Ty_base TyB_unit, e1,e2)
        }
| e=lexp COMMA es=separated_nonempty_list(COMMA,lexp)
        {
            E_tuple (e::es)
        }

| e=lexp { e }

arg_ty_unparen:
| p=pat { p, None }
| p=apat COL ty=aty  {P_tyConstr(p,ty), Some ty} /* todo: check that Some(ty) is taken into account */

arg_ty:
| a=arg_ty_unparen
| LPAREN a=arg_ty_unparen RPAREN { a }

arg_ty_atomic:
| LPAREN p=pat RPAREN { p, None }
| sz=size_param_fun_decl { P_var (gensym ()), Some (Ty_size sz) }
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
| e=aexp WHERE REC bs=bindings_and(pat,exp) { 
          let (ps,es) = bs in
          let p = group_ps ps in
          E_letIn(p, Types.new_ty_unknown(),
              mk_loc (with_file $loc) @@
              E_app(E_var "fixpoint", 
                  mk_loc (with_file $loc) @@
                  E_fun(p,(Types.new_ty_unknown(),Types.new_tyB_unknown()),
                    mk_loc (with_file $loc) @@ group_es es)), e) } 
| IF e1=exp THEN e2_e3=if_end
        { let (e2,e3) = e2_e3 in E_if(e1,e2,e3) }
| LET b=after_let(IN) e2=exp
        { let (p,e1) = b in
          E_letIn(p,Types.new_ty_unknown(),e1,e2) }
| LPAREN e1=lexp PIPE_PIPE 
         es=separated_nonempty_list(PIPE_PIPE,lexp) 
  RPAREN
        {
            E_par(e1::es)
        }
| LBRACKET e1=lexp PIPE_PIPE 
         es=separated_nonempty_list(PIPE_PIPE,lexp)
  RBRACKET
        { let es' = e1::es in
          E_letIn(group_ps (List.map (fun _ -> P_unit) es'), 
                  Types.new_ty_unknown(), E_par(es'),E_const Unit)
        }

equations:
| REC eqs=separated_nonempty_list(AND,separated_pair(apat,EQ,exp)) 
| eqs=separated_nonempty_list(SEMI,separated_pair(apat,EQ,app_exp)) 
    { let p' = group_ps (List.map fst eqs) in
       p',E_app(E_var "fixpoint", E_fun(p', (Types.new_ty_unknown(),Types.new_tyB_unknown()),
                                    group_es (List.map snd eqs))) }

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
| p=P COL ty=ty EQ e=E { p,ty_annot ~ty e }
| p=P EQ e=E           { p,e }

app_exp:
  e=app_exp_desc { mk_loc (with_file $loc) e }

app_exp_desc:
| ASSERT e=aexp { E_assert(e,with_file ($loc(e))) }
| ARRAY_MAKE LT sz=size GT e=aexp { E_array_make(sz, e, with_file $loc) }
| ARRAY_CREATE LT sz=size GT LPAREN RPAREN
| CREATE LT sz=size GT LPAREN RPAREN { E_array_create(sz, (Ast.gensym ~prefix:"l" (), with_file $loc)) }
| e1=aexp e2=aexp AT v=lvalue
| e1=aexp e2=aexp AT_AT v=lvalue 
    { 
        E_app(e1,E_tuple[e2;v]) 
    }
| ex=aexp COL_EQ e=app_exp { E_set(ex,e) }
| GET_START e=lexp { match un_deco e with 
               | E_tuple[e0;e1] ->
                   (match un_deco e0 with
                    | E_var x -> E_array_get_start((x,loc_of e0),e1)
                    | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                             ~msg:"... array set start" ())
               | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                   ~msg:"... array set start" () }
| GET_END e=lexp { match un_deco e with 
                   | E_var x -> E_array_get_end(x,loc_of e) 
                   | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                   ~msg:"... array get end" () }
| IMMEDIATE x=IDENT e=lexp { 
    match x with
    | "set" -> (match un_annot e with
               | E_tuple[e0;e1;e2] -> 
                   (match un_annot e0 with
                   | E_var x -> mk_loc (with_file $loc) @@ 
                                E_array_set_immediate((x,loc_of e0),e1,e2)
                   | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                             ~msg:"first argument of `immediate set` should be a variable" ())
               | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                         ~msg:"`immediate set` expects 3 arguments" ())
    | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc(x))
              ~msg:("unknown operation `immediate "^x^"`") ()
  }
| SIZE_CREATE n=INT_LIT { E_const(C_size (Sz_lit n)) 
    (* [size_create<n>], or simply: <n>, see rule const_without_vect: *)}
| e=aexp  es=aexp+
      { match un_deco e,es with
        | E_const Ref,[e1] -> E_ref e1 (* note: due to un_deco, this 
                                            could result in a loss 
                                            of type annotation *)
        | E_const Get,[e1] -> let loc_x = loc_of e in
                                 (match un_deco e1 with
                                  | E_tuple[e1;e2] -> 
                                     (match un_deco e1 with
                                      | E_var x -> E_array_get((x, loc_x), e2) 
                                      | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                                               ~msg:"the first argument of function `get` should be a variable" ())
                                  | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                                           ~msg:"`get` expects 2 arguments" ())
        | E_const Set,[e1] -> let loc_x = loc_of e in
                                 (match un_deco e1 with
                                  | E_tuple[e1;e2;e3] ->
                                      (match un_deco e1 with
                                       | E_var x -> E_array_set((x, loc_x), e2,e3) 
                                       | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                                               ~msg:"the first argument of function `set` should be a variable" ())
                                  | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                                           ~msg:"`set` expects 3 arguments" ())
        | _ -> E_app((mk_loc (with_file $loc(e)) e), mk_loc (with_file $loc(es)) @@ group_es es) }
      (* 
        let es = match es with
                 | [] | [_] -> es
                 | e0::e1::es' ->
                   (match un_deco e0 with
                   | E_const(C_size _) -> 
                       (mk_loc (with_file $loc(es)) @@ E_tuple[e0;e1])::es'
                   | _ -> es) in *)

         (* List.fold_left (fun ef ei -> E_app(ef,ei)) e es *)
        (*match un_annot e::es with
        | [e1;e2] -> (match un_annot e1 with
                      | E_var _ | E_const _ | E_fun _ -> 
                          E_app(e,e2) (* NB: e is annotated, e1 is not *)
                      | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                   ~msg:"expression in functional position should be a variable or a constante" ())
    (* E_if(e2,e1,E_app(E_const (Op(Runtime(External_fun("Default.create",
        new_ty_unknown ())))),E_const(Unit))) *)
        | _ -> List.fold_left (fun ef ei -> E_app(ef,ei)) e es*)
| MINUS e1=aexp %prec prec_unary_minus { E_app(E_const(Op(Runtime(External_fun("Int.neg",new_ty_unknown ())))),e1) } 

| e1=app_exp k=WHEN e2=app_exp 
     { E_app((mk_loc (with_file $loc(k)) (E_var "when_")),
            (mk_loc (with_file $loc) @@ E_tuple[(mk_loc (with_file $loc(e1)) @@ E_fun(P_unit,
              (new_ty_unknown (),new_tyB_unknown ()),e1)); e2])) } 

| e1=app_exp op=binop e2=app_exp
        { E_app (mk_loc (with_file $loc(op)) @@ E_const (Op (Runtime(op))),
                 mk_loc (with_file $loc) @@ E_tuple [e1;e2])
        }
| e1=app_exp AMP_AMP e2=app_exp
        { let e3 = mk_loc (with_file $loc) @@ E_const (Bool false) in
          E_if(e1,e2,e3)
        }
(* | e1=app_exp OR e3=app_exp
        { let e2 = mk_loc (with_file $loc) @@ E_const (Bool true) in
          E_if(e1,e2,e3)
        }*)
| REGISTER ev=exp INIT e0=aexp
       { mk_loc (with_file $loc) @@
         match un_deco ev with
         | E_fun(p,(ty,tyB),e1) ->
            E_reg((p,tyB, mk_loc (with_file $loc(ev)) @@ ty_annot ~ty e1),e0,Ast.gensym ())
         | _ -> 
            let y = gensym () in
            E_reg((P_var y,Types.new_tyB_unknown(),
              mk_loc (with_file $loc(ev)) (E_app(ev,E_var y))),
              e0, Ast.gensym ())
       }
| EXEC e1=exp
       { E_exec(e1,E_app(E_const (Op(Runtime(External_fun("Default.create",new_ty_unknown ())))),E_const(Unit)),None,"") }
| EXEC e1=exp DEFAULT e2=lexp
       { E_exec(e1,e2,None,"") }
| EXEC e1=exp DEFAULT e2=lexp RESET e3=app_exp
       { E_exec(e1,e2,Some e3,"") }
| MACRO_GENERATE LT sz3=size TO sz4=size GT ef1=aexp INIT e_init2=aexp
  { let z = Ast.gensym () in
    E_generate((P_var z,(Types.new_ty_unknown(),Types.new_tyB_unknown()),  E_app(ef1,E_var z)),e_init2,sz3,sz4,with_file $loc) }

(* | EXEC e1=exp 
    {
        Prelude.Errors.raise_error ~loc:(with_file $loc)
            ~msg:"missing ``default'' close; `exec e default e` expected" ()
    }*)
| EXEC e1=exp DEFAULT
    {
        Prelude.Errors.raise_error ~loc:(with_file $loc)
            ~msg:"missing expression after keyword ``default''; `exec e default e` expected" ()
    }
/*| REGISTER e1=exp LAST
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
    }*/

| RUN i=UP_IDENT e=aexp 
     { E_run(i, e, gensym()) }
| e1=aexp RIGHT_ARROW e2=app_exp { E_app(E_var "arrow", E_tuple[e1;e2]) }


| EMIT x=IDENT e=aexp?
| EMIT x=UP_IDENT e=aexp? { 
    match e with None -> E_emit(x,E_const (Bool true))
    | Some e ->  E_emit(x,e) 
}
| SIGNAL e=aexp { E_sig_create e }
| SIGNAL NEQ { E_sig_create (E_app(E_const (Op(Runtime(External_fun("Default.create",new_ty_unknown ())))),E_const(Unit))) }
| e=aexp { e }
| TRAP LPAREN RPAREN { E_trap(new_tyB_unknown()) }
| TRAP x=UP_IDENT IN e=exp
| TRAP x=IDENT IN e=exp { E_letIn(P_var x,new_ty_unknown(),E_trap(new_tyB_unknown()), e) }
| EXIT x=IDENT
| EXIT x=UP_IDENT { E_exit(x,E_const(Unit)) }
| SUSPEND e=app_exp WHEN x=IDENT { E_suspend(e,x) }

after_dot:
| DOT x=IDENT { `R x }
| DOT LPAREN e1=exp RPAREN { `A e1 }
| DOT LPAREN e1=exp RPAREN LEFT_ARROW e2=app_exp { `W (e1,e2) }

lpar_unmatched: 
INIT | SEMI_SEMI | IN 
| REGISTER | EXEC | DEFAULT 
| RBRACKET | NODE | MATCH | WITH 
| END | RUN | IF | THEN | ELSE | RESET | WHERE 
| LET | REC | AND | FIX {}


aexp:
  e=aexp_desc { mk_loc (with_file $loc) e }

aexp_desc:
| /* QUESTION_MARK x=IDENT */ x=SIZE_VAR_IDENT { E_sig_get(x) }
| BANG ex=aexp { E_get(ex) }
| LPAREN e=exp RPAREN { e }
| LPAREN e=exp COL ty=ty RPAREN { ty_annot ~ty e }
(* | LPAREN RPAREN { E_const Unit } *)
| e=const_exp { e }
| p=LPAREN e=exp lpar_unmatched { 
    Prelude.Errors.syntax_error 
            ~msg:"This '(' might be unmatched"
            (with_file ($loc(p))) }
| v=VECT_CREATE LT k=size GT 
    { let tyB = new_tyB_unknown() in
      E_const (Op(Runtime(
        External_fun("Vect.create",
                     Ty_fun(Ty_base tyB,Dur_int 0,vect_ k tyB))))) }

| v=RESIZE_INT LT k=size GT 
    { E_const (Op(Runtime(
        External_fun("Int.resize",
                     Ty_fun(new_ty_unknown(),Dur_int 0,TyB_int k))))) }
| TUPLE_OF_INT LT k=INT_LIT GT { E_const (Op(Runtime(Tuple_of_int k))) }
| INT_OF_TUPLE LT k=INT_LIT GT { E_const (Op(Runtime(Int_of_tuple k))) }
| VECTOR_MAPI e=aexp {
                let loc_e = loc_of e in
                match un_deco e with
                | E_tuple[v;e] -> assert (is_variable v || evaluated v); (* todo: error *)
                                  let x = Ast.gensym () in
                                  E_vector_mapi(false,(P_var x,(Types.new_tyB_unknown(),Types.new_tyB_unknown()),E_app(v,E_var x)), 
                                     mk_loc loc_e e, new_size_unknown ())
                | _ -> assert false (* todo error *) }

| x=UNROLL AT k=INT_LIT { E_const (Op(Runtime(Unroll k))) }
| ae=aexp a=after_dot { match un_deco ae,a with 
                         | E_var x,`A e1 -> E_array_get((x,with_file $loc(ae)),e1)
                         | E_var x,`W (e1,e2) -> E_array_set((x,with_file $loc(ae)),e1,e2)
                         | _,`R y -> E_record_field(ae,y,new_tyB_unknown()) 
                         | _ -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                              ~msg:("variable expected") ()
                       }
| LCUR bs=separated_list(SEMI,record_binding) RCUR 
    { let rec loop m = function
      | [] -> ()
      | (x,_)::bs' -> if SMap.mem x m 
                      then Prelude.Errors.raise_error ~loc:(with_file $loc)
                              ~msg:("Two occurrences of field "^x^ " in the same record") ()
                      else loop (SMap.add x () m) bs'
      in loop SMap.empty bs;
      E_record(bs) }
| LCUR e1=aexp WITH x2=IDENT EQ e2=lexp RCUR { E_record_update(e1,x2,e2,new_tyB_unknown()) }
| x=IDENT { let open Ast_mk in
            let loc_x = with_file $loc(x) in
            let mk e = mk_loc loc_x e in
            mk @@ 
            match x with
            | "ref" -> mk @@ E_const Ref
            | "pause" -> let x = Ast.gensym() in
                         E_fun(P_var x, (Types.new_ty_unknown(),Types.new_tyB_unknown()), 
                           mk@@E_pause (Ast.gensym(), mk@@E_var x))
            | "abs" -> mk_prim "Int.absv"
            | "not" -> mk_prim "Bool.lnot"
            | "print" -> mk_prim "Print.print_value"
            | "print_ascii" -> mk_prim "Print.print_ascii"
            | "print_string" -> mk_prim "Print.print_string"
            | "print_newline" -> mk_prim "Print.print_newline"
            | "print_int" -> mk_prim "Int.print"
            | "string_length" -> mk_prim "Print.print_string"
            | "array_length" | "length" ->
                E_fun(P_var x, (Types.new_ty_unknown(),Types.new_tyB_unknown()), 
                          mk @@ E_array_length (x, loc_x))
            | "get" -> mk @@ E_const Get (* let x = gensym () in let y = gensym () in
                       E_fun(P_tuple[P_var x;P_var y], (Types.new_ty_unknown(),Types.new_tyB_unknown()), 
                          mk @@ E_array_get((x,(with_file $loc)),mk @@ E_var y))*)
            | "set" -> mk @@ E_const Set 
            | "int" -> mk @@ E_const (Op(Int_of_size loc_x))
            | "array_from_file" -> let arr = gensym() in
                                   let name = gensym() in 
                                   E_fun(P_tuple[P_var arr;P_var name],(Types.new_ty_unknown(),Types.new_tyB_unknown()),
                                     mk@@E_array_from_file(arr,mk@@E_var name))
            | "_" -> Prelude.Errors.raise_error ~loc:(with_file $loc)
                         ~msg:"wildcard \"_\" not expected." ()
            | _ -> assert (not @@ Hashtbl.mem special_operator_table x);
                   E_var x }

| MATCH e=exp WITH
    PIPE? cases=match_case_const*
    IDENT RIGHT_ARROW otherwise=exp END?
      { E_case(e,cases,otherwise) }

| MATCH e=exp WITH
    PIPE? rev_cases=match_cases END?
      { let (hs,eo) = rev_cases in
        E_match(e,List.rev hs,eo) }
| FOR i=IDENT EQ e1=exp TO e2=exp sz=by_do e=exp DONE
      { match Ast_undecorated.remove_deco e2 with
        | E_app(E_const(Op(Int_of_size loc_x)), E_sig_get(n)) ->
            let mk e = mk_loc loc_x e in
            E_for(i,e1,mk @@ E_app(mk @@ E_const(Op(Int_of_size loc_x)), 
                                   mk @@ E_const(C_size(decl_size_var n))),e,sz, with_file $loc)
        | _ -> E_for(i, e1,e2,e,sz, with_file $loc) 
        (* let loop = gensym ~prefix:"loop" () in
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
              *)  }

| PARFOR x=IDENT EQ sz1=size TO sz2=size DO e=exp DONE 
      { E_parfor(x,sz1,sz2,e,with_file $loc) }
| LOOP e=exp END { E_loop(e) }

by_do:
| DO { Types.Sz_lit 1 }
| BY sz=size DO { sz }

record_binding:
| x=IDENT EQ e1=lexp { x,e1 }

const_exp:
| c=const_without_vect { E_const c}
| LBRACKET es=separated_nonempty_list(COMMA,lexp) RBRACKET
    { E_vector es }

match_case_const:
| cs=separated_nonempty_list(PIPE,const) RIGHT_ARROW e=exp PIPE { (cs,e) }

match_cases:
| e=wildcard_case                         { [],Some e }
| h=match_case                            { [h],None }
| h=match_case PIPE rev_cases=match_cases { let (hs,eo) = rev_cases in h::hs,eo }

match_case:
| x=UP_IDENT p=apat RIGHT_ARROW e=exp { (x,(p,e)) }
| x=UP_IDENT RIGHT_ARROW e=exp { (x,(P_unit,e)) }

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
| x=IDENT { 
    if Hashtbl.mem special_operator_table x then 
          Prelude.Errors.error ~loc:(with_file @@ $loc)
             (fun fmt -> Format.fprintf fmt 
                            "special operator %s cannot be redefined" x);
    P_var x 
}
| LPAREN p=apat COL ty=ty RPAREN { P_tyConstr(p,ty) }
| LT_LT sz=size GT_GT            { P_tyConstr(P_var (Ast.gensym ()),Ty_size sz) }

(* const_with_neg_int:
| c=const | LPAREN c=const_with_neg_int RPAREN  { c }
| MINUS n=INT_LIT { Int(- n,unknown()) }
*)

const:
| c=const_without_vect {c}
| LBRACKET cs=separated_nonempty_list(COMMA,const) RBRACKET
    { C_vector cs }

const_without_vect:
| LPAREN RPAREN { Unit }
| b=BOOL_LIT    { Bool b }
| c=CHAR_LIT    { Char c }
| n=INT_LIT     { Int (n,new_size_unknown()) }
| n=INT_LIT QUOTE k=INT_LIT { 
    if Float.log2 (float n) >= float (k-1) then
       Prelude.Errors.raise_error ~loc:(with_file $loc)
          ~msg:("Integer literal "^
                string_of_int n^
                " exceeds the range of representable integers of type int<"^
                string_of_int k ^">") ()
      else Int (n,Sz_lit k) 
  }
| s=STRING_LIT           { String s }
| x=OPERATOR_IDENT       { Op(Runtime(External_fun(x,new_ty_unknown()))) }
| x=UP_IDENT             { Inj x }
| LT_LT sz=size GT_GT    { C_size sz }
| LT_LT GT_GT            { C_size (new_size_unknown()) }
| LPAREN op=binop RPAREN { Op(Runtime(op)) }

%inline binop:
/*| x=SYM        { let y = match Hashtbl.find_opt infix_operators x with 
                       | None -> x
                       | Some y -> y in
                 External_fun(y,new_ty_unknown ())
                }*/
| OR         { External_fun("Bool.lor",new_ty_unknown ()) }
| PLUS       { External_fun("Int.add",new_ty_unknown ()) }
| MINUS      { External_fun("Int.sub",new_ty_unknown ()) }
| TIMES      { External_fun("Int.mul",new_ty_unknown ()) }
| DIV        { External_fun("Int.div",new_ty_unknown ()) }
| MOD        { External_fun("Int.modulo",new_ty_unknown ()) }
| LT         { External_fun("Int.lt",new_ty_unknown ()) }
| GT         { External_fun("Int.gt",new_ty_unknown ()) }
| LE         { External_fun("Int.le",new_ty_unknown ()) }
| GE         { External_fun("Int.ge",new_ty_unknown ()) }
| EQ | EQ_EQ { External_fun("Values.equal",new_ty_unknown ()) }
| NEQ        { External_fun("Int.neq",new_ty_unknown ()) }
| AMP        { External_fun("Bool.land",new_ty_unknown ()) }
| XOR        { External_fun("Bool.lxor",new_ty_unknown ()) }
| LXOR       { External_fun("Int.lxor",new_ty_unknown ()) }
| LAND       { External_fun("Int.land",new_ty_unknown ()) }
| LOR        { External_fun("Int.lor",new_ty_unknown ()) }
| LSL        { External_fun("Int.lsl",new_ty_unknown ()) }
| LSR        { External_fun("Int.lsr",new_ty_unknown ()) }
| ASR        { External_fun("Int.asr",new_ty_unknown ()) }

arguments_eof:
| es=separated_list(SEMI,lexp) EOF { es }

