open Types
open Ast

let print_signature_flag = ref false

let relax_flag = ref false

let pp_ty = Ast_pprint.pp_ty

let monomorphic = ref false
let signatures = Hashtbl.create 10

let accept_ref_arg_flag = ref true (* see lambda-lifting.ml *)

let rec occur v ty =
  let exception Found in
  let rec f = function
  | T_var {contents=Unknown v'} ->
      if v = v' then raise Found
  | T_var {contents=Ty ty} -> f ty
  | T_const(TInt tz) -> f tz
  | T_const _ -> ()
  | T_tuple ts ->
      List.iter f ts
  | T_fun{arg;dur;ret} ->
      f arg; f dur; f ret
  | T_string tz ->
      f tz
  | T_sum cs ->
      List.iter (fun (_,ty) -> f ty) cs
  | T_ref(t) -> f t
  | T_array{elem=t;size=tz} ->
      f t; f tz
  | T_matrix{elem=t;size=tz} ->
      f t; f tz
  | T_static t -> f t
  | T_forall(_,t1,t2) ->
      f t1; f t2
  | T_size _ -> ()
  | (T_response_time _ | T_infinity) -> ()
  | T_max(tz1,tz2) | T_add(tz1,tz2) | T_le(tz1,tz2) ->
      f tz1; f tz2
  in try f ty; false with Found -> true

  module Vs = Set.Make(Int)

  type scheme = Forall of (Vs.t * ty)

  let vars_of_type t =
    let rec vars s = function
    | T_const(TInt tz) -> vars s tz
    | T_const _ -> s
    | T_tuple ts ->
       List.fold_left vars s ts
    | T_var vt ->
         (match !vt with
             Unknown n ->
               Vs.add n s
           | Ty t ->
               vars s t)
    | T_fun{arg;dur;ret} ->
        vars (vars (vars s arg) dur) ret
    | T_string tz ->
        vars s tz
    | T_sum cs ->
        List.fold_left (fun s (_,t) -> vars s t) s cs
    | T_ref t -> vars s t
    | T_array{elem=t;size=tz} ->
        vars (vars s t) tz
    | T_matrix{elem=t;size=tz} ->
        vars (vars s t) tz
    | T_static tz -> vars s tz
    | T_forall(_,t1,t2) ->
        vars (vars s t1) t2
    | T_size _ -> s
    | T_response_time _ | T_infinity -> s
    | T_max(t1,t2) | T_add(t1,t2) | T_le(t1,t2) ->
        vars (vars s t1) t2
    in
    vars Vs.empty t

  let free_vars_of_type (bv,t) =
    Vs.diff (vars_of_type t) bv

(* let free_vars_of_type2 (bv,t) = (* generalize only the left part of a functional part *)
    match canon t with
    | T_fun{arg=t;dur=d;ret=r}
    |  T_var {contents=Ty (T_fun{arg=t;dur=d;ret=r})} ->
        let bv2 = Vs.union bv (vars_of_type r) in
        Vs.union (Vs.diff (vars_of_type t) bv2)
                 (Vs.diff (vars_of_type d) bv2)
    | t ->
       Vs.diff (vars_of_type t) bv
*)

  let instance (Forall(vs,ty)) =
    let unknowns = Hashtbl.create (Vs.cardinal vs) in
    Vs.iter (fun n -> Hashtbl.add unknowns n (unknown())) vs;
     let rec instance = function
       T_var {contents=(Unknown n)} as t ->
          (try Hashtbl.find unknowns n with Not_found -> t)
     | T_var {contents=(Ty t)} ->
         instance t
     | (T_const(TInt tz)) ->
         T_const(TInt (instance tz))
     | (T_const _) as t ->
         t
     | T_tuple ts ->
         T_tuple(List.map instance ts)
     | T_fun{arg;dur;ret} ->
          T_fun{ arg = instance arg;
                 dur = instance dur;
                 ret = instance ret }
     | T_string tz ->
        T_string(instance tz)
     | T_sum cs -> T_sum (List.map (fun (x,t) -> (x,instance t)) cs)
     | T_ref t -> T_ref (instance t)
     | T_array{elem=t;size=tz} ->
        T_array{elem=instance t;size=instance tz}
     | T_matrix{elem=t;size=tz} ->
        T_matrix{elem=instance t;size=instance tz}
     | T_forall(x,t1,t2) ->
        T_forall(x,instance t1,instance t2)
     | T_size _ as t -> t
     | (T_response_time _ | T_infinity) as t -> t
     | T_max(t1,t2) -> T_max(instance t1,instance t2)
     | T_add(t1,t2) -> T_add(instance t1,instance t2)
     | T_le(t1,t2) -> T_le(instance t1,instance t2)
     | T_static t1 -> T_static (instance t1)
     in
       instance ty

  type env = scheme SMap.t

  let free_vars_of_type_env l =
    List.fold_left (fun vs (x,(Forall (v,t))) ->
                    Vs.union vs (free_vars_of_type (v,t)) )
      Vs.empty l

  let generalize r ty =
    match canon ty with
    | T_var{contents=Ty (T_fun _)} | T_fun _ ->
        let fvg = free_vars_of_type_env r in
        Forall(free_vars_of_type (fvg,ty),ty)
    | ty -> Forall(Vs.empty,ty)

  let tvar_a = T_var (ref (Unknown (-1)))
  let tvar_b = T_var (ref (Unknown (-2)))
  let tvar_z = T_var (ref (Unknown (-3)))

  let id = function
  | T_var {contents=Unknown n} -> n
  | _ -> assert false

  let forall vs t =
    Forall(Vs.(of_list vs),t)



exception Cyclic of int * ty * Prelude.loc
exception CannotUnify of ty * ty * Prelude.loc


(* unify t1 and t2 with subtyping: t1 <= t2 *)
let rec unify ~loc t1 t2 =
  (* Format.fprintf Format.std_formatter "----> %a ~? %a\n" pp_ty t1 pp_ty t2; *)
  let unify_tconst tc1 tc2 =
    match tc1,tc2 with
    | TInt tz, TInt tz' ->
        unify ~loc tz tz'
    | TBool,TBool | TUnit,TUnit -> ()
    | _ -> raise @@ CannotUnify(t1,t2,loc)
  in
  (* Format.(fprintf std_formatter "%a / %a\n" pp_ty  t1 pp_ty t2); *)
  let t1,t2 = canon t1, canon t2 in
  match t1, t2 with
  | T_var {contents=(Unknown n)},
    T_var ({contents=Unknown m} as v) ->
      if n = m then () else v := Ty t1
  | T_var ({contents=Ty t1'}),
    T_var ({contents=Ty t2'}) ->
       unify ~loc t1' t2'
  | T_var ({contents=Ty t} as v),t' ->
       v := Ty t';
       unify ~loc t t'
  | T_var ({contents=Unknown n} as v),t ->
      if occur n t then raise (Cyclic(n,t,loc));
      v := Ty t
  | t1,(T_var _ as t2) ->
       (** NB: [t2 and t1] are swapped to preserve the direction
           of the subtyping relation (i.e. [t1 <= t2])
        *)
       unify ~loc t2 t1
  | T_const tc, T_const tc' ->
      unify_tconst tc tc'
  | T_tuple ts, T_tuple ts' ->
      if List.compare_lengths ts ts' <> 0 then raise (CannotUnify (t1,t2,loc));
      List.iter2 (unify ~loc) ts ts'
  | T_fun{arg;dur;ret},T_fun{arg=a;dur=d;ret=r} ->
      unify ~loc arg a;
      unify ~loc dur d;
      unify ~loc ret r
  | T_string tz,T_string tz' ->
      unify ~loc tz tz'
  | T_sum cs, T_sum cs' ->
      if List.compare_lengths cs cs' <> 0 then raise (CannotUnify (t1,t2,loc));
      List.iter2 (fun (x1,t1) (x2,t2) ->
        if x1 <> x2 then raise (CannotUnify (t1,t2,loc));
        unify ~loc t1 t2) cs cs'
  | T_ref t1', T_ref t2' ->
      unify ~loc t1' t2'
  | T_array{elem=t;size=tz},T_array{elem=t';size=tz'} ->
      unify ~loc t t';
      unify ~loc tz tz'
  | T_matrix{elem=t;size=tz},T_matrix{elem=t';size=tz'} ->
      unify ~loc t t';
      unify ~loc tz tz'
  | T_static t1', T_static t2' ->
      unify ~loc t1' t2'
  | T_forall(x,tt1,tt2), T_forall(x',tt1',tt2') ->
      if x <> x' then raise (CannotUnify (t1,t2,loc));
      unify ~loc tt1 tt1';
      unify ~loc tt2 tt2'
  | T_size n,T_size m ->
      if n <> m then raise (CannotUnify (t1,t2,loc))
  | (T_response_time _ | T_infinity | T_add _ | T_max _ | T_le _),
    (T_response_time _ | T_infinity | T_add _ | T_max _ | T_le _) ->
    let t1 = simplify_response_time t1 in
    let t2 = simplify_response_time t2 in
    (match t1, t2 with
    | T_response_time n, T_response_time m ->
        if n <> m then raise (CannotUnify (t1,t2,loc))
    | T_response_time n, T_add(T_response_time m,a)
    | T_add(T_size m,a), T_response_time n ->
        if n >= m then
          unify ~loc (T_response_time (n - m)) a
        else raise (CannotUnify (t1,t2,loc))
    | T_infinity, T_response_time _ | T_response_time _,T_infinity ->
        raise (CannotUnify (t1,t2,loc))
    | _ -> ())
    | _ -> raise (CannotUnify (t1,t2,loc))

exception PatTypeError

let rec ty_bindings ~loc p t =
  match p,canon t with
  | P_unit,T_const TUnit -> SMap.empty
  | P_var x,t -> SMap.singleton x t
  | P_tuple ps,T_tuple ts ->
      if List.compare_lengths ps ts <> 0 then
        let ts_expected = List.map (fun _ -> unknown ()) ps in
        raise (CannotUnify (T_tuple ts_expected,t,loc))
      else
      List.fold_left2 (fun m p t -> ty_bindings ~loc p t ++ m) SMap.empty ps ts
  | P_unit,t ->
      unify ~loc t (T_const TUnit);
      ty_bindings ~loc p t
  | P_tuple ps,t ->
      unify ~loc t (T_tuple (List.map (fun _ -> unknown ()) ps));
      ty_bindings ~loc p t


let initial_typing_env = SMap.empty

let env_extend ~loc ?(gen=false) g p scm = (* scm: scheme or type ?? *)
  g ++ SMap.map (fun t ->
                   let scm = if gen then generalize (SMap.bindings g) t
                              else Forall(Vs.empty,t) in
                 scm) (ty_bindings ~loc p scm)

exception UnboundVariable of x * Prelude.loc

let typ_ident g x loc =
  match SMap.find_opt x g with
  | None -> raise (UnboundVariable (x,loc))
  | Some t -> instance t

let ty_op ~loc op =
  match op with
  | Runtime p ->
      Operators.ty_op p
  | Wait n ->
      let v = unknown () in
      fun_ty v (T_response_time n) v
  | TyConstr ty ->
     let v = unknown() in
     unify ~loc ty v;
     fun_ty ty (T_response_time 0) v
  | GetTuple{pos;arity} ->
      let ts = List.init arity (fun _ -> unknown ()) in
      assert (0 <= pos && pos <= arity);
      fun_ty (group_ts ts) (T_response_time 0) (List.nth ts pos)

let rec typ_const ~loc g = function
| Int(n,tz) -> (* TODO, add a type constraint according to the size of the literal *)
    (match canon tz with
    | T_var({contents=Ty (T_size k)}) | T_size k ->
        if int_of_float (Float.ceil (Float.log2 (float (n+1)))) > k then (* unsigned *)
          let open Prelude.Errors in
          error ~loc (fun fmt ->
          Format.fprintf fmt
          "@[<v>Literal interger %d cannot be represented using an int<%d>.@]"
                n k)
    | _ -> () (* uncomplete verification, will be complete during type checking of the generate code *)
    );
    tint tz
| Bool _ -> tbool
| Unit -> tunit
| String s -> T_string (T_size (String.length s))
| Op op -> ty_op ~loc op
| (V_loc _) ->
    (* not in source program: handled in the typer *)
    unknown()
| C_tuple(cs) -> T_tuple(List.map (typ_const ~loc g) cs)
| Inj x ->
    typ_ident g x loc

(* seems not required *)
let rec non_expansive = function
  | E_deco(e,_) -> non_expansive e
  | E_app(e1,_) ->
      (match un_deco e1 with
      | E_const _ -> true
      | _ -> false)
  | E_if(e1,e2,e3) ->
      non_expansive e1 && non_expansive e2 && non_expansive e3
  | E_case(e1,hs,e_els) ->
      non_expansive e1 && List.for_all (fun (_,e) -> non_expansive e) hs && non_expansive e_els
  | E_match(e1,hs,eo) ->
      non_expansive e1 && List.for_all (fun (_,(_,ei)) -> non_expansive ei) hs &&
      (match eo with
       | None -> true
       | Some e -> non_expansive e)
  | E_tuple es ->
      List.for_all non_expansive es
  | E_letIn(_,e1,e2) ->
      non_expansive e1 && non_expansive e2
  | _ -> true (* ok ? *)

exception Functional

let base_type t =
  let exception Not_a_base_type in
  let rec aux t =
    match t with
    | T_const _ -> ()
    | T_var{contents=Unknown _} -> ()
    | T_var{contents=Ty t} -> aux t
    | T_tuple (ts) ->
        List.iter aux ts
    | T_fun _ ->
        raise Not_a_base_type
    | T_string _ ->
        raise Not_a_base_type
    | T_sum(cs) ->
        List.iter (fun (_,t) -> aux t) cs
    | T_ref _
    | T_array _
    | T_matrix _ -> raise Not_a_base_type
    | T_forall(_,_,t2) -> aux t2
    | (T_size _ | T_response_time _ | T_infinity | T_add _ | T_max _ | T_le _) ->
        assert false
    | T_static t1 -> aux t1
  in
  try aux t; true with
  | Not_a_base_type -> false

let check_base_type ~loc t =
  if base_type t then () else
  let open Prelude.Errors in
      error ~loc (fun fmt ->
      Format.fprintf fmt
        "@[<v>This expression has type %a. It should be a base type.@]"
                 (emph_pp green pp_ty) (canon t))


let rec contain_fun t =
  match t with
  | T_const _ -> ()
  | T_var{contents=Unknown _} -> ()
  | T_var{contents=Ty t} -> contain_fun t
  | T_tuple (ts) ->
      List.iter contain_fun ts
  | T_fun _ ->
      raise Functional
  | T_string _ -> ()
  | T_sum(cs) ->
      List.iter (fun (_,t) -> contain_fun t) cs
  | T_ref t1 -> contain_fun t1
  | T_array {elem=t;size=tz} -> contain_fun t
  | T_matrix {elem=t;size=tz} -> contain_fun t
  | T_forall(_,_,t2) -> contain_fun t2
  | T_size _ -> ()
  | (T_response_time _ | T_infinity | T_add _ | T_max _ | T_le _) -> ()
  | T_static t1 -> contain_fun t1

let check_conditional_shape ~loc e t =
  try contain_fun t with
  | Functional ->
      let open Prelude.Errors in
      error ~loc (fun fmt ->
      Format.fprintf fmt
        "@[<v>expression %a has type %a.@,Conditional cannot have a type\
        \ containing functional types.@,Note: it is a current limitation\
        \ of the compiler.@]" (* Hint: use eta-expansion. *)
                 (emph_pp purple Ast_pprint.pp_exp) e
                 (emph_pp green pp_ty) t)



let check_app_shape ~loc e tret =
  match e with
  | E_app(E_const(Op (TyConstr _)),_) -> ()
  | _ ->
    try contain_fun tret with
    | Functional ->
        let open Prelude.Errors in
        error ~loc (fun fmt ->
        Format.fprintf fmt
          "@[<v>expression %a has type %a.@,Application cannot have a type\
          \ containing functional types.@,Note: it is a current limitation\
          \ of the compiler.@]"
                   (emph_pp purple Ast_pprint.pp_exp) e
                   (emph_pp green pp_ty) tret)


(* Subtyping_relation *)
module Response_time = struct
  let zero = T_response_time 0
  let one = T_response_time 1
  let infinity = T_infinity
  let max n m = T_max (n,m)
  let add n m = T_add (n,m)
end

let is_TyConstr = function
| E_const(Op(TyConstr _)) -> true
| _ -> false

let typ_ident_static ~loc x statics =
  match List.assoc_opt x statics with
  | None ->
     let open Prelude.Errors in
      error ~loc (fun fmt ->
        Format.fprintf fmt
          "@[<v>variables %s should be defined at toplevel with keyword ``static''.@]"
                x)
  | Some t -> t

let typ_lc ~statics g ~loc lc =
  match lc with
  | St_var l' ->
      typ_ident g l' loc
  | St_const c ->
      typ_const ~loc g c

let trace_last_exp = ref (E_const Unit) (* fake *)

let rec typ_exp ?(collect_sig=false) ~statics ~sums ~toplevel ~loc (g:env) e =
  trace_last_exp := e;
  match e with
  | E_deco(e,loc) -> typ_exp ~collect_sig ~statics ~sums ~toplevel ~loc g e
  | E_const c ->
      (typ_const ~loc g c, Response_time.zero)
  | E_var(x) ->
      (* lookup *)
      let tx = if List.mem_assoc x statics then typ_ident_static ~loc x statics
               else typ_ident g x loc in
      (tx, Response_time.zero)
  | E_if(e1,e2,e3) ->
      let t1,n1 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e1 in
      let t2,n2 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e2
      and t3,n3 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e3 in
      unify ~loc t1 tbool;
      unify ~loc t2 t3;
      (** NB: [t2 <= t3] (according to the subtyping relation).
          So the conditional has type [t3] (that is not [t2] !) *)
      let t = t3 in
      (* check_base_type ~loc t; *)
      (t,Response_time.(add n1 (max n2 n3)))
  | E_case(e1,hs,e_els) ->
      let t1,n1 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e1 in
      List.iter (fun (c,_) -> unify ~loc (typ_const ~loc g c) t1) hs;
      let t_els,n_els = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e_els in
      let ns = List.map (fun (_,ei) ->
        let t,n = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g ei in
        unify ~loc:(loc_of ei) t_els t; n) hs in
      let n = Response_time.add n1 (List.fold_left Response_time.max n_els ns) in
      t_els,n
  | E_match(e1,hs,eo) ->
      let error_unbound_constructor ctor =
        Prelude.Errors.error ~loc (fun fmt -> Format.fprintf fmt "Unbound constructor %s" ctor)
      in
      let t1,n1 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e1 in
      let c_witness = match hs with (x,_)::_ -> x | _ -> assert false in
      let _,sum,_ = try Types.find_ctor c_witness sums
                    with Not_found -> error_unbound_constructor c_witness in
      let v = unknown () in
      let r = ref Response_time.zero in
      List.iter (fun (inj,(p,ei)) ->
         let t_inj = match List.assoc_opt inj sum with
                     | Some t -> t
                     | None -> error_unbound_constructor inj
         in
         let g' = env_extend ~loc g p t_inj in
         let t',dur = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g' ei in
         unify ~loc t' v;
         r := T_max(!r,dur)) hs;

      Option.iter (fun ew ->
         let t',dur = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g ew in
         unify ~loc t' v) eo;

      let ctors = smap_of_list (List.map (fun (x,_) -> x,()) hs) in

      if eo = None && SMap.cardinal ctors < List.length sum then (
        Prelude.Errors.error ~loc (fun fmt ->
          Format.fprintf fmt "This pattern-matching is not exhaustive.")
      );
      let t = !r in
      check_base_type ~loc t;
      v,T_add(n1,t)
  | E_tuple(es) ->
      let ts,ns = List.split @@ List.map (fun ei ->
                    typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g ei) es
      in
      let n = List.fold_left Response_time.add Response_time.zero ns in
      T_tuple ts,n
  | E_fun(p,e1) ->
      let v = unknown() in
      let g' = env_extend ~loc g p v in
      let t,dur = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g' e1 in
      check_base_type ~loc t;
      (* Format.fprintf Format.std_formatter "=========> %a\n" pp_ty ( v); *)
      (T_fun{arg=v;dur;ret=t}, Response_time.zero)
  | E_app(e1,e2) ->
      (match un_deco e1 with
      | E_const (Op (TyConstr ty)) ->
         let t2,n2 = typ_exp ~collect_sig ~statics ~sums ~toplevel:toplevel ~loc:(loc_of e2) g e2 in
         unify ~loc:(loc_of e2) ty t2;
         t2,n2
      | _ ->
        let t1,n1 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc:(loc_of e1) g e1 in
        let t2,n2 = typ_exp ~collect_sig ~statics ~sums ~toplevel:(toplevel && is_TyConstr e1) ~loc:(loc_of e2) g e2 in
        let t = unknown () in
        let n = unknown () in
        unify ~loc:(loc_of e1) (T_fun{arg=t2;dur=n;ret=t}) t1; (* t1 in second for subtyping *)
        (* Format.fprintf Format.std_formatter "~~~~~~====> %a\n" pp_ty (t); *)
        check_base_type ~loc t;
        if collect_sig then (
          match e1 with
          | E_var f -> Hashtbl.add signatures f t1
          | E_const _ -> ()
          | _ -> assert false (* compilation error ! *)
        );
        (t, Response_time.(add n (add n1 n2))))
  | E_letIn(p,e1,e2) ->
      let t1,n1 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e1 in
      let g' = env_extend ~loc ~gen:(not !monomorphic) g p t1 in
      (if toplevel && !print_signature_flag then
       begin
        let open Prelude.Errors in
        let open Format in
        if not (evaluated e1 || is_variable e1) then begin
          error ~loc (fun fmt ->
            fprintf fmt "Toplevel declarations like";
            let xs = vars_of_p p in
            pp_print_list
               ~pp_sep:(fun fmt () -> fprintf fmt ",")
                (fun fmt (x,_) -> fprintf fmt " %s" x) fmt (SMap.bindings xs);
            fprintf fmt " should be values or variables."
        ) end;
        fprintf std_formatter "val %a : %a\n" Ast_pprint.pp_pat p pp_ty (canon t1);
       end);
      let t2,n2 = typ_exp ~collect_sig ~statics ~sums ~toplevel ~loc:(loc_of e2) g' e2 in
      (t2, Response_time.add n1 n2)
  | E_fix(f,(x,e1)) ->
      let t1 = unknown () in
      let t2 = unknown () in
      let tf = T_fun{ arg = t1;
                      dur = Response_time.(add one (unknown()));
                      ret = t2 } in
      let g' = env_extend ~loc g (P_var f) tf in
      let t,n = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g' (E_fun(x,e1)) in
      (** NB: [t <= tf] (according to the subtyping relation) *)
      unify ~loc:(loc_of e1) tf t;
      (* check_base_type ~loc t1;*)
      check_base_type ~loc t2;
      (tf, n)
  | E_reg((p,e1),e0,_) ->
     let t0,n0 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e0 in
     let g' = env_extend ~loc g p t0 in
     let t1,n1 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g' e1 in
     unify ~loc t0 t1;
     unify ~loc n0 Response_time.zero;
     unify ~loc n1 Response_time.zero;
     check_base_type ~loc t0;
     (t0, Response_time.zero)
  | E_exec(e1,e2,_) ->
     let t1,_ = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e1 in
     let t2,n2 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e2 in
     unify ~loc t1 t2;
     unify ~loc n2 Response_time.zero;
     check_base_type ~loc:(loc_of e1) t1;
     (T_tuple[t1;tbool], Response_time.zero)
  | E_par(es) ->
    let ts,ns = List.split @@ List.map (fun ei ->
                    let ti,n = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g ei in
                    check_base_type ~loc:(loc_of ei) ti;
                    ti,n) es
    in
    let n = List.fold_left Response_time.max Response_time.zero ns in
    T_tuple ts,n
  | E_ref(e1) ->
      let t1,n = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e1 in
      check_base_type ~loc:(loc_of e1) t1;
      (T_ref(t1),n)
  | E_get(e1) ->
     let t1,n = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e1 in
     let t2 = unknown () in
     unify ~loc:(loc_of e1) (T_ref(t2)) t1;
     check_base_type ~loc t2;
     (t2, n)
  | E_set (e1,e2) ->
     let t1,n = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e1 in
     let t2,n = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e2 in  
     unify ~loc:(loc_of e1) (T_ref(t2)) t1;
     check_base_type ~loc t2;
     (T_const TUnit, n)
  | E_local_static_array (c,n) ->
     ((T_array{elem=typ_const ~loc g c; size=T_size n}),Response_time.zero)
  | E_array_length(x) ->
     let tx = typ_ident g x loc in
     unify ~loc (T_array{elem=unknown();size=unknown()}) tx;
     (tint (unknown()), Response_time.zero)
  | E_array_get(x,e1) ->
     let t1,n = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e1 in
     unify ~loc t1 (tint (unknown()));
     let tx = typ_ident g x loc in
     let t3 = unknown() in
     unify ~loc (T_array{elem=t3;size=(unknown())}) tx;
     (t3, n)
  | E_array_set(x,e1,e2) ->
     let t1,n = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e1 in
     let t2,m = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e2 in
     unify ~loc t1 (tint (unknown()));
     let t3 = typ_ident g x loc in
     unify ~loc (T_array{elem=t2;size=(unknown())}) t3;
     (T_const TUnit, T_add(n,m))
  | E_matrix_size(x,n) -> (* TODO: error if n >= dim(x) *)
     let tx = typ_ident g x loc in
     let tz = unknown() in
     unify ~loc (T_matrix{elem=unknown();size=tz}) tx;
     let t = tint (unknown()) in
     t, Response_time.zero
  | E_matrix_get(x,es) ->
     let ts,ns = List.split @@ List.map (fun ei ->
                    typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g ei) es in
     List.iter (fun ti -> unify ~loc ti (tint (unknown()))) ts;
     let tx = typ_ident g x loc in
     let t3 = unknown() in
     unify ~loc (T_matrix{elem=t3;size=T_tuple (List.map (fun _ -> unknown()) es)}) tx;
     let n = List.fold_left Response_time.add Response_time.zero ns in
     (t3, n)
  | E_matrix_set(x,es,e2) ->
     let ts,ns = List.split @@ List.map (fun ei ->
                    typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g ei) es in
     List.iter (fun ti -> unify ~loc ti (tint (unknown()))) ts;

     let t2,m = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e2 in
     let tx = typ_ident g x loc in
     unify ~loc (T_matrix{elem=t2;size=T_tuple (List.map (fun _ -> unknown()) es)}) tx;
     let n = List.fold_left Response_time.add m ns in
     (T_const TUnit, n)
  | E_absLabel(l,e1) ->
      let t2 = unknown() in
      let statics' = (l,t2)::statics in
      let t1,n1 = typ_exp ~collect_sig ~statics:statics' ~sums ~toplevel:false ~loc g e1 in
      T_forall(l,t2,t1), n1
  | E_appLabel(e1,l,lc) -> (* avoir un environnement des variables liées par grand lambda + statics *)
      let tl = typ_lc ~statics g ~loc lc in
      let t1,n1 = typ_exp ~collect_sig ~statics:((l,tl)::statics)
                           ~sums ~toplevel:false ~loc g e1 in
      let t2 = unknown() in
      unify ~loc t1 (T_forall(l,tl,t2));
      t2,n1
  | E_for(x,e_st1,e_st2,e3,_) ->
      let v = unknown() in
      let intv = tint v in
      let t1,n1 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e_st1 in
      unify ~loc t1 intv;
      unify ~loc n1 Response_time.zero;
      let t2,n2 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e_st2 in
      unify ~loc t2 intv;
      unify ~loc n2 Response_time.zero;
      let g' = env_extend ~loc g (P_var x) intv in
      typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g' e3
  | E_generate((p,e1),e2,e_st3,_) ->
      let t3,n3 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e_st3 in
      unify ~loc t3 (tint (unknown()));
      unify ~loc n3 Response_time.zero;

      let v = unknown() in
      let t2,n = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e2 in
      let intv = tint v in
      let g' = env_extend ~loc g p (T_tuple[intv;t2]) in
      let t1,n1 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g' e1 in
      t2,n1 (* n1+n1+ ... n fois *)


let typing_handler ?(msg="") f () =
  let open Prelude.Errors in
  try
    f ()
  with CannotUnify(t1,t2,loc) ->
    error ~loc (fun fmt ->
      Format.fprintf fmt "%s@,An expression has type %a but was expected of type %a"
                 msg
                (* (emph_pp purple Ast_pprint.pp_exp) !trace_last_exp *)
                 (emph_pp bold pp_ty) t1
                 (emph_pp bold pp_ty) t2)
  | Cyclic(n,t,loc) ->
      error ~loc (fun fmt ->
      Format.fprintf fmt "%s@,expression %a has a cyclic type %a\n"
          msg  (emph_pp purple Ast_pprint.pp_exp) !trace_last_exp
                 (emph_pp bold pp_ty) t)
  | UnboundVariable(x,loc) ->
      Prelude.Errors.raise_error ~loc ~msg:("unbound variable "^x) ()


let typing ?collect_sig ?(env=SMap.empty) ?(msg="") ~statics ~sums e =
  Hashtbl.clear signatures;
  typing_handler (fun () ->
    let t,n = typ_exp ?collect_sig ~statics ~sums ~toplevel:true ~loc:(loc_of e) env e in
    (match t with
    | T_fun{ret=t2} ->
        check_base_type ~loc:Prelude.dloc t2;
    | _ -> ());
    canon t, simplify_response_time n) ()


(** [fun_shape ty] returns a type [ty -{'a}-> 'b]
    where ['a] and ['b] are fresh type variable. *)
let fun_shape (t_arg : ty) : ty =
  fun_ty t_arg
         (unknown())
         (unknown())

let typing_static g =
  match g with
  | Static_array(c,n) ->
      let elem = typ_const ~loc:Prelude.dloc SMap.empty c in  (*todo loc *)
      T_array{elem;size=T_size n}
  | Static_matrix(c,n_list) ->
      let elem = typ_const ~loc:Prelude.dloc SMap.empty c in  (*todo loc *)
      T_matrix{elem;size=T_tuple (List.map (fun n -> T_size n) n_list) }
  | Static_const c ->
      typ_const ~loc:Prelude.dloc SMap.empty c

(** Typing of the program [e].
   The program must be a function of type [t1 -> t2] that takes
   an input flow [arg_list], each element of the flow being of type [t1].

   Returns the type of [e] and a type abstraction of its reponse time.
 *)
let typing_with_argument ?collect_sig ({statics;sums;main} : pi) (arg_list : e list) : ty * ty =

  let t_arg = unknown() in
  let env = SMap.empty in
  let env = List.fold_left (fun env (x,g) ->
                               let ty = typing_static g in
                                SMap.add x (Forall(Vs.empty,ty)) env) env statics
  in
  let env = List.fold_left (fun env (x,cases) ->
                               let t = T_sum (cases) in
                               List.fold_left (fun env (ctor,targ) ->
                                SMap.add ctor (generalize (SMap.bindings env) (targ ==> t)) env) env cases) env sums
  in
  let loc = loc_of main in
  let e = mk_loc loc @@ ty_annot ~ty:(fun_shape t_arg) main in
  let statics_env = List.map (fun (x,st) -> x,typing_static st) statics in
  let (ty,response_time) =
    typing ?collect_sig ~env ~statics:statics_env ~sums e
  in
  (if !relax_flag then () else
    let t = canon ty in
    match t with
    | T_fun{dur} ->
        if simplify_response_time (canon dur) <> T_response_time 0 then
          let open Prelude.Errors in
          error (fun fmt ->
          Format.fprintf fmt
        "@[<v>This program has type %a. It is not reactive. @]" (* Hint: use eta-expansion. *)
                 (emph_pp green pp_ty) t)
    | _ -> assert false);
  List.iter (fun a -> typing ~env ~msg:"checking inputs given by option -arg, " 
                         ~statics:statics_env ~sums
                         (ty_annot ~ty:t_arg a)
                      |> ignore) arg_list;

  (ty, response_time)


let when_repl : ((p * e) * Prelude.loc) -> unit =
  let r = ref SMap.empty in
  fun ((p,e),loc) ->
    let (ty,_) = typing ~env:!r ~statics:[] ~sums:[] e in
    r := typing_handler (fun () -> (env_extend ~loc ~gen:(not !monomorphic) !r p ty)) ();
    Format.fprintf Format.std_formatter "val %a : %a@."  Ast_pprint.pp_pat p pp_ty ty










module Typing2 = struct

  open Ty

  exception PatTypeError

  exception CannotUnify_tyB of Prelude.loc * ty * ty
  exception CannotUnify_ty of Prelude.loc * ty * ty
  exception CannotUnify_dur of Prelude.loc * dur * dur
  exception CannotUnify_size of Prelude.loc * size * size

  let rec unify_size ~loc sz1 sz2 =
    match canon_size sz1, canon_size sz2 with
    | sz1,Sz_var {contents=Is sz2}
    | Sz_var {contents=Is sz1},sz2 -> unify_size ~loc sz1 sz2
    | Sz_var {contents=(Unknown n)},
        Sz_var ({contents=Unknown m} as v) ->
        if n = m then () else v := Is sz1
    | Sz_var ({contents=(Unknown n)} as r1),sz2 ->
        r1 := Is sz2
    | sz1,Sz_var ({contents=(Unknown n)} as r2) ->
        r2 := Is sz1
    | _ -> raise @@ CannotUnify_size(loc,sz1,sz2)


  let rec unify_tyB ~loc tyB1 tyB2 =
    let tyB1,tyB2 = canon_tyB tyB1, canon_tyB tyB2 in
    (* Format.fprintf Format.std_formatter "-- ====> %a / %a\n"  pp_tyB  tyB1  pp_tyB  tyB2;*)
    match tyB1, tyB2 with
    | tyB1,TyB_var {contents=Is tyB2}
    | TyB_var {contents=Is tyB1},tyB2 -> unify_tyB ~loc tyB1 tyB2
    | TyB_bool,TyB_bool
    | TyB_unit,TyB_unit -> ()
    | TyB_int _, TyB_int _ -> () (* todo *)
    | TyB_tuple tyB_list1, TyB_tuple tyB_list2 ->
       if List.compare_lengths tyB_list1 tyB_list2 <> 0 then
         raise @@ CannotUnify_tyB(loc,Ty_base tyB1,Ty_base tyB2);
         List.iter2 (unify_tyB ~loc) tyB_list1 tyB_list2
    | TyB_var {contents=(Unknown n)},
      TyB_var ({contents=Unknown m} as v) ->
        if n = m then () else v := Is tyB1
    | TyB_var ({contents=(Unknown n)} as r1),tyB2 ->
        r1 := Is tyB2
    | tyB1,TyB_var ({contents=(Unknown n)} as r2) ->
        r2 := Is tyB1
    | _ -> raise @@ CannotUnify_tyB(loc,Ty_base tyB1,Ty_base tyB2)

  let rec unify_dur ~loc d1 d2 =
    let d1,d2 = canon_dur d1,canon_dur d2 in
    (* Format.fprintf Format.std_formatter "-- ====> %a / %a\n"  pp_dur  d1  pp_dur  d2;
    *)match d1,d2 with
    | Dur_zero,Dur_zero
    | Dur_one,Dur_one -> ()
    | d1,Dur_var {contents=Is d2}
    | Dur_var {contents=Is d1},d2 -> unify_dur ~loc d1 d2
    | Dur_var {contents=(Unknown n)},
      Dur_var ({contents=Unknown m} as v) ->
        if n = m then () else v := Is d1
    | Dur_var ({contents=Unknown n} as r),d
    | d,Dur_var ({contents=Unknown n} as r) ->
        r := Is d
    (* | Dur_one,Dur_var ({contents=Unknown n} as r) ->
         r := Is (Dur_one)*)
    | Dur_max(Dur_zero,d1),d2
    | Dur_max(d1,Dur_zero),d2
    | d1,Dur_max(d2,Dur_zero)
    | d1,Dur_max(Dur_zero,d2) -> unify_dur ~loc d1 d2
    | Dur_max(Dur_one,_),d
    | Dur_max(_,Dur_one),d
    | d,Dur_max(Dur_one,_)
    | d,Dur_max(_,Dur_one) ->  unify_dur ~loc d Dur_one
    | _ ->
      raise @@ CannotUnify_dur(loc,d1,d2)

  let rec unify_ty ~loc ty1 ty2 =
    let ty1,ty2 = canon_ty ty1, canon_ty ty2 in
    (*Format.fprintf Format.std_formatter "====> %a / %a\n"  pp_ty  ty1  pp_ty  ty2;
    *)
    match ty1,ty2 with
    (*
    | Ty_var ({contents=Unknown n} as r),Ty_base tyB2 ->
        r := Is (Ty_base tyB2)*)
    (*
    | Ty_var{contents=Is ty1}, ty2
    | ty1, Ty_var{contents=Is ty2} ->
        unify_ty ~loc ty1 ty2*)
    | Ty_var {contents=(Unknown n)},
      Ty_var ({contents=Unknown m} as v) ->
        if n = m then () else v := Is ty1
    | Ty_var ({contents=(Unknown n)} as r1),ty2 ->
        r1 := Is ty2
    | ty1,Ty_var ({contents=(Unknown n)} as r2) ->
        r2 := Is ty1

    (*| Ty_base tyB, Ty_var {contents=(Unknown n)} ->*)

(*
    | Ty_base ((TyB_var {contents=(Unknown n)}) as tyB),
      Ty_base (TyB_var ({contents=Unknown m} as v)) ->
        if n = m then () else v := Is tyB
    | Ty_base (Ty_var ({contents=(Unknown n)} as r1)),ty2 ->
        r1 := Is ty2
    | ty1,Ty_base (Ty_var ({contents=(Unknown n)} as r2)) ->
        r2 := Is ty1*)

    (* | Ty_base tyB1, Ty_base tyB2
       | Ty_base tyB1, Ty_var {contents=Is (Ty_base tyB2)}
       | Ty_var {contents=Is (Ty_base tyB2)},Ty_base tyB1 ->
        unify_tyB ~loc tyB1 tyB2
       | Ty_base tyB1, Ty_var ({contents=Unknown n} as r) ->
        r := Is (Ty_base tyB1)*)
    | Ty_base tyB1, Ty_base tyB2 ->
        unify_tyB ~loc tyB1 tyB2
    | Ty_base tyB, Ty_tuple ty_list
    | Ty_tuple ty_list,Ty_base tyB ->
        let tyB_list = List.map (fun _ -> new_tyB_unknown ()) ty_list in
        unify_tyB ~loc tyB (TyB_tuple tyB_list);
        List.iter2 (fun tyB ty -> unify_ty ~loc (Ty_base tyB) ty)
          tyB_list ty_list
    | Ty_tuple ty_list1, Ty_tuple ty_list2 ->
      if List.compare_lengths ty_list1 ty_list2 <> 0 then raise @@ CannotUnify_ty(loc,ty1,ty2);
      List.iter2 (unify_ty ~loc) ty_list1 ty_list2
    (* | Ty_tuple ty_list,Ty_base (TyB_tuple tyB_list)
       | Ty_base (TyB_tuple tyB_list), Ty_tuple ty_list ->
        if List.compare_lengths tyB_list ty_list <> 0 then raise @@ CannotUnify_tyB(loc,ty1,ty2);
        List.iter2 (fun tyB ty -> unify_ty ~loc (Ty_base tyB) ty)
          tyB_list ty_list*)
    (*   | Ty_base (TyB_tuple tyB_list),Ty_var ({contents=(Unknown n)} as r)

         | Ty_var ({contents=(Unknown n)} as r),Ty_base (TyB_tuple tyB_list) ->
           r := Is (Ty_base (TyB_tuple tyB_list))*)
    | Ty_fun(ty1,d1,tyB1),Ty_fun(ty2,d2,tyB2) ->
        unify_ty ~loc ty1 ty2; (* todo dur *)
        unify_dur ~loc d1 d2;
        unify_tyB ~loc tyB1 tyB2
    | Ty_ref(tyB1),Ty_ref(tyB2) ->
        unify_tyB ~loc tyB1 tyB2
    | Ty_array(sz1,tyB1),Ty_array(sz2,tyB2) ->
        unify_size ~loc sz1 sz2;
        unify_tyB ~loc tyB1 tyB2
    | Ty_matrix(sz_list1,tyB1),Ty_matrix(sz_list2,tyB2) ->
        List.iter2 (unify_size ~loc) sz_list1 sz_list2;
        unify_tyB ~loc tyB1 tyB2
    | Ty_base _,_ | _,Ty_base _ ->
        raise @@ CannotUnify_tyB(loc,ty1,ty2)
    | _ -> raise @@ CannotUnify_ty(loc,ty1,ty2)

  let rec subtyping_dur ~loc d1 d2 =
    let d1,d2 = canon_dur d1,canon_dur d2 in
    match d1,d2 with
    | Dur_zero,_
    | _,Dur_one -> ()
    | d1,Dur_var {contents=Is d2} -> subtyping_dur ~loc d1 d2
    | Dur_var {contents=Is d1},d2 -> subtyping_dur ~loc d1 d2
    | Dur_var ({contents=Unknown n} as r),Dur_zero ->
        r := Is (Dur_zero)
    | Dur_one,Dur_var ({contents=Unknown n} as r) ->
        r := Is (Dur_one)
    | Dur_max(Dur_zero,d),Dur_zero
    | Dur_max(d,Dur_zero),Dur_zero -> subtyping_dur ~loc d Dur_zero
    (* | Dur_var ({contents=(Unknown n)} as v),
      Dur_var {contents=Unknown m} ->
      if n = m then () else v := Is d2*)
    | _ ->
       raise @@ CannotUnify_dur(loc,d1,d2)
    (* | d,({contents=Unknown n} as r) ->
        let v = new_dur_unknown () in
        r := Is (Dur_add(d,v))*)

  let rec ty_bindings ~loc p ty =
    match p,canon_ty ty with
    | P_var x,t -> SMap.singleton x t
    | P_tuple ps,Ty_tuple ts ->
        if List.compare_lengths ps ts <> 0 then
          let _ts_expected = List.map (fun _ -> unknown ()) ps in
          raise (PatTypeError) (*  (Ty_tuple ts_expected,t,loc))*)
        else
          List.fold_left2 (fun m p t -> ty_bindings ~loc p t ++ m) SMap.empty ps ts
    | P_unit,ty ->
        unify_ty ~loc ty (Ty_base TyB_unit);
        SMap.empty
    | P_tuple ps,ty ->
        let ty_list = List.map (fun _ -> new_ty_unknown ()) ps in
        unify_ty ~loc ty (Ty_tuple ty_list);
        List.fold_left2 (fun m p t -> ty_bindings ~loc p t ++ m) SMap.empty ps ty_list


  let env_extend ~loc ?(gen=false) g p scm = (* scm: scheme or type ?? *)
    g ++ SMap.map (fun t ->
        let scm = if gen then generalize (SMap.bindings g) t
          else Forall(Vs.empty,t) in
        scm) (ty_bindings ~loc p scm)

  exception UnboundVariable of x * Prelude.loc

  let typ_ident g x loc =
    match SMap.find_opt x g with
    | None -> raise (UnboundVariable (x,loc))
    | Some t -> instance t

  let group_tyB_list = function
    | [] -> TyB_unit
    | [tyB] -> tyB
    | tyB_list -> TyB_tuple tyB_list

  let ty_op op =
    match op with
    | Runtime(p) ->
        Operators.ty_op2 p
    | Wait n ->
        let tyB = new_tyB_unknown () in
        Ty_fun(Ty_base tyB,Dur_one,tyB)
    | TyConstr ty -> new_ty_unknown () (* (* todo *)
                                          let v = unknown() in
                                          unify ~loc ty v;
                                          fun_ty ty (T_response_time 0) v
                                          Ty_fun(Ty_base tyB,Dur_zero,tyB) *)
    | GetTuple{pos;arity} ->
        let tyB_list = List.init arity (fun _ -> new_tyB_unknown ()) in
        assert (0 <= pos && pos <= arity);
        Ty_fun(Ty_base (group_tyB_list tyB_list),
               Dur_zero,
               List.nth tyB_list pos)

  let rec typ_const g = function
    | Int(n,sz) ->
        let v = new_size_unknown () in
        TyB_int v
(*
| Int(n,sz) ->
    (match canon_size sz with
    | Sz_var({contents=Is (Sz_lit k)}) | Sz_lit k ->
        if int_of_float (Float.ceil (Float.log2 (float (n+1)))) > k then (* unsigned *)
          let open Prelude.Errors in
          error ~loc (fun fmt ->
          Format.fprintf fmt
          "@[<v>Literal interger %d cannot be represented using an int<%d>.@]"
                n k)
    | _ -> () (* uncomplete verification, will be complete during type checking of the generate code *)
    );
    Ty_base(TyB_int (sz))*)
    | Bool _ -> TyB_bool
    | Unit -> TyB_unit
    | String s -> TyB_string (Sz_lit (String.length s))
    (* | Op op -> ty_op op*)
    (*| (V_loc _) ->
        (* not in source program: handled in the typer *)
        unknown()*)
    | C_tuple(cs) -> TyB_tuple(List.map (typ_const g) cs)
  (* | Inj x ->
      typ_ident g x loc *)

  let rec typ_exp ?(collect_sig=false) ~statics ~sums ?(toplevel=false) ~loc g e =
    match e with
    | E_const (Op op) -> ty_op op,Dur_zero
    | E_const c ->
        (Ty_base (typ_const g c), Dur_zero)
    | E_var(x) ->
        (* lookup *)
        (typ_ident g x Prelude.dloc,Dur_zero)
    | E_deco(e1,loc) ->
        typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e1
    | E_if(e1,e2,e3) ->
        let t1,d1 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e1 in
        let t2,d2 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e2 in
        let t3,d3 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e3 in
        unify_ty ~loc t1 (Ty_base TyB_bool);
        let vTyB = new_tyB_unknown () in
        unify_ty ~loc:(loc_of e2) t2 (Ty_base vTyB);
        unify_ty ~loc:(loc_of e3) t3 (Ty_base vTyB);
        t2,Dur_max(d1,Dur_max(d2,d3))
    | E_case(e1,hs,e_els) ->
        let ty1,d1 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e1 in
        List.iter (fun (c,_) -> unify_ty ~loc (Ty_base (typ_const g c)) ty1) hs;
        let ty_els,d_els = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e_els in
        let d_list = List.map (fun (_,ei) ->
          let ty,d = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g ei in
          unify_ty ~loc:(loc_of ei) ty_els ty; d) hs in
        let d = List.fold_left (fun d1 d2 -> Dur_max(d1,d2)) d_els d_list in
        ty_els,Dur_max(d,d1)

    | E_letIn(p,e1,e2) ->
        let ty1,d1 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc:(loc_of e1) g e1 in
        (* Format.fprintf Format.std_formatter "--->%a\n" pp_ty  ty1;*)
        let g' = env_extend ~loc:Prelude.dloc ~gen:true(* :(non_expansive e1)*) g p ty1 in
      (*    Format.fprintf Format.std_formatter "--->%a\n" pp_ty  ty2;*)

        (if toplevel && !print_signature_flag then
           begin
             let open Prelude.Errors in
             let open Format in
             if not (evaluated e1 || is_variable e1) then begin
               error ~loc (fun fmt ->
                   fprintf fmt "Toplevel declarations like";
                   let xs = vars_of_p p in
                   pp_print_list
                     ~pp_sep:(fun fmt () -> fprintf fmt ",")
                     (fun fmt (x,_) -> fprintf fmt " %s" x) fmt (SMap.bindings xs);
                   fprintf fmt " should be values or variables."
                 ) end;
             fprintf std_formatter "val %a : %a | %a\n"
             Ast_pprint.pp_pat p pp_ty (canon_ty ty1) pp_dur (canon_dur d1);
           end);

        let ty2,d2 = typ_exp ~collect_sig ~statics ~sums ~toplevel:true ~loc:(loc_of e2) g' e2 in

        (ty2, Dur_max(d1,d2))

    | E_tuple(es) ->
        let ts,ds = List.split @@ List.map (fun ei ->
            typ_exp ~collect_sig ~statics ~sums
              ~toplevel:false ~loc:(loc_of ei) g ei) es in
        let d = List.fold_left (fun d1 d2 -> Dur_max(d1,d2)) Dur_zero ds in
        Ty_tuple ts,d

    | E_fun(p,e1) ->
        let v = new_ty_unknown() in
        let g' = env_extend ~loc:Prelude.dloc g p v in
        let ty,dur = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc:(loc_of e1) g' e1 in
        let tyB = new_tyB_unknown() in
        unify_ty ~loc ty (Ty_base tyB);
        (Ty_fun(v,dur,tyB), Dur_zero)

    | E_fix(f,(x,e1)) ->
        let ty1 = new_ty_unknown () in
        let tyB2 = new_tyB_unknown () in
        let tf = Ty_fun(ty1,Dur_one,tyB2) in
        let g' = env_extend ~loc g (P_var f) tf in
        let ty,d = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g' (E_fun(x,e1)) in
        (* unify_ty ~loc:(loc_of e1) tf ty;*)
        (tf, d)
    | E_app(e1,e2) ->
        (match un_deco e1 with
         (*
        | E_const (Op (TyConstr ty)) ->
           let t2,d2 = typ_exp ~loc:(loc_of e2) g e2 in
           unify_ty ~loc:(loc_of e2) ty t2;
           t2,d2*)
         | _ ->
           let ty1,d1 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false g ~loc:(loc_of e1) e1 in
           let ty2,d2 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false g ~loc:(loc_of e2) e2 in
           let tyB = new_tyB_unknown () in
           let d = new_dur_unknown () in
           unify_ty ~loc (Ty_fun(ty2,d,tyB)) ty1;
         (* (match canon_ty ty1 with
            | Ty_fun(_,d2,_) -> subtyping_dur ~loc d2 d
            | _ -> assert false);*)
         (* if collect_sig then (
            match e1 with
            | E_var f -> Hashtbl.add signatures f t1
            | E_const _ -> ()
            | _ -> assert false (* compilation error ! *)
            );*)
         Ty_base tyB, Dur_max(Dur_max(d1,d2),d))


    | E_par(es) ->
        let ts,ds = List.split @@ List.map (fun ei ->
            let ti,d = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc:(loc_of ei) g ei in
          ti,d) es
        in
        let d = List.fold_left (fun d1 d2 -> Dur_max(d1,d2)) Dur_zero ds in
        Ty_tuple ts,d

    (* *************************************************** *)
    | E_reg((p,e1),e0,_) ->
        let ty0,d0 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e0 in
        let g' = env_extend ~loc g p ty0 in
        let ty1,d1 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g' e1 in
        unify_ty ~loc ty0 ty1;
        subtyping_dur ~loc d0 Dur_zero;
        subtyping_dur ~loc d1 Dur_zero;
        let tyB = new_tyB_unknown () in
        unify_ty ~loc:(loc_of e1) (Ty_base tyB) ty0;
        (ty0, Dur_zero)

    | E_exec(e1,e2,_) ->
        let ty1,_ = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e1 in
        let ty2,d2 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e2 in
        unify_ty ~loc ty1 ty2;
        subtyping_dur ~loc d2 Dur_zero;
        let tyB = new_tyB_unknown () in
        unify_ty ~loc:(loc_of e1) (Ty_base tyB) ty1;
        (Ty_base (TyB_tuple[tyB;TyB_bool]), Dur_zero)

    (* *************************************************** *)

    | E_ref(e1) ->
        let ty,d = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc:(loc_of e1) g e1 in
        Format.fprintf Format.std_formatter "|///|||--->%a\n" pp_ty  ty;
        let tyB = new_tyB_unknown () in
        unify_ty ~loc (Ty_base tyB) ty;
        Format.fprintf Format.std_formatter "||||--->%a\n" pp_tyB  tyB;
        (Ty_ref(tyB),d)
    | E_get(e1) ->
        let ty1,d = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e1 in
        let tyB = new_tyB_unknown () in
        unify_ty ~loc:(loc_of e1) (Ty_ref tyB) ty1;
        (Ty_base tyB, d)
    | E_set (e1,e2) ->
        let ty1,d1 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e1 in
        let ty2,d2 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e2 in
        let tyB = new_tyB_unknown () in
        unify_ty ~loc:(loc_of e1) (Ty_ref tyB) ty1;
        unify_ty ~loc:(loc_of e2) (Ty_base tyB) ty2;
        (Ty_base TyB_unit, Dur_max(d1,d2))

    (* *************************************************** *)

    | E_array_length(x) ->
        let tyx = typ_ident g x loc in
        let sz = new_size_unknown () in
        let v = new_tyB_unknown () in
        unify_ty ~loc (Ty_array(sz,v)) tyx;
        (Ty_base (TyB_int (new_size_unknown ())), Dur_zero)
    | E_array_get(x,e1) ->
        let ty1,d = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e1 in
        unify_ty ~loc ty1 (Ty_base (TyB_int (new_size_unknown ())));
        let tyx = typ_ident g x loc in
        let tyB = new_tyB_unknown () in
        unify_ty ~loc (Ty_array(new_size_unknown(),tyB)) tyx;
        (Ty_base tyB,d)
    | E_array_set(x,e1,e2) ->
        let ty1,d1 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e1 in
        let ty2,d2 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e2 in
        unify_ty ~loc ty1 (Ty_base (TyB_int (new_size_unknown ())));
        let tyx = typ_ident g x loc in
        let tyB = new_tyB_unknown () in
        unify_ty ~loc ty2 (Ty_base tyB);
        unify_ty ~loc (Ty_array(new_size_unknown(),tyB)) tyx;
        (Ty_base TyB_unit, Dur_max(d1,d2))


    (* | E_matrix_size(x,n) ->
         let tyx = typ_ident g x loc in
         let tz = new_size_unknown() in
         unify_ty ~loc (Ty_matrix(new_size_unknown (), new_tyB_unknown())) tyx;
         let ty = TyB_int (new_size_unknown()) in
         ty, Response_time.zero
*)
    | E_matrix_get(x,es) ->
       let ty_list,d_list = List.split @@ List.map (fun ei ->
                      typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g ei) es in
       List.iter (fun ty_i -> unify_ty ~loc ty_i (Ty_base (TyB_int (new_size_unknown())))) ty_list;
       let tyx = typ_ident g x loc in
       let tyB_elem = new_tyB_unknown() in
       unify_ty ~loc (Ty_matrix(List.map (fun _ -> new_size_unknown ()) ty_list, tyB_elem)) tyx;
       let d = List.fold_left (fun d1 d2 -> Dur_max(d1,d2)) Dur_zero d_list in
       (Ty_base tyB_elem, d)
    | E_matrix_set(x,es,e2) ->
       let ty_list,d_list = List.split @@ List.map (fun ei ->
                      typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g ei) es in
       List.iter (fun ty_i -> unify_ty ~loc ty_i (Ty_base (TyB_int (new_size_unknown())))) ty_list;
       let ty2,d2 = typ_exp ~collect_sig ~statics ~sums ~toplevel:false ~loc g e2 in
       let tyx = typ_ident g x loc in
       let tyB_elem = new_tyB_unknown() in
       unify_ty ~loc ty2 (Ty_base tyB_elem);
       unify_ty ~loc (Ty_matrix(List.map (fun _ -> new_size_unknown ()) ty_list, tyB_elem)) tyx;
       let d = List.fold_left (fun d1 d2 -> Dur_max(d1,d2)) d2 d_list in
       (Ty_base TyB_unit, d)

      | _ -> assert false


  let typing_handler ?(msg="") f () =
    let open Prelude.Errors in
    try f () with
      CannotUnify_ty(loc,t1,t2)
    | CannotUnify_tyB(loc,t1,t2) ->
        let t1,t2 = canon_ty t1, canon_ty t2 in
        error ~loc (fun fmt ->
          Format.fprintf fmt "%s@,An expression has type %a but was expected of type %a"
            msg
            (* (emph_pp purple Ast_pprint.pp_exp) !trace_last_exp *)
            (emph_pp bold pp_ty) t1
            (emph_pp bold pp_ty) t2)
       | CannotUnify_dur(loc,d1,d2) ->
           let d1,d2 = canon_dur d1, canon_dur d2 in
           error ~loc (fun fmt ->
             Format.fprintf fmt "%s@,An expression has response time %a but was expected of response time %a"
               msg
               (* (emph_pp purple Ast_pprint.pp_exp) !trace_last_exp *)
               (emph_pp bold pp_dur) d1
               (emph_pp bold pp_dur) d2)
       (* | Cyclic(n,t,loc) ->
           error ~loc (fun fmt ->
           Format.fprintf fmt "%s@,expression %a has a cyclic type %a\n"
          msg  (emph_pp purple Ast_pprint.pp_exp) !trace_last_exp
                 (emph_pp bold pp_ty) t)*)
       | UnboundVariable(x,loc) ->
         Prelude.Errors.raise_error ~loc ~msg:("unbound variable "^x) ()


  let typing ?collect_sig ?(env=SMap.empty) ?(msg="") ~statics ~sums e =
    Hashtbl.clear signatures;
    let loc = loc_of e in
    typing_handler (fun () ->
        let t,n = typ_exp ?collect_sig ~statics ~sums ~toplevel:true ~loc env e in
        let tyB = new_tyB_unknown () in
        unify_ty ~loc (Ty_fun(Ty_base tyB,new_dur_unknown(),new_tyB_unknown())) t;
        (* (match t with
           | T_fun{ret=t2} ->
            check_base_type ~loc:Prelude.dloc t2;
           | _ -> ());*)
        canon_ty t, n) ()



end

