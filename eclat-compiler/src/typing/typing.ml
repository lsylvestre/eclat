open Types
open Ast

let print_signature_flag = ref false

let relax_flag = ref false

let monomorphic = ref false
let signatures : (x,ty) Hashtbl.t = Hashtbl.create 10

let accept_ref_arg_flag = ref true (* see lambda-lifting.ml *)

let trace_last_exp = ref (E_const Unit) (* fake *)


exception PatTypeError

exception CannotUnify_tyB of Prelude.loc * ty * ty
exception CannotUnify_ty of Prelude.loc * ty * ty
exception CannotUnify_dur of Prelude.loc * dur * dur
exception CannotUnify_size of Prelude.loc * size * size

exception Cyclic_size of int * size * Prelude.loc
exception Cyclic_dur of int * dur * Prelude.loc
exception Cyclic_ty of int * ty * Prelude.loc

let rec unify_size ~loc sz1 sz2 =
  (*Format.fprintf Format.std_formatter "####-- ====> %a / %a\n"  pp_size  sz1  pp_size  sz2;
  *)let sz1, sz2 = canon_size sz1, canon_size sz2 in
  match sz1, sz2 with
  | sz1,Sz_var {contents=Is sz2}
  | Sz_var {contents=Is sz1},sz2 -> unify_size ~loc sz1 sz2
  | Sz_var {contents=(Unknown n)},
    Sz_var ({contents=Unknown m} as v) ->
    if n = m then () else v := Is sz1
  | Sz_var ({contents=(Unknown n)} as r1),sz2 ->
    if test_occur (occur_size n) sz2 then raise (Cyclic_size(n,sz2,loc));
    r1 := Is sz2
  | sz1,Sz_var ({contents=(Unknown n)} as r2) ->
    if test_occur (occur_size n) sz1 then raise (Cyclic_size(n,sz1,loc));
    r2 := Is sz1
  | Sz_lit n1, Sz_lit n2 ->
    if n1 <= 0 || n2 <= 0 then
      Prelude.Errors.error ~loc (fun fmt -> Format.fprintf fmt "type size should be strictly positive")
    else 
      if n1 <> n2 then raise @@ CannotUnify_size(loc,sz1,sz2)


let rec unify_dur ~loc d1 d2 =

  let warning_loss_of_precision d d' var =
    Prelude.Errors.warning ~loc (fun fmt ->
        Format.fprintf fmt "don't know how to unify durations %a and %a.\nVariable %a is arbitrarily set to 1 (this is safe but result in a loss of precision)"
          pp_dur d pp_dur d' pp_dur (Dur_var var))
  in

  let d1,d2 = canon_dur d1,canon_dur d2 in
  (* Format.fprintf Format.std_formatter "-- ====> %a / %a\n"  pp_dur  d1  pp_dur  d2;  *)
  match d1,d2 with
  | Dur_zero,Dur_zero
  | Dur_one,Dur_one -> ()
  | d1,Dur_var {contents=Is d2}
  | Dur_var {contents=Is d1},d2 ->
      unify_dur ~loc d1 d2
  | Dur_var {contents=(Unknown n)},
    Dur_var ({contents=Unknown m} as v) ->
      if n = m then () else v := Is d1
  | d1,Dur_var ({contents=Unknown n} as r2) ->
      if test_occur (occur_dur n) d1 then raise (Cyclic_dur(n,d1,loc));
      r2 := Is d1
  | Dur_var ({contents=Unknown n} as r1),d2 ->
      if test_occur (occur_dur n) d2 then raise (Cyclic_dur(n,d2,loc));
      r1 := Is d2
  | Dur_max(Dur_zero,d1),d2
  | Dur_max(d1,Dur_zero),d2
  | d1,Dur_max(d2,Dur_zero)
  | d1,Dur_max(Dur_zero,d2) -> unify_dur ~loc d1 d2
  | Dur_max(Dur_one,_),d
  | Dur_max(_,Dur_one),d
  | d,Dur_max(Dur_one,_)
  | d,Dur_max(_,Dur_one) ->
      unify_dur ~loc d Dur_one
  | Dur_max(d1,d2),Dur_zero
  | Dur_zero,Dur_max(d1,d2) ->
      unify_dur ~loc d1 Dur_zero;
      unify_dur ~loc d1 Dur_zero;
  | Dur_max(Dur_var ({contents=(Unknown n)} as r1),Dur_var ({contents=(Unknown m)} as r2)),d2 ->
    if n = m then let d = Dur_var r1 in r2 := Is d; unify_dur ~loc d d2 else
      (warning_loss_of_precision d1 d2 r1;
       r1 := Is Dur_one;
       unify_dur ~loc Dur_one d2)
  | _,Dur_max(Dur_var ({contents=(Unknown n)} as r3),Dur_var ({contents=(Unknown m)} as r4)) ->
    if n = m then let d = Dur_var r3 in r4 := Is d; unify_dur ~loc d1 d else
      (warning_loss_of_precision d1 d2 r3;
       r3 := Is Dur_one;
       unify_dur ~loc d1 Dur_one)
  | _ ->
    raise @@ CannotUnify_dur(loc,d1,d2)

(* let () =  (* test *)
   Printf.printf "===>\n";
   unify_dur ~loc:Prelude.dloc (Dur_max(new_dur_unknown(),new_dur_unknown()))
                              (Dur_max(new_dur_unknown(),new_dur_unknown())) *)

let rec unify_tyB ~loc tyB1 tyB2 =
  let tyB1,tyB2 = canon_tyB tyB1, canon_tyB tyB2 in
  (* Format.fprintf Format.std_formatter "&&      [tyB]====> %a / %a\n"  pp_tyB  tyB1  pp_tyB  tyB2; *)
  match tyB1, tyB2 with
  | TyB_var {contents=(Unknown n)},
    TyB_var ({contents=Unknown m} as v) ->
    if n = m then () else v := Is tyB1;
  | TyB_var ({contents=(Unknown n)} as r1),tyB2 ->
    if test_occur (occur_tyB n) tyB2 then raise (Cyclic_ty(n,Ty_base tyB2,loc));
    r1 := Is tyB2
  | tyB1,TyB_var ({contents=(Unknown n)} as r2) ->
    if test_occur (occur_tyB n) tyB1 then raise (Cyclic_ty(n,Ty_base tyB1,loc));
    r2 := Is tyB1
  | tyB1,TyB_var {contents=Is tyB2}
  | TyB_var {contents=Is tyB1},tyB2 -> unify_tyB ~loc tyB1 tyB2
  | TyB_bool,TyB_bool
  | TyB_unit,TyB_unit -> ()
  | TyB_int sz1, TyB_int sz2 ->
      (try unify_size ~loc sz1 sz2
      with CannotUnify_size _  -> 
        raise @@ CannotUnify_tyB(loc,Ty_base tyB1,Ty_base tyB2))
  | TyB_string sz1, TyB_string sz2 ->
    (try unify_size ~loc sz1 sz2
     with CannotUnify_size _ -> raise @@ CannotUnify_tyB(loc,Ty_base tyB1,Ty_base tyB2))
  | TyB_abstract(x1,szs1,tyB_list1),TyB_abstract(x2,szs2,tyB_list2) ->
      if x1 <> x2 || List.compare_lengths tyB_list1 tyB_list2 <> 0
                  || List.compare_lengths szs1 szs2 <> 0
      then raise @@ CannotUnify_tyB(loc,Ty_base tyB1,Ty_base tyB2) 
      else
      (try List.iter2 (unify_size ~loc) szs1 szs2;
           List.iter2 (unify_tyB ~loc) tyB_list1 tyB_list2
       with CannotUnify_size _ | CannotUnify_tyB _ -> 
        raise @@ CannotUnify_tyB(loc,Ty_base tyB1,Ty_base tyB2))

  | TyB_tuple tyB_list1, TyB_tuple tyB_list2 ->
    if List.compare_lengths tyB_list1 tyB_list2 <> 0 then
      raise @@ CannotUnify_tyB(loc,Ty_base tyB1,Ty_base tyB2);
    List.iter2 (unify_tyB ~loc) tyB_list1 tyB_list2
  | TyB_size sz1, TyB_size sz2 -> 
      (try unify_size ~loc sz1 sz2
      with CannotUnify_size _ ->
        raise @@ CannotUnify_tyB(loc,Ty_base tyB1,Ty_base tyB2))
  | TyB_sum(ctors),TyB_sum(ctors') ->
    if List.compare_lengths ctors ctors' <> 0 then raise (CannotUnify_tyB (loc,Ty_base tyB1,Ty_base tyB2));
    List.iter2 (fun (x1,tyB1) (x2,tyB2) ->
        if x1 <> x2 then raise (CannotUnify_tyB (loc,Ty_base tyB1,Ty_base tyB2));
        unify_tyB ~loc tyB1 tyB2) ctors ctors'
  | _ -> raise @@ CannotUnify_tyB(loc,Ty_base tyB1,Ty_base tyB2)

let rec unify_ty ~loc ty1 ty2 =
  let ty1,ty2 = canon_ty ty1, canon_ty ty2 in
  (* Format.fprintf Format.std_formatter "          [ty]====> %a / %a\n"  pp_ty  ty1  pp_ty  ty2;  *)
  match ty1,ty2 with
  | Ty_var {contents=(Unknown n)},
    Ty_var ({contents=Unknown m} as v) ->
    if n = m then () else v := Is ty1
  | (Ty_base(TyB_var {contents=(Unknown n)}),
     Ty_var ({contents=Unknown m} as v)) ->
    (* if n = m then () else*) v := Is ty1
  | (Ty_var ({contents=Unknown m} as v),
     Ty_base(TyB_var {contents=(Unknown n)})) ->
    (* if n = m then () else*) v := Is ty2
  (*| (Ty_base(TyB_var {contents=(Is tyB1)}),
    Ty_var ({contents=Unknown n} as r2)) ->
      if test_occur (occur_ty n) ty2 then raise (Cyclic_ty(n,ty2,loc));
      r2 := Is (Ty_base tyB1)
    | (Ty_var ({contents=Unknown n} as r1)),
    Ty_base(TyB_var {contents=(Is tyB2)}) ->
      if test_occur (occur_ty n) ty1 then raise (Cyclic_ty(n,ty1,loc));
      r1 := Is (Ty_base tyB2)*)
  | Ty_var ({contents=(Unknown n)} as r1),ty2 ->
    if test_occur (occur_ty n) ty2 then raise (Cyclic_ty(n,ty2,loc));
    r1 := Is ty2
  | ty1,Ty_var ({contents=(Unknown n)} as r2) ->
    if test_occur (occur_ty n) ty1 then raise (Cyclic_ty(n,ty1,loc));
    r2 := Is ty1
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
  | Ty_fun(ty1,d1,tyB1),Ty_fun(ty2,d2,tyB2) ->
    unify_ty ~loc ty1 ty2;
    unify_dur ~loc d1 d2;
    unify_tyB ~loc tyB1 tyB2
  | Ty_ref(tyB1),Ty_ref(tyB2) ->
    unify_tyB ~loc tyB1 tyB2
  | Ty_array(sz1,tyB1),Ty_array(sz2,tyB2) ->
    unify_size ~loc sz1 sz2;
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
      let _ts_expected = List.map (fun _ -> new_ty_unknown ()) ps in
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
  (* Format.(fprintf std_formatter "~~~~~%a/%b\n") Ast_pprint.pp_pat p gen; *)
  g ++ SMap.map (fun t ->
      let scm = if gen then generalize (SMap.bindings g) t
        else Forall(Vs.empty,t) in
      scm) (ty_bindings ~loc p scm)

exception UnboundVariable of x * Prelude.loc

let typ_ident ~loc g x =
  match SMap.find_opt x g with
  | None -> raise (UnboundVariable (x,loc))
  | Some t -> instance t


let typ_ident_static ~loc x statics =
  match List.assoc_opt x statics with
  | None -> raise @@ UnboundVariable(x,loc)
  | Some t -> t

let group_tyB_list = function
  | [] -> TyB_unit
  | [tyB] -> tyB
  | tyB_list -> TyB_tuple tyB_list

let ty_op ~externals ~loc op =
  let ty = 
    match op with
    | Runtime(p) ->
      Operators.ty_op ~externals p
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
  in
  let tyB1 = new_tyB_unknown () in
  let tyB2 = new_tyB_unknown () in
  unify_ty ~loc ty (Ty_fun(Ty_base tyB1,Dur_zero,tyB2));
  ty

let rec typ_const ~loc g = function
  | Int(n,sz) ->
    TyB_int sz
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
  | C_tuple(cs) -> TyB_tuple(List.map (typ_const ~loc g) cs)
  | C_vector(cs) ->
    let v = new_tyB_unknown () in
    List.iter (fun c -> unify_tyB ~loc v (typ_const ~loc g c)) cs;
    Operators.vect_ (Sz_lit (List.length cs)) v
  | C_size n -> TyB_size (Sz_lit n)
  | C_appInj(x,c,tyB) ->
    let tx = typ_ident ~loc g x in
    let tc = typ_const ~loc g c in
    unify_ty ~loc tx (Ty_fun(Ty_base tc,Dur_zero,tyB));
    tyB

  | Inj _ -> assert false (* to remove from constants *)
  | _ -> assert false

let rec typ_exp ?(collect_sig=false) ~statics ~externals ~sums ?(toplevel=false) ~loc g e =
  match e with
  | E_const (Op (Runtime(External_fun(x,tyx)))) ->
      let t = typ_ident ~loc g x in
      unify_ty ~loc tyx t;
      t, Dur_zero
  | E_const (Op op) -> 
      let t = ty_op ~externals ~loc op in
      (* Format.fprintf Format.std_formatter "|///|||--->%a\n" pp_ty  t; *)
      t,Dur_zero
  | E_const(Inj x) ->
    let tx = typ_ident ~loc g x in
    tx,Dur_zero
  | E_const c ->
    (Ty_base (typ_const ~loc g c), Dur_zero)
  | E_var(x) ->
    (* lookup *)
    let tx = (* try*) typ_ident ~loc g x
    (*with UnboundVariable _ -> typ_ident_static ~loc x statics*) in
    (tx,Dur_zero)
  | E_deco(e1,loc) ->
    typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e1
  | E_if(e1,e2,e3) ->
    let t1,d1 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e1 in
    let t2,d2 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e2 in
    let t3,d3 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e3 in
    unify_ty ~loc t1 (Ty_base TyB_bool);
    let vTyB = new_tyB_unknown () in
    unify_ty ~loc:(loc_of e2) t2 (Ty_base vTyB);
    unify_ty ~loc:(loc_of e3) t3 (Ty_base vTyB);
    t2,Dur_max(d1,Dur_max(d2,d3))
  | E_case(e1,hs,e_els) ->
    let ty1,d1 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e1 in
    List.iter (fun (cs,_) -> List.iter (fun c -> unify_ty ~loc (Ty_base (typ_const ~loc g c)) ty1) cs) hs;
    let ty_els,d_els = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e_els in
    let d_list = List.map (fun (_,ei) ->
        let ty,d = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g ei in
        unify_ty ~loc:(loc_of ei) ty_els ty; d) hs in
    let d = List.fold_left (fun d1 d2 -> Dur_max(d1,d2)) d_els d_list in
    ty_els,Dur_max(d,d1)

  | E_letIn(p,e1,e2) ->
    let ty1,d1 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc:(loc_of e1) g e1 in
    (* Format.fprintf Format.std_formatter "--->%a\n" pp_ty  ty1;*)
    let gen = evaluated e1 (* && match un_deco e1 with E_fix _ -> false | _ -> true *) in
    let g' = env_extend ~loc:Prelude.dloc ~gen g p ty1 in

    (if toplevel && !print_signature_flag then
       begin
         let open Prelude.Errors in
         let open Format in
         (* if not (evaluated e1 || is_variable e1) then begin
            error ~loc (fun fmt ->
               fprintf fmt "Toplevel declarations like";
               let xs = vars_of_p p in
               pp_print_list
                 ~pp_sep:(fun fmt () -> fprintf fmt ",")
                 (fun fmt (x,_) -> fprintf fmt " %s" x) fmt (SMap.bindings xs);
               fprintf fmt " should be values or variables."
             ) end; *)
         fprintf std_formatter "val %a : %a | %a\n"
           Ast_pprint.pp_pat p pp_ty (canon_ty ty1) pp_dur (canon_dur d1);
       end);

    let ty2,d2 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:true ~loc:(loc_of e2) g' e2 in

    (ty2, Dur_max(d1,d2))

  | E_tuple(es) ->
    let ts,ds = List.split @@ List.map (fun ei ->
        typ_exp ~collect_sig ~statics ~externals ~sums
          ~toplevel:false ~loc:(loc_of ei) g ei) es in
    let d = List.fold_left (fun d1 d2 -> Dur_max(d1,d2)) Dur_zero ds in
    Ty_tuple ts,d

  | E_fun(p,e1) ->
    let v = new_ty_unknown() in
    let g' = env_extend ~loc:Prelude.dloc g p v in
    let ty,dur = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc:(loc_of e1) g' e1 in
    let tyB = new_tyB_unknown() in
    unify_ty ~loc ty (Ty_base tyB);
    (Ty_fun(v,canon_dur dur,tyB), Dur_zero)

  | E_fix(f,(p,e1)) ->
    let ty1 = new_ty_unknown () in
    let tyB2 = new_tyB_unknown () in
    let tf = Ty_fun(ty1,Dur_one,tyB2) in
    let g' = env_extend ~loc g (P_var f) tf in
    let g' = env_extend ~loc g' p ty1 in
    let ty,d = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g' e1 in
    unify_ty ~loc:(loc_of e1) ty (Ty_base tyB2);
    (tf, Dur_zero)
  | E_app(e1,e2) ->
    (match un_deco e1 with
     | E_const (Op (TyConstr ty)) ->
       let ty2,d2 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false g ~loc:(loc_of e2) e2 in
       unify_ty ~loc:(loc_of e2) ty ty2;
       ty2,d2
     | _ ->
       let ty1,d1 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false g ~loc:(loc_of e1) e1 in
       let ty2,d2 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false g ~loc:(loc_of e2) e2 in
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
       if collect_sig then (
         match Ast_undecorated.remove_all e1 with
         | E_var f -> Hashtbl.add signatures f ty1
         | E_const _ -> ()
         | _ -> assert false (* compilation error ! *)
       );
       Ty_base tyB, canon_dur (Dur_max(Dur_max(d1,d2),d)))


  | E_par(es) ->
    let ts,ds = List.split @@ List.map (fun ei ->
        let ti,d = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc:(loc_of ei) g ei in
        ti,d) es
    in
    let d = List.fold_left (fun d1 d2 -> Dur_max(d1,d2)) Dur_zero ds in
    Ty_tuple ts,canon_dur d

  (* *************************************************** *)
  | E_reg((p,e1),e0,_) ->
    let ty0,d0 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e0 in
    let g' = env_extend ~loc g p ty0 in
    let ty1,d1 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g' e1 in
    unify_ty ~loc ty0 ty1;
    unify_dur ~loc d0 Dur_zero;
    unify_dur ~loc d1 Dur_zero;
    let tyB = new_tyB_unknown () in
    unify_ty ~loc:(loc_of e1) (Ty_base tyB) ty0;
    (ty0, Dur_zero)

  | E_exec(e1,e2,eo,_) ->
    let ty1,_ = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e1 in
    let ty2,d2 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e2 in
    unify_ty ~loc ty1 ty2;
    unify_dur ~loc d2 Dur_zero;
    let tyB = new_tyB_unknown () in
    unify_ty ~loc:(loc_of e1) (Ty_base tyB) ty1;
    Option.iter (fun e3 ->
        let ty3,d3 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e3 in
        let loc = loc_of e3 in
        unify_ty ~loc ty3 (Ty_base (TyB_bool));
        unify_dur ~loc d3 Dur_zero) eo;
    (Ty_base (TyB_tuple[tyB;TyB_bool]), Dur_zero)

  (* *************************************************** *)

  | E_match(e1,hs,eo) ->
    let error_unbound_constructor ctor =
      Prelude.Errors.error ~loc (fun fmt -> Format.fprintf fmt "Unbound constructor %s" ctor)
    in
    let ty1,d1 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e1 in
    (* todo : error if there is no clause *)
    let c_witness = match hs with (x,_)::_ -> x | _ -> assert false in
    let _,sum,_ = try Types.find_ctor c_witness sums
      with Not_found -> error_unbound_constructor c_witness in
    let ty_result = (Ty_base (new_tyB_unknown ())) in
    let r = ref d1 in
    List.iter (fun (inj,(p,ei)) ->
        let t_inj = match List.assoc_opt inj sum with
          | Some t -> Ty_base t
          | None -> error_unbound_constructor inj
        in
        let g' = env_extend ~loc g p t_inj in
        let tyi,di = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g' ei in
        unify_ty ~loc tyi ty_result;
        r := Dur_max(!r,di)) hs;

    Option.iter (fun ew -> 
        (* wildcard clause *)
        let tyw,dw = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g ew in
        unify_ty ~loc tyw ty_result;
        r := Dur_max(!r,dw)) eo;

    let ctors = smap_of_list (List.map (fun (x,_) -> x,()) hs) in

    if eo = None && SMap.cardinal ctors < List.length sum then (
      Prelude.Errors.error ~loc (fun fmt ->
          Format.fprintf fmt "This pattern-matching is not exhaustive.")
    );
    ty_result,!r

  (* *************************************************** *)

  | E_ref(e1) ->
    let ty,d = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc:(loc_of e1) g e1 in
    (* Format.fprintf Format.std_formatter "|///|||--->%a\n" pp_ty  ty; *)
    let tyB = new_tyB_unknown () in
    unify_ty ~loc (Ty_base tyB) ty;
    (* Format.fprintf Format.std_formatter "||||--->%a\n" pp_tyB  tyB; *)
    (Ty_ref(tyB),d)
  | E_get(e1) ->
    let ty1,d = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e1 in
    let tyB = new_tyB_unknown () in
    unify_ty ~loc:(loc_of e1) (Ty_ref tyB) ty1;
    (Ty_base tyB, d)
  | E_set (e1,e2) ->
    let ty1,d1 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e1 in
    let ty2,d2 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e2 in
    let tyB = new_tyB_unknown () in
    unify_ty ~loc:(loc_of e1) (Ty_ref tyB) ty1;
    unify_ty ~loc:(loc_of e2) (Ty_base tyB) ty2;
    (Ty_base TyB_unit, Dur_max(d1,d2))

  (* *************************************************** *)

  | E_array_make(sz,e1,_) ->
      let tyB = new_tyB_unknown() in
      let ty1,d = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e1 in
      unify_ty ~loc ty1 (Ty_base tyB);
      (Ty_array(sz,tyB)),Dur_one
  | E_array_create(sz,_) ->
      let tyB = new_tyB_unknown() in
      (Ty_array(sz,tyB)),Dur_zero
  | E_array_length(x) ->
    let tyx = typ_ident ~loc g x in
    let sz = new_size_unknown () in
    let v = new_tyB_unknown () in
    unify_ty ~loc (Ty_array(sz,v)) tyx;
    (Ty_base (TyB_int (new_size_unknown ())), Dur_zero)
  | E_array_get(x,e1) ->
    let ty1,d = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e1 in
    unify_ty ~loc ty1 (Ty_base (TyB_int (new_size_unknown ())));
    let tyx = typ_ident ~loc g x in
    let tyB = new_tyB_unknown () in
    unify_ty ~loc (Ty_array(new_size_unknown(),tyB)) tyx;
    (Ty_base tyB,dur_add d Dur_one)
  | E_array_set(x,e1,e2) ->
    let ty1,d1 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e1 in
    let ty2,d2 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e2 in
    unify_ty ~loc ty1 (Ty_base (TyB_int (new_size_unknown ())));
    let tyx = typ_ident ~loc g x in
    let tyB = new_tyB_unknown () in
    unify_ty ~loc ty2 (Ty_base tyB);
    unify_ty ~loc (Ty_array(new_size_unknown(),tyB)) tyx;
    (Ty_base TyB_unit, dur_add (Dur_max(d1,d2)) Dur_one)

  | E_for(x,e_st1,e_st2,e3,_) ->
    let  vsize1 = new_size_unknown() in
      let  vsize2 = new_size_unknown() in
    let intv1 = Ty_base (TyB_size vsize1) in
    let intv2 = Ty_base (TyB_size vsize2) in
    let ty1,d1 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e_st1 in
    unify_ty ~loc ty1 intv1;
    unify_dur ~loc d1 Dur_zero;
    let t2,d2 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e_st2 in
    unify_ty ~loc t2 intv2;
    unify_dur ~loc d2 Dur_zero;
    let g' = env_extend ~loc g (P_var x) (Ty_base (TyB_int (new_size_unknown()))) in
    let (ty3,d3) = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g' e3 in (* todo *)
    unify_ty ~loc ty3 (Ty_base TyB_unit);
    (Ty_base TyB_unit, d3)

  | E_generate((p,e1),e2,e_st3,_) ->
    let ty3,d3 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g e_st3 in
    let  vsize0 = new_size_unknown() in
    let intv0 = Ty_base (TyB_int vsize0) in
    unify_ty ~loc ty3 intv0;
    unify_dur ~loc d3 Dur_zero;
    let vsize1 = new_size_unknown() in
    let intv1 = Ty_base (TyB_int vsize1) in
    let ty2,d2 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc:(loc_of e2) g e2 in (* TODO: force ty2 to be a base type *)
    let g' = env_extend ~loc g p (Ty_tuple[intv1;ty2]) in
    let ty1,d1 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc:(loc_of e1) g' e1 in
    ty1,Dur_max(d1,d2) (* n1+n1+ ... n fois *)

  | E_vector(es) ->
    let v = new_tyB_unknown () in
    let ns = List.map (fun ei ->
        let t,n = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc g ei in
        unify_ty ~loc:(loc_of ei) t (Ty_base v);
        n) es
    in
    let n = List.fold_left (fun acc n -> Dur_max(acc,n)) Dur_zero ns in
    Ty_base(Operators.vect_ (Sz_lit(List.length es)) v),n

  | E_vector_mapi(_,(p,e1),e2,size_vect) ->
    let elem = new_tyB_unknown () in
    let vsize1 = new_size_unknown() in
    let intv1 = TyB_int vsize1 in
    let ty2,d2 = typ_exp ~collect_sig ~statics ~externals ~sums
                    ~toplevel:false ~loc:(loc_of e2) g e2 in (* TODO: force ty2 to be a base type *)
    let w = new_tyB_unknown () in
    unify_ty ~loc:(loc_of e2) (Ty_base w) ty2;
    unify_tyB ~loc:(loc_of e2) w (Operators.vect_ size_vect elem);
    let g' = env_extend ~loc g p (Ty_base (TyB_tuple[intv1;elem])) in 
    (* todo: better type error message, here, it says:
       "An expression has type (int<~z76>, ...) but was expected of type ..." 
    *)
    let ty1,d1 = typ_exp ~collect_sig ~statics ~externals ~sums
                      ~toplevel:false ~loc:(loc_of e2) g' e1 in
    let w1 = new_tyB_unknown () in
    unify_ty ~loc:(loc_of e1) (Ty_base w1) ty1;
    Ty_base (Operators.vect_  size_vect w1),d1 (* n times d1 *)


  | E_int_mapi(_,(p,e1),e2,_) ->
    let elem = TyB_bool in
    let size_vect = new_size_unknown() in
    let vsize1 = new_size_unknown() in
    let intv1 = TyB_int vsize1 in
    let ty2,d2 = typ_exp ~collect_sig ~statics ~externals ~sums
                       ~toplevel:false ~loc:(loc_of e2) g e2 in (* TODO: force ty2 to be a base type *)
    let w = new_tyB_unknown () in
    unify_ty ~loc:(loc_of e2) (Ty_base w) ty2;
    unify_tyB ~loc:(loc_of e2) w (TyB_int(size_vect));
    let g' = env_extend ~loc g p (Ty_base (TyB_tuple[intv1;elem])) in
    let ty1,d1 = typ_exp ~collect_sig ~statics ~externals ~sums ~toplevel:false ~loc:(loc_of e2) g' e1 in
    let w1 = new_tyB_unknown () in
    unify_ty ~loc:(loc_of e1) (Ty_base w1) ty1;
    (Ty_base (TyB_int(size_vect))),d1 (* n times d1 *)

  | E_run (i,e1) ->
    let ty1,d1 = typ_exp ~collect_sig ~statics ~externals ~sums
                       ~toplevel ~loc:(loc_of e1) g e1 in (* TODO: force ty2 to be a base type *)
    (match List.assoc_opt i (fst externals) with
    | Some (ty,shared) ->
        let d2 = (if shared then Dur_one else Dur_zero) in
        let tyB = new_tyB_unknown () in
        let d3 = new_dur_unknown() in
        let ty2 = Ty_fun(ty1, d3, tyB) in
        unify_ty ~loc ty ty2;
        Ty_base tyB,Dur_max(Dur_max(d1,d2),d3)
    | None -> Prelude.Errors.raise_error ~msg:("unbound external circuit "^i) ())

let typing_handler ?(msg="") f () =
  let open Format in
  let open Prelude.Errors in
  try f () with
    CannotUnify_ty(loc,t1,t2)
  | CannotUnify_tyB(loc,t1,t2) ->
    let t1,t2 = canon_ty t1, canon_ty t2 in
    error ~loc (fun fmt ->
        fprintf fmt "%s@,An expression has type %a but was expected of type %a"
          msg
          (emph_pp bold pp_ty) t1
          (emph_pp bold pp_ty) t2)
  | CannotUnify_dur(loc,d1,d2) ->
    let d1,d2 = canon_dur d1, canon_dur d2 in
    error ~loc (fun fmt ->
        Format.fprintf fmt "%s@,An expression has response time %a but was expected of response time %a"
          msg
          (emph_pp bold pp_dur) d1
          (emph_pp bold pp_dur) d2)
  | Cyclic_ty(n,t,loc) ->
    error ~loc (fun fmt ->
        fprintf fmt "%s@,expression %a has a cyclic type %a\n"
          msg  (emph_pp purple Ast_pprint.pp_exp) !trace_last_exp
          (emph_pp bold pp_ty) t)
  | Cyclic_size(n,sz,loc) ->
    error ~loc (fun fmt ->
        fprintf fmt "The type of size %a is cyclic\n"
          (emph_pp bold pp_size) sz)
  | Cyclic_dur(n,dur,loc) ->
    error ~loc (fun fmt ->
        fprintf fmt "The type of response time %a is cyclic\n"
          (emph_pp bold pp_dur) dur)
  | UnboundVariable(x,loc) ->
    Prelude.Errors.raise_error ~loc ~msg:("unbound variable "^x) ()


let typing_static ~loc g glob =
  match glob with
  | Static_array_of (t,_) ->
    Ty_array(new_size_unknown(),new_tyB_unknown()) (* todo: translate [t] *)
  | Static_array(c,n) ->
    let elem = typ_const ~loc g c in  (*todo loc *)
    Ty_array(Sz_lit n,elem)
  | Static_const c ->
    Ty_base (typ_const ~loc g c)


let env_extend_statics env statics =
  List.fold_left (fun env (x,glob) ->
    let ty = typing_static ~loc:Prelude.dloc env glob in
    SMap.add x (Forall(Vs.empty,ty)) env) env statics ;;


let env_extend_externals env externals =
  List.fold_left (fun env (x,(ty,_)) ->
    SMap.add x (generalize (SMap.bindings env) ty) env) env (snd externals) ;;



let typing ?collect_sig ?(env=SMap.empty) ?(msg="") ~statics ~externals ~sums e =
  Hashtbl.clear signatures;
  let loc = loc_of e in
  typing_handler (fun () ->

      let env = env_extend_statics env statics in
      let env = env_extend_externals env externals in

      let t,n = typ_exp ?collect_sig ~statics ~externals ~sums ~toplevel:true ~loc env e in
      (* let tyB = new_tyB_unknown () in*)
      (* unify_ty ~loc (Ty_fun(Ty_base tyB,new_dur_unknown(),new_tyB_unknown())) t; *)
      canon_ty t, n) ()
;;

let when_repl externals statics sums : bool -> ((p * e) * Prelude.loc) -> unit =
  let r = ref SMap.empty in

  fun show_val ((p,e),loc) ->
    typing_handler (fun () ->

        (* todo: manage externals *)

        let env = List.fold_left (fun env (x,cases) ->
            let tyB = TyB_sum (cases) in
            List.fold_left (fun env (ctor,targ) ->
                SMap.add ctor (generalize (SMap.bindings env) (Ty_fun(Ty_base targ,Dur_zero,tyB))) env)
              env cases) !r sums
        in
        let env = env_extend_statics env statics in
        let env = env_extend_externals env externals in
        
        r := env;

        let (ty,_) = typing ~env ~statics ~externals ~sums e in
        r := typing_handler (fun () -> (env_extend ~loc ~gen:(evaluated e) !r p ty)) ();
        if show_val then Format.fprintf Format.std_formatter "val %a : %a@."  Ast_pprint.pp_pat p pp_ty ty
      ) ()

let get_vector_size_ref = ref true ;;


(** [fun_shape ty] returns a type [ty -{'a}-> 'b]
    where ['a] and ['b] are fresh type variable. *)
let fun_shape (t_arg : ty) : ty =
  Ty_fun(t_arg,new_dur_unknown(),new_tyB_unknown())

let typing_with_argument ?(get_vector_size=true) ?collect_sig ({statics;externals;sums;main} : pi) (arg_list : e list) : ty * dur =
  typing_handler (fun () ->
      (* caution: [typing_handler] put a message about **expressions**
         when CannotUnify is raised, wherever it is raised *)
      get_vector_size_ref := get_vector_size;
      let t_arg = new_ty_unknown() in
      let env = SMap.empty in
      let env = List.fold_left (fun env (x,cases) ->
          let t = TyB_sum (cases) in
          List.fold_left (fun env (ctor,targ) ->
              SMap.add ctor (generalize (SMap.bindings env) (Ty_fun(Ty_base targ,Dur_zero,t))) env) env cases) env sums
      in
      let env = env_extend_statics env statics in
      let env = env_extend_externals env externals in
      let loc = loc_of main in
      let e = mk_loc loc @@ ty_annot ~ty:(fun_shape t_arg) main in
      let statics_env = [] (*TODO: List.map (fun (x,st) -> x,typing_static env st) statics*) in
      let (ty,response_time) =
        typing ?collect_sig ~env ~statics:statics_env ~externals ~sums e
      in
      (if !relax_flag then () else
         let t = canon_ty ty in
         match t with
         | Ty_fun(_,dur,_) ->
           if (canon_dur dur) <> Dur_zero then
             let open Prelude.Errors in
             error (fun fmt ->
                 Format.fprintf fmt
                   "@[<v>This program has type %a. It is not reactive. @]" (* Hint: use eta-expansion. *)
                   (emph_pp green pp_ty) t)
         | _ -> assert false);
      List.iter (fun a -> typing ~env ~msg:"checking inputs given by option -arg, "
                    ~statics:statics_env ~externals ~sums
                    (ty_annot ~ty:t_arg a)
                          |> ignore) arg_list;

      unify_ty ~loc:(Prelude.dloc) (Ty_fun(t_arg,new_dur_unknown(),new_tyB_unknown())) ty;

      (canon_ty ty, response_time)
    ) ()


