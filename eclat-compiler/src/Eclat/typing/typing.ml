open Types
open Ast

let print_signature_flag = ref false

let relax_flag = ref false

let monomorphic = ref false

let accept_ref_arg_flag = ref true (* see lambda-lifting.ml *)

let trace_last_exp = ref (E_const Unit) (* fake *)


exception PatTypeError

type kind = Ty of ty * ty 
       | TyB of tyB * tyB 
       | Dur of dur * dur 
       | Size of size * size
       | Imcompatible_length of ty * ty
       | Ty_TyB of ty * tyB
       | TyB_Ty of tyB * ty
       | Pat_ty of p * ty
       | AbstractTy_mismatch of x * x
       | Cyclic_Ty of int * ty
       | Cyclic_Dur of int * dur
       | Cyclic_Size of int * size

exception CannotUnify of Prelude.loc * kind list

exception Cyclic_size of int * size * Prelude.loc
exception Cyclic_dur of int * dur * Prelude.loc
exception Cyclic_ty of int * ty * Prelude.loc

let rec unify_size ~loc sz1 sz2 =
  let sz1, sz2 = canon_size sz1, canon_size sz2 in
  match sz1, sz2 with
  | sz1,Sz_var {contents=Is sz2}
  | Sz_var {contents=Is sz1},sz2 -> unify_size ~loc sz1 sz2
  | Sz_var ({contents=(Unknown{id=n;_})} as v),
    Sz_var {contents=Unknown{id=m;_}} ->
    if n = m then () else v := Is sz2
  | Sz_var ({contents=(Unknown{id=n;_})} as r1),sz2 ->
    if test_occur (occur_size n) sz2 then (
      r1 := Is (Sz_lit 0);
      unify_size ~loc sz2 (Sz_var r1)
    ) else
    r1 := Is sz2
  | sz1,Sz_var ({contents=(Unknown{id=n;_})} as r2) ->
    if test_occur (occur_size n) sz1 then (
      r2 := Is (Sz_lit 0);
      unify_size ~loc sz1 (Sz_var r2)
    ) else
    r2 := Is sz1
  | Sz_lit n1, Sz_lit n2 ->
    if n1 < 0 || n2 < 0 then
      Prelude.Errors.error ~loc (fun fmt -> Format.fprintf fmt "type size should be positive")
    else 
      if n1 <> n2 then raise @@ CannotUnify(loc,[Size(sz1,sz2)])
  | Sz_add(sz,n),Sz_add(sz',n') -> 
      let m = n'-n in
      if m = 0 then unify_size ~loc sz sz' else
      unify_size ~loc sz (Sz_add(sz',m))
  | Sz_add(sz,n),Sz_lit n' | Sz_lit n',Sz_add(sz,n) -> 
      if n' >= n then unify_size ~loc sz (Sz_lit (n'-n))
      else raise @@ CannotUnify(loc,[Size(sz1,sz2)])
  | Sz_twice(sz),Sz_twice(sz') -> unify_size ~loc sz sz'
  | Sz_twice(sz),Sz_add(sz',n) | Sz_add(sz',n),Sz_twice(sz) ->  
      if n = 0 then unify_size ~loc (Sz_twice(sz)) sz' 
      else (
      let sz2 = Types.new_size_unknown() in    
      if n mod 2 = 0 then (
        (** =====================
          for instance, with n := 2.
          2A ~? B + 2 :
          - we have: A > 0
          - let B := 2C
          - we want :
          - 2A ~? 2C + 2
          - A ~? C+1

          2A ~? B + N :
          - we have: A > 0 because B >= 0 and N > 0
          - B is even because N is even and we want 2A ~ B + N
          - let B := 2C
          - we want :
          - 2A ~? 2C + N
          - A ~? C+M where M:=N/2

          ===================== *)
        unify_size ~loc sz' (Sz_twice sz2);
        unify_size ~loc sz (Sz_add(sz2,n/2))) 
      else (
        (** =====================
          2A ~? B + N :
          - we have: A > 0
          - if B = 0 then we have not 2A ~? N because N is odd.
          - so B > 0
          - let B := 2C + 1
          - we want :
          - 2A ~? (2C+1)+N
          - 2A ~? 2C+(N+1)
          - A ~? C+M where M:=(N+1)/2
          ===================== *)
        unify_size ~loc sz' (Sz_add(Sz_twice(sz2),1));
        unify_size ~loc sz (Sz_add(sz2,(n+1)/2)))
    )
  | Sz_twice(sz),Sz_lit n | Sz_lit n, Sz_twice(sz) -> 
      if n mod 2 = 0 then unify_size ~loc sz (Sz_lit (n/2))
      else raise @@ CannotUnify(loc,[Size(sz1,sz2)])
  
let rec unify_dur ~loc d1 d2 =

  let warning_loss_of_precision d d' var =
    Prelude.Errors.warning ~loc (fun fmt ->
        Format.fprintf fmt "don't know how to unify durations %a and %a.\nVariable %a is set to 1 (this is safe, with loss of precision)\n"
          pp_dur d pp_dur d' pp_dur (Dur_var var))
  in

  let d1,d2 = canon_dur d1,canon_dur d2 in
  (* Format.fprintf Format.std_formatter "-- ====> %a / %a\n"  pp_dur  d1  pp_dur  d2; *)
  match d1,d2 with
  | Dur_zero,Dur_one -> () (* 0 <= 1 *)
  | Dur_zero,Dur_zero
  | Dur_one,Dur_one -> ()
  | d1,Dur_var {contents=Is d2}
  | Dur_var {contents=Is d1},d2 ->
      unify_dur ~loc d1 d2
  | Dur_var ({contents=(Unknown{id=n;_})} as v),
    Dur_var {contents=Unknown{id=m;_}} ->
      if n = m then () else v := Is d2
  | d1,Dur_var ({contents=Unknown{id=n;_}} as r2) ->
      if test_occur (occur_dur n) d1 then raise (Cyclic_dur(n,d1,loc));
      r2 := Is d1
  | Dur_var ({contents=Unknown{id=n;_}} as r1),d2 ->
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
      unify_dur ~loc d1 Dur_zero
  | Dur_max(Dur_var ({contents=(Unknown{id=n;_})} as r1),Dur_var ({contents=(Unknown{id=m;_})} as r2)),d2 ->
    if n = m then let d = Dur_var r1 in r2 := Is d; unify_dur ~loc d d2 else
      (warning_loss_of_precision d1 d2 r1;
       r1 := Is Dur_one;
       unify_dur ~loc Dur_one d2)
  | _,Dur_max(Dur_var ({contents=(Unknown{id=n;_})} as r3),Dur_var ({contents=(Unknown{id=m;_})} as r4)) ->
    if n = m then let d = Dur_var r3 in r4 := Is d; unify_dur ~loc d1 d else
      (warning_loss_of_precision d1 d2 r3;
       r3 := Is Dur_one;
       unify_dur ~loc d1 Dur_one)
  | _ ->
    raise @@ CannotUnify(loc,[Dur(d1,d2)])

(* let () =  (* test *)
   Printf.printf "===>\n";
   unify_dur ~loc (Dur_max(new_dur_unknown(),new_dur_unknown()))
                              (Dur_max(new_dur_unknown(),new_dur_unknown())) *)

let rec unify_tyB ~loc tyB1 tyB2 =
  let tyB1,tyB2 = canon_tyB tyB1, canon_tyB tyB2 in
   (*Format.fprintf Format.std_formatter "&&      [tyB]====> %a / %a\n"  pp_tyB  tyB1  pp_tyB  tyB2;
  *)match tyB1, tyB2 with
  | TyB_var ({contents=(Unknown{id=n;name})} as v),
    TyB_var {contents=Unknown({id=m;_} as r)} ->
    if n = m then () else 
       begin 
            v := Is tyB2;
            (match r.name with None -> r.name <- name | _ -> ());   
      end
  | TyB_var ({contents=(Unknown{id=n;_})} as r1),tyB2 ->
    if test_occur (occur_tyB n) tyB2 then 
      raise @@ CannotUnify(loc,(Cyclic_Ty(n,Ty_base tyB2))::[]);
    r1 := Is tyB2
  | tyB1,TyB_var ({contents=(Unknown{id=n;_})} as r2) ->
    if test_occur (occur_tyB n) tyB1 then
      raise @@ CannotUnify(loc,(Cyclic_Ty(n,Ty_base tyB1))::[]);
    r2 := Is tyB1
  | tyB1,TyB_var {contents=Is tyB2}
  | TyB_var {contents=Is tyB1},tyB2 -> unify_tyB ~loc tyB1 tyB2
  | TyB_bool,TyB_bool
  | TyB_unit,TyB_unit -> ()
  | TyB_int sz1, TyB_int sz2 ->
      (try unify_size ~loc sz1 sz2
      with CannotUnify(_,l) ->
        raise @@ CannotUnify(loc,(TyB(tyB1,tyB2))::l))
  | TyB_string sz1, TyB_string sz2 -> ()
    (* (try unify_size ~loc sz1 sz2
     with CannotUnify_size _ -> raise @@ CannotUnify_tyB(loc,Ty_base tyB1,Ty_base tyB2))*)
  | TyB_abstract(x1,szs1,tyB_list1),TyB_abstract(x2,szs2,tyB_list2) ->
      if x1 <> x2 then raise @@ CannotUnify(loc,(AbstractTy_mismatch(x1,x2))::[])
      else if List.compare_lengths tyB_list1 tyB_list2 <> 0
                  || List.compare_lengths szs1 szs2 <> 0
      then raise @@ CannotUnify(loc,(Imcompatible_length(Ty_base tyB1, Ty_base tyB2))::[])
      else
      (try List.iter2 (unify_size ~loc) szs1 szs2;
           List.iter2 (unify_tyB ~loc) tyB_list1 tyB_list2
       with CannotUnify(_,l) -> 
        raise @@ CannotUnify(loc,(TyB(tyB1,tyB2))::l))

  | TyB_tuple tyB_list1, TyB_tuple tyB_list2 ->
    if List.compare_lengths tyB_list1 tyB_list2 <> 0 then
      raise @@ CannotUnify(loc,(Imcompatible_length(Ty_base tyB1, Ty_base tyB2))::[]);
    (try List.iter2 (unify_tyB ~loc) tyB_list1 tyB_list2 
     with CannotUnify(_,l) ->
            raise @@ CannotUnify(loc,TyB(tyB1,tyB2)::l))
  | TyB_sum(ctors),TyB_sum(ctors') ->
    if List.compare_lengths ctors ctors' <> 0 then
      raise @@ CannotUnify(loc,(Imcompatible_length(Ty_base tyB1, Ty_base tyB2))::[]);
    List.iter2 (fun (x1,tyBi1) (x2,tyBi2) ->
        if x1 <> x2 then raise @@ CannotUnify(loc,AbstractTy_mismatch(x1,x2)::[]);
        (try unify_tyB ~loc tyBi1 tyBi2
         with CannotUnify(_,l) ->
                raise @@ CannotUnify(loc,TyB(tyB1,tyB2)::l))) ctors ctors'
  | _ -> raise @@ CannotUnify(loc,TyB(tyB1,tyB2)::[])

(** unify actual type ty1 and expected type ty2;
    raise an exception [CannotUnify_ty(loc,ty1,ty2)]
    if both types are incompatible, while ensuring [loc] 
    is the location of an expression of type [ty2]. *)
let unify_ty ~loc ty1 ty2 =
  let ty1,ty2 = canon_ty ty1, canon_ty ty2 in
  let rec unify ~loc ty1 ty2 =
    let ty1,ty2 = canon_ty ty1, canon_ty ty2 in
    (* Format.fprintf Format.std_formatter "          [ty]====> %a / %a\n"  pp_ty  ty1  pp_ty  ty2;
    *)match ty1,ty2 with
    | Ty_var ({contents=(Unknown{id=n;name})} as v),
      Ty_var {contents=Unknown({id=m;_} as u)} ->
        if n = m then () else 
          begin  
            v := Is ty2;
            (match u.name with None -> u.name <- name | _ -> ())
          end
    | (Ty_base(TyB_var {contents=Unknown u}),
       Ty_var ({contents=Unknown{id=m;name}} as v)) ->
        (* if n = m then () else*) 
          begin
            v := Is ty1;
            (match u.name with None -> u.name <- name | _ -> ())
          end
    | (Ty_var ({contents=Unknown{id=m;name}} as v),
       Ty_base(TyB_var {contents=Unknown u})) ->
      (* if n = m then () else*) 
         begin
            v := Is ty2;
            (match u.name with None -> u.name <- name | _ -> ())
         end
    (*| (Ty_base(TyB_var {contents=(Is tyB1)}),
      Ty_var ({contents=Unknown n} as r2)) ->
        if test_occur (occur_ty n) ty2 then raise (Cyclic_ty(n,ty2,loc));
        r2 := Is (Ty_base tyB1)
      | (Ty_var ({contents=Unknown n} as r1)),
      Ty_base(TyB_var {contents=(Is tyB2)}) ->
        if test_occur (occur_ty n) ty1 then raise (Cyclic_ty(n,ty1,loc));
        r1 := Is (Ty_base tyB2)*)
    | Ty_var ({contents=(Unknown{id=n;_})} as r1),ty2 ->
      if test_occur (occur_ty n) ty2 then
        raise @@ CannotUnify(loc,(Cyclic_Ty(n,ty2))::[]);
      r1 := Is ty2
    | ty1,Ty_var ({contents=(Unknown{id=n;_})} as r2) ->
      if test_occur (occur_ty n) ty1 then
        raise @@ CannotUnify(loc,(Cyclic_Ty(n,ty1))::[]);
      r2 := Is ty1
    | Ty_base tyB1, Ty_base tyB2 ->
        unify_tyB ~loc tyB1 tyB2
    | Ty_tuple ty_list1, Ty_tuple ty_list2 ->
      if List.compare_lengths ty_list1 ty_list2 <> 0 then
        raise @@ CannotUnify(loc,(Imcompatible_length(ty1, ty2))::[]);
      List.iter2 (unify ~loc) ty_list1 ty_list2
    | Ty_fun(ty1,d1,tyB1),Ty_fun(ty2,d2,tyB2) ->
        unify ~loc ty1 ty2;
        unify_dur ~loc d1 d2;
        unify_tyB ~loc tyB1 tyB2
    | Ty_ref(tyB1),Ty_ref(tyB2) ->
        unify_tyB ~loc tyB1 tyB2 
    | Ty_array(sz1,tyB1),Ty_array(sz2,tyB2) ->
        unify_size ~loc sz1 sz2;
        unify_tyB ~loc tyB1 tyB2
    | Ty_signal(tyB1),Ty_signal(tyB2) ->
        unify_tyB ~loc tyB1 tyB2
    | Ty_trap(tyB1),Ty_trap(tyB2) ->
        unify_tyB ~loc tyB1 tyB2
    | Ty_base (TyB_tuple tyB_list), Ty_tuple ty_list ->
        if List.compare_lengths tyB_list ty_list <> 0 then
          raise @@ CannotUnify(loc,(Imcompatible_length(ty1, ty2))::[]);
        List.iter2 (fun tyB ty -> unify ~loc (Ty_base tyB) ty) tyB_list ty_list
    | Ty_tuple ty_list, Ty_base (TyB_tuple tyB_list) ->
        if List.compare_lengths tyB_list ty_list <> 0 then
          raise @@ CannotUnify(loc,(Imcompatible_length(ty1, ty2))::[]);
        List.iter2 (fun ty tyB -> unify ~loc ty (Ty_base tyB)) ty_list tyB_list

    | Ty_base tyB, Ty_tuple ty_list ->
        let tyB_list = List.map (fun _ -> new_tyB_unknown()) ty_list in
        unify ~loc ty1 (Ty_base (TyB_tuple tyB_list));
        unify ~loc ty1 ty2;
    | Ty_tuple ty_list, Ty_base tyB ->
        List.iter (fun ty -> unify ~loc ty (Ty_base (new_tyB_unknown()))) ty_list;
        unify ~loc ty1 ty2

    | Ty_base tyB1,_ ->
        raise @@ CannotUnify(loc,(TyB_Ty(tyB1,ty2))::[]);
    | _,Ty_base tyB2 ->
        raise @@ CannotUnify(loc,(Ty_TyB(ty1,tyB2))::[]);
    | Ty_size sz1, Ty_size sz2 -> 
      (try unify_size ~loc sz1 sz2
       with CannotUnify(_,l) ->
        raise @@ CannotUnify(loc,Ty(ty1,ty2)::l))

    | _ ->  raise @@ CannotUnify(loc,(Ty(ty1,ty2))::[]);
  in
  try unify ~loc ty1 ty2 with
  | CannotUnify(loc0,l) ->
      if loc = loc0 then raise @@ CannotUnify(loc,l) 
      else raise @@ CannotUnify(loc,(Ty(ty1,ty2))::l);
  | Cyclic_ty(n,_,_)
  | Cyclic_dur(n,_,_)
  | Cyclic_size(n,_,_) -> raise @@ Cyclic_ty(n,ty1,loc)

let rec subtyping_dur ~loc d1 d2 =
  let d1,d2 = canon_dur d1,canon_dur d2 in
  match d1,d2 with
  | Dur_zero,_
  | _,Dur_one -> ()
  | d1,Dur_var {contents=Is d2} -> subtyping_dur ~loc d1 d2
  | Dur_var {contents=Is d1},d2 -> subtyping_dur ~loc d1 d2
  | Dur_var ({contents=Unknown{id=n;_}} as r),Dur_zero ->
    r := Is (Dur_zero)
  | Dur_one,Dur_var ({contents=Unknown n} as r) ->
    r := Is (Dur_one)
  | Dur_max(Dur_zero,d),Dur_zero
  | Dur_max(d,Dur_zero),Dur_zero -> subtyping_dur ~loc d Dur_zero
  (* | Dur_var ({contents=(Unknown n)} as v),
     Dur_var {contents=Unknown m} ->
     if n = m then () else v := Is d2*)
  | _ ->
    raise @@ CannotUnify(loc,(Dur(d1,d2))::[])
(* | d,({contents=Unknown n} as r) ->
    let v = new_dur_unknown () in
    r := Is (Dur_add(d,v))*)

let ty_bindings ~loc p ty =
  let rec ty_bindings_aux ~loc p ty =
    match p,canon_ty ty with
    | P_var x,t -> SMap.singleton x t
    | P_tuple ps,Ty_tuple ts ->
      if List.compare_lengths ps ts <> 0 then
        let _ts_found = List.map (fun _ -> new_ty_unknown ()) ps in
        raise @@ CannotUnify(loc,Imcompatible_length(Ty_tuple _ts_found, ty)::[])
      else
        List.fold_left2 (fun m p t -> ty_bindings_aux ~loc p t ++ m) SMap.empty ps ts
    | P_unit,ty ->
      unify_ty ~loc ty (Ty_base TyB_unit);
      SMap.empty
    | P_tuple ps,ty ->
      let ty_list = List.map (fun _ -> new_ty_unknown ()) ps in
      unify_ty ~loc ty (Ty_tuple ty_list);
      List.fold_left2 (fun m p t -> ty_bindings_aux ~loc p t ++ m) SMap.empty ps ty_list
  in 
  try ty_bindings_aux ~loc p ty with
  | CannotUnify(loc0,l) ->
      raise @@ CannotUnify(loc,(Pat_ty (p,ty))::l)

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

let group_tyB_list = function
  | [] -> TyB_unit
  | [tyB] -> tyB
  | tyB_list -> TyB_tuple tyB_list

let ty_op ~genv ~loc op =
  let ty = 
    match op with
    | Runtime(p) ->
      Operators.ty_op ~externals:genv.operators p
    | Wait n ->
      let tyB = new_tyB_unknown () in
      Ty_fun(Ty_base tyB,Dur_one,tyB)
    | TyConstr ty -> new_ty_unknown () (* (* todo *)
                                          let v = unknown() in
                                          unify ~loc ty v;
                                          fun_ty ty (T_response_time 0) v
                                          Ty_fun(Ty_base tyB,Dur_zero,tyB) *)
    | GetTuple _ -> assert false (* {pos;arity} ->
      let ty_list = List.init arity (fun _ -> new_ty_unknown ()) in
      assert (0 <= pos && pos <= arity);
      let tyB = new_tyB_unknown () in
      unify_ty ~loc (Ty_base tyB) (List.nth ty_list pos);
      Ty_fun(Ty_tuple ty_list,
             Dur_zero,
             tyB) *)
  in
  (* let tyB1 = new_tyB_unknown () in*)
  let ty1 = new_ty_unknown () in
  let tyB2 = new_tyB_unknown () in
  unify_ty ~loc ty (Ty_fun(ty1,Dur_zero,tyB2));
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
  | Char _ -> Operators.char_
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
  | C_size _ -> assert false
  | C_appInj(x,c,tyB) ->
    let tx = typ_ident ~loc g x in (* todo loc x *)
    let tc = typ_const ~loc g c in (* todo loc c *)
    unify_ty ~loc tx (Ty_fun(Ty_base tc,Dur_zero,tyB)); (* todo loc x *)
    tyB

  | Inj _ -> assert false (* to remove from constants *)
  | _ -> assert false

let rec typ_exp ?(collect_sig=false) ~statics ~genv ~ctors ?(toplevel=false) ~loc g e =
  match e with
  | E_const (Op (Runtime(External_fun(x,tyx)))) ->
      let t = typ_ident ~loc g x in
      unify_ty ~loc tyx (Types.copy_ty t);
      tyx, Dur_zero
  | E_app(E_const (Op(GetTuple{pos;arity})),e1) ->
      let ty_list = List.init arity (fun _ -> new_ty_unknown ()) in
      assert (0 <= pos && pos <= arity);
      let t1,d1 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel ~loc g e1 in
      unify_ty ~loc:(loc_of e1) t1 (Ty_tuple ty_list);
      List.nth ty_list pos,d1
  | E_const (Op op) -> 
      let t = ty_op ~genv ~loc op in
      t,Dur_zero
  | E_const(Inj x) -> (* Prelude.Errors.warning ~loc (fun fmt -> ()); *)
      let tx = typ_ident ~loc g x in
      tx,Dur_zero
  | E_const (C_size sz) -> 
      (Ty_size sz, Dur_zero)
  | E_const c ->
    (Ty_base (typ_const ~loc g c), Dur_zero)
  | E_var(x) ->
    (* lookup *)
    let tx = typ_ident ~loc g x in
    (tx,Dur_zero)
  | E_deco(e1,loc) ->
    typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel ~loc g e1
  | E_if(e1,e2,e3) ->
    let t1,d1 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e1) g e1 in
    let t2,d2 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e2) g e2 in
    let t3,d3 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e3) g e3 in
    unify_ty ~loc:(loc_of e1) t1 (Ty_base TyB_bool);
    let vTyB = new_tyB_unknown () in
    unify_ty ~loc:(loc_of e2) t2 (Ty_base vTyB);
    unify_ty ~loc:(loc_of e3) t3 (Ty_base vTyB);
    t2,Dur_max(d1,Dur_max(d2,d3))
  | E_case(e1,hs,e_els) ->
    let ty1,d1 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e1) g e1 in
    List.iter (fun (cs,_) -> List.iter (fun c -> unify_ty ~loc (Ty_base (typ_const ~loc g c)) ty1) cs) hs; (* todo: loc *)
    let ty_els,d_els = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e_els) g e_els in
    let d_list = List.map (fun (_,ei) ->
        let ty,d = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of ei) g ei in
        unify_ty ~loc:(loc_of ei) ty_els ty; d) hs in
    let d = List.fold_left (fun d1 d2 -> Dur_max(d1,d2)) d_els d_list in
    ty_els,Dur_max(d,d1)
  | E_letIn(p,typ,e1,e2) ->
    (* Format.fprintf Format.std_formatter "--->(%a : %a)\n" Ast_pprint.pp_exp (Pattern.pat2exp p) Types.pp_ty  typ;*)
    let ty1,d1 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e1) g e1 in
    unify_ty ~loc:(loc_of e1) typ ty1;
    let gen = evaluated e1 (* && match un_deco e1 with E_fix _ -> false | _ -> true *) in
    let g' = env_extend ~loc:(loc_of e1) ~gen g p ty1 in (* todo: loc of pattern *)
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
        begin
           let open Prelude.Errors in
           let open Format in
           fprintf std_formatter "val %a : " Ast_pprint.pp_pat p;
           if not(!monomorphic) then
           (match p with P_var x -> (let (Forall(xs,_)) = SMap.find x g' in
               if Vs.cardinal xs > 0 then (
                 fprintf std_formatter "forall ";
                 Vs.iter (fun x -> match x.name with
                                   | None -> fprintf std_formatter "%d " x.id
                                   | Some y -> fprintf std_formatter "%s%d " y x.id) xs;
                 fprintf std_formatter ". "))
           | _ -> ());
           fprintf std_formatter "%a | %a\n"
             pp_ty (canon_ty ty1) pp_dur (canon_dur d1)
         end
       end);

    let ty2,d2 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel ~loc:(loc_of e2) g' e2 in

    (ty2, Dur_max(d1,d2))

  | E_tuple(es) ->
    let ts,ds = List.split @@ List.map (fun ei ->
        typ_exp ~collect_sig ~statics ~genv ~ctors
          ~toplevel:false ~loc:(loc_of ei) g ei) es in
    let d = List.fold_left (fun d1 d2 -> Dur_max(d1,d2)) Dur_zero ds in
    Ty_tuple ts,d

  | E_fun(p,(ty,tyB),e1) ->
    let g' = env_extend ~loc g p ty in (* todo: loc of pattern *)
    let ty1,dur = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e1) g' e1 in
    unify_ty ~loc:(loc_of e1) ty1 (Ty_base tyB);
    (Ty_fun(ty,canon_dur dur,tyB), Dur_zero)

  | E_fix(f,(p,(ty,tyB),e1)) ->
    let tf = Ty_fun(ty,Dur_one,tyB) in
    let g' = env_extend ~loc g (P_var f) tf in
    let g' = env_extend ~loc g' p ty in (* todo: precise loc *)
    let ty1,d = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e1) g' e1 in
    unify_ty ~loc:(loc_of e1) ty1 (Ty_base tyB);
    (tf, Dur_zero)
  | E_app(e1,e2) ->
    (match un_deco e1 with
     | E_const (Op (TyConstr ty)) ->
       let ty2,d2 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel g ~loc:(loc_of e2) e2 in
       unify_ty ~loc:(loc_of e2) ty2 ty;
       ty2,d2
     | _ ->
       let ty1,d1 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false g ~loc:(loc_of e1) e1 in
       let ty2,d2 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false g ~loc:(loc_of e2) e2 in
       let tyB = new_tyB_unknown () in
       let d = new_dur_unknown () in
       unify_ty ~loc:(loc_of e1) (Ty_fun(ty2,d,tyB)) ty1;
       Ty_base tyB, canon_dur (Dur_max(Dur_max(d1,d2),d)))


  | E_par(es) ->
    let ts,ds = List.split @@ List.map (fun ei ->
        let ti,d = typ_exp ~collect_sig ~statics ~genv
                           ~ctors ~toplevel:false ~loc:(loc_of ei) g ei in
        ti,d) es
    in
    let d = List.fold_left (fun d1 d2 -> Dur_max(d1,d2)) Dur_zero ds in
    Ty_tuple ts,canon_dur d

  (* *************************************************** *)
  | E_reg((p,tyB,e1),e0,_) ->
    let ty0,d0 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc g e0 in
    let g' = env_extend ~loc g p ty0 in
    let ty1,d1 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc g' e1 in
    unify_ty ~loc ty0 ty1;
    unify_dur ~loc:(loc_of e0) d0 Dur_zero;
    unify_dur ~loc:(loc_of e1) d1 Dur_zero;
    unify_ty ~loc:(loc_of e0) (Ty_base tyB) ty0;
    (ty0, Dur_zero)

  | E_exec(e1,e2,eo,_) ->
    let ty1,_ = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc g e1 in
    let ty2,d2 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc g e2 in
    unify_ty ~loc:(loc_of e1) ty1 ty2;
    unify_dur ~loc:(loc_of e2) d2 Dur_zero;
    let tyB = new_tyB_unknown () in
    unify_ty ~loc:(loc_of e1) (Ty_base tyB) ty1;
    Option.iter (fun e3 ->
        let ty3,d3 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc g e3 in
        let loc_e3 = loc_of e3 in
        unify_ty ~loc:loc_e3 ty3 (Ty_base (TyB_bool));
        unify_dur ~loc:loc_e3 d3 Dur_zero) eo;
    (Ty_base (TyB_tuple[tyB;TyB_bool]), Dur_zero)

  (* *************************************************** *)

  | E_match(e1,hs,eo) ->
    let error_unbound_constructor ctor =
      Prelude.Errors.error ~loc (fun fmt -> Format.fprintf fmt "Unbound constructor %s" ctor)
    in
    let ty1,d1 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e1) g e1 in
    (* todo : error if there is no clause *)
    let x_witness = match hs with (x,_)::_ -> x | _ -> assert false in
    let (xt,typ_param,n_ctors,sum) = match SMap.find_opt x_witness ctors with
                  | Some (xt,typ_param,n,sum) -> xt,typ_param,n,sum
                  | None -> error_unbound_constructor x_witness in (* try Types.find_ctor c_witness sums
      with Not_found -> error_unbound_constructor c_witness in*)
    let ty_result = (Ty_base (new_tyB_unknown ())) in
    let r = ref d1 in
    List.iter (fun (inj,(p,ei)) ->
        let loc_of_ie = loc_of ei in
        let typ_param = match SMap.find_opt inj ctors with
                        | Some (_,typ_param,_,sum) -> 
                                    (match List.assoc_opt inj sum with
                                     | Some tyB -> unify_tyB ~loc:loc_of_ie typ_param tyB
                                     | None -> assert false);
                             Ty_base typ_param
                        | None -> error_unbound_constructor inj
        in
        let g' = env_extend ~loc:loc_of_ie g p typ_param in
        let tyi,di = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of ei) g' ei in
        unify_ty ~loc:loc_of_ie tyi ty_result;
        r := Dur_max(!r,di)) hs;

    Option.iter (fun ew -> 
        (* wildcard clause *)
        let tyw,dw = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of ew) g ew in
        unify_ty ~loc:(loc_of ew) tyw ty_result;
        r := Dur_max(!r,dw)) eo;

    if eo = None && List.length hs < n_ctors then (
      Prelude.Errors.error ~loc (fun fmt ->
          Format.fprintf fmt "This pattern-matching is not exhaustive.")
    );
    ty_result,!r

  (* *************************************************** *)

  | E_ref(e1) ->
    let ty,d = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e1) g e1 in
    (* Format.fprintf Format.std_formatter "|///|||--->%a\n" pp_ty  ty; *)
    let tyB = new_tyB_unknown () in
    unify_ty ~loc:(loc_of e1) (Ty_base tyB) ty;
    (* Format.fprintf Format.std_formatter "||||--->%a\n" pp_tyB  tyB; *)
    (Ty_ref(tyB),d)
  | E_get(e1) ->
    let ty1,d = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e1) g e1 in
    let tyB = new_tyB_unknown () in
    unify_ty ~loc:(loc_of e1) (Ty_ref tyB) ty1;
    (Ty_base tyB, Dur_max(d,Dur_one))
  | E_set (e1,e2) ->
    let ty1,d1 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e1) g e1 in
    let ty2,d2 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e2) g e2 in
    let tyB = new_tyB_unknown () in
    unify_ty ~loc:(loc_of e1) (Ty_ref tyB) ty1;
    unify_ty ~loc:(loc_of e2) (Ty_base tyB) ty2;
    (Ty_base TyB_unit, Dur_max(Dur_max(d1,d2),Dur_one))

  (* *************************************************** *)

  | E_array_make(sz,e1,_) ->
      let tyB = new_tyB_unknown() in
      let ty1,d = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e1) g e1 in
      unify_ty ~loc:(loc_of e1) ty1 (Ty_base tyB);
      (Ty_array(sz,tyB)),Dur_one
  | E_array_create(sz,_) ->
      let tyB = new_tyB_unknown() in
      (Ty_array(sz,tyB)),Dur_zero
  | E_array_length(x,loc_x) ->
    let tyx = typ_ident ~loc:loc_x g x in
    let sz = new_size_unknown () in
    let v = new_tyB_unknown () in
    unify_ty ~loc:loc_x (Ty_array(sz,v)) tyx;
    (Ty_base (TyB_int (new_size_unknown ())), Dur_zero)
  | E_array_get((x,loc_x),e1) ->
    let ty1,d = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc g e1 in
    unify_ty ~loc:(loc_of e1) ty1 (Ty_base (TyB_int (new_size_unknown ())));
    let tyx = typ_ident ~loc:loc_x g x in
    let tyB = new_tyB_unknown () in
    unify_ty ~loc:loc_x (Ty_array(new_size_unknown(),tyB)) tyx;
    (Ty_base tyB,dur_add d Dur_one)
  | E_array_set((x,loc_x),e1,e2) ->
    let ty1,d1 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e1) g e1 in
    let ty2,d2 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e2) g e2 in
    unify_ty ~loc:(loc_of e1) ty1 (Ty_base (TyB_int (new_size_unknown ())));
    let tyx = typ_ident ~loc:loc_x g x in
    let tyB = new_tyB_unknown () in
    unify_ty ~loc:(loc_of e2) ty2 (Ty_base tyB);
    unify_ty ~loc:loc_x (Ty_array(new_size_unknown(),tyB)) tyx;
    (Ty_base TyB_unit, dur_add (Dur_max(d1,d2)) Dur_one)
  | E_array_get_start((x,loc_x),e1) ->
    let ty1,d = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e1) g e1 in
    unify_ty ~loc ty1 (Ty_base (TyB_int (new_size_unknown ())));
    let tyx = typ_ident ~loc:loc_x g x in
    let tyB = new_tyB_unknown () in
    unify_ty ~loc:loc_x (Ty_array(new_size_unknown(),tyB)) tyx;
    (Ty_base TyB_unit, d)
  | E_array_get_end(x,loc_x) ->
      let tyx = typ_ident ~loc:loc_x g x in
      let tyB = new_tyB_unknown () in
      unify_ty ~loc:loc_x (Ty_array(new_size_unknown(),tyB)) tyx;
      (Ty_base tyB, Dur_zero)
  | E_array_set_immediate((x,loc_x),e1,e2) ->
    let ty1,d1 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e1) g e1 in
    let ty2,d2 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e2) g e2 in
    unify_ty ~loc ty1 (Ty_base (TyB_int (new_size_unknown ())));
    let tyx = typ_ident ~loc:loc_x g x in
    let tyB = new_tyB_unknown () in
    unify_ty ~loc:(loc_of e2) ty2 (Ty_base tyB);
    unify_ty ~loc:loc_x (Ty_array(new_size_unknown(),tyB)) tyx;
    (Ty_base TyB_unit, (Dur_max(d1,d2)))
  | E_array_from_file(x,e1) ->
      let tyx = typ_ident ~loc g x in (* todo loc of x *)
      let ty1,_d1 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e1) g e1 in
      unify_ty ~loc:(loc_of e1) (Ty_base(TyB_string(new_size_unknown()))) ty1;
      unify_ty ~loc (Ty_array(new_size_unknown(),new_tyB_unknown())) tyx; (* todo loc of x *)
      (Ty_base TyB_unit, Dur_one)

  | E_for(x,_,_,e3,_) ->
    let g' = env_extend ~loc g (P_var x) (Ty_base (TyB_int (new_size_unknown()))) in (* todo loc of pattern *)
    let (ty3,d3) = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e3) g' e3 in (* todo *)
    unify_ty ~loc:(loc_of e3) ty3 (Ty_base TyB_unit);
    (Ty_base TyB_unit, d3)

  | E_generate((p,(ty,tyB),e1),e2,_,_,_) ->
    let vsize1 = new_size_unknown() in
    let intv1 = Ty_base (TyB_int vsize1) in
    let ty2,d2 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e2) g e2 in
    unify_ty ~loc:(loc_of e2) ty2 (Ty_base tyB);
    let g' = env_extend ~loc g p (Ty_tuple[intv1;ty2]) in
    let ty1,d1 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e1) g' e1 in
    ty1,Dur_max(d1,d2) (* n1+n1+ ... n fois *)

  | E_vector(es) ->
    let v = new_tyB_unknown () in
    let ns = List.map (fun ei ->
        let t,n = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of ei) g ei in
        unify_ty ~loc:(loc_of ei) t (Ty_base v);
        n) es
    in
    let n = List.fold_left (fun acc n -> Dur_max(acc,n)) Dur_zero ns in
    Ty_base(Operators.vect_ (Sz_lit(List.length es)) v),n

  | E_vector_mapi(_,(p,(tyB1,tyB2),e1),e2,size_vect) ->
    let vsize1 = new_size_unknown() in
    let intv1 = TyB_int vsize1 in
    let ty2,d2 = typ_exp ~collect_sig ~statics ~genv ~ctors
                    ~toplevel:false ~loc:(loc_of e2) g e2 in (* TODO: force ty2 to be a base type *)
    let w = new_tyB_unknown () in
    unify_ty ~loc:(loc_of e2) (Ty_base w) ty2;
    unify_tyB ~loc:(loc_of e2) w (Operators.vect_ size_vect tyB1); (* todo: loc ok ? *)
    let g' = env_extend ~loc g p (Ty_base (TyB_tuple[intv1;tyB1])) in 
    (* todo: better type error message, here, it says:
       "An expression has type (int<~z76>, ...) but was expected of type ..." 
    *)
    let ty1,d1 = typ_exp ~collect_sig ~statics ~genv ~ctors
                      ~toplevel:false ~loc:(loc_of e1) g' e1 in
    unify_ty ~loc:(loc_of e1) (Ty_base tyB2) ty1;
    Ty_base (Operators.vect_  size_vect tyB2),d1 (* n times d1 *)

  | E_run (i,e1,_) ->
    let ty1,d1 = typ_exp ~collect_sig ~statics ~genv ~ctors
                       ~toplevel ~loc:(loc_of e1) g e1 in (* TODO: force ty2 to be a base type *)
    (match List.assoc_opt i genv.externals with
    | Some (ty,shared,_) ->
        let d2 = (if shared then Dur_one else Dur_zero) in
        let tyB = new_tyB_unknown () in
        let d3 = new_dur_unknown() in
        let ty2 = Ty_fun(ty1, d3, tyB) in
        unify_ty ~loc ty ty2; (* todo loc *)
        Ty_base tyB,Dur_max(Dur_max(d1,d2),d3)
    | None -> Prelude.Errors.raise_error ~msg:("unbound external circuit "^i) ())
    | E_pause (_,e1) -> 
        let ty1,_ = typ_exp ~collect_sig ~statics ~genv ~ctors
                        ~toplevel ~loc:(loc_of e1) g e1 in
        (ty1, Dur_one)
    | E_sig_get(x) ->
      let tyx = typ_ident ~loc g x in (* todo loc of x *)
      let v = new_tyB_unknown () in
      unify_ty ~loc (Ty_signal(v)) tyx; (* todo loc of x *)
      (Ty_base v, Dur_zero)
  | E_emit(x,e1) ->
      let tyx = typ_ident ~loc g x in
      let ty1,d1 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc g e1 in
      let v = new_tyB_unknown () in
      unify_ty ~loc:(loc_of e1) ty1 (Ty_base v);
      unify_ty ~loc (Ty_signal(v)) tyx; (* todo loc of x *)
      (Ty_base TyB_unit, d1)
  | E_sig_create(e1) ->
      (* let ty1,d1 = typ_exp ~collect_sig ~statics ~externals ~sums ~ctors ~toplevel:false ~loc g e1 in
      unify_dur ~loc:(loc_of e1) d1 Dur_zero;
      let v = new_tyB_unknown () in
      unify_ty ~loc:(loc_of e1) ty1 (Ty_base v);
      (Ty_signal v, Dur_zero)*)
      let v = new_tyB_unknown () in
      (Ty_signal v, Dur_zero)
  | E_loop(e1) ->
      let ty1,d1 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e1) g e1 in
      unify_ty ~loc:(loc_of e1) ty1 (Ty_base TyB_unit);
      (Ty_base TyB_unit, Dur_one)
  | E_trap(tyB) ->
      (Ty_trap tyB, Dur_zero)
  | E_exit(x,e1) ->
      let tyx = typ_ident ~loc g x in (* todo loc of x *)
      let ty1,d1 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e1) g e1 in
      let v = new_tyB_unknown () in
      unify_ty ~loc:(loc_of e1) ty1 (Ty_base v);
      unify_ty ~loc (Ty_trap(v)) tyx; (* todo loc of x *)
      (Ty_base TyB_unit, d1)
  | E_suspend(e1,x) ->
      let tyx = typ_ident ~loc g x in (* todo loc of x *)
      let ty1,d1 = typ_exp ~collect_sig ~statics ~genv ~ctors 
                           ~toplevel:false ~loc:(loc_of e1) g e1 in
      unify_ty ~loc (Ty_signal(TyB_bool)) tyx; (* todo loc of x *)
      (ty1, d1)
  | E_assert(e1,loc_e1) ->
      let ty1,d1 = typ_exp ~collect_sig ~statics ~genv ~ctors ~toplevel:false ~loc:(loc_of e1) g e1 in
      unify_ty ~loc:loc_e1 ty1 (Ty_base TyB_bool);
      (Ty_base TyB_unit, d1)

let typing_handler ?(msg="") f () =
  let open Format in
  let open Prelude.Errors in
  try f () with
  | CannotUnify(loc,cannot::l) ->
      error ~loc (fun fmt ->
        fprintf fmt "%s@," msg;
        ((match cannot with
         | Ty(ty1,ty2) ->
              fprintf fmt "This expression has type %a but was expected of type %a"
                (emph_pp bold pp_ty) (canon_ty ty1)
                (emph_pp bold pp_ty) (canon_ty ty2)
         | TyB(tyB1,tyB2) ->
             fprintf fmt "This expression has basic type %a but was expected of basic type %a"
                (emph_pp bold pp_tyB) (canon_tyB tyB1)
                (emph_pp bold pp_tyB) (canon_tyB tyB2)
          | Ty_TyB(ty1,tyB2) ->
             fprintf fmt "This expression has type %a but was expected of basic type %a"
              
              (emph_pp bold pp_ty) (canon_ty ty1)
              (emph_pp bold pp_tyB) (canon_tyB tyB2)
          | TyB_Ty(tyB1,ty2) ->
             fprintf fmt "@[<v>This expression has basic type %a but was expected of type %a@]"
              
              (emph_pp bold pp_tyB) (canon_tyB tyB1)
              (emph_pp bold pp_ty) (canon_ty ty2)
          | Size(sz1,sz2) ->
              fprintf fmt "A type has size %a but was expected of size %a"
                (emph_pp bold pp_size) (canon_size sz1)
                (emph_pp bold pp_size) (canon_size sz2)
          | Dur(d1,d2) ->
              fprintf fmt "response time %a should be %a"        
                (emph_pp bold pp_dur) (canon_dur d1)
                (emph_pp bold pp_dur) (canon_dur d2)
          | Imcompatible_length(ty1, ty2) ->
              fprintf fmt "wrong number of elements:@,this expression has type %a but was expected of type %a"
                (emph_pp bold pp_ty) (canon_ty ty1)
                (emph_pp bold pp_ty) (canon_ty ty2)
          | Pat_ty (p,ty) ->
              fprintf fmt "pattern %a should have type %a"
                (emph_pp bold Ast_pprint.pp_pat) p 
                (emph_pp bold pp_ty) (canon_ty ty)
          | AbstractTy_mismatch(x1,x2) ->
                fprintf fmt "This expression has type %a but was expected of type %a"
                (emph_pp bold (fun fmt () -> fprintf fmt "%s" x1)) () 
                (emph_pp bold (fun fmt () -> fprintf fmt "%s" x2)) () 
          | Cyclic_Ty(n,t) ->
              fprintf fmt "%s@,An expression %a has a cyclic type %a\n"
                msg  (emph_pp purple Ast_pprint.pp_exp) !trace_last_exp
                (emph_pp bold pp_ty) (canon_ty t)
          | Cyclic_Dur(n,dur) ->
              fprintf fmt "The type of response time %a is cyclic\n"
              (emph_pp bold pp_dur) (canon_dur dur)
          | Cyclic_Size(n,sz) ->
              fprintf fmt "The type of size %a is cyclic\n"
              (emph_pp bold pp_size) (canon_size sz)
          );
          List.iter (fun _ -> fprintf fmt "@]") l;
          fprintf fmt "@,"
         );
        let rec inspect = function
        | [] -> ()
        | v::l' ->   
         fprintf fmt "@[<v 2>@,";
         emph_pp purple (fun fmt () -> fprintf fmt "Hint: ") fmt ();
         (match v with
          | Ty(ty1,ty2) ->
              fprintf fmt "An expression has type %a but was expected of type %a"
                (emph_pp bold pp_ty) (canon_ty ty1)
                (emph_pp bold pp_ty) (canon_ty ty2)
          | TyB(tyB1,tyB2) ->
             fprintf fmt "An expression has basic type %a but was expected of basic type %a"
                (emph_pp bold pp_tyB) (canon_tyB tyB1)
                (emph_pp bold pp_tyB) (canon_tyB tyB2)
          | Ty_TyB(ty1,tyB2) ->
             fprintf fmt "An expression has type %a but was expected of basic type %a"
              (emph_pp bold pp_ty) (canon_ty ty1)
              (emph_pp bold pp_tyB) (canon_tyB tyB2)
          | TyB_Ty(tyB1,ty2) ->
             fprintf fmt "@[<v>An expression has basic type %a but was expected of type %a@]"
              (emph_pp bold pp_tyB) (canon_tyB tyB1)
              (emph_pp bold pp_ty) (canon_ty ty2)
          | Size(sz1,sz2) ->
              fprintf fmt "A type has size %a but was expected of size %a"
                (emph_pp bold pp_size) (canon_size sz1)
                (emph_pp bold pp_size) (canon_size sz2)
          | Dur(d1,d2) ->
              fprintf fmt "response time %a should be %a"  
                (emph_pp bold pp_dur) (canon_dur d1)
                (emph_pp bold pp_dur) (canon_dur d2)
          | Imcompatible_length(ty1, ty2) ->
              fprintf fmt "wrong number of elements:@,an expression has type %a but was expected of type %a"
                (emph_pp bold pp_ty) (canon_ty ty1)
                (emph_pp bold pp_ty) (canon_ty ty2)
          | Pat_ty (p,ty) ->
              fprintf fmt "pattern %a should have type %a"
                (emph_pp purple Ast_pprint.pp_pat) p 
                (emph_pp purple pp_ty) (canon_ty ty)
          | AbstractTy_mismatch(x1,x2) ->
                fprintf fmt "An expression has type %a but was expected of type %a"
                (emph_pp bold (fun fmt () -> fprintf fmt "%s" x1)) () 
                (emph_pp bold (fun fmt () -> fprintf fmt "%s" x2)) () 
       | Cyclic_Ty(n,t) ->
          fprintf fmt "%s@,An expression %a has a cyclic type %a\n"
            msg  (emph_pp purple Ast_pprint.pp_exp) !trace_last_exp
            (emph_pp bold pp_ty) (canon_ty t)
       | Cyclic_Dur(n,dur) ->
          fprintf fmt "The type of response time %a is cyclic\n"
          (emph_pp bold pp_dur) (canon_dur dur)
       | Cyclic_Size(n,sz) ->
          fprintf fmt "The type of size %a is cyclic\n"
          (emph_pp bold pp_size) (canon_size sz)
        );
          inspect l';
          List.iter (fun _ -> fprintf fmt "@]") l;
          fprintf fmt "@,"
        in
        inspect l)
  | UnboundVariable(x,loc) ->
    Prelude.Errors.raise_error ~loc ~msg:("unbound variable "^x) ()


let typing_static ~loc g glob =
  match glob with
  | Static_array_of (t,_) ->
    (* Ty_array(new_size_unknown(),new_tyB_unknown())*) t (* todo: translate [t] *)
  | Static_array(c,n) ->
    let elem = typ_const ~loc g c in  (*todo loc *)
    Ty_array(Sz_lit n,elem)
  | Static_const c ->
    Ty_base (typ_const ~loc g c)


let env_extend_statics env statics =
  List.fold_left (fun env (x,global_decl) ->
    let ty_glob = typing_static ~loc:Prelude.dloc env global_decl in
    SMap.add x (Forall(Vs.empty,ty_glob)) env) env statics ;;


let env_extend_operators env operators =
  SMap.fold (fun x (ty,_) env ->
    SMap.add x (generalize (SMap.bindings env) ty) env) operators env ;;

let env_extend_externals env externals =
  List.fold_left (fun env (x,(ty,_,_)) ->
    SMap.add x (Forall(Vs.empty, ty)) env) env externals ;;

let typing ?collect_sig ?(env=SMap.empty) ?(msg="") ~statics ~genv e =
  let loc = loc_of e in
  typing_handler (fun () ->

      let env = env_extend_statics env statics in
      let env = env_extend_operators env genv.operators in
      let env = env_extend_externals env genv.externals in

      let ctors = List.fold_left (fun ctors (xt,sum) ->
                      let n = List.length sum in
                      List.fold_left (fun ctors (x,typ_param) ->
                        (* Printf.printf "~~> %s\n" x; Stdlib.(flush stdout);*)
                                         SMap.add x (xt,typ_param,n,sum) ctors) ctors sum
                       ) SMap.empty genv.sums in

      let t,n = typ_exp ?collect_sig ~statics ~genv ~ctors ~toplevel:true ~loc env e in
      (* let tyB = new_tyB_unknown () in*)
      (* unify_ty ~loc (Ty_fun(Ty_base tyB,new_dur_unknown(),new_tyB_unknown())) t; *)
      canon_ty t, n) ()
;;

let when_repl statics ~genv : bool -> ((p * e) * Prelude.loc) -> unit =
  let r = ref SMap.empty in

  fun show_val ((p,e),loc) ->
    typing_handler (fun () ->

        let env = List.fold_left (fun env (x,cases) ->
            let tyB = TyB_sum (cases) in
            List.fold_left (fun env (ctor,targ) ->
                SMap.add ctor (generalize (SMap.bindings env) (Ty_fun(Ty_base targ,Dur_zero,tyB))) env)
              env cases) !r genv.sums
        in
        let env = env_extend_statics env statics in
        let env = env_extend_operators env genv.operators in
        let env = env_extend_externals env genv.externals in
        
        r := env;
        let (ty,d) = typing ~env ~statics ~genv e in
        r := typing_handler (fun () -> (env_extend ~loc:(loc_of e) ~gen:(evaluated e) !r p ty)) ();
        if show_val then
          begin
           let open Prelude.Errors in
           let open Format in
           fprintf std_formatter "val %a : " Ast_pprint.pp_pat p;
           if not(!monomorphic) then
           (match p with P_var x -> (let (Forall(xs,_)) = SMap.find x !r in
               if Vs.cardinal xs > 0 then (
                 fprintf std_formatter "forall ";
                 Vs.iter (fun x -> match x.name with
                                   | None -> fprintf std_formatter "'%d " x.id
                                   | Some y -> fprintf std_formatter "%s%d " y x.id) xs;
                 fprintf std_formatter ". "))
           | _ -> ());
           fprintf std_formatter "%a | %a@."
             pp_ty (canon_ty ty) pp_dur (canon_dur d)
         end
      ) ()

let get_vector_size_ref = ref true ;;


(** [fun_shape ty] returns a type [ty -{'a}-> 'b]
    where ['a] and ['b] are fresh type variable. *)
let fun_shape (t_arg : ty) : ty =
  Ty_fun(t_arg,new_dur_unknown(),new_tyB_unknown())

let typing_with_argument ?(get_vector_size=true) ?collect_sig ({genv;main} : pi) (arg_list : e list) : ty * dur =
  let {statics;externals;sums} = genv in
  typing_handler (fun () ->
      (* caution: [typing_handler] put a message about **expressions**
         when CannotUnify is raised, wherever it is raised *)
      get_vector_size_ref := get_vector_size;
      let t_arg = new_ty_unknown() in
      let env = SMap.empty in

      let env = List.fold_left (fun env (x,cases) ->
          let t = TyB_sum (cases) in
          List.fold_left (fun env (ctor,targ) ->
              SMap.add ctor (Forall(Vs.empty,(Ty_fun(Ty_base targ,Dur_zero,t)))) env) env cases) env sums
      in
      let env = env_extend_statics env statics in
      let env = env_extend_externals env genv.externals in
      let loc = loc_of main in
      
      let e = mk_loc loc @@ ty_annot ~ty:(fun_shape t_arg) main in
      let statics_env = [] (*TODO: List.map (fun (x,st) -> x,typing_static env st) statics*) in
      let genv = genv in
      let (ty,response_time) =
        typing ?collect_sig ~env ~statics:statics_env ~genv e
      in
      (if !relax_flag then () else
         let t = canon_ty ty in
         match t with
         | Ty_fun(_,dur,_) ->
            (try unify_dur ~loc:(loc_of e) dur Dur_zero
             with CannotUnify _ -> 
               let open Prelude.Errors in
               error (fun fmt ->
                   Format.fprintf fmt
                     "@[<v>This program has type %a. It is not reactive. @]" (* Hint: use eta-expansion. *)
                     (emph_pp green pp_ty) t))
         | _ -> assert false);

      let () =
         let t = canon_ty ty in
         match t with
         | Ty_fun(ty_arg,_,_) ->
           (try
              unify_ty ~loc:(loc_of e) 
                 ty_arg (Ty_base (new_tyB_unknown()));
            with CannotUnify _ ->
             let open Prelude.Errors in
             error (fun fmt ->
                 Format.fprintf fmt
                   "@[<v>The type of the program input should be a basic type. @]"))
         | _ -> assert false
      in
      List.iter (fun a -> typing ~env ~msg:"checking inputs given by option -arg, "
                    ~statics:statics_env ~genv
                    (ty_annot ~ty:t_arg a)
                          |> ignore) arg_list;

      unify_ty ~loc (Ty_fun(t_arg,new_dur_unknown(),new_tyB_unknown())) ty;

      (canon_ty ty, response_time)
    ) ()


let typing_pi pi =
  typing_with_argument pi []