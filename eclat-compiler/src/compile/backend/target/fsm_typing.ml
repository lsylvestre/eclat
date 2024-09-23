open Fsm_syntax

let emit_warning_flag = ref false

(** [canon t] put the type [t] in canonical form by replacing
  instantiated variables free in [t] by their definition,
  themselves put in canonical form. *)
let rec canon = function
  | TVar{contents=V _} as t -> t
  | TVar({contents=T t'} as v) ->
      let t2 = canon t' in
      v := T t2; t2
  | TInt tz -> TInt (canon tz)
  | TBool | TUnit | TString _ as t -> t
  | TStatic{elem;size} -> TStatic{elem=canon elem;size=canon size}
  | TTuple ts -> TTuple(List.map canon ts)
  | TVector{elem;size} -> TVector{elem=canon elem;size=canon size}
  | TVect _ as t -> t
  | TSize _ as t -> t
  | TAbstract(x,ns,ts) -> TAbstract(x,List.map canon ns,List.map canon ts)
(** [size_ty t] returns the size (in number of bytes) of type [t]
  Unspecified size are fixe to 32 bits by default
  (customizable via argument [?when_tvar]) *)
let rec size_ty =
  let when_tvar = 32 in
  fun t ->
    match canon t with
    | TInt n -> size_ty n
    | TBool -> 1
    | TUnit -> 1
    | TTuple ts -> List.fold_left (+) 0 (List.map size_ty ts)
    | TVar _ ->
          let open Prelude.Errors in
          if !emit_warning_flag then warning (fun fmt -> 
            Format.fprintf fmt "Unknown value size in the generated code replaced by a %d bits range size.\n" when_tvar);
        
          when_tvar
    | TString tz -> (size_ty tz * 8)
    | TStatic{elem;size} | TVector{elem;size} -> size_ty elem * size_ty size
    | TVect n -> n
    | TSize n -> n
    | TAbstract(x,ns,tys) ->
        let prod_ns = List.fold_left ( * ) 1 (List.map size_ty ns) in
        let sum_ts = if tys = [] then 1 else List.fold_left (+) 0 (List.map size_ty tys) in
        match Hashtbl.find_opt Ast.typ_decl_abstract x with
        | None ->
            let k = prod_ns * sum_ts in
            if List.length tys > 1 then   
              Prelude.Errors.warning (fun fmt -> 
                Format.fprintf fmt "unknown size for type %s is replaced by %d\n" x k
              );
            k
        | Some ("only_size_sum",_,_) -> List.fold_left (+) 0 (List.map size_ty ns)
        | Some ("mul",_,_) -> prod_ns * sum_ts
        | Some ("only_size",_,_) -> prod_ns
        | _ -> assert false (* todo *)

let rec string_of_ty = function
  | TInt tz -> "int<"^string_of_ty tz^">"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TString tz -> "string<"^string_of_ty tz^">"
  | TTuple ts -> "("^(String.concat "*" @@ List.map string_of_ty ts)^")"
  | TVar{contents=T t} -> string_of_ty t
  | TVar{contents=V s} -> s
  | TVect n -> "vect<"^string_of_int n^">"
  | TSize n -> "size<"^string_of_int n^">"
  | TVector {elem ; size} -> string_of_ty elem ^ " vector<" ^ string_of_ty size ^ ">"
  | TStatic {elem ; size} -> string_of_ty elem ^ " static<" ^ string_of_ty size ^ ">"
  | TAbstract(x,ns,ts) ->  "("^(String.concat "," @@ List.map string_of_ty ts)^") " 
                           ^ x^"<"^ (String.concat "," @@ List.map string_of_ty ns) ^">" 
exception CannotUnify of (ty*ty)

let rec unify t1 t2 =
  let cannot_unify t1 t2 = raise (CannotUnify(t1,t2)) in
  (* todo: check cycle (eg. 'a ~ ('a * 'a)) *)
  (*Printf.printf "%s" ("---->"^("unify "^string_of_ty(canon t1)^" and "^string_of_ty (canon t2) ^"\n")); *)
  match canon t1,canon t2 with
  | TInt tz1, TInt tz2 ->
      if not (Fix_int_lit_size.is_set ()) then () else
      begin
         unify tz1 (TSize (Fix_int_lit_size.get_size_type ()))
      end;
      unify tz1 tz2
  | TVect n, TVect m
  | TSize n, TSize m -> if n <> m then cannot_unify t1 t2
  | TBool,TBool -> ()
  | TUnit,TUnit -> ()
  | TTuple ts,TTuple ts' ->
    if List.compare_lengths ts ts' <> 0 then cannot_unify t1 t2
    else List.iter2 unify ts ts'
  | TVector{elem=te;size=tz},TVector{elem=te';size=tz'} ->
      unify te te';
      unify tz tz'
  | TVar ({contents=V n} as r),TVar {contents=V n'} -> if n = n' then () else r := T t2; ()
  | TVar {contents=T t1},t2 | t1,TVar {contents=T t2} -> unify t1 t2
  | TVar ({contents=V _} as r),t | t,TVar ({contents=V _} as r) ->
    r := T t
  | TString tz,TString tz' ->
      unify tz tz'
  | TStatic{elem=te;size=tz},TStatic{elem=te';size=tz'} ->
      unify te te';
      unify tz tz'
  | TAbstract(x,ns,ts),TAbstract(x',ns',ts') ->
      if x <> x' 
      || List.compare_lengths ts ts' <> 0
      || List.compare_lengths ns ns' <> 0 
      then cannot_unify t1 t2
      else (List.iter2 unify ts ts'; List.iter2 unify ns ns')
  | t1,t2 -> cannot_unify t1 t2

let add_typing_env h (x:string) (t:ty) =
  (match Hashtbl.find_opt h x with
   | None -> Hashtbl.add h x (canon t)
   | Some t' -> unify t t'; Hashtbl.replace h x (canon t'))

let compute_tag_size cs =
  let n = int_of_float @@ Float.ceil @@ Float.log2 @@ float @@ List.length cs in
  max 4 n


let rec translate_tyB =
  let open Types in
  let hvar = Hashtbl.create 10 in
  function
  | TyB_int sz -> TInt (translate_size sz)
  | TyB_bool -> TBool
  | TyB_unit -> TUnit
  | TyB_tuple tyBs -> TTuple (List.map translate_tyB tyBs)
  | TyB_var r -> 
     if Hashtbl.mem hvar r then Hashtbl.find hvar r else
      (let t = TVar(ref @@ match !r with
                   | Unknown n -> V (string_of_int n)
                   | Is t -> T (translate_tyB t)) in
      Hashtbl.add hvar r t;
      t)
  | TyB_abstract(x,szs,tyB_list) -> 
      TAbstract(x,List.map  translate_size szs, List.map translate_tyB tyB_list)
  | TyB_sum(cs) ->
      let size_tag = compute_tag_size cs in
      let n = List.fold_left (max) 0 @@ List.map (fun (_,t) -> size_ty (translate_tyB t)) cs in
      TTuple[TInt(TSize size_tag);TVect(n)]
  | TyB_string sz -> TString (translate_size sz)
  | TyB_size sz -> translate_size sz

  and translate_size = 
    let open Types in
  let hvar = Hashtbl.create 10 in
  function
  | Sz_lit n -> TSize n
  | Sz_var r -> 
     if Hashtbl.mem hvar r then Hashtbl.find hvar r else
      (let t = TVar(ref @@ match !r with
                   | Unknown n -> V (string_of_int n)
                   | Is sz -> T (translate_size sz)) in
      Hashtbl.add hvar r t;
      t)

let rec translate_ty =
  let hvar = Hashtbl.create 10 in
  let open Types in
  function
  | Ty_base(tyB) -> translate_tyB tyB
  | Ty_tuple(ts) -> TTuple (List.map translate_ty ts)
  | Ty_var r -> 
     if Hashtbl.mem hvar r then Hashtbl.find hvar r else
      (let t = TVar(ref @@ match !r with
                   | Unknown n -> V (string_of_int n)
                   | Is t -> T (translate_ty t)) in
      Hashtbl.add hvar r t;
      t) (*todo*) 
  (* *)
  (* | Types.T_static(t) -> translate_ty t
  *)
  | Ty_ref tyB -> translate_tyB tyB
  | Ty_array(sz,tyB) -> TStatic{elem=translate_tyB tyB;size=translate_size sz}
  | Ty_fun _ -> assert false

let rec typing_c = function
  |  Unit -> TUnit
  |  (Int{value=_;tsize=tz}) ->
       if not (Fix_int_lit_size.is_set ()) then () else begin
         unify tz (TSize (Fix_int_lit_size.get_size_type ()))
       end;
       TInt tz
  |  (Bool _) -> TBool
  |  (Enum _) -> (new_tvar ()) (* TODO! *)
  |  (CTuple cs) -> TTuple(List.map typing_c cs)
  |  (CVector cs) -> 
       let v = new_tvar() in
       List.iter (fun c -> unify (typing_c c) v) cs;
       TAbstract("vect",[TSize (List.length cs)],[v])
  |  (String s) -> TString (TSize(String.length s))
  |  (CSize n) -> TSize n
  | C_encode(c,n) ->
      let _ = typing_c c in
      TVect n

let rec typing_op ~externals h t op =
  match op with
  | Runtime (External_fun (x,tyy)) ->
      (match List.assoc_opt x (snd externals) with
       | Some (ty,_) -> 
          let ty = Types.(instance (generalize [] ty)) in
          Typing.unify_ty ~loc:Prelude.dloc ty tyy;
          (match Types.canon_ty ty with
           | Types.Ty_fun(arg,_,ret) ->
              unify (translate_ty arg) t;
              translate_tyB ret
           | _ -> assert false)
       | _ -> assert false)
  | Runtime p ->
      (match Operators.ty_op ~externals p with
       | Types.Ty_fun(arg,dur,ret) ->
          unify (translate_ty arg) t;
          translate_tyB ret
       | _ -> assert false)
  | If ->
         let a = new_tvar () in
         unify (TTuple [TBool;a;a]) t;
         a
  | GetTuple(i,n,ty) ->
        unify ty t;
        let ts = List.init n (fun _ -> new_tvar()) in
        unify ty (TTuple (ts));
        List.nth ts i
  | TyConstr ty ->
      unify ty t;
      t

let trace_last_exp = ref (A_const Unit)

let rec typing_a ~externals h a =
  trace_last_exp := a;
  match a with
  | A_const c ->
      typing_c c
  | A_var x ->
      let t = (new_tvar ()) in
      add_typing_env h x t;
      t
  | A_call(op,args) ->
      let t = typing_a ~externals h args in
      typing_op ~externals h t op
  | A_tuple es ->
      TTuple (List.map (typing_a ~externals h) es)
  | A_vector es ->
       let v = new_tvar() in
       List.iter (fun a -> unify (typing_a ~externals h a) v) es;
       TAbstract("vect",[TSize (List.length es)],[v])
  | A_letIn(x,a1,a2) ->
      let t = typing_a ~externals h a1 in
      add_typing_env h x t;
      typing_a ~externals h a2
  | A_string_get(sx,ix) ->
      add_typing_env h sx (TString (new_tvar()));
      add_typing_env h ix (TInt (TSize 32));
      TInt(TSize 8)

  | A_ptr_taken(x) 
  | A_ptr_write_taken(x) ->
      (*let telem = new_tvar () in
      let tz = new_tvar () in
       add_typing_env h x (TStatic{elem=telem;size=tz}); *)
      TBool

  | A_buffer_get(xb) ->
      let telem = new_tvar () in
      let tz = new_tvar () in
      add_typing_env h xb (TStatic{elem=telem;size=tz});
      telem

  | A_buffer_length(x,ty) ->
      add_typing_env h x (TStatic{elem=new_tvar();size=new_tvar()});
      TInt ty

  | A_encode(x,ty,n) ->
      add_typing_env h x ty;
      assert (size_ty ty <= n);
      TVect n

  | A_decode(x,ty) ->
      add_typing_env h x (new_tvar());
      ty

let error_unbound_external x =
  Prelude.Errors.error (fun fmt -> 
    Format.fprintf fmt "@,unbound external %s" x
  ) 

let rec typing_s ~externals ~result h s =
  (* Printf.printf "==> %d\n" (Hashtbl.length h); flush stdout; *)
  match s with
  | S_skip -> ()
  | S_set(x,a) ->
      let t = typing_a ~externals h a in
      (* (Format.fprintf Format.std_formatter "======> (%s : %a)\n" x Fsm_syntax.Debug.pp_ty (canon t)); *)
      add_typing_env h x t
  | S_acquire_lock(l) 
  | S_release_lock(l) ->
      ()
      (* let telem = new_tvar () in
      let tz = new_tvar () in
      add_typing_env h l (TStatic{elem=telem;size=tz}) *)
  | S_read_start(x,idx) ->
      let telem = new_tvar () in
      let tz = new_tvar () in
      let tz2 = new_tvar () in
      add_typing_env h x (TStatic{elem=telem;size=tz});
      let tidx = typing_a ~externals h idx in
      unify tidx (TInt tz2)
  | S_read_stop(x,l) ->
      let telem = new_tvar () in
      let tz = new_tvar () in
      add_typing_env h x telem;
      add_typing_env h l (TStatic{elem=telem;size=tz})
  | S_write_start(x,idx,a) ->
      let telem = typing_a ~externals h a in
      let tz = new_tvar () in
      let tz2 = new_tvar () in
      add_typing_env h x (TStatic{elem=telem;size=tz});
      let tidx = typing_a ~externals h idx in
      unify tidx (TInt tz2)
  | S_write_stop(x) ->
      let t = new_tvar () in
      add_typing_env h x t 
  | S_if(x,s,so) ->
      add_typing_env h x TBool;
      typing_s ~externals ~result h s;
      Option.iter (typing_s ~externals ~result h) so
  | S_case(x,hs,os) ->
      let t = new_tvar () in
      add_typing_env h x t;
      List.iter (fun (cs,s) ->
          List.iter (fun c -> unify (typing_c c) t) cs;
          typing_s ~externals ~result h s) hs;
      (match os with
      | None -> ()
      | Some s_els -> typing_s ~externals ~result h s_els)
  | S_seq(s1,s2) ->
      typing_s ~externals ~result h s1; typing_s ~externals ~result h s2
  | S_continue _ ->
      ()
  | S_letIn(x,a,s) ->
      (add_typing_env h x (typing_a ~externals h a));
      typing_s ~externals ~result h s
  | S_fsm(_,rdy,result2,_,ts,s) ->
      typing_fsm h ~externals ~rdy ~result:result2 ~ty_result:(new_tvar()) (ts,s)
  | S_in_fsm(_,s) ->
      typing_s ~externals ~result h s
  | S_call(op,args) ->
      let t = typing_a ~externals h args in
      unify (typing_op ~externals h t (Runtime op)) TUnit
  | S_external_run(f,i,res,rdy,a) ->
      (match List.assoc_opt f (fst externals) with
      | None -> error_unbound_external(f)
      | Some (ty,_) -> 
          (match Types.canon_ty ty with
          | Types.Ty_fun(arg,_,ret_ty) ->
              add_typing_env h res (translate_tyB ret_ty);
              add_typing_env h rdy TBool;
              unify (translate_ty arg) (typing_a ~externals h a)
          | _ -> assert false))

(* typing of an fsm *)
and typing_fsm h ~externals ~rdy ~result ~ty_result (ts,s) =
  add_typing_env h rdy TBool;
  add_typing_env h result ty_result;
  typing_s ~externals ~result h s;
  List.iter (fun (q,s) ->
      typing_s ~externals ~result h s) ts


let typing_error_handler f =
  try
    f ()
  with CannotUnify(t1,t2) ->
    let open Prelude.Errors in
    error (fun fmt ->
    Format.fprintf fmt "@,In the generated code, expression %a has type %a but an expression was expected of type %a"
               (emph_pp purple Debug.pp_a) !trace_last_exp
               (emph_pp green Debug.pp_ty) t1
               (emph_pp green Debug.pp_ty) t2)


let typing_circuit ~statics ~externals ty (rdy,result,fsm) =
  typing_error_handler @@ fun () ->
    let h = Hashtbl.create 64 in

    List.iter (function 
      | x,Static_array_of ty -> add_typing_env h x ty
      | x,Static_array(c,n) -> add_typing_env h x (TStatic{elem=typing_c c;size=TSize n})
          ) statics;

    let t1,t2 = match ty with Types.Ty_fun(t1,_,t2) -> t1,t2 | _ -> assert false (* err *)
    in
    typing_fsm h ~externals ~rdy ~result ~ty_result:(translate_tyB t2) fsm;


    (* why two times ? *)
    List.iter (function 
      | x,Static_array_of ty -> add_typing_env h x ty
      | x,Static_array(c,n) -> add_typing_env h x (TStatic{elem=typing_c c;size=TSize n})
    ) statics;

    add_typing_env h "argument" (translate_ty @@ Types.canon_ty t1);   (* NB: does not work without canon *)
    add_typing_env h result (translate_tyB @@ Types.canon_tyB t2);

    h
