open Fsm_syntax

let emit_warning_flag = ref false

(** [canon t] put the type [t] in canonical form by replacing
  instantiated variables free in [t] by their definition,
  themselves put in canonical form. *)
let rec canon = function
  | TVar{contents=V _} as t -> t
  | TVar{contents=T t} -> canon t
  | TInt tz -> TInt (canon tz)
  | TBool | TUnit | TString _ as t -> t
  | TStatic{elem;size} ->TStatic{elem=canon elem;size=canon size}
  | TTuple ts -> TTuple(List.map canon ts)
  | TVect _ as t -> t
  | TSize _ as t -> t

(** [size_ty t] returns the size (in number of bytes) of type [t]
  Unspecified size are fixe to 32 bits by default
  (customizable via argument [?when_tvar]) *)
let rec size_ty =
  let seen = ref false in
  let when_tvar = 32 in
  fun t ->
    match canon t with
    | TInt n -> size_ty n
    | TBool -> 1
    | TUnit -> 1
    | TTuple ts -> List.fold_left (+) 0 (List.map size_ty ts)
    | TVar _ ->
        if not !seen then begin seen := true;
          let open Prelude.Errors in
          if !emit_warning_flag then warning (fun fmt -> 
            Format.fprintf fmt "Unknown value size in the generated code replaced by a %d bits range size.\n" when_tvar);
        end;
        when_tvar
    | TString tz -> (size_ty tz * 8)
    | TStatic{elem;size} -> size_ty elem * size_ty size
    | TVect n -> n
    | TSize n -> n


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
  | TStatic {elem ; size} -> string_of_ty elem ^ " buffer<" ^ string_of_ty size ^ ">"

exception CannotUnify of (ty*ty)

let rec unify t1 t2 =
  let cannot_unify t1 t2 = raise (CannotUnify(t1,t2)) in
  (* todo: check cycle (eg. 'a ~ ('a * 'a)) *)
  (* Printf.printf "%s" ("---->"^("unify "^string_of_ty(canon t1)^" and "^string_of_ty (canon t2) ^"\n")); *)
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
  | TVar ({contents=V n} as r),TVar {contents=V n'} -> if n = n' then () else r := T t2; ()
  | TVar {contents=T t1},t2 | t1,TVar {contents=T t2} -> unify t1 t2
  | TVar ({contents=V _} as r),t | t,TVar ({contents=V _} as r) ->
    r := T t
  | TString tz,TString tz' ->
      unify tz tz'
  | TStatic{elem=te;size=tz},TStatic{elem=te';size=tz'} ->
      unify te te';
      unify tz tz'
  | t1,t2 -> cannot_unify t1 t2

let add_typing_env h (x:string) (t:ty) =
  (match Hashtbl.find_opt h x with
   | None -> Hashtbl.add h x (canon t)
   | Some t' -> unify t t'; Hashtbl.replace h x (canon t'))

let compute_tag_size cs =
  let n = int_of_float @@ Float.ceil @@ Float.log2 @@ float @@ List.length cs in
  max 4 n

let rec translate_ty =
  let hvar = Hashtbl.create 10 in
  function
  | Types.T_const(TInt tz) -> TInt (translate_ty tz)
  | Types.T_const(TBool) -> TBool
  | Types.T_const(TUnit) -> TUnit
  | Types.T_tuple(ts) -> TTuple (List.map translate_ty ts)
  | Types.T_var r ->
      if Hashtbl.mem hvar r then Hashtbl.find hvar r else
      (let t = TVar(ref @@ match !r with
                   | Unknown n -> V (string_of_int n)
                   | Ty t -> T (translate_ty t)) in
      Hashtbl.add hvar r t;
      t)
  | Types.T_string tz ->
      (match Types.canon tz with
      | T_size n -> TString (TSize (n*8))
      | T_var _ -> new_tvar()
      | _ -> assert false) (* TODO *)
  | Types.T_static(t) -> translate_ty t
  | Types.T_sum cs ->
      let size_tag = compute_tag_size cs in
      let n = List.fold_left (max) 0 @@ List.map (fun (_,t) -> size_ty (translate_ty t)) cs in
      TTuple[TInt(TSize size_tag);TVect(n)]
  | Types.T_size n -> TSize n
  | Types.T_ref t -> translate_ty t
  | Types.T_array{elem=te;size=tz} -> TStatic{elem=translate_ty te;size=translate_ty tz}
  | Types.T_matrix{elem=te;size=tz} -> TStatic{elem=translate_ty te;size=translate_ty tz}
  | Types.(T_response_time _|T_infinity|T_fun _|T_add (_, _)|T_max (_, _)|T_le (_, _)) ->
     assert false (* already expanded *)
  | Types.T_forall _ -> assert false (* already expanded *)


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
  |  (String s) -> TString (TSize(String.length s))

let rec typing_op h t op =
  match op with
  | Runtime p ->
      (match Operators.ty_op p with
       | Types.T_fun{arg;dur;ret} ->
          unify (translate_ty arg) t;
          translate_ty ret
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
  | Compute_address ->
      let w = (new_tvar ()) in
      unify (TTuple [TInt (TSize 32);TInt w]) t;
      t


let trace_last_exp = ref (A_const Unit)

let rec typing_a h a =
  trace_last_exp := a;
  match a with
  | A_const c ->
      typing_c c
  | A_var x ->
      let t = (new_tvar ()) in
      add_typing_env h x t;
      t
  | A_call(op,args) ->
      let t = typing_a h args in
      typing_op h t op
  | A_tuple es ->
      TTuple (List.map (typing_a h) es)
  | A_letIn(x,a1,a2) ->
      let t = typing_a h a1 in
      add_typing_env h x t;
      typing_a h a2
  | A_string_get(sx,ix) ->
      add_typing_env h sx (TString (new_tvar()));
      add_typing_env h ix (TInt (TSize 32));
      TInt(TSize 8)

  | A_ptr_taken(x) 
  | A_ptr_write_taken(x) ->
      let telem = new_tvar () in
      let tz = new_tvar () in
      add_typing_env h x (TStatic{elem=telem;size=tz});
      TBool

  | A_buffer_get(xb) ->
      let telem = new_tvar () in
      let tz = new_tvar () in
      add_typing_env h xb (TStatic{elem=telem;size=tz});
      telem

  | A_buffer_length(x,ty) ->
      add_typing_env h x (TStatic{elem=new_tvar();size=new_tvar()});
      TInt ty

  | A_buffer_matrix_length(x,n,ty) ->
      add_typing_env h x (TStatic{elem=new_tvar();
                                  size=new_tvar()});
      TInt ty

  | A_encode(x,ty,n) ->
      add_typing_env h x ty;
      assert (size_ty ty <= n);
      TVect n

  | A_decode(x,ty) ->
      add_typing_env h x (new_tvar());
      ty


let rec typing_s ~result h = function
  | S_skip -> ()
  | S_set(x,a) ->
      let t = typing_a h a in
      (* (Format.fprintf Format.std_formatter "======> (%s : %a)\n" x Fsm_syntax.Debug.pp_ty (canon t)); *)
      add_typing_env h x t
  | S_setptr_read(x,idx) ->
      let telem = new_tvar () in
      let tz = new_tvar () in
      let tz2 = new_tvar () in
      add_typing_env h x (TStatic{elem=telem;size=tz});
      let tidx = typing_a h idx in
      unify tidx (TInt tz2)
  | S_setptr_write(x,idx,a) ->
      let telem = typing_a h a in
      let tz = new_tvar () in
      let tz2 = new_tvar () in
      add_typing_env h x (TStatic{elem=telem;size=tz});
      let tidx = typing_a h idx in
      unify tidx (TInt tz2)
  | S_setptr_matrix_read(x,idx_list) ->
      let telem = new_tvar () in
      let tz_list = List.map (fun _ -> new_tvar ()) idx_list in
      let tz2 = new_tvar () in
      add_typing_env h x (TStatic{elem=telem;size=TTuple tz_list});
      let tidx_list = List.map (typing_a h) idx_list in
      List.iter2 (fun tidx tz -> unify tidx (TInt tz2)) tidx_list tz_list
  | S_setptr_matrix_write(x,idx_list,a) ->
      let telem = typing_a h a in
      let tz_list = List.map (fun _ -> new_tvar ()) idx_list in
      let tz2 = new_tvar () in
      add_typing_env h x (TStatic{elem=telem;size=TTuple tz_list});
      let tidx_list = List.map (typing_a h) idx_list in
      List.iter2 (fun tidx tz -> unify tidx (TInt tz2)) tidx_list tz_list
  | S_ptr_take(x,_)
  | S_ptr_write_take(x,_) ->
      let telem = new_tvar () in
      let tz = new_tvar () in
      add_typing_env h x (TStatic{elem=telem;size=tz})
  | S_buffer_set(x) ->
      let t = new_tvar () in
      add_typing_env h x t
  | S_if(x,s,so) ->
      add_typing_env h x TBool;
      typing_s ~result h s;
      Option.iter (typing_s ~result h) so
  | S_case(x,hs,os) ->
      let t = new_tvar () in
      add_typing_env h x t;
      List.iter (fun (c,s) ->
          unify (typing_c c) t;
          typing_s ~result h s) hs;
      (match os with
      | None -> ()
      | Some s_els -> typing_s ~result h s_els)
  | S_seq(s1,s2) ->
      typing_s ~result h s1; typing_s ~result h s2
  | S_continue _ ->
      ()
  | S_letIn(x,a,s) ->
      (add_typing_env h x (typing_a h a));
      typing_s ~result h s
  | S_fsm(_,rdy,result2,_,ts,s,_) ->
      typing_fsm h ~rdy ~result:result2 ~ty_result:(new_tvar()) (ts,s)
  | S_in_fsm(_,s) ->
      typing_s ~result h s
  | S_call(op,args) ->
      let t = typing_a h args in
      unify (typing_op h t (Runtime op)) TUnit

(* typing of an fsm *)
and typing_fsm h ~rdy ~result ~ty_result (ts,s) =
  add_typing_env h rdy TBool;
  add_typing_env h result ty_result;
  typing_s ~result h s;
  List.iter (fun (q,s) ->
      typing_s ~result h s) ts


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


let typing_circuit ~statics ty (rdy,result,fsm) =
  typing_error_handler @@ fun () ->
    let h = Hashtbl.create 64 in

    List.iter (function 
      | x,Static_array(c,n) -> add_typing_env h x (TStatic{elem=typing_c c;size=TSize n})
      | x,Static_matrix(c,n_list) -> 
            add_typing_env h x (TStatic{elem=typing_c c;
                                        size=TTuple (List.map (fun n -> TSize n) n_list)})
          ) statics;

    let t1,t2 = match ty with Types.T_fun{arg=t1;dur=_;ret=t2} -> t1,t2 | _ -> assert false (* err *)
    in
    typing_fsm h ~rdy ~result ~ty_result:(translate_ty t2) fsm;

    List.iter (function 
      | x,Static_array(c,n) -> add_typing_env h x (TStatic{elem=typing_c c;size=TSize n})
      | x,Static_matrix(c,n_list) -> add_typing_env h x (TStatic{elem=typing_c c;size=TTuple (List.map (fun n -> TSize n) n_list)})
    ) statics;

    add_typing_env h "argument" (translate_ty @@ Types.canon t1);   (* NB: does not work without canon *)
    add_typing_env h result (translate_ty @@ Types.canon t2);

    h
