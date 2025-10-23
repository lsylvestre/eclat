open Ast

let ref_set_lock_flag = ref true

module NameC = Naming_convention

module SMap = Map.Make(String)

(** each function definition (refered by its name [f]) is associated to
    a mapping between instance numbers of [f] and their corresponding
    continuations (i.e., target instructions). *)
module IMap = Map.Make(Int)

let (++) = (++)

let (+++) s1 s2 =
  IMap.fold IMap.add s1 s2

let (++>) s1 s2 =
  SMap.union (fun x s1 s2 -> Some (s1 +++ s2)) s1 s2

(** warning: (++>) has the same type than (++) but behaves differently *)

let instance_of_int n =
  "instance" ^ string_of_int n

let new_instance =
  let c  = ref 0 in
  (fun () -> incr c; !c)

let mk_idle id = id ^"_idle"
let mk_state id = id ^"_state"
let mk_rdy id = id ^"_rdy"
let mk_result id = id ^"_result"

let sum_global : (x * (Types.ty * (x * Types.tyB) list)) list ref = ref []

let idle_result_to_result ?(otherwise=None) id k e' =
  let idle = mk_idle id in
  let result = mk_result id in
  E_match(e',[idle,(P_var result, (E_var result))],otherwise)


let rec conjunction ids =
  match ids with
  | [] -> E_const (Bool true)
  | [id] -> E_var (mk_rdy id)
  | [id1;id2] -> 
       E_app(E_const(Op(Runtime (External_fun("Bool.land",Types.new_ty_unknown())))), 
              E_tuple([E_var(mk_rdy id1); E_var(mk_rdy id2)]))
  | id1::ids' -> let e = conjunction ids' in 
                  E_app(E_const(Op(Runtime (External_fun("Bool.land",Types.new_ty_unknown())))), E_tuple([E_var(mk_rdy id1);e]))


let default_value tyB =
   E_app(E_const(Op(Runtime (Default (Types.new_tyB_unknown())))), E_const Unit)
  (* let open Types in
  let open Ast in
  match Types.canon_ty ty with
  | Ty_base tyB -> 
     let rec default_value_tyB = function
     | TyB_var _ -> assert false
     | TyB_int sz -> Int(0,sz)
     | TyB_bool -> Bool false
     | TyB_unit -> Unit
     | TyB_tuple tyBs -> C_tuple (List.map default_value_tyB tyBs)
       (* | TyB_sum of (tag * tyB) list
         | TyB_string of size
         | TyB_size of size
         | TyB_abstract of x * size list * tyB list (* size in number of bits *)
*)
      | _ -> assert false (* todo *)
    in default_value_tyB tyB
  | _ -> assert false *)


let rec insert_kont w ~id tyB_result e =
   (* Format.fprintf Format.std_formatter "[%a]\n" Ast_pprint.pp_exp  e; Stdlib.(flush stdout);*)
  match e with
  | E_app(E_var q0,_) -> Printf.printf "===>%s %b\n" q0 (NameC.is_return q0);

     if NameC.is_return q0 then (Printf.printf "hoho\n";

          let rq0 = NameC.return_name q0 in
          (* let arg = E_tuple[E_var (NameC.formal_param_of_fun rq0); E_var (NameC.instance_id_of_fun rq0)]  in
          let e0 = E_app(E_const (Inj rq0), arg) in*)
          (match SMap.find_opt q0 w with
           | None -> Printf.printf "===>TADA\n"; E_app(E_const(Inj rq0), 
            E_tuple[default_value tyB_result; E_var (NameC.instance_id_of_fun rq0)])
           (*default_value (Types.new_tyB_unknown())*)(* ok? caution: no environment precedes the function call ? *)
           | Some u ->
           let l = List.of_seq (IMap.to_seq u) in
           (* sum_global := (id,(Types.new_ty_unknown(),List.map (fun (n,_) -> string_of_int n,Types.new_tyB_unknown()) l))::!sum_global; *)
           (match l with
            | [] -> assert false (* e0*) (* ok? *)
            | _ ->
               E_match(E_var (NameC.instance_id_of_fun rq0),
                   (List.map (fun (m,(p,k)) ->
                               (instance_of_int m),(group_ps (List.map fst p), (insert_kont w ~id tyB_result) k
                        )) l),None)))
        ) else e
| e -> Ast_mapper.map (insert_kont w ~id tyB_result) e


let optimization_filter_env' ~kont env_es =

  let m = Free_vars.fv kont in
  let rec loop acc p tyB =
    match p with
    | E_const Unit -> acc
    | E_var x -> if (NameC.is_instance_id_of_fun x)
                   || SMap.mem x m then (p,tyB)::acc else acc
    | E_tuple ps -> (* let w = Free_vars.fv p in 
                   if SMap.exists (fun x _ -> SMap.mem x m) w then (p,tyB)::acc else acc *)
                   (match Types.canon_tyB tyB with
                     | TyB_tuple ts -> 
                          List.fold_left2 loop acc ps ts
                     | TyB_var _ -> let ts = List.map (fun _ -> Types.new_tyB_unknown ()) ps in
                                    List.fold_left2 loop acc ps ts)
                   
  in
   let env' = List.fold_left (fun acc (p,t) -> loop acc p t) [] env_es in
   (* Printf.printf "CAFE: %d/%d\n" (List.length env_es) (List.length env'); *)
   env'

let optimization_filter_env ~kont env_es = optimization_filter_env' ~kont env_es


let env_extend ?(tyB=Types.new_tyB_unknown()) p env_es = 
  (* optimization *)
  if P_unit = p then env_es else
  (Pattern.pat2exp p,tyB)::env_es
  
  (* match p with 
  | P_unit -> env_es
  | P_var x -> if List.exists (fun (e,_) -> SMap.mem x (Free_vars.fv e)) env_es then 
  (Printf.printf "~~~found!\n"; env_es) else (Pattern.pat2exp p,tyB)::env_es
  | _ ->    (Pattern.pat2exp p,tyB)::env_es*)


let identity_kont ~idle =
  (fun u -> (E_app(E_const (Inj idle), u)))

(* Debug/Display *)
let [@warning "-26"] show q w =
   SMap.iter (fun x s -> Printf.printf "%s {%s : [" q x;
                        IMap.iter (fun i _ -> Printf.printf "%d," i) s;
                        Printf.printf "]}\n") w


(** [compile_exp ~externals id env_es ~statics ~externals gs e x k] translates expression [e] 
    to a target instruction setting a result in variable [x], 
    then executing instruction [k].
    [gs] is the names of the functions accessible 
    from [e] by a tail-call *)
let rec compile_exp ~externals id env_es gs e k =
  if Instantaneous.combinational ~externals e then (SMap.empty,SMap.empty,k e) else
  match e with
  | E_if(a,e1,e2) ->
      (* if Instantaneous.instantaneous ~externals e1 
        && Instantaneous.instantaneous ~externals e2 
      then (** Optimization to avoid duplication of the continuation [k] 
               Hypothesis: compiled of an instantaneous expression [e] with 
                 a given continuation [k'] is a triplet of the form <w,ts,e'> 
                 where [w] and [ts] are empty and e' has the type of k'[e0]
                 for a given expression [e0] having the type of [e].
               Idea: choose an empty continuation such as [k'[e0] = e0],
                compile instantaneous sub-expressions of [e] with [k'] and 
                form the resulting instantaneous expression equivalent to [e]. 
                the apply [k] to [e] **)
        let k' : e -> e = fun u -> u in
        let w1,ts1,e1' = compile_exp ~externals id [] gs e1 k' in
        let w2,ts2,e2' = compile_exp ~externals id [] gs e2 k' in
        assert (SMap.is_empty ts1);
        assert (SMap.is_empty ts2);
        assert (SMap.is_empty w1);
        assert (SMap.is_empty w2);
        SMap.empty, SMap.empty, k (E_if(a,e1', e2'))
      else*)
        (***********************************************)
        let w1,ts1,e1' = compile_exp ~externals id (optimization_filter_env ~kont:(k e1) env_es) gs e1 k in
        let w2,ts2,e2' = compile_exp ~externals id (optimization_filter_env ~kont:(k e2) env_es) gs e2 k in
        let z = gensym ~prefix:"tmp2-" () in
        (w1++>w2), (ts1 ++ ts2), E_if(a,e1', e2')
        (***********************************************)
  | E_case(a,hs,e_els) -> 
      let optimizable () =
        List.for_all (fun (_,e) -> Instantaneous.instantaneous ~externals e) hs 
        && Instantaneous.instantaneous ~externals e_els 
      in
      if optimizable () then
        let k' = fun u -> u in
        let ws,tss,es' = Prelude.map_split3 (fun (_,ei) -> compile_exp ~externals id (optimization_filter_env ~kont:(k' ei) env_es) gs ei k') hs in
        let wn,tsn,e_els' = compile_exp ~externals id (optimization_filter_env ~kont:(k' e_els) env_es) gs e_els k' in
        let ts = List.fold_left (++) tsn tss in
        let w' = List.fold_left (++>) wn ws in
        assert (SMap.is_empty ts);
        assert (SMap.is_empty w');
        let hs' = List.map2 (fun e' (n,_) -> (n,e')) es' hs in
        SMap.empty, ts, k (E_case(a,hs', e_els'))
      else
        (***********************************************)
        let ws,tss,es' = Prelude.map_split3 (fun (_,ei) -> compile_exp ~externals id (optimization_filter_env ~kont:(k ei) env_es) gs ei k) hs in
        let wn,tsn,e_els' = compile_exp ~externals id (optimization_filter_env ~kont:(k e_els) env_es) gs e_els k in
        let ts = List.fold_left (++) tsn tss in
        let w' = List.fold_left (++>) wn ws in
        let hs' = List.map2 (fun e (n,_) ->  (n,e)) es' hs in
        w', ts, E_case(a,hs', e_els')
        (***********************************************)
  | E_match(a,hs,eo) ->
      let optimizable () =
        (List.for_all (fun (_,(_,e)) -> Instantaneous.instantaneous ~externals e) hs 
        && (match eo with None -> true | Some e -> Instantaneous.combinational ~externals e)) 
      in
      if optimizable () then
        let k' = fun u -> u in
        let ws,tss,es' = Prelude.map_split3 (fun (_,(p,ei)) -> 
                  let env_es = env_extend p (optimization_filter_env ~kont:(k' ei) env_es) in
                  compile_exp ~externals id env_es gs ei k') hs in
        let wn,tsn,eo' = match eo with
                        | None -> SMap.empty,SMap.empty,None
                                  (* Some skip rather than None because
                                     the number of cases in the generated code
                                     must be a power of 2 *)
                        | Some e -> let w,ts,e' = compile_exp ~externals id (optimization_filter_env ~kont:(k' e) env_es) gs e k' in
                                    (w,ts,Some e')
        in
        let ts = List.fold_left (++) tsn tss in
        let w' = List.fold_left (++>) wn ws in
        assert (SMap.is_empty ts);
        assert (SMap.is_empty w');
        let hs'' = List.map2 (fun e' (ctor,(p,_)) -> (ctor,(p,e'))) es' hs in
        SMap.empty, ts, k (E_match(a,hs'', eo'))
      else
        (***********************************************)
        let ws,tss,es' = Prelude.map_split3 (fun (_,(p,ei)) -> 
                            let env_es = env_extend p (optimization_filter_env ~kont:(k ei) env_es) in
                             compile_exp ~externals id env_es gs ei k) hs in
        let wn,tsn,eo' = match eo with
                        | None -> SMap.empty,SMap.empty,None
                                  (* Some skip rather than None because
                                     the number of cases in the generated code
                                     must be a power of 2 *)
                        | Some e -> let w,ts,e' = compile_exp ~externals id (optimization_filter_env ~kont:(k e) env_es)  gs e k in
                                    (w,ts,Some e')
        in
        let ts = List.fold_left (++) tsn tss in
        let w' = List.fold_left (++>) wn ws in
        let hs'' = List.map2 (fun e' (ctor,(p,_)) -> (ctor,(p,e'))) es' hs in
        w', ts, E_match(a,hs'', eo')
        (***********************************************)
  (*| E_letIn(P_var f,tf,(E_fun(_,_,_) as e_fun),e1) ->
      let id2 = (gensym ~prefix:"id"()) in
      let tyB_res = Types.new_tyB_unknown() in
      let e1' = compile_bloc ~externals ((f,`Fun)::gs) id2 (Types.Ty_base tyB_res) e1 in
      let result = mk_result id2 in
      SMap.empty, SMap.empty, E_letIn(P_var f, tf,compile_fun ~externals gs e_fun, 
          E_letIn(P_var (mk_state id2),Types.new_ty_unknown (),e1',
            E_match(E_var (mk_state id2),
                  [mk_idle id2, (P_tuple([P_var (mk_result id2);P_var (mk_rdy id2)]), k(E_var (mk_result id2)))],
                  Some (E_var (mk_state id2)))))
      (* E_match(E_var (mk_state id2),[idle,(P_var result, E_tuple([E_var result;E_const (Bool true)]))],
                                             Some (E_tuple([default_value tyB_res;E_const (Bool false)])))
*)*)
  (*| E_letIn(P_var f,tyf,E_fix(h,(p,(ty,tyB),e1)),e2) when f <> h ->
      compile_exp ~externals id env_es gs (E_letIn(P_var f,tyf,(E_fix(f,(p,(ty,tyB),Ast_subst.subst_e h (E_var f) e1))),e2)) k
  *)| E_letIn(P_var f,_,(E_fix(h,(p,(ty,tyB),e1))),e2) as ee ->
     let tyB_arg = match Types.canon_ty ty with
                   | Types.Ty_base tyB_arg -> tyB_arg
                   | _ -> Types.new_tyB_unknown() in
     assert (f = h);
     let env_es' = env_extend ~tyB:tyB_arg p @@
                   env_extend (P_var(NameC.instance_id_of_fun f)) [](*env_es*) in
     let f' = NameC.mark_return f in
     let w1,ts1,e1' = compile_exp ~externals id (env_es') ((f',`Return)::(f,`Tail)::gs) e1
       (fun u -> (E_letIn(P_var(NameC.result_of_fun f),Types.Ty_base tyB,u,(E_app(E_var(f'),E_const  Unit))))) in
     let w2,ts2,e2' = compile_exp ~externals id (optimization_filter_env' ~kont:(k ee) env_es) ((f,`Direct)::gs) e2 k in
     (w1++>w2),(SMap.add f ((P_tuple[p; P_var (NameC.instance_id_of_fun f)]),e1') ts1)++ts2,e2'
  | E_letIn(p,t_y,e1,e2) ->
      let tyB = match Types.canon_ty t_y with
                      | Types.Ty_base tyB -> tyB
                      | _ -> Types.new_tyB_unknown()(*  assert false*) in
      (* [LET] *)
      let env' = env_extend ~tyB p (* (List.filter (function (E_var x,_) -> SMap.mem x uu | _ -> true) *) (optimization_filter_env ~kont:(k e2) env_es) in
      let w2,ts2,e2' = compile_exp ~externals id env' (*(E_var x,tyB)::env_es*) gs e2 k in
      (*!!!!!!!!!!*
      if Instantaneous.combinational ~externals e1 then
        w2,ts2,seq_ (set_ y (to_a ~externals ~sums e1)) s2
      else*!!!!!!!!*)
        let w1,ts1,e1' = compile_exp ~externals id (optimization_filter_env ~kont:(E_tuple[k e2'; e1]) env_es) gs e1 (fun u -> E_letIn(p,t_y,u,e2')) in (* (fun u -> E_letIn(P_unit,Ty_base TyB_unit,(E_set(E_var x,u)) *)
        w1++>w2,ts2++ts1,e1'
  | E_app(E_var f,a) as fa ->
      Printf.printf "-%s\n" f; Stdlib.(flush stdout);

      (match List.assoc_opt f gs with
       | Some `Tail -> (* [TAIL-CALL] *)
          SMap.empty, SMap.empty, (E_app(E_const (Inj f),E_tuple[a; E_var (NameC.instance_id_of_fun f)]))
       | Some `Direct -> 
          (* [DIRECT-CALL] *)Printf.printf "DIREct\n";
          let n = new_instance () in
          let w = SMap.singleton (NameC.mark_return f) (IMap.singleton n (List.map (fun (e,ty) -> Pattern.exp2pat e,ty) env_es, k (E_var (NameC.result_of_fun f)))) (* (seq_ (set_ x (A_var (NameC.result_of_fun f))) k))*) in
          w, SMap.empty, (E_app(E_const (Inj f),E_tuple[a; E_app(E_const (Inj (instance_of_int n)),group_es (List.map fst env_es))]))
       | Some `Fun ->
            SMap.empty, SMap.empty, (
            let res = gensym ~prefix:"result"() in
            E_letIn(P_var res,Types.new_ty_unknown(),E_app(E_var f,a),k(E_var res)))
       | Some `Return -> SMap.empty, SMap.empty, fa
       | _ -> Printf.printf "~~~~> %s\n" f; SMap.empty, SMap.empty, fa (* assert false)*)) (* ok ? *)
  | E_app(E_const _ as c,a) ->
      (* in case of instantaneous call which is not combinatorial,
         e.g., a display function for debug  *)
         (*let x = gensym ~prefix:"tmp1-" () in*)
      SMap.empty, SMap.empty, k (E_app(c, a))(* E_letIn(P_var x,Types.new_ty_unknown(),E_app(c, a), k (E_var x))*)
 
  (* | E_ref(a) -> SMap.empty, SMap.empty, 
                return_ (set_ x (to_a ~externals ~sums a))
  | E_get(ay) -> 
      (match ay with
      | E_var y ->
         if !ref_set_lock_flag then
           let q_wait = gensym ~prefix:"get_wait" () in
           let s0 = S_if(y^"_lock", (S_continue q_wait), Some (
                           (return_ (set_ x (A_var y))))) in
            (SMap.empty, SMap.add q_wait s0 SMap.empty, s0)
         else
           SMap.empty, SMap.empty, return_ (set_ x (A_var y))
      | _ -> assert false)
  | E_set(ay,a) ->
      if !ref_set_lock_flag then
      (match ay with
       | E_var y ->
          let q_wait = gensym ~prefix:"get_wait" () in
          let q1 = gensym ~prefix:"get_pause" () in
          let w,ts,s = compile_exp ~externals id env_es ~statics ~externals ~sums gs (E_const Unit) x k in
          let s0 = S_if(y^"_lock", 
                       S_continue q_wait, Some(seq_ (set_ (y^"_lock") (A_const (Bool true))) @@
                        seq_ (set_ y (to_a ~externals ~sums a)) @@ (S_continue q1))) in
          (w, (SMap.add q1 (seq_ (set_ (y^"_lock") (A_const (Bool false))) s) @@
               SMap.add q_wait s0 ts), s0)
       | _ -> assert false) else
      (match ay with
       | E_var y ->
          let w,ts,s = compile_exp ~externals id env_es ~statics ~externals ~sums gs (E_const Unit) x k in
          (w, ts, seq_ (set_ y (to_a ~externals ~sums a)) s)
       | _ -> assert false) 
  *)| E_array_get(x,idx) ->
        let env_es = (optimization_filter_env ~kont:(k e) env_es) in
        let q_then = gensym ~prefix:"q_then" () in
        let q_wait = gensym ~prefix:"q_wait" () in
        
        let e' = E_letIn(P_unit, Types.Ty_base TyB_unit, E_app(E_const(Op(Runtime (Acquire))), E_var x),
                 E_letIn(P_unit, Types.Ty_base TyB_unit, E_app(E_const(Op(Runtime (Start_read (Types.new_size_unknown())))), E_tuple[E_var x;idx]),
                 E_app(E_const (Inj q_then),group_es (List.map fst env_es)))) in
        let wait_e' = E_if(E_app(E_const(Op(Runtime (Locked))), E_var x), E_app(E_const (Inj q_wait),group_es (List.map fst env_es)), e') in
        let env_ps = group_ps (List.map Pattern.exp2pat (List.map fst env_es)) in
        let z = gensym ~prefix:"z" () in
        let ts = SMap.add q_wait (env_ps, wait_e') @@
                 SMap.singleton q_then ( env_ps, E_letIn(P_var z, Types.new_ty_unknown (), E_app(E_const(Op(Runtime (End_read))), E_var x),
                                                 E_letIn(P_unit, Types.Ty_base TyB_unit, E_app(E_const(Op(Runtime (Release))), E_var x),
                                                  k (E_var z)))

                 ) in
        SMap.empty,ts,wait_e'

  | E_array_set(x,idx,a) ->
let env_es = (optimization_filter_env ~kont:(k e) env_es) in
        let q_then = gensym ~prefix:"q_then" () in
        let q_wait = gensym ~prefix:"q_wait" () in
        let e' = E_letIn(P_unit, Types.Ty_base TyB_unit, E_app(E_const(Op(Runtime (Acquire))), E_var x),
                 E_letIn(P_unit, Types.Ty_base TyB_unit, E_app(E_const(Op(Runtime (Start_write (Types.new_size_unknown())))), E_tuple[E_var x;idx;a]),
                 E_app(E_const (Inj q_then),group_es (List.map fst env_es)))) in
        let env_ps = group_ps (List.map Pattern.exp2pat (List.map fst env_es)) in
        let wait_e' = E_if(E_app(E_const(Op(Runtime (Locked))), E_var x), E_app(E_const (Inj q_wait),group_es (List.map fst env_es)), e') in
        let ts = SMap.add q_wait (env_ps, wait_e') @@
                 SMap.singleton q_then ( env_ps, E_letIn(P_unit, Types.Ty_base TyB_unit, E_app(E_const(Op(Runtime (End_write))), E_var x),
                                                 E_letIn(P_unit, Types.Ty_base TyB_unit, E_app(E_const(Op(Runtime (Release))), E_var x),
                                                  k (E_const Unit)))

                 ) in
        SMap.empty,ts,wait_e'

  | E_reg((p,typ,e1),a,l) ->
      let env_es' = (optimization_filter_env ~kont:e1 env_es) in
      let w1,ts1,e1' = compile_exp ~externals id env_es' [] e1 (fun u -> E_letIn(p,Ty_base typ,u,Pattern.pat2exp p)) in
      assert (SMap.is_empty w1);
      assert (SMap.is_empty ts1);
      (SMap.empty, SMap.empty, k (E_reg((p,typ,e1'),a,l)))

  | E_exec(e1,e0,eo,id) ->
      (* assume e0 is combinational *)
      let tyB_res = (Types.new_tyB_unknown ()) in
      let tyB_state = (Types.new_tyB_unknown ()) in
      let idle = mk_idle id in
      let result = mk_result id in
      let d = default_value tyB_res in
      let e1' = E_reg((P_var (mk_state id),tyB_state, 
                      let e' = compile_bloc ~externals (optimization_filter_env ~kont:e1 env_es) gs id (Types.Ty_base tyB_res) e1 in
                      (match eo with
                       | None -> e'
                       | Some e2 -> E_if(e2,E_app(E_const (Inj idle), d), e'))),
                      (E_app(E_const (Inj idle), d)),id) in
      let e'' =
        E_letIn(P_var (mk_state id), Types.Ty_base tyB_state,e1',
        E_match(E_var (mk_state id),[idle,(P_var result, E_tuple([E_var result;E_const (Bool true)]))],
                                             Some (E_tuple([e0;E_const (Bool false)]))))
      in 
      let x = gensym ~prefix:"tmp" () in
      SMap.empty, SMap.empty, E_letIn(P_var x, Types.Ty_base (Types.TyB_tuple [tyB_res;TyB_bool]), e'',k (E_var x))

  | E_par(es) -> 
      let ids = List.map (fun e -> gensym ~prefix:"id" ()) es in
      let es' = List.map2 (fun id e -> let env_es =optimization_filter_env' ~kont:e env_es in compile_bloc ~externals env_es [] id (Types.new_ty_unknown()) e) ids es in
      let head = function
                 | E_match(_,(_,(_,e_idle))::hs,_) -> e_idle 
                 | _ -> assert false in
      let tail = function
                 | E_match(e1,(idle,(p,_))::hs,o) -> E_match(e1,(idle,(p,E_app(E_const (Inj idle),Pattern.pat2exp p)))::hs,o) 
                 | _ -> assert false in
      let x_test = gensym ~prefix:"x_test" () in
      let q1 = gensym ~prefix:"par" () in
      let e0 = let rec loop es ids1 = match es,ids1 with
                | [],[] -> E_letIn(P_var x_test,Types.Ty_base TyB_bool, conjunction ids,
                            E_if(E_var x_test,k (group_es (List.map (fun x -> E_var (mk_result x)) ids)),
                              E_app(E_const(Inj q1), (group_es (List.map (fun x -> E_var (mk_state x)) ids @ List.map fst env_es)))))
                | (e'::es'),(id::ids') ->
                   E_letIn(P_var (mk_state id),Types.new_ty_unknown(),head e',
                   E_letIn(P_tuple [P_var (mk_result id);
                                    P_var (mk_rdy id)], Types.new_ty_unknown(),
                                                                E_match(E_var (mk_state id),
                                                                [mk_idle id,(P_var (mk_result id), E_tuple[E_var (mk_result id);E_const (Bool true)])], 
                                                                Some(E_tuple [default_value (Types.new_tyB_unknown()); E_const (Bool false)])),
                    loop es' ids'))
                 in loop es' ids
      in
      let p_env = group_ps (List.map (fun x -> P_var (mk_state x)) ids @  List.map (fun (e,_) -> Pattern.exp2pat e) env_es) in
      let e1 =          
       let rec loop es ids1 = match es,ids1 with
        | [],[] -> E_letIn(P_var x_test,Types.Ty_base TyB_bool, conjunction ids,
                    E_if(E_var x_test,k (group_es (List.map (fun x -> E_var (mk_result x)) ids)),
                      E_app(E_const(Inj q1), (group_es (List.map (fun x -> E_var (mk_state x)) ids @ List.map fst env_es)))))
        | (e'::es'),(id::ids') ->
           E_letIn(P_var (mk_state id),Types.new_ty_unknown(),tail e',
           E_letIn(P_tuple [P_var (mk_result id);
                            P_var (mk_rdy id)], Types.new_ty_unknown(),
                                                        E_match(E_var (mk_state id),
                                                        [mk_idle id,(P_var (mk_result id), E_tuple[E_var (mk_result id);E_const (Bool true)])], 
                                                        Some(E_tuple [default_value (Types.new_tyB_unknown()); E_const (Bool false)])),
                    loop es' ids'))
                 in loop es' ids
       in
       SMap.empty,SMap.singleton q1 (p_env,e1), e0
  | E_run(f,a,l) as e1 ->
      let y = gensym ~prefix:"external_result" () in
      let cis,_ = externals in
      let ty,shared = List.assoc f cis in 
      let tyB_result,dur = match Types.canon_ty ty with
                      | Ty_fun(_,d,tyB) -> tyB,d
                      | _ -> assert false in
      
      (match dur with
      | Dur_zero -> SMap.empty, SMap.empty, E_letIn(P_var y,Ty_base tyB_result, e1,k (E_var y))
      | Dur_one ->
           let rdy = gensym ~prefix:"external_rdy" () in
           let q1 = gensym ~prefix:"q1" () in
           (* caution: both occurrences of expression [e1], i.e. [E_run(f,a,l)], share a same id [l] *)
           let env_ps = group_ps (List.map Pattern.exp2pat (List.map fst env_es)) in
           let env_es_e = group_es (List.map fst env_es) in
           let e1 = if shared then E_run(f,a,f) else E_run(f,a,l) in
            SMap.empty, SMap.singleton q1 (env_ps,E_letIn(P_tuple[P_var y;P_var rdy],Ty_base (TyB_tuple[tyB_result;TyB_bool]), e1,
                                             E_if(E_var rdy, k (E_var y),E_app(E_const(Inj q1),env_es_e)))), 
          E_letIn(P_tuple[P_var y;P_var rdy],Ty_base (TyB_tuple[tyB_result;TyB_bool]), e1,E_if(E_var rdy, k (E_var y),E_app(E_const(Inj q1),env_es_e))))
(*
      let es' = List.map snd @@ List.map (fun e -> 
        let id = gensym ~prefix:"id" () in
        compile_bloc ~externals id env_es (Types.new_ty_unknown()) e) es in
      let state_of = function E_match(ex,_,_) -> ex | _ -> assert false in
      let is_rdy = function E_match(ex,(idle,(p,_))::hs,_) -> E_match(ex,[(idle,(p,E_const (Bool true)))],Some (E_const(Bool false))) | _ -> assert false in

      
      let e1 = let rec loop = function
      | [] -> assert false(* List.fold_right (fun e' acc -> es' (E_if(E_const (Bool true)),
                                                   k (E_const Unit)
                                                   E_tuple(E_app(E_const(Inj q1),E_tuple(List.map state_of es'))::env_es) *)
      | e'::es' ->
         E_letIn(P_tuple(List.map Pattern.exp2pat ((state_of e')::env_es)),Types.new_ty_unknown(),tail e', loop es') 
       in loop es'
      in*)

      (* (let e0 = let rec loop = function
      | [] -> E_tuple(E_app(E_const(Inj q1),E_tuple(List.map state_of es'))::env_es)
      | e'::es' ->
         E_letIn(P_tuple(List.map Pattern.exp2pat ((state_of e')::env_es)),Types.new_ty_unknown(),head e', loop es') 
       in loop es' in
      let e1 = let rec loop = function
      | [] -> assert false
      | e'::es' ->
         (E_letIn(P_var p::List.map tail es',Types.new_ty_unknown(),head e', loop es') 
       in loop es'
      in*)
      (* SMap.empty, SMap.singeton q0 e1, e0*)
 (*
      let id_s = List.map (fun _ -> gensym ~prefix:"id" ()) es in
      let pi_s = List.map (fun e -> compile @@ {statics;externals;sums;main=e}) es in
      if List.for_all (function (_,_,_,([],_)) -> true | _ -> false) pi_s then
        let xs = List.map (fun (_,res_i,_,_) -> A_var res_i) pi_s in
        let s_list = List.map2 (fun id_i (rdy_i,res_i,idle_i,(ts_i,s_i)) -> 
                        S_fsm(id_i,rdy_i,res_i,idle_i,ts_i,s_i)) id_s pi_s
        in
        SMap.empty,SMap.empty,seq_ (seq_ (seq_list_ s_list) (set_ x (A_tuple xs))) k
      else
      let q = gensym ~prefix:"par" () in
      let ts = SMap.singleton q (
        let s_fin =
          let rdys = List.map (fun (rdy_i,_,_,_) -> A_var rdy_i) pi_s in
          let ress = List.map (fun (_,res_i,_,_) -> A_var res_i) pi_s in
          let_plug_s (conjonction_atoms rdys) (fun z ->
                       S_if(z,
                        (seq_ (S_set(x,(A_tuple ress))) @@ k),Some (S_continue q)))
        in
        let s_list' = List.map2 (fun id_i (rdy_i,res_i,idle_i,(ts_i,s_i)) -> 
                        S_fsm(id_i,rdy_i,res_i,idle_i,ts_i,S_skip)) id_s pi_s
        in seq_ (seq_list_ s_list') s_fin
      ) in
      let s_inFsm_list = List.map2 (fun id_i (rdy_i,res_i,idle_i,(ts_i,s_i)) -> 
                                   (S_in_fsm(id_i,s_i))) id_s pi_s in
      SMap.empty,ts, seq_ (seq_list_ s_inFsm_list)
                     (S_continue q)
*)
(*
  | E_for _ -> assert false (* already expanded *)
 
  | E_fun _ | E_fix _ -> 
     (* can occur in case of higher order function that does not use its argument,
        e.g.: [let rec f g = f g in f (fun x -> x)].
        We can safely ignore it in this case *)
  SMap.empty,SMap.empty,return_ (set_ x (A_const Unit))



  | E_run(f,e) ->
      let args = to_a ~externals ~sums e in
      let externals =
        let e1, e2 = externals in
        List.fold_left (fun env (x,(ty,shared)) -> Types.SMap.add x (ty, shared) env) Types.SMap.empty e1,
        List.fold_left (fun env (x,ty) -> Types.SMap.add x ty env) Types.SMap.empty e2 in
      begin
        match SMap.find_opt f (fst externals) with
        | Some(ty,shared) ->
            (* let is_instantaneous = match Types.canon_ty ty with
                                    | Ty_fun(_,Dur_zero,_) -> true
                                    | _ -> false in*)
            let id = if shared then 0 else gen_external_id () in              
            if not(shared) then 
              let q1 = gensym ~prefix:"pause_external" () in
              let external_result = gensym ~prefix:"external_result" () in
              let external_rdy = gensym ~prefix:"external_rdy" () in
              let s = seq_ (S_external_run(f,id,external_result,external_rdy,args)) @@
                            S_if(external_rdy,return_ (set_ x (A_var external_result)),
                              Some (S_continue q1)) in 
              let ts = SMap.add q1 s SMap.empty in 
              (SMap.empty, ts, s)
            else 
              let qwait = gensym ~prefix:"external_wait_lock" () in
              let q1 = gensym ~prefix:"pause_external" () in
              let external_result = gensym ~prefix:"external_result" () in
              let external_rdy = gensym ~prefix:"external_rdy" () in
              let s = seq_ (S_external_run(f,id,external_result,external_rdy,args)) @@
                            S_if(external_rdy,return_ (seq_ (S_release_lock(f)) @@ 
                                                       set_ x (A_var external_result)),
                              Some (S_continue q1)) 
              in
              let s0 = let_plug_s (A_ptr_taken(f)) @@ fun z ->
                       S_if(z,S_continue qwait,
                          Some (seq_ (S_acquire_lock(f)) @@ S_continue q1)) in
              let ts = SMap.add q1 s @@
                       SMap.add qwait s0 SMap.empty in 
              (SMap.empty, ts, s0)
            (* else if shared then assert false*)
              (* let q1 = gensym ~prefix:"pause_externalI" () in
              let q2 = gensym ~prefix:"pause_externalII" () in
              let ts = SMap.add q2 k @@
                      SMap.add q1 (seq_ (S_ptr_take(y,false))
                                        (seq_ (set_ x (A_get_result(y))) (S_continue q2))) SMap.empty in 
              let s = seq_ (S_set_arg(y,args)) (S_continue q1) in
              let q_wait = gensym ~prefix:"q_wait" () in
              let s' = let_plug_s (A_ptr_taken(y)) @@ fun z ->
                        S_if(z, (S_continue q_wait),
                                Some (seq_ (S_ptr_take(y,true)) @@ s)) in
              (SMap.empty, SMap.add q_wait s' ts, s') *)
              
              (*let q1 = gensym ~prefix:"pause_externalI" () in
              let ts = SMap.add q1 ((* seq_ (S_start(y,id,false))*) k) SMap.empty in 
              let s2 = seq_ (S_external_set_go(y,id)) @@
                       let_plug_s (A_is_rdy(y,id)) @@ fun z ->
                       S_if(z,seq_ (set_ x (A_get_result(y,id))) @@
                      return_ (S_release_lock(y)), Some (S_continue q1)) in
              let s = seq_ (S_external_set_arg(y,id,args)) @@
                      seq_ (S_external_restart(y,id)) @@
                      S_continue q1 in
              let q_wait = gensym ~prefix:"q_wait" () in
              let s' = let_plug_s (A_ptr_taken(y)) @@ fun z ->
                        seq_ (S_external_set_go(y,id)) @@
                        S_if(z, (S_continue q_wait),
                                Some (seq_ (S_acquire_lock(y)) @@ s)) in
              (SMap.empty, SMap.add q1 s2 @@
                           SMap.add q_wait s' ts, s')
            else
              let t1 = seq_ (S_external_set_go(y,id)) @@
                       seq_ (S_external_set_arg(y,id,args)) @@
                        (* seq_ ( *)return_ (set_ x (A_get_result(y,id)))(* ) k *)
              in
              (SMap.empty, SMap.empty, t1) *)
        | None -> assert false (* ill-typed *)
      end
 *)
   | E_pause e1 ->
      let q = gensym ~prefix:"pause" () in
      let env' =optimization_filter_env' ~kont:(k e1) env_es in
      let w1,ts1,e1' = compile_exp ~externals id env' gs e1 k in
      (w1, SMap.add q (group_ps (List.map (fun (e,_) -> Pattern.exp2pat e) env'), e1') ts1, 
        E_app(E_const (Inj q),group_es (List.map fst env')))
  (*| E_equations(p,eqs) ->
      let acc = ref [] in
      let rec compile_le clks p x le =
        let rec aux p ax = 
          match p with
          | P_var z -> set_ z ax
          | P_tuple ps -> 
              let n = List.length ps in
              seq_list_ @@ List.mapi (fun i p -> aux p (A_call(GetTuple (i,n,new_tvar()), ax))) ps
          | P_unit -> S_skip
        in 
        match le with
        | Exp e1 -> let w1,ts1,s1 = compile_exp ~externals id env_es ~statics ~externals ~sums [] e1 x (aux p (A_var x)) in
                    assert (SMap.is_empty w1);    
                    assert (SMap.is_empty ts1);
                    s1
        | Fby(le1, le2) -> let x_init = gensym ~prefix:(x^"init") () in
                           let x_mem = gensym ~prefix:(x^"mem") () in
                           let s1 = compile_le clks p x le1 in
                           let s2 = compile_le clks p x_mem le2 in
                           acc := (clks,s2)::!acc;
                           S_if(x_init, S_skip, (* (aux p (A_var x_mem)),*)
                                     Some(seq_ (set_ x_init (A_const (Bool true))) @@ s1))
        | When(le1, e2) -> let_plug_s (to_a ~externals ~sums e2) @@ fun z -> 
                            let s1 = compile_le (z::clks) p x le1 in
                            S_if(z,s1,None)
        | Merge(le1, le2, e3) -> let s1 = compile_le clks p x le1 in
                                 let s2 = compile_le clks p x le2 in
                                 let_plug_s (to_a ~externals ~sums e3) @@ fun z ->
                                 seq_ (S_if(z,s1,Some(s2))) (aux p (A_var x))
      in
      let ss = List.map (fun (p,le) ->
                  let y = gensym () in
                  compile_le [] p y le
                  ) eqs
      in SMap.empty,SMap.empty, seq_ (seq_list_ ss) (seq_ (set_ x (to_a ~externals ~sums  (Pattern.pat2exp p))) @@ 
      seq_ (seq_list_  
      (List.map (fun (clks,s) -> 
            let_plug_s (conjonction_atoms (List.map (fun clk -> A_var clk) clks))
            @@ fun z -> S_if(z,s,None)) !acc)) k)
      (* let p_tuple = P_tuple (List.map fst eqs) in
      let eqs' = eqs (* List.map (fun (p,e) -> let p' = Ast_rename.rename_pat ~statics:(List.map fst statics) p in p',e) eqs*) in
      let rec aux p1 p2 = 
        match p1,p2 with
        | P_var x1, P_var x2 -> set_ x1 (A_var x2)
        | P_tuple ps1, P_tuple ps2 -> seq_list_ @@ List.map2 aux ps1 ps2
        | P_unit,P_unit -> S_skip
        | _ -> assert false
      in
      (* let s_init = aux p_tuple_pre p_tuple in *)
      let rec assign_p_p' p p' = match p,p' with 
      | P_var z1,P_var z2 -> set_ z1 (A_var z2)
      | P_tuple ps', P_var z2 -> 
          let n = List.length ps' in 
          seq_list_ (List.mapi (fun i p -> 
              (* assume p is a variable *)
              match p with
              | P_var zp ->
                 set_ zp (to_a ~externals ~sums @@ Matching.get_tuple i n (E_var z2))) ps')
      | P_tuple ps, P_tuple ps' -> seq_list_ (List.map2 assign_p_p' ps ps')
      | P_unit,P_unit -> S_skip
      | _ -> assert false in
      let s' = List.fold_right  (fun (pi,ei) s ->
        let z' = gensym () in
        let _w,_ts,s' = compile_exp ~externals id env_es ~statics ~externals ~sums [] ei (* (Ast_subst.subst_p_e p_tuple (Pattern.pat2exp p_tuple_pre) ei) *) z' S_skip in
        assert (SMap.is_empty _w);
        assert (SMap.is_empty _ts);
        seq_ s' @@
        seq_ (assign_p_p' pi (P_var z')) s) eqs' (seq_list_ (List.map2 (fun (p,_) (p',_) -> 
            assign_p_p' p p'
          ) eqs eqs')) in
      SMap.empty,SMap.empty,seq_ (seq_ S_skip (seq_ s' (set_ x (to_a ~externals ~sums @@ Pattern.pat2exp p))))
                                 k 
                                 *)
                                 
                                 (* (aux p_tuple p_tuple_pre)*)

    (*let p_tuple = P_tuple (List.map fst eqs) in
      let p_tuple_pre = Ast_rename.rename_pat ~statics:(List.map fst statics) p_tuple in
      let rec aux p1 p2 = 
        match p1,p2 with
        | P_var x1, P_var x2 -> set_ x1 (A_var x2)
        | P_tuple ps1, P_tuple ps2 -> seq_list_ @@ List.map2 aux ps1 ps2
        | P_unit,P_unit -> S_skip
        | _ -> assert false
      in
      let s_init = aux p_tuple_pre p_tuple in
      let s' = List.fold_right  (fun (pi,ei) s ->
        match pi with
        | P_var z ->
            let _w,_ts,s' = compile_exp ~externals id env_es ~statics ~externals ~sums [] ei (* (Ast_subst.subst_p_e p_tuple (Pattern.pat2exp p_tuple_pre) ei) *) z S_skip in
            assert (SMap.is_empty _w);
            assert (SMap.is_empty _ts);
            seq_ s' s) eqs S_skip in
      SMap.empty,SMap.empty,seq_ (seq_ S_skip (seq_ s' (set_ x (to_a ~externals ~sums @@ Pattern.pat2exp p))))
                                 k (* (aux p_tuple p_tuple_pre)*)
*)
*****************************************************)
   | E_deco _ -> Format.fprintf Format.std_formatter "[%a]\n" Ast_pprint.pp_exp  e; assert false
   | E_letIn(_,_,(E_fix(h,(p,(ty,_),e1)) as phi),e2)-> Format.fprintf Format.std_formatter "[%a]\n" Ast_pprint.pp_exp  e; assert false
   | E_letIn(P_var x,_,_,_) -> Format.fprintf Format.std_formatter "[%a]\n" Ast_pprint.pp_exp  e; assert false
   | E_letIn _ -> Format.fprintf Format.std_formatter "[%a]\n" Ast_pprint.pp_exp  e; assert false
   | E_app _ ->Format.fprintf Format.std_formatter "[%a]\n" Ast_pprint.pp_exp  e; assert false
   | E_exec _ -> Format.fprintf Format.std_formatter "[%a]\n" Ast_pprint.pp_exp  e; assert false
   | e -> Format.fprintf Format.std_formatter "[%a]\n" Ast_pprint.pp_exp  e; assert false (* todo *)

(* takes a program and translates it into an FSM *)

and compile_bloc ~externals env_es gs id tyB_result e_main =
  let idle = mk_idle id in
  let state = mk_state id in
  let result = mk_result id in
  let w,ts, e' = compile_exp ~externals id env_es gs e_main (identity_kont ~idle) in
  
  SMap.iter (fun f u ->
            let l = List.of_seq (IMap.to_seq u) in
            sum_global := (id,(Types.new_ty_unknown(),List.map (fun (n,(ps,_)) -> 
                                 instance_of_int n,Ast.group_tyBs (List.map (fun (_,tyB) -> tyB) ps)) l))
                          ::!sum_global;
        ) w;
  let qs = idle ::(List.map fst @@ SMap.bindings ts) in
  let e'' =
  (E_match(E_var state, (idle,(P_var result,e'))::SMap.bindings ts,None))
      |> (fun e -> Printf.printf "foo!\n"; (Stdlib.flush stdout); insert_kont w ~id tyB_result e)
  in 
   Printf.printf "go!\n"; (Stdlib.flush stdout);
  sum_global := (id,((* ty_result*) Types.new_ty_unknown(),List.map (fun q -> q,Types.new_tyB_unknown()) qs))::!sum_global;
  e''


(** compilation of instantaneous expressions *)
and compile_instantaneous_exp ~externals id env_es e =
  match e with
  | E_var _ | E_const _ | E_app _ | E_tuple _ -> e
  | E_if(a,e1,e2) ->
      let e1' = compile_instantaneous_exp ~externals id env_es e1 in
      let e2' = compile_instantaneous_exp ~externals id env_es e2 in
      E_if(a, e1', e2')
  | E_case(a,hs,e_els) -> 
      let hs' = List.map (fun (c,ec) -> c, compile_instantaneous_exp ~externals id env_es ec) hs in
      let e_els' = compile_instantaneous_exp ~externals id env_es e_els in
      E_case(a,hs',e_els')
  | E_match(a,hs,eo) ->
      let hs' = List.map (fun (ctor,(p,ec)) ->
                             let env_es' = env_extend ~tyB:(Types.new_tyB_unknown ()) p env_es in (* todo, lookup in sums *)
                             ctor, (p,compile_instantaneous_exp ~externals id env_es' ec)) hs in
      let eo' = Option.map (compile_instantaneous_exp ~externals id env_es) eo in
      E_match(a,hs',eo')
  | E_letIn(p,t_y,e1,e2) ->
      let e1' = compile_instantaneous_exp ~externals id env_es e1 in
      let tyB = Types.new_tyB_unknown() in
      Typing.unify_ty ~loc:Prelude.dloc t_y (Ty_base tyB);
      let env_es' = env_extend ~tyB p env_es in
      let e2' = compile_instantaneous_exp ~externals id env_es' e2 in
      E_letIn(p,t_y,e1',e2')
  | E_reg((p,tyB,e1),a,l) ->
      let env_es' = env_extend ~tyB:tyB p env_es in
      let e1' = compile_instantaneous_exp ~externals id env_es' e1 in
      (E_reg((p,tyB,e1'),a,l))
  | E_exec(e1,e0,eo,id) ->
      (* assume e0 is combinational *)
      let tyB_res = (Types.new_tyB_unknown ()) in
      let tyB_state = (Types.new_tyB_unknown ()) in
      let idle = mk_idle id in
      let result = mk_result id in
      let d = default_value tyB_res in
      let e1' = compile_bloc ~externals (optimization_filter_env ~kont:e1 env_es) [] id (Types.Ty_base tyB_res) e1 in
      let e_exec' = E_reg((P_var (mk_state id),tyB_state, 
                          (match eo with
                           | None -> e1'
                           | Some e2 -> E_if(e2,E_app(E_const (Inj idle), d), e1'))),
                                             (E_app(E_const (Inj idle), d)),id) in
      E_letIn(P_var (mk_state id), Types.Ty_base tyB_state,e_exec',
              E_match(E_var (mk_state id),[idle,(P_var result, E_tuple([E_var result;E_const (Bool true)]))],
                      Some (E_tuple([e0;E_const (Bool false)]))))
  | E_par(es) ->
      (** [(e1 || e2)] has same behavior than [(e1, e2)]
          if [e1] and [e2] are instantaneous *)
      compile_instantaneous_exp ~externals id env_es (E_tuple es)
  | E_run _ as e -> e
  | e -> Format.fprintf Format.std_formatter "error while compiling expression [%a]"
            Ast_pprint.pp_exp e; 
         assert false      


(* assume [e_fun] is a close instantaneous function *)
and compile_fun ~externals gs e_fun =
  match e_fun with
  | E_fun(p,(ty,tyB),e1) ->
      let id = gensym ~prefix:"id" () in
      let tyB_arg = match Types.canon_ty ty with Ty_base tyB_arg -> tyB_arg | _ -> assert false in
      let e1' = compile_instantaneous_exp ~externals id (env_extend ~tyB:tyB_arg p []) e1 in
      E_fun(p,(ty,tyB),e1')
  | E_letIn(P_var f,ty,(E_fun _ as e_fun),e1) ->
     (match Types.canon_ty ty with
      | Types.Ty_fun(ty1,_,tyB2) ->
          E_letIn(P_var f,Types.Ty_fun(ty1,Types.Dur_zero,tyB2),compile_fun ~externals gs e_fun, compile_fun ~externals ((f,`Fun)::gs) e1)
      | _ -> assert false)
  | E_letIn(P_var f,ty,E_fix _,e1) ->
      (* should not happen ? *)
      compile_fun ~externals gs e1

  (*| E_letIn(P_var f,ty,(E_fix(_,(p,tys,e2)) as e_fun),e1) ->
      let g = gensym ~prefix:"g" () in
      compile_fun ~externals gs (E_letIn(P_var g,ty,E_fun (p,tys,
                      E_letIn(P_var f,ty,e_fun, E_app(E_var g,Pattern.pat2exp p))),e1))*)
  | E_letIn(P_var f,ty,(E_array_create _ as e1), e2) ->
      E_letIn(P_var f,ty,e1, compile_fun ~externals gs e2)
  | E_letIn(P_var f,ty,a1, e2) ->
      E_letIn(P_var f,ty,a1, compile_fun ~externals gs e2)
  (* | E_var f ->
      let x = gensym ~prefix:"arg" () in
      compile_fun ~externals gs (E_fun(P_var x,(Types.new_ty_unknown(),Types.new_tyB_unknown()),E_app(E_var f,E_var x)))
 *) 
  | e -> Format.fprintf Format.std_formatter "[%a]" Ast_pprint.pp_exp e;  assert false      

let compile_pi pi =
  sum_global := [] ;
  let open Ast in
  let e = compile_fun ~externals:pi.externals [] pi.main in
  {pi with main = e; sums = (List.map (fun (x,(_,ts)) -> x,ts) !sum_global) @ pi.sums}



(* todo: globalize les id18_state ... *)