open Fsm_syntax

let ref_set_lock_flag = ref true

module NameC = Naming_convention

module SMap = Map.Make(String)

(** each function definition (refered by its name [f]) is associated to
    a mapping between instance numbers of [f] and their corresponding
    continuations (i.e., target instructions). *)
module IMap = Map.Make(Int)

let (++) = Ast.(++)

let (+++) s1 s2 =
  IMap.fold IMap.add s1 s2

let (++>) s1 s2 =
  SMap.union (fun x s1 s2 -> Some (s1 +++ s2)) s1 s2

(** warning: (++>) has the same type than (++) but behaves differently *)

let mk_int n size =
  assert (n >= 0 && size > 0);
  (Int {value=n;tsize=TSize (max 4 size)})

let new_instance =
  let c  = ref 0 in
  (fun () -> incr c; !c)

let let_plug_s (a:a) (f : x -> s) : s =
  match a with
  | A_var x -> f x
  | _ ->
    let y = Ast.gensym () in
    S_letIn(y,a,f y)

let let_plug_a (a:a) (f : x -> a) : a =
  match a with
  | A_var x -> f x
  | _ ->
    let y = Ast.gensym () in
    A_letIn(y,a,f y)

let gen_external_id =
  let c  = ref 0 in
  (fun () -> incr c; !c)



(** currently, instance numbers are encoded using 12 bits.
    TODO(enhancement): use an enumeration type instead. *)
let id_size = 12 ;;

let contain_return s =
  let rec aux s =
  match s with
  | S_continue q ->
      NameC.is_return q
  | S_skip
  | S_acquire_lock _ 
  | S_release_lock _
  | S_read_start _
  | S_read_stop _
  | S_write_start _
  | S_write_stop _
  | S_call _
  | S_set _ -> false
  | S_letIn(_,_,s1) -> aux s1
  | S_seq(s1,s2) -> aux s1 || aux s2
  | S_if(_,s1,so2) -> aux s1 || (match so2 with None -> false | Some s -> aux s)
  | S_case(_,hs,so) ->
      List.exists (fun (_,s) -> aux s) hs || (match so with None -> false | Some s1 -> aux s1)
  | S_fsm _ | S_in_fsm _ -> false (* ok? *)
  | S_external_run _ -> false in
  aux s


let find_ctor x sums =
  let (n,sum,t) = Types.find_ctor x sums in
  let arg_size = List.fold_left (max) 0 @@ List.map (fun (_,t) -> Fsm_typing.(size_ty (translate_tyB t))) sum in
  let sz = Fsm_typing.compute_tag_size sum in
   (mk_int n sz,arg_size,Fsm_typing.translate_tyB t)

let rec insert_kont w ~idle ~x s =
  let rec aux s =
    match s with
    | S_continue q0 ->
        if NameC.is_return q0 then
          let rq0 = NameC.return_name q0 in
          (match SMap.find_opt q0 w with
           | None -> S_continue rq0
           | Some u ->
           let l = List.of_seq (IMap.to_seq u) in
           (match l with
            | [] -> S_skip
            | [(_,k)] -> aux k
            | (_,k)::tt when List.for_all (fun (_,k') ->
                               (* would be possible with physical equality ? *)
                               k = k') tt ->
               aux k
            | _ ->
               S_case(NameC.instance_id_of_fun rq0,
                   (List.map (fun (m,k) ->
                              [mk_int m id_size], aux k
                        ) l),Some S_skip)))
        else s
    | S_if(z,s1,so) ->
        S_if(z,aux s1,Option.map aux so)
    | S_case(z,hs,so) -> S_case(z,List.map (fun (c,si) -> c, aux si) hs,Option.map aux so)
    | S_seq(s1,s2) ->  S_seq(aux s1,aux s2)
    | S_letIn(x,a,s) -> S_letIn(x,a,aux s)
    | (S_fsm _ | S_in_fsm _) as s -> s (* already compiled *)
    | S_skip
    | S_acquire_lock _ 
    | S_release_lock _
    | S_read_start _
    | S_read_stop _
    | S_write_start _
    | S_write_stop _
    | S_call _
    | S_set _
    | S_external_run _ -> s
  in
  Some (aux s)

let rec to_c ~sums = function
| Ast.Unit -> Unit
| Ast.Int (n,tz) -> Int {value=n;tsize=Fsm_typing.translate_size tz}
| Ast.Bool b -> Bool b
| Ast.String s -> String s
| Ast.C_tuple cs -> CTuple (List.map (to_c ~sums) cs)
| Ast.C_vector cs -> CVector (List.map (to_c ~sums) cs)
| Ast.C_size n -> CSize n
| Ast.C_appInj(x,c,tyB) ->
    let n,arg_size,ty_n = find_ctor x sums in
    CTuple[n;C_encode(to_c ~sums c,arg_size)]
| Ast.Inj _ -> assert false (* no partial application in the generated code *)
| Ast.(Op _ | V_loc _) -> assert false


let to_op = function
| Ast.TyConstr ty -> TyConstr (Fsm_typing.translate_ty ty)
| Ast.Runtime (External_fun(x,ty)) -> Runtime (External_fun(x,ty)) (* Types.new_ty_unknown())) *)
| Ast.Runtime p -> Runtime p
| Ast.GetTuple {pos=i;arity=n} -> GetTuple (i,n,new_tvar())
| Ast.(Wait _) -> assert false



let rec to_a ~externals ~sums (e:Ast.e) : a =
  match e with
  | Ast.E_var x -> A_var x
  | Ast.E_const c -> A_const (to_c ~sums c)
  | Ast.E_app(E_const(Op((Runtime (External_fun (x,_))) as op)),e1) ->
      (match e1 with
      | E_tuple _ ->
         A_call(to_op op,to_a ~externals ~sums e1)
      | _ -> 
        match List.assoc_opt x (snd externals) with
        | Some(ty,_) ->
            let targ = match Types.canon_ty ty with
                       | Ty_fun(targ,_,_) -> targ
                       | _ -> assert false in
            let n = match Types.canon_ty targ with
                    | Ty_base(TyB_tuple tyBs) -> List.length tyBs
                    | Ty_base(_) -> 1
                    | _ -> assert false (* ill-typed *) in
            if n = 1 then A_call(to_op op,to_a ~externals ~sums e1) else 
            let xs = List.init n (fun x -> Ast.gensym ~prefix:"tmp" ()) in
            let_plug_a (to_a ~externals ~sums e1) @@ (fun z ->
              let rec loop i = function
              | [] -> A_call(to_op op,A_tuple(List.map (fun y -> A_var y) xs))
              | x::xs ->
                 A_letIn(x,A_call((to_op @@ GetTuple{pos=i;arity=n}),A_var z), loop (i+1) xs)
              in loop 0 xs
            )
        | None -> assert false (* ill-typed *))
  | Ast.E_app(E_const(Op op),e) ->
      A_call(to_op op,to_a ~externals ~sums e)
  | Ast.E_if(e1,e2,e3) -> A_call(If,A_tuple [to_a ~externals ~sums e1;to_a ~externals ~sums e2;to_a ~externals ~sums e3])
  | Ast.E_tuple(es) -> A_tuple (List.map (to_a ~externals ~sums) es)
  | Ast.E_letIn(P_var x,e1,e2) -> A_letIn(x,to_a ~externals ~sums e1,to_a ~externals ~sums e2)
  | Ast.E_array_length x -> A_buffer_length(x,new_tvar())
  | E_app(E_const(Inj y),e) ->
      let_plug_a (to_a ~externals ~sums e) @@ (fun z ->
        let n,arg_size,ty_n = find_ctor y sums in
        A_tuple[A_const(n);A_encode(z,ty_n,arg_size)])
  | Ast.E_vector(es) -> A_vector (List.map (to_a ~externals ~sums) es) 
  | _ ->
      Format.fprintf Format.std_formatter "--> %a\n"  Ast_pprint.pp_exp  e; assert false


let rec seq_list_ ss = match ss with 
  | [] -> S_skip
  | [s] -> s
  | s::ss' -> seq_ s (seq_list_ ss')

let rec conjonction_atoms alist =
  match alist with
  | [] -> A_const (Bool true)
  | [a1;a2] -> A_call(Runtime (External_fun("Bool.land",Types.new_ty_unknown())), A_tuple(alist))
  | a1::alist' -> let a2 = conjonction_atoms alist' in 
                  A_call(Runtime (External_fun("Bool.land",Types.new_ty_unknown())), A_tuple([a1;a2]))

let replace_arg e =
  match e with
  | Ast.E_fix(f,(P_var x,e1)) -> Ast_subst.subst_e x (E_var (NameC.formal_param_of_fun f)) e1
  | e -> e

(* Debug/Display *)
let [@warning "-26"] show q w =
   SMap.iter (fun x s -> Printf.printf "%s {%s : [" q x;
                        IMap.iter (fun i _ -> Printf.printf "%d," i) s;
                        Printf.printf "]}\n") w


(** [to_s ~statics ~externals gs e x k] translates expression [e] 
    to a target instruction setting a result in variable [x], 
    then executing instruction [k].
    [gs] is the names of the functions accessible 
    from [e] by a tail-call *)
let rec to_s ~statics ~externals ~sums gs e x k =
  let return_ s = seq_ s k in
  if Instantaneous.combinational ~externals e then 
    SMap.empty,SMap.empty,return_ (set_ x (to_a ~externals ~sums e)) 
  else
  match e with
  | Ast.E_if(a,e1,e2) ->
      (** [IF] *)
      let optimizable () =
        k <> S_skip (** <- needed to ensure termination *)
        && Instantaneous.instantaneous ~externals e1 
        && Instantaneous.instantaneous ~externals e2 
      in
      if optimizable () then 
        (** optimization if there is no function call 
            in both [e1] and [e2]: avoiding the duplication 
            of the continuation *)
        let k_cut = S_skip in
        let w,ts,s = to_s ~statics ~externals ~sums gs e x k_cut in
        assert (SMap.is_empty ts);
        w,ts,seq_ s k  
      else
        let w1,ts1,s1 = to_s ~statics ~externals ~sums gs e1 x k in
        let w2,ts2,s2 = to_s ~statics ~externals ~sums gs e2 x k in
        let z = Ast.gensym () in
        (w1++>w2),(ts1 ++ ts2),S_letIn(z,to_a ~externals ~sums a,S_if(z,s1,Some s2))
  | E_case(a,hs,e_els) ->
      (** [MATCH (for integers only)] *)
      let optimizable () =
        k <> S_skip (** <- needed to ensure termination *)
        && List.for_all (fun (_,e) -> Instantaneous.instantaneous ~externals e) hs 
        && Instantaneous.instantaneous ~externals e_els
      in
      if optimizable () then
        (** optimization if there is no function call 
            in each expression [ei] in [hs]: avoiding the duplication 
            of the continuation *)
        let k_cut = S_skip in
        let w,ts,s = to_s ~statics ~externals ~sums gs e x k_cut in
        assert (SMap.is_empty ts);
        w,ts,seq_ s k
        (* ************************** *)  
      else
      let ws,tss,hs' = Prelude.map_split3 (fun (cs,e) ->
                         let w,ts,s = to_s ~statics ~externals ~sums gs e x k in
                         w,ts,(List.map (to_c ~sums) cs,s)
                       ) hs
      in
      let ts = List.fold_left (++) SMap.empty tss in
      let w1,ts1,s1 = to_s ~statics ~externals ~sums gs e_els x k in
      let w' = List.fold_left (++>) w1 ws in
      w',ts1 ++ ts,let z = Ast.gensym () in
      S_letIn(z,to_a ~externals ~sums a, S_case(z,hs',Some s1))
  | E_match(a,hs,eo) ->
      (* [MATCH] *)
      if ( k <> S_skip (* needed to ensure termination *) )
         && 
           (List.for_all (fun (_,(_,e)) -> Instantaneous.combinational ~externals e) hs 
            && (match eo with None -> true | Some e -> Instantaneous.combinational ~externals e)) 
      then
        (* ************************** *)
        (* optimization avoiding the duplication of the continuation *)
        let k_cut = S_skip in
        let w,ts,s = to_s ~statics ~externals ~sums gs e x k_cut in
        assert (SMap.is_empty ts);
        w,ts,seq_ s k
        (* ************************** *)
      else
      let z2 = Ast.gensym () in
      let ws,tss,hs' = Prelude.map_split3 (fun (inj,(py,e)) ->
                         let y = match py with Ast.P_var y -> y | _ -> assert false in
                         let w,ts,s = to_s ~statics ~externals ~sums gs e x k in
                         let n,_,ty_n = find_ctor inj sums in
                         w,ts,([n],(seq_ (set_ y (A_decode(z2,ty_n))) @@ s))
                       ) hs
      in
      let wn,tsn,so = match eo with
                      | None -> SMap.empty,SMap.empty,Some S_skip
                                (* Some skip rather than None because
                                   the number of cases in the generated code
                                   must be a power of 2 *)
                      | Some e -> let w,ts,s = to_s ~statics ~externals ~sums gs e x k in
                                  (w,ts,Some s)
      in
      let ts = List.fold_left (++) tsn tss in
      let w' = List.fold_left (++>) wn ws in
      w',ts,let z = Ast.gensym () in
            let z1 = Ast.gensym () in
            S_letIn(z,to_a ~externals ~sums a,
            S_letIn(z1,A_call((to_op @@ (GetTuple{pos=0;arity=2}),A_var z)),
            S_letIn(z2,A_call((to_op @@ (GetTuple{pos=1;arity=2}),A_var z)),

            S_case(z1,hs',so))))
  | E_letIn(P_var f,(E_fix(h,(p,e1)) as phi),e2) ->
     assert (f = h);
     let e1 = replace_arg phi in
     let f' = NameC.mark_return f in
     let w1,ts1,s1 = to_s ~statics ~externals ~sums (f::gs) e1 (NameC.result_of_fun f) (S_continue f') in
     let w2,ts2,s2 = to_s ~statics ~externals ~sums gs e2 x k in
     (w1++>w2),(SMap.add f s1 ts1)++ts2,s2
  | E_letIn(P_unit,e1,e2) ->
      (* [SEQ] *)
      let w2,ts2,s2 = to_s ~statics ~externals ~sums gs e2 x k in
      if Instantaneous.combinational ~externals e1 then (* todo: emit a warning ? *) (w2,ts2,s2) else
      let w1,ts1,s1 = to_s ~statics ~externals ~sums gs e1 (Ast.gensym ()) s2 in
      w1++>w2,ts2++ts1,s1
  | E_letIn(P_var y,e1,e2) ->
      (* [LET] *)
      let w2,ts2,s2 = to_s ~statics ~externals ~sums gs e2 x k in
      if Instantaneous.combinational ~externals e1 then
        w2,ts2,seq_ (set_ y (to_a ~externals ~sums e1)) s2
      else
        let w1,ts1,s1 = to_s ~statics ~externals ~sums gs e1 y s2 in
        w1++>w2,ts2++ts1,s1
  | E_app(E_var f,a) ->
      if List.mem f gs then
          (* [TAIL-CALL] *)
          let s = seq_ (set_ (NameC.formal_param_of_fun f) (to_a ~externals ~sums a)) @@
                        S_continue f in
           (SMap.empty,SMap.empty,s)
      else
          (* [DIRECT-CALL] *)
          let n = new_instance () in
          let w = SMap.singleton (NameC.mark_return f) (IMap.singleton n (seq_ (set_ x (A_var (NameC.result_of_fun f))) k)) in
          let s = seq_ (set_ (NameC.instance_id_of_fun f) (A_const (mk_int n id_size))) @@
                  seq_ (set_ (NameC.formal_param_of_fun f) (to_a ~externals ~sums a)) @@
                       S_continue f in
          (w,SMap.empty,s)
  | E_app(E_const(Op((Runtime _) as op)),a) ->
      (* in case of instantaneous call which is not combinatorial,
         e.g., a display function for debug  *)
      SMap.empty, SMap.empty, return_ (set_ x (A_call(to_op op,to_a ~externals ~sums a)))
                               

  | E_ref(a) -> SMap.empty, SMap.empty, 
                return_ (set_ x (to_a ~externals ~sums a))
  | E_get(ay) -> 
      (match ay with
      | E_var y ->
         if !ref_set_lock_flag then
           let q_wait = Ast.gensym ~prefix:"get_wait" () in
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
          let q_wait = Ast.gensym ~prefix:"get_wait" () in
          let q1 = Ast.gensym ~prefix:"get_pause" () in
          let w,ts,s = to_s ~statics ~externals ~sums gs (E_const Unit) x k in
          let s0 = S_if(y^"_lock", 
                       S_continue q_wait, Some(seq_ (set_ (y^"_lock") (A_const (Bool true))) @@
                        seq_ (set_ y (to_a ~externals ~sums a)) @@ (S_continue q1))) in
          (w, (SMap.add q1 (seq_ (set_ (y^"_lock") (A_const (Bool false))) s) @@
               SMap.add q_wait s0 ts), s0)
       | _ -> assert false) else
      (match ay with
       | E_var y ->
          let w,ts,s = to_s ~statics ~externals ~sums gs (E_const Unit) x k in
          (w, ts, seq_ (set_ y (to_a ~externals ~sums a)) s)
       | _ -> assert false)
  | E_array_get(y,idx) ->
      if !Flag_mealy.mealy_flag then (
        let a = to_a ~externals ~sums idx in
        let q = Ast.gensym ~prefix:"pause_get" () in
        let ts = SMap.add q (return_ @@ 
                    seq_ (S_read_stop(x,y)) 
                         (S_release_lock(y))) SMap.empty in 
        let s = seq_ (S_acquire_lock(y)) @@
                seq_ (S_read_start(y,a)) (S_continue q) in
          let q_wait = Ast.gensym ~prefix:"q_wait" () in
          let s' = let_plug_s (A_ptr_taken(y)) @@ fun z ->
                   S_if(z, (S_continue q_wait), Some s) in
          (SMap.empty, SMap.add q_wait s' ts, s')
      ) else (
        let a = to_a ~externals ~sums idx in
        let q1 = Ast.gensym ~prefix:"pause_getI" () in
        let q2 = Ast.gensym ~prefix:"pause_getII" () in
        let ts = SMap.add q1 (S_continue q2) @@
                 SMap.add q2 (seq_ (S_read_stop(x,y)) 
                                   (return_ @@ (S_release_lock(y)))) SMap.empty in 
        let s = seq_ (S_read_start(y,a)) (S_continue q1) in
          let q_wait = Ast.gensym ~prefix:"q_wait" () in
          let s' = let_plug_s (A_ptr_taken(y)) @@ fun z ->
                   S_if(z, (S_continue q_wait),
                             Some (seq_ (S_acquire_lock(y)) @@ s)) in
          (SMap.empty, SMap.add q_wait s' ts, s')
      )
  | E_array_set(y,idx,e_upd) ->
      if !Flag_mealy.mealy_flag then (
        let a = to_a ~externals ~sums idx in
        let a_upd = to_a ~externals ~sums e_upd in
        let q = Ast.gensym ~prefix:"pause_set" () in
        let ts = SMap.add q (
              return_ @@ (seq_ (S_write_stop(y)) @@
                          seq_ (S_release_lock(y)) @@ 
                          set_ x (A_const Unit))) SMap.empty  in
        let q_wait = Ast.gensym ~prefix:"q_wait" () in
        let s' = let_plug_s (A_ptr_taken(y)) @@ fun z ->
                   S_if(z, (S_continue q_wait),
                             Some (seq_ (S_acquire_lock(y)) @@
                                   seq_ (S_write_start(y,a,a_upd)) @@
                                        (S_continue q))) in
        (SMap.empty, SMap.add q_wait s' ts, s')
        (* todo: pas besoin de dupliquer s': l'écriture en tant que telle ne prend que 1 cycle : on peut la faire démarrer un cycle plus tard *)
      ) else (
          let a = to_a ~externals ~sums idx in
          let a_upd = to_a ~externals ~sums e_upd in
          let q1 = Ast.gensym ~prefix:"pause_setI" () in
          let q2 = Ast.gensym ~prefix:"pause_setII" () in
          let ts = SMap.add q1 (seq_ (S_write_stop(y)) (S_continue q2)) @@
                   SMap.add q2 (seq_ (S_release_lock(y)) @@
                                     (return_ @@ (set_ x (A_const Unit)))) SMap.empty  in
          let q_wait = Ast.gensym ~prefix:"q_wait" () in
          let s' = let_plug_s (A_ptr_write_taken(y)) @@ fun z ->
                     S_if(z, (S_continue q_wait),
                               Some (seq_ (S_acquire_lock(y)) @@
                                     seq_ (S_write_start(y,a,a_upd)) @@
                                     (S_continue q1))) in
          (SMap.empty, SMap.add q_wait s' ts, s')
      )
  | E_reg((p,e1),e0,l) ->
      let y = match p with
              | P_var y -> y 
              | _ -> assert false 
      in

      let w1,ts1,s1 = to_s ~statics ~externals ~sums [] e1 y S_skip in
      let w0,ts0,s0 = to_s ~statics ~externals ~sums [] e0 y S_skip in
      assert (SMap.is_empty w1 (* && SMap.is_empty ts1)*));
      assert (SMap.is_empty w0 (* && SMap.is_empty ts0)*));
      (SMap.empty, ts1++ts0,
      seq_ (S_if(l, S_skip, Some (seq_ (set_ l (A_const (Bool true))) s0))) @@
      seq_ s1 @@
      return_ @@ set_ x (A_var y))
  | E_exec(e1,e0,eo,l) ->
      (* assume e0 is combinational *)
      let pi = Ast.{statics;externals;sums;main=e1} in
      let rdy,res,idle,(ts,s1) = compile (* ~result:x*) pi in
      let id = Ast.gensym ~prefix:"id" () in
      let s1' = S_fsm(id,rdy,res,idle,ts,s1) in
      let s2 = seq_ (S_if(rdy, S_skip, Some (set_ res (to_a ~externals ~sums e0)))) @@
               return_ @@ set_ x (A_tuple[A_var res;A_var rdy]) in
      let s = seq_ s1' s2 in
      (match eo with
      | None -> (SMap.empty, SMap.empty, s)
      | Some e3 ->
         (* assume e3 is combinational *)
         let s_not_rdy = let_plug_s (to_a ~externals ~sums e3) (fun zz ->
                            S_if(zz, S_in_fsm(id,S_continue idle), None)) in
         let s4 = seq_ (S_if(rdy, S_skip, Some(seq_ (set_ res (to_a ~externals ~sums e0)) s_not_rdy))) @@
               return_ @@ set_ x (A_tuple[A_var res;A_var rdy]) in
          let s5 = seq_ s1' s4 in
          (SMap.empty, SMap.empty, s5))
  | E_par(es) ->
      let id_s = List.map (fun _ -> Ast.gensym ~prefix:"id" ()) es in
      let pi_s = List.map (fun e -> compile @@ Ast.{statics;externals;sums;main=e}) es in
      let q = Ast.gensym ~prefix:"par" () in
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
              let q1 = Ast.gensym ~prefix:"pause_external" () in
              let external_result = Ast.gensym ~prefix:"external_result" () in
              let external_rdy = Ast.gensym ~prefix:"external_rdy" () in
              let s = seq_ (S_external_run(f,id,external_result,external_rdy,args)) @@
                            S_if(external_rdy,return_ (set_ x (A_var external_result)),
                              Some (S_continue q1)) in 
              let ts = SMap.add q1 s SMap.empty in 
              (SMap.empty, ts, s)
            else 
              let qwait = Ast.gensym ~prefix:"external_wait_lock" () in
              let q1 = Ast.gensym ~prefix:"pause_external" () in
              let external_result = Ast.gensym ~prefix:"external_result" () in
              let external_rdy = Ast.gensym ~prefix:"external_rdy" () in
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
              (* let q1 = Ast.gensym ~prefix:"pause_externalI" () in
              let q2 = Ast.gensym ~prefix:"pause_externalII" () in
              let ts = SMap.add q2 k @@
                      SMap.add q1 (seq_ (S_ptr_take(y,false))
                                        (seq_ (set_ x (A_get_result(y))) (S_continue q2))) SMap.empty in 
              let s = seq_ (S_set_arg(y,args)) (S_continue q1) in
              let q_wait = Ast.gensym ~prefix:"q_wait" () in
              let s' = let_plug_s (A_ptr_taken(y)) @@ fun z ->
                        S_if(z, (S_continue q_wait),
                                Some (seq_ (S_ptr_take(y,true)) @@ s)) in
              (SMap.empty, SMap.add q_wait s' ts, s') *)
              
              (*let q1 = Ast.gensym ~prefix:"pause_externalI" () in
              let ts = SMap.add q1 ((* seq_ (S_start(y,id,false))*) k) SMap.empty in 
              let s2 = seq_ (S_external_set_go(y,id)) @@
                       let_plug_s (A_is_rdy(y,id)) @@ fun z ->
                       S_if(z,seq_ (set_ x (A_get_result(y,id))) @@
                      return_ (S_release_lock(y)), Some (S_continue q1)) in
              let s = seq_ (S_external_set_arg(y,id,args)) @@
                      seq_ (S_external_restart(y,id)) @@
                      S_continue q1 in
              let q_wait = Ast.gensym ~prefix:"q_wait" () in
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
 


  | e -> Ast_pprint.pp_exp Format.std_formatter e; assert false (* todo *)

(* takes a program and translates it into an FSM *)

and compile ?(result=(Ast.gensym ~prefix:"result" ())) pi =
  let open Ast in

  let statics = pi.statics in
  let externals = pi.externals in
  let sums = pi.sums in
  let x = result in
  let rdy = gensym ~prefix:"rdy" () in
  let idle = gensym ~prefix:"idle" () in

  let k = seq_ (set_ rdy (A_const (Bool true))) (S_continue idle) in
  let w0,ts0,s0 = to_s ~statics ~externals ~sums [idle] pi.main x k in

 (* show idle w0; *)

  let wmain = SMap.add idle IMap.empty w0 in
  let s' = match insert_kont wmain ~idle ~x s0 with Some s -> s | None -> s0 in
  let ts_res = (SMap.bindings ts0) in
  let rec loop ts_res =
    let has_changed = ref false in
    let ts_res = List.filter_map (fun (q_aux,s) ->
                  if contain_return s then (
                    (* has_changed := true; *)
                    (match insert_kont wmain ~idle ~x s with
                    | Some s2 -> Some(q_aux,s2)
                    | None -> None))
                  else Some(q_aux,s)) ts_res in
    if (!has_changed) then loop ts_res else ts_res in
  let ts_res = loop ts_res in

  rdy,x,idle,(ts_res,seq_ (set_ rdy (A_const (Bool false))) s')

