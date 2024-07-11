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

(** currently, instance numbers are encoded using 12 bits.
    TODO(enhancement): use an enumeration type instead. *)
let id_size = 12 ;;

let contain_return s =
  let rec aux s =
  match s with
  | S_continue q ->
      NameC.is_return q
  | S_skip
  | S_setptr_read _
  | S_setptr_write _
  | S_setptr_matrix_read _
  | S_setptr_matrix_write _
  | S_ptr_take _
  | S_ptr_write_take _
  | S_buffer_set _
  | S_call _
  | S_set _ -> false
  | S_letIn(_,_,s1) -> aux s1
  | S_seq(s1,s2) -> aux s1 || aux s2
  | S_if(_,s1,so2) -> aux s1 || (match so2 with None -> false | Some s -> aux s)
  | S_case(_,hs,so) ->
      List.exists (fun (_,s) -> aux s) hs || (match so with None -> false | Some s1 -> aux s1)
  | S_fsm _ | S_in_fsm _ -> false (* ok? *) in
  aux s


let rec insert_kont w ~compute ~x s =
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
                              mk_int m id_size, aux k
                        ) l),Some S_skip)))
        else s
    | S_if(a,s1,so) ->
        S_if(a,aux s1,Option.map aux so)
    | S_case(z,hs,so) -> S_case(z,List.map (fun (c,si) -> c, aux si) hs,Option.map aux so)
    | S_seq(s1,s2) ->  S_seq(aux s1,aux s2)
    | S_letIn(x,a,s) -> S_letIn(x,a,aux s)
    | (S_fsm _ | S_in_fsm _) as s -> s (* already compiled *)
    | S_skip
    | S_setptr_read _
    | S_setptr_write _
    | S_setptr_matrix_read _
    | S_setptr_matrix_write _
    | S_ptr_take _
    | S_ptr_write_take _
    | S_buffer_set _
    | S_call _
    | S_set _ -> s
  in
  Some (aux s)

let rec to_c = function
| Ast.Unit -> Unit
| Ast.Int (n,tz) -> Int {value=n;tsize=Fsm_typing.translate_ty tz}
| Ast.Bool b -> Bool b
| Ast.String s -> String s
| Ast.C_tuple cs -> CTuple (List.map to_c cs)
| Ast.C_vector cs -> CVector (List.map to_c cs)
| Ast.C_size n -> CSize n
| Ast.Inj _ -> assert false (* no partial application in the generated code *)
| Ast.(Op _ | V_loc _) -> assert false


let to_op = function
| Ast.TyConstr ty -> TyConstr (Fsm_typing.translate_ty ty)
| Ast.Runtime p -> Runtime p
| Ast.GetTuple {pos=i;arity=n} -> GetTuple (i,n,new_tvar())
| Ast.(Wait _) -> assert false


let find_ctor x sums =
  let (n,sum,t) = Types.find_ctor x sums in
  let arg_size = List.fold_left (max) 0 @@ List.map (fun (_,t) -> Fsm_typing.(size_ty (translate_ty t))) sum in
  let sz = Fsm_typing.compute_tag_size sum in
   (mk_int n sz,arg_size,Fsm_typing.translate_ty t)


let rec to_a ~sums (e:Ast.e) : a =
  match e with
  | Ast.E_var x -> A_var x
  | Ast.E_const c -> A_const (to_c c)
  | Ast.E_app(E_const(Op op),e) ->
      A_call(to_op op,to_a ~sums e)
  | Ast.E_if(e1,e2,e3) -> A_call(If,A_tuple [to_a ~sums e1;to_a ~sums e2;to_a ~sums e3])
  | Ast.E_tuple(es) -> A_tuple (List.map (to_a ~sums) es)
  | Ast.E_letIn(P_var x,e1,e2) -> A_letIn(x,to_a ~sums e1,to_a ~sums e2)
  | Ast.E_array_length x -> A_buffer_length(x,new_tvar())
  | Ast.E_matrix_size(x,n) -> A_buffer_matrix_length(x,n,new_tvar())
  | E_app(E_const(Inj y),e) ->
      let_plug_a (to_a ~sums e) @@ (fun z ->
        let n,arg_size,ty_n = find_ctor y sums in
        A_tuple[A_const(n);A_encode(z,ty_n,arg_size)])
  | Ast.E_vector(es) -> A_vector (List.map (to_a ~sums) es) 
  | _ ->
      Format.fprintf Format.std_formatter "--> %a\n"  Ast_pprint.pp_exp  e; assert false


let rec seq_list_ ss = match ss with 
  | [] -> S_skip
  | [s] -> s
  | s::ss' -> seq_ s (seq_list_ ss')

let rec conjonction_atoms alist =
  match alist with
  | [] -> A_const (Bool true)
  | [a1;a2] -> A_call(Runtime And, A_tuple(alist))
  | a1::alist' -> let a2 = conjonction_atoms alist' in 
                  A_call(Runtime And, A_tuple([a1;a2]))

let replace_arg e =
  match e with
  | Ast.E_fix(f,(P_var x,e1)) -> Ast_subst.subst_e x (E_var (NameC.formal_param_of_fun f)) e1
  | e -> e

(* Debug/Display *)
let [@warning "-26"] show q w =
   SMap.iter (fun x s -> Printf.printf "%s {%s : [" q x;
                        IMap.iter (fun i _ -> Printf.printf "%d," i) s;
                        Printf.printf "]}\n") w


(** [to_s ~statics gs e x k] translates expression [e] to a target instruction
    setting a result in variable [x], then execution instruction [k].
    [gs] is the names of the functions that are accessible by a tail-call *)
let rec to_s ~statics ~sums gs e x k =
  let return_ s =
    seq_ s k
  in
  if Combinational.combinational e then SMap.empty,SMap.empty,return_ (set_ x (to_a ~sums e)) else
  match e with
  | Ast.E_if(a,e1,e2) ->
      (* [IF] *)
      (* ************************** *)
      (* no use-less duplication: 
         [assert(not(Combinational.combinational e1 && Combinational.combinational e2))] 
       *)
      (* ************************** *)
      let w1,ts1,s1 = to_s ~statics ~sums gs e1 x k in
      let w2,ts2,s2 = to_s ~statics ~sums gs e2 x k in
      let z = Ast.gensym () in
      (w1++>w2),(ts1 ++ ts2),S_letIn(z,to_a ~sums a,S_if(z,s1,Some s2))
  | E_case(a,hs,e_els) ->
      (* [MATCH (for integers)] *)
      if ( k <> S_skip (* needed to ensure termination *) )
         && 
          (List.for_all (fun (_,e) -> Combinational.combinational e) hs 
           && Combinational.combinational e_els)
      then
        (* ************************** *)
        (* optimization avoiding the duplication of the continuation *)
        let k_cut = S_skip in
        let w,ts,s = to_s ~statics ~sums gs e x k_cut in
        assert (SMap.is_empty ts);
        w,ts,seq_ s k
        (* ************************** *)  
      else
      let ws,tss,hs' = Prelude.map_split3 (fun (c,e) ->
                         let w,ts,s = to_s ~statics ~sums gs e x k in
                         w,ts,(to_c c,s)
                       ) hs
      in
      let ts = List.fold_left (++) SMap.empty tss in
      let w1,ts1,s1 = to_s ~statics ~sums gs e_els x k in
      let w' = List.fold_left (++>) w1 ws in
      w',ts1 ++ ts,let z = Ast.gensym () in
      S_letIn(z,to_a ~sums a, S_case(z,hs',Some s1))
  | E_match(a,hs,eo) ->
      (* [MATCH] *)
      if ( k <> S_skip (* needed to ensure termination *) )
         && 
           (List.for_all (fun (_,(_,e)) -> Combinational.combinational e) hs 
            && (match eo with None -> true | Some e -> Combinational.combinational e)) 
      then
        (* ************************** *)
        (* optimization avoiding the duplication of the continuation *)
        let k_cut = S_skip in
        let w,ts,s = to_s ~statics ~sums gs e x k_cut in
        assert (SMap.is_empty ts);
        w,ts,seq_ s k
        (* ************************** *)
      else
      let z2 = Ast.gensym () in
      let ws,tss,hs' = Prelude.map_split3 (fun (inj,(py,e)) ->
                         let y = match py with Ast.P_var y -> y | _ -> assert false in
                         let w,ts,s = to_s ~statics ~sums gs e x k in
                         let n,_,ty_n = find_ctor inj sums in
                         w,ts,(n,(seq_ (set_ y (A_decode(z2,ty_n))) @@ s))
                       ) hs
      in
      let wn,tsn,so = match eo with
                      | None -> SMap.empty,SMap.empty,Some S_skip
                                (* Some skip rather than None because
                                   the number of cases in the generated code
                                   must be a power of 2 *)
                      | Some e -> let w,ts,s = to_s ~statics ~sums gs e x k in
                                  (w,ts,Some s)
      in
      let ts = List.fold_left (++) tsn tss in
      let w' = List.fold_left (++>) wn ws in
      w',ts,let z = Ast.gensym () in
            let z1 = Ast.gensym () in
            S_letIn(z,to_a ~sums a,
            S_letIn(z1,A_call((to_op @@ (GetTuple{pos=0;arity=2}),A_var z)),
            S_letIn(z2,A_call((to_op @@ (GetTuple{pos=1;arity=2}),A_var z)),

            S_case(z1,hs',so))))
  | E_letIn(P_var f,(E_fix(h,(p,e1)) as phi),e2) ->
     assert (f = h);
     let e1 = replace_arg phi in
     let f' = NameC.mark_return f in
     let w1,ts1,s1 = to_s ~statics ~sums (f::gs) e1 (NameC.result_of_fun f) (S_continue f') in
     let w2,ts2,s2 = to_s ~statics ~sums gs e2 x k in
     (w1++>w2),(SMap.add f s1 ts1)++ts2,s2
  | E_letIn(P_unit,e1,e2) ->
      (* [SEQ] *)
      let w2,ts2,s2 = to_s ~statics ~sums gs e2 x k in
      if Combinational.combinational e1 then (* todo: emit a warning ? *) (w2,ts2,s2) else
      let w1,ts1,s1 = to_s ~statics ~sums gs e1 (Ast.gensym ()) s2 in
      w1++>w2,ts2++ts1,s1
  | E_letIn(P_var y,e1,e2) ->
      (* [LET] *)
      let w2,ts2,s2 = to_s ~statics ~sums gs e2 x k in
      if Combinational.combinational e1 then
        w2,ts2,seq_ (set_ y (to_a ~sums e1)) s2
      else
        let w1,ts1,s1 = to_s ~statics ~sums gs e1 y s2 in
        w1++>w2,ts2++ts1,s1
  | E_app(E_var f,a) ->
      if List.mem f gs then
          (* [TAIL-CALL] *)
          let s = seq_ (set_ (NameC.formal_param_of_fun f) (to_a ~sums a)) @@
                        S_continue f in
           (SMap.empty,SMap.empty,s)
      else
          (* [DIRECT-CALL] *)
          let n = new_instance () in
          let w = SMap.singleton (NameC.mark_return f) (IMap.singleton n (seq_ (set_ x (A_var (NameC.result_of_fun f))) k)) in
          let s = seq_ (set_ (NameC.instance_id_of_fun f) (A_const (mk_int n id_size))) @@
                  seq_ (set_ (NameC.formal_param_of_fun f) (to_a ~sums a)) @@
                       S_continue f in
          (w,SMap.empty,s)
  | E_app(E_const(Op(Runtime op)),a) ->
      (* in case of instantaneous call which is not combinatorial,
         e.g., a display function for debug  *)
      SMap.empty, SMap.empty, seq_ (set_ x (A_const Unit)) @@
                              return_ (S_call(op,to_a ~sums a))

  | E_ref(a) -> SMap.empty, SMap.empty, 
                return_ (set_ x (to_a ~sums a))
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
          let w,ts,s = to_s ~statics ~sums gs (E_const Unit) x k in
          let s0 = S_if(y^"_lock", 
                       S_continue q_wait, Some(seq_ (set_ (y^"_lock") (A_const (Bool true))) @@
                        seq_ (set_ y (to_a ~sums a)) @@ (S_continue q1))) in
          (w, (SMap.add q1 (seq_ (set_ (y^"_lock") (A_const (Bool false))) s) @@
               SMap.add q_wait s0 ts), s0)
       | _ -> assert false) else
      (match ay with
       | E_var y ->
          let w,ts,s = to_s ~statics ~sums gs (E_const Unit) x k in
          (w, ts, seq_ (set_ y (to_a ~sums a)) s)
       | _ -> assert false)
  | E_array_get(y,idx) ->
      let a = to_a ~sums idx in
      let q = Ast.gensym ~prefix:"pause_get" () in
      let ts = SMap.add q (seq_ (S_ptr_take(y,false))
                                 (return_ @@ (set_ x (A_buffer_get(y))))) SMap.empty in 
      let s = seq_ (S_setptr_read(y,a)) (S_continue q) in
        let q_wait = Ast.gensym ~prefix:"q_wait" () in
        let s' = let_plug_s (A_ptr_taken(y)) @@ fun z ->
                 S_if(z, (S_continue q_wait),
                           Some (seq_ (S_ptr_take(y,true)) @@ s)) in
        (SMap.empty, SMap.add q_wait s' ts, s')
  | E_array_set(y,idx,e_upd) ->
      let a = to_a ~sums idx in
      let a_upd = to_a ~sums e_upd in
      let q = Ast.gensym ~prefix:"pause_setII" () in
      let ts = SMap.add q (seq_ (S_buffer_set(y)) @@
                           seq_ (S_ptr_write_take(y,false)) @@
                                 (return_ @@ (set_ x (A_const Unit)))) SMap.empty  in
      let q_wait = Ast.gensym ~prefix:"q_wait" () in
      let s' = let_plug_s (A_ptr_write_taken(y)) @@ fun z ->
                 S_if(z, (S_continue q_wait),
                           Some (seq_ (S_ptr_write_take(y,true)) @@
                                 seq_ (S_setptr_write(y,a,a_upd)) @@
                                 (S_continue q))) in
      (SMap.empty, SMap.add q_wait s' ts, s')
      (* todo: pas besoin de dupliquer s': l'écriture en tant que telle ne prend que 1 cycle : on peut la faire démarrer un cycle plus tard *)

  | E_matrix_get(y,idx_list) -> (* TODO: adapt, same as array_get *)
      let a_list = List.map (to_a ~sums) idx_list in
      let q1 = Ast.gensym ~prefix:"pause_getI" () in
      let q2 = Ast.gensym ~prefix:"pause_getII" () in
      let ts = SMap.add q1 (S_continue q2) @@
               SMap.add q2 (seq_ (S_ptr_take(y,false))
                                 (return_ @@ (set_ x (A_buffer_get(y))))) SMap.empty in 
      let s = seq_ (S_setptr_matrix_read(y,a_list)) (S_continue q1) in
        let q_wait = Ast.gensym ~prefix:"q_wait" () in
        let s' = let_plug_s (A_ptr_taken(y)) @@ fun z ->
                 S_if(z, (S_continue q_wait),
                           Some (seq_ (S_ptr_take(y,true)) @@
                                 seq_ s (S_continue q1))) in
        (SMap.empty, SMap.add q_wait s' ts, s')

  | E_matrix_set(y,idx_list,e_upd) ->
      let a_list = List.map (to_a ~sums) idx_list in
      let a_upd = to_a ~sums e_upd in
      let q1 = Ast.gensym ~prefix:"pause_setI" () in
      let q2 = Ast.gensym ~prefix:"pause_setII" () in
      let ts = SMap.add q1 (seq_ (S_buffer_set(y)) (S_continue q2)) @@
               SMap.add q2 (seq_ (S_ptr_write_take(y,false)) @@
                                 (return_ @@ (set_ x (A_const Unit)))) SMap.empty  in
      let q_wait = Ast.gensym ~prefix:"q_wait" () in
      let s' = let_plug_s (A_ptr_write_taken(y)) @@ fun z ->
                 S_if(z, (S_continue q_wait),
                           Some (seq_ (S_ptr_write_take(y,true)) @@
                                 seq_ (S_setptr_matrix_write(y,a_list,a_upd)) @@
                                 (S_continue q1))) in
      (SMap.empty, SMap.add q_wait s' ts, s')

  | E_reg((p,e1),e0,l) ->
      let y = match p with
              | P_var y -> y 
              | _ -> assert false 
      in

      let w1,ts1,s1 = to_s ~statics ~sums [] e1 y S_skip in
      let w0,ts0,s0 = to_s ~statics ~sums [] e0 y S_skip in
      assert (SMap.is_empty w1 && SMap.is_empty ts1);
      assert (SMap.is_empty w0 && SMap.is_empty ts0);
      (SMap.empty, SMap.empty,
      seq_ (let_plug_s (A_call(Runtime(Not),A_var l)) (fun z ->
             S_if(z, (seq_ (set_ l (A_const (Bool true))) s0), None))) @@
      seq_ s1 @@
      return_ @@ set_ x (A_var y))
  | E_exec(e1,e0,eo,l) ->
      (* assume e0 is combinational *)
      let pi = Ast.{statics;sums;main=e1} in
      let rdy,res,compute,(ts,s1) = compile (* ~result:x*) pi in
      let id = Ast.gensym ~prefix:"id" () in
      let s1' = S_fsm(id,rdy,res,compute,ts,s1,false) in
      let s2 = seq_ (let_plug_s (A_call(Runtime(Not),A_var rdy)) (fun z ->
                  S_if(z, set_ res (to_a ~sums e0), None))) @@
               return_ @@ set_ x (A_tuple[A_var res;A_var rdy]) in
      let s = seq_ s1' s2 in
      (match eo with
      | None -> (SMap.empty, SMap.empty, s)
      | Some e3 ->
         (* assume e3 is combinational *)
         let s_not_rdy = let_plug_s (to_a ~sums e3) (fun zz ->
                            S_if(zz, S_in_fsm(id,S_continue compute), None))
           in
         let s4 = seq_ (let_plug_s (A_call(Runtime(Not),A_var rdy)) (fun z ->
                  S_if(z, (seq_ (set_ res (to_a ~sums e0)) s_not_rdy), None))) @@
               return_ @@ set_ x (A_tuple[A_var res;A_var rdy]) in
          let s5 = seq_ s1' s4 in
          (SMap.empty, SMap.empty, s5))
  | E_par(es) ->
      let id_s = List.map (fun _ -> Ast.gensym ~prefix:"id" ()) es in
      let pi_s = List.map (fun e -> compile @@ Ast.{statics;sums;main=e}) es in
      let q = Ast.gensym ~prefix:"par" () in
      let ts = SMap.singleton q (
        let s_fin =
          let rdys = List.map (fun (rdy_i,_,_,_) -> A_var rdy_i) pi_s in
          let ress = List.map (fun (_,res_i,_,_) -> A_var res_i) pi_s in
          let_plug_s (conjonction_atoms rdys) (fun z ->
                       S_if(z,
                        (seq_ (S_set(x,(A_tuple ress))) @@ k),Some (S_continue q)))
        in
        let s_list' = List.map2 (fun id_i (rdy_i,res_i,compute_i,(ts_i,s_i)) -> 
                        S_fsm(id_i,rdy_i,res_i,compute_i,ts_i,S_skip,false)) id_s pi_s
        in seq_ (seq_list_ s_list') s_fin
      ) in
      let s_inFsm_list = List.map2 (fun id_i (rdy_i,res_i,compute_i,(ts_i,s_i)) -> 
                                   (S_in_fsm(id_i,s_i))) id_s pi_s in
      SMap.empty,ts, seq_ (seq_list_ s_inFsm_list)
                     (S_continue q)
  (* simplified version with only two branches:
     -----------------------------------------  
    | E_par[e1;e2] ->
      let id1 = Ast.gensym ~prefix:"id" () in
      let id2 = Ast.gensym ~prefix:"id" () in

      let pi1 = Ast.{statics;sums;main=e1} in
      let pi2 = Ast.{statics;sums;main=e2} in

      let rdy1,res1,compute1,(ts1,s1) = compile pi1 in
      let rdy2,res2,compute2,(ts2,s2) = compile pi2 in

      let q = Ast.gensym ~prefix:"par" () in
      let ts = SMap.singleton q (
        let s1' = S_fsm(id1,rdy1,res1,compute1,ts1,S_skip,false) in
        let s2' = S_fsm(id2,rdy2,res2,compute2,ts2,S_skip,false) in
        seq_ s1' @@
        seq_ s2' @@
                  let_plug_s (A_call(Runtime And,A_tuple[A_var rdy1;A_var rdy2])) (fun z ->
                  S_if(z,
                      (seq_ (S_set(x,(A_tuple[A_var res1;A_var res2]))) @@ k),Some (S_continue q)))
      ) in
      SMap.empty,ts, seq_ (S_in_fsm(id1,s1)) @@
                     seq_ (S_in_fsm(id2,s2)) @@
                     S_continue q*)

  | E_for _ -> assert false (* already expanded *)
 
  | E_fun _ | E_fix _ -> 
     (* can occur in case of higher order function that does not use its argument,
        e.g.: [let rec f g = f g in f (fun x -> x)].
        We can safely ignore it in this case *)
  SMap.empty,SMap.empty,return_ (set_ x (A_const Unit))
 

  | e -> Ast_pprint.pp_exp Format.std_formatter e; assert false (* todo *)

(* takes a program and translates it into an FSM *)

and compile ?(result=(Ast.gensym ~prefix:"result" ())) pi =
  let open Ast in

  let statics = pi.statics in
  let sums = pi.sums in
  let x = result in
  let rdy = gensym ~prefix:"rdy" () in
  let compute = gensym ~prefix:"compute" () in

  let k = seq_ (set_ rdy (A_const (Bool true))) (S_continue compute) in
  let w0,ts0,s0 = to_s ~statics ~sums [compute] pi.main x k in

 (* show compute w0; *)

  let wmain = SMap.add compute IMap.empty w0 in
  let s' = match insert_kont wmain ~compute ~x s0 with Some s -> s | None -> s0 in
  let ts_res = (SMap.bindings ts0) in
  let rec loop ts_res =
    let has_changed = ref false in
    let ts_res = List.filter_map (fun (q_aux,s) ->
                  if contain_return s then (
                    (* has_changed := true; *)
                    (match insert_kont wmain ~compute ~x s with
                    | Some s2 -> Some(q_aux,s2)
                    | None -> None))
                  else Some(q_aux,s)) ts_res in
    if (!has_changed) then loop ts_res else ts_res in
  let ts_res = loop ts_res in

  rdy,x,compute,(ts_res,seq_ (set_ rdy (A_const (Bool false))) s')

