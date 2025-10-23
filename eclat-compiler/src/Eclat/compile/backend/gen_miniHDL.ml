open MiniHDL_syntax

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
  | S_set _ 
  | S_array_set _ -> false
  | S_letIn(_,_,s1) -> aux s1
  | S_seq(s1,s2) -> aux s1 || aux s2
  | S_if(_,s1,so2) -> aux s1 || (match so2 with None -> false | Some s -> aux s)
  | S_case(_,hs,so) ->
      List.exists (fun (_,s) -> aux s) hs || (match so with None -> false | Some s1 -> aux s1)
  | S_fsm _ | S_in_fsm _ -> false (* ok? *)
  | S_external_run _ -> false in
  aux s


let find_ctor ?(tyB=None) x sums =
  let (n,sum,t) = Types.find_ctor x sums in
  let t = match tyB with None -> Types.canon_tyB t | Some t -> t in
  let arg_size = List.fold_left (max) 0 @@ List.map (fun (_,t) -> MiniHDL_typing.(size_ty (translate_tyB (Types.canon_tyB t)))) sum in
  let sz = MiniHDL_typing.compute_tag_size sum in
  let n = if n = 0 then int_of_float (2. ** float sz) - 1 else n - 1 in
  (mk_int n sz,arg_size,MiniHDL_typing.translate_tyB t)

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
    | S_external_run _ 
    | S_array_set _ -> s
  in
  Some (aux s)

let rec to_c ~sums = function
| Ast.Unit -> Unit
| Ast.Int (n,tz) -> Int {value=n;tsize=MiniHDL_typing.translate_size tz}
| Ast.Bool b -> Bool b
| Ast.String s -> String s
| Ast.C_tuple cs -> CTuple (List.map (to_c ~sums) cs)
| Ast.C_vector cs -> CVector (List.map (to_c ~sums) cs)
| Ast.C_size n -> CSize n
| Ast.C_appInj(x,c,tyB) ->
    let n,arg_size,ty_n = find_ctor ~tyB:(Some tyB) x sums in
    CTuple[n;C_encode(to_c ~sums c,arg_size)]
| Ast.Inj _ -> assert false (* no partial application in the generated code *)
| Ast.(Op _ | V_loc _) -> assert false


let to_op = function
| Ast.TyConstr ty -> TyConstr (MiniHDL_typing.translate_ty ty)
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
  | Ast.E_letIn(P_var x,_,e1,e2) -> A_letIn(x,to_a ~externals ~sums e1,to_a ~externals ~sums e2)
  | Ast.E_array_length x -> A_array_length(x,new_tvar())
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
  | Ast.E_fix(f,(P_var x,_,e1)) -> Ast_subst.subst_e x (E_var (NameC.formal_param_of_fun f)) e1
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
let rec to_s ~statics ~externals ~sums gs e x =
  if Instantaneous.combinational ~externals e then 
    set_ x (to_a ~externals ~sums e) 
  else
  match e with
  | Ast.E_if(a,e1,e2) ->
      let s1 = to_s ~statics ~externals ~sums gs e1 x in
      let s2 = to_s ~statics ~externals ~sums gs e2 x in
      let z = Ast.gensym () in
      S_letIn(z,to_a ~externals ~sums a,S_if(z,s1,Some s2))
  | E_case(a,hs,e_els) ->
      let hs' = List.map (fun (cs,e) ->
                         let s = to_s ~statics ~externals ~sums gs e x in
                         (List.map (to_c ~sums) cs,s)
                       ) hs
      in
      let s_els = to_s ~statics ~externals ~sums gs e_els x in
      let z = Ast.gensym () in
      S_letIn(z,to_a ~externals ~sums a, S_case(z,hs',Some s_els))
  | E_match(a,hs,eo) ->
      (* [MATCH] *)
      let z2 = Ast.gensym () in
      let hs' = List.map (fun (inj,(py,e)) ->
                         let y = match py with Ast.P_var y -> y | _ -> assert false in
                         let s = to_s ~statics ~externals ~sums gs e x in
                         let n,_,ty_n = find_ctor inj sums in
                         ([n],(seq_ (set_ y (A_decode(z2,ty_n))) @@ s))
                       ) hs
      in
      let so = match eo with
                | None -> Some S_skip
                          (* Some skip rather than None because
                             the number of cases in the generated code
                             must be a power of 2 *)
                | Some e -> let s = to_s ~statics ~externals ~sums gs e x in
                            (Some s)
      in
      let z = Ast.gensym () in
      let z1 = Ast.gensym () in
      S_letIn(z,to_a ~externals ~sums a,
      S_letIn(z1,A_call((to_op @@ (GetTuple{pos=0;arity=2}),A_var z)),
      S_letIn(z2,A_call((to_op @@ (GetTuple{pos=1;arity=2}),A_var z)),
      S_case(z1,hs',so))))
  | E_letIn(P_var y,_,e1,e2) ->
      (* [LET] *)
      seq_ (to_s ~statics ~externals ~sums gs e1 y)
           (to_s ~statics ~externals ~sums gs e2 x)
  | E_app(E_const(Op((Runtime Locked) as op)),E_var l) ->
      set_ x (A_ptr_taken l)
  | E_app(E_const(Op((Runtime Acquire) as op)),E_var l) ->
     S_acquire_lock(l)
  | E_app(E_const(Op((Runtime Release) as op)),E_var l) ->
     S_release_lock(l)
  | E_app(E_const(Op((Runtime Start_read _) as op)),E_tuple[E_var z;idx]) ->
     S_read_start(z,to_a ~externals ~sums idx)
  | E_app(E_const(Op((Runtime End_read) as op)),E_var z) ->
     S_read_stop(x,z)
  | E_app(E_const(Op((Runtime Start_write _) as op)),E_tuple[E_var z;idx;a]) ->
     S_write_start(z,to_a ~externals ~sums idx,to_a ~externals ~sums a)
  | E_app(E_const(Op((Runtime End_write) as op)),E_var z) ->
     S_write_stop(z)
  | E_app(E_const(Op((Runtime _) as op)),a) ->
      (* in case of instantaneous call which is not combinatorial,
         e.g., a display function for debug  *)
      set_ x (A_call(to_op op,to_a ~externals ~sums a))
                               

  (*| E_ref(a) -> SMap.empty, SMap.empty, 
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
          let s' = let_plug_s (A_ptr_taken(y)) @@ fun z ->
                     S_if(z, (S_continue q_wait),
                               Some (seq_ (S_acquire_lock(y)) @@
                                     seq_ (S_write_start(y,a,a_upd)) @@
                                     (S_continue q1))) in
          (SMap.empty, SMap.add q_wait s' ts, s')
      )
  *)| E_reg((p,_,e1),e0,l) ->
      let y = match p with
              | P_var y -> y 
              | _ -> assert false 
      in

      let s1 = to_s ~statics ~externals ~sums [] e1 y in
      let s0 = to_s ~statics ~externals ~sums [] e0 y in
      seq_ (S_if(l, S_skip, Some (seq_ (set_ l (A_const (Bool true))) s0))) @@
      seq_ s1 @@ set_ x (A_var y)
  | E_exec _ | E_par _ | E_pause _ -> assert false
 
  | E_for _ -> assert false (* already expanded *)
 
  | E_fun _ | E_fix _ -> 
     (* can occur in case of higher order function that does not use its argument,
        e.g.: [let rec f g = f g in f (fun x -> x)].
        We can safely ignore it in this case *)
     (set_ x (A_const Unit))

  (*| E_run(f,e) ->
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
 *)
  (*| E_equations(p,eqs) ->
      let acc = ref [] in
      let rec compile_le clks p x le =
        let rec aux p ax = 
          match p with
          | Ast.P_var z -> set_ z ax
          | P_tuple ps -> 
              let n = List.length ps in
              seq_list_ @@ List.mapi (fun i p -> aux p (A_call(GetTuple (i,n,new_tvar()), ax))) ps
          | P_unit -> S_skip
        in 
        match le with
        | Ast.Exp e1 -> let w1,ts1,s1 = to_s ~statics ~externals ~sums [] e1 x (aux p (A_var x)) in
                    assert (SMap.is_empty w1);    
                    assert (SMap.is_empty ts1);
                    s1
        | Fby(le1, le2) -> let x_init = Ast.gensym ~prefix:(x^"init") () in
                           let x_mem = Ast.gensym ~prefix:(x^"mem") () in
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
                  let y = Ast.gensym () in
                  compile_le [] p y le
                  ) eqs
      in SMap.empty,SMap.empty, seq_ (seq_list_ ss) (seq_ (set_ x (to_a ~externals ~sums  (Pattern.pat2exp p))) @@ 
      seq_ (seq_list_  
      (List.map (fun (clks,s) -> 
            let_plug_s (conjonction_atoms (List.map (fun clk -> A_var clk) clks))
            @@ fun z -> S_if(z,s,None)) !acc)) k)
      (* let p_tuple = Ast.P_tuple (List.map fst eqs) in
      let eqs' = eqs (* List.map (fun (p,e) -> let p' = Ast_rename.rename_pat ~statics:(List.map fst statics) p in p',e) eqs*) in
      let rec aux p1 p2 = 
        match p1,p2 with
        | Ast.P_var x1, Ast.P_var x2 -> set_ x1 (A_var x2)
        | P_tuple ps1, P_tuple ps2 -> seq_list_ @@ List.map2 aux ps1 ps2
        | P_unit,P_unit -> S_skip
        | _ -> assert false
      in
      (* let s_init = aux p_tuple_pre p_tuple in *)
      let rec assign_p_p' p p' = match p,p' with 
      | Ast.P_var z1,Ast.P_var z2 -> set_ z1 (A_var z2)
      | Ast.P_tuple ps', Ast.P_var z2 -> 
          let n = List.length ps' in 
          seq_list_ (List.mapi (fun i p -> 
              (* assume p is a variable *)
              match p with
              | Ast.P_var zp ->
                 set_ zp (to_a ~externals ~sums @@ Matching.get_tuple i n (E_var z2))) ps')
      | Ast.P_tuple ps, Ast.P_tuple ps' -> seq_list_ (List.map2 assign_p_p' ps ps')
      | Ast.P_unit,Ast.P_unit -> S_skip
      | _ -> assert false in
      let s' = List.fold_right  (fun (pi,ei) s ->
        let z' = Ast.gensym () in
        let _w,_ts,s' = to_s ~statics ~externals ~sums [] ei (* (Ast_subst.subst_p_e p_tuple (Pattern.pat2exp p_tuple_pre) ei) *) z' S_skip in
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

    (*let p_tuple = Ast.P_tuple (List.map fst eqs) in
      let p_tuple_pre = Ast_rename.rename_pat ~statics:(List.map fst statics) p_tuple in
      let rec aux p1 p2 = 
        match p1,p2 with
        | Ast.P_var x1, Ast.P_var x2 -> set_ x1 (A_var x2)
        | P_tuple ps1, P_tuple ps2 -> seq_list_ @@ List.map2 aux ps1 ps2
        | P_unit,P_unit -> S_skip
        | _ -> assert false
      in
      let s_init = aux p_tuple_pre p_tuple in
      let s' = List.fold_right  (fun (pi,ei) s ->
        match pi with
        | Ast.P_var z ->
            let _w,_ts,s' = to_s ~statics ~externals ~sums [] ei (* (Ast_subst.subst_p_e p_tuple (Pattern.pat2exp p_tuple_pre) ei) *) z S_skip in
            assert (SMap.is_empty _w);
            assert (SMap.is_empty _ts);
            seq_ s' s) eqs S_skip in
      SMap.empty,SMap.empty,seq_ (seq_ S_skip (seq_ s' (set_ x (to_a ~externals ~sums @@ Pattern.pat2exp p))))
                                 k (* (aux p_tuple p_tuple_pre)*)
*)*)
  | e -> Ast_pprint.pp_exp Format.std_formatter e; assert false (* todo *)

(* takes a program and translates it into an FSM *)

and compile ?(result=(Ast.gensym ~prefix:"result" ())) pi =
  let open Ast in

  let statics = pi.statics in
  let externals = pi.externals in
  let sums = pi.sums in

  List.iter (fun (x,sum) ->
      List.iter (fun (ctor,tyB) ->
     let open Types in
   let unknowns = Hashtbl.create 100 in
        let rec inst_size = function
  | Sz_var ({contents=Unknown n} as sz) ->
          (try find_unsafe unknowns n |> ignore
           with Not_found -> sz := Is (Sz_lit(32)))
  | Sz_var {contents=Is sz} ->
      inst_size sz
  | Sz_lit _ -> () in
  let rec inst_tyB = function
  | TyB_var {contents=Unknown n} as tyB ->
      ()
  | TyB_var {contents=Is tyB} ->
      inst_tyB tyB
  | TyB_bool | TyB_unit as tyB -> ()
  | TyB_int sz -> inst_size sz
  | TyB_tuple tyB_list ->
      List.iter inst_tyB tyB_list
  | TyB_size sz ->
      (inst_size sz)
  | TyB_sum ctors ->
       (List.iter (fun (_,tyB) -> inst_tyB tyB) ctors)
  | TyB_string sz ->  (inst_size sz)
  | TyB_abstract(x,szs,tyB_list) ->
      List.iter inst_size szs;
      List.iter inst_tyB tyB_list
  in

     inst_tyB tyB;

   Format.(fprintf std_formatter "[%s] %s %a\n" x ctor Types.pp_tyB tyB)
      ) sum) sums;
   (* afficher les types sommes maintenant ! *)

  let x = result in
  let rdy = gensym ~prefix:"rdy" () in
  let idle = gensym ~prefix:"idle" () in
  Printf.printf "gi!"; Format.(flush stdout);
  let s = to_s ~statics ~externals ~sums [idle] pi.main x in
Printf.printf "ga!"; Format.(flush stdout);
  rdy,x,idle,([],seq_ (set_ rdy (A_const (Bool false))) s)

