[@@@warning "-8"] (* todo *)

open Ast
open Special_subst

let () = Random.self_init ();;

type r = {
  mu : e SMap.t ;
  statics : c array SMap.t ;
}

let r_init = {
  mu = SMap.empty ;
  statics = SMap.empty ;
}

(** [evaluated e] returns [true] iff [e] is a value *)
let rec evaluated (e:e) : bool =
  match un_deco e with
  | E_const _ | E_fun _ | E_fix _ -> true
  | E_tuple es -> List.for_all evaluated es
  (* | E_app(E_const(Op(TyConstr _)),e) -> evaluated e*)
  | _ -> false


let add_r x v r =
  { r with mu = SMap.add x v r.mu }

let remove_r x r =
  { r with mu = SMap.remove x r.mu }


let list_update n v' l =
  let rec aux i = function
  | [] -> []
  | v::r -> if i = 0 then v'::r else v::aux (i-1) r
  in aux n l

let check_bounds ~index:i ~size:n =
if i < 0 || i >= n then
  Prelude.Errors.error ~error_kw:"Runtime error"
    (fun fmt -> Format.fprintf fmt "index out of bounds")

let error_cannot_be_reduced e =
  (* should not happen since the language must be type safe *)
  Prelude.Errors.error (fun fmt ->
                       Format.fprintf fmt "expression %a cannot be reduced."
                       Ast_pprint.pp_exp e)

 

 let subst_sz_var env e =
  let rec subs sz =
    match sz with
    | Types.Sz_lit _ -> sz
    | Sz_var {contents=Unknown id} -> (match List.assoc_opt id env with
                                       | None -> sz
                                       | Some n -> Sz_lit n) 
     | Sz_var {contents=Is sz2} -> subs sz2 in
  let rec subse e =
    match e with
    | E_const(Int(l,sz)) -> E_const(Int(l,subs sz))
    | E_const(Op(Runtime op)) ->
       let op' = match op with
       | Resize_int sz -> Operators.Resize_int (subs sz)
       | Vector_create sz -> Operators.Vector_create (subs sz)
       | _ -> op
       in E_const(Op(Runtime op'))
    | e -> Ast_mapper.map subse e
  in 
  subse e


let instance ty v e =
  let size_match_val sz1 sz2 = match Types.canon_size sz1,Types.canon_size sz2 with 
    Types.Sz_var {contents=Unknown id},Types.Sz_lit n -> (* Printf.printf "===>%d|%d\n" id n; *) [(id,n)]
    | Sz_lit _ , _ -> [] 
    | _ -> [] (* could never occur if the type-checker assigned
                 a default size for non-generalized size variables
                 (to be improved) *)
  in
  let rec match_ty ty ty0 =
    match Types.canon_ty ty,Types.canon_ty ty0 with
    | Types.Ty_base (TyB_int sz1), Types.Ty_base (TyB_int sz2) -> size_match_val sz1 sz2
    | Ty_tuple(tys),Ty_tuple tys2 -> List.concat (List.map2 match_ty tys tys2)
    | Ty_base (TyB_tuple(tyBs)),Ty_base (TyB_tuple(tyBs2)) -> List.concat (List.map2 (fun tyB tyB2-> match_ty (Ty_base tyB) (Ty_base tyB2)) tyBs tyBs2)
    | Ty_fun(ty1,d,tyB2),Ty_fun(ty1',_,tyB2') ->
          match_ty ty1 ty1' @ match_ty (Ty_base tyB2) (Ty_base tyB2')
    | _ -> [] in
  let rec type_of v = 
    match v with
    | E_const(Inj _ | C_appInj _) -> 
        Prelude.Errors.raise_error 
           ~msg:"sum type not supported in evaluation mode" ()
    | E_const(c) -> Types.Ty_base (Typing.typ_const ~loc:Prelude.dloc SMap.empty c)
    | E_tuple(es) -> (Ty_tuple (List.map type_of es))
    | E_fun(_,(ty,tyr),_) -> Types.Ty_fun(ty,Types.new_dur_unknown(), tyr)
    | E_fix(_,(_,(ty,tyr),_)) -> Types.Ty_fun(ty,Types.new_dur_unknown(), tyr)
  (* let rec match_val ty v =
    match Types.canon_ty ty,v with
    | Types.Ty_base (TyB_int sz1),E_const (Int(_,sz2)) -> size_match_val sz1 sz2
    | Ty_tuple(tys),E_tuple vs -> List.concat (List.map2 match_val tys vs)
    | Ty_base (TyB_tuple(tyBs)),E_tuple vs -> List.concat (List.map2 (fun tyB v -> match_val (Ty_base tyB) v) tyBs vs)
    | Ty_fun(ty1,d,tyB2),E_fun(_,ty1',tyB2'),_) ->
    | _ -> []*)
  in
  subst_sz_var (match_ty ty (type_of v)) e

let set_buffer x e1 e2 r =
  match e1 with
  | E_const(Int(n,_)) ->
      (match SMap.find_opt x r.statics, e2 with
      | Some a, E_const c ->
          a.(n) <- c; (* todo: avoid the side effect *)
          r
      | _ -> assert false (* ill typed *)
    )
  | _ -> assert false (* ill typed *)

let buffer_get x e r =
  match SMap.find_opt x r.statics, e with
  | Some a, E_const(Int (i,_)) ->
      check_bounds ~index:i ~size:(Array.length a);
      a.(i)
  | _ -> assert false (* ill typed *)


let buffer_length x r =
  match SMap.find_opt x r.statics with
  | Some a ->
      Int(Array.length a,Types.new_size_unknown())
  | None -> assert false (* ill typed *)

let rec eq_const c1 c2 =
  match c1,c2 with
  | (Int (n,_)),(Int (m,_)) -> n = m
  | Bool _, Bool _
  | String _, String _ -> c1 = c2
  | C_tuple(cs),C_tuple cs' -> List.compare_lengths cs cs' = 0 
                               && List.for_all2 eq_const cs cs'
  | _ -> false

let unify_sz tz tz' = 
  match Types.canon_size tz, Types.canon_size tz' with
  | Types.Sz_lit n,_ | _,Types.Sz_lit n -> n
  | _ -> 32 (* could never occur if the type-checker assigned
               a default size for non-generalized size variables
               (to be improved) *)

let app_const e e2 r =
  let c = match e with
          | E_const c -> c
          | _ -> error_cannot_be_reduced @@ E_app(e,e2) in
  match c with
  | Op (Runtime (External_fun(op,_))) -> begin
      match op,e2 with
      | ("Int.add"),  E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> 
          let k = unify_sz tz tz' in E_const (Int ((n+m) mod (1 lsl (k-1)), Sz_lit k)), r
      | ("Int.sub"),  E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> let k = unify_sz tz tz' in  E_const (Int ((n-m) mod (1 lsl (k-1)),Sz_lit k)), r
      | ("Int.mul"), E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> let k = unify_sz tz tz' in   E_const (Int ((n*m) mod (1 lsl (k-1)),Sz_lit k)), r
      | ("Int.div"),  E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> let k = unify_sz tz tz' in  E_const (Int ((n/m) mod (1 lsl (k-1)),Sz_lit k)), r
      | ("Int.modulo"),  E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> let k = unify_sz tz tz' in   E_const (Int ((n mod m) mod (1 lsl (k-1)),Sz_lit k)), r
      | ("Int.lt"),   E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> let _k = unify_sz tz tz' in  E_const (Bool (n<m)), r
      | ("Int.le"),   E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> let _k = unify_sz tz tz' in   E_const (Bool (n<=m)), r
      | ("Int.gt"),   E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> let _k = unify_sz tz tz' in  E_const (Bool (n>m)), r
      | ("Int.ge"),   E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> let _k = unify_sz tz tz' in  E_const (Bool (n>=m)), r
      | ("Int.eq"),   E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> let _k = unify_sz tz tz' in  E_const (Bool (n==m)), r
      | ("Int.neq"),  E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> let _k = unify_sz tz tz' in  E_const (Bool (n!=m)), r
      | ("Bool.land"),  E_tuple [E_const (Bool a); E_const (Bool b)] -> E_const (Bool (a&&b)), r
      | ("Bool.lor"),  E_tuple  [E_const (Bool a); E_const (Bool b)] -> E_const (Bool (a||b)), r
      | ("Bool.lxor"),  E_tuple [E_const (Bool a); E_const (Bool b)] -> E_const (Bool (if a then not b else b)), r
      | ("Bool.lnot"),  E_const (Bool b) -> E_const (Bool (not b)), r
      | ("Int.abs"),  E_const (Int (n,tz)) -> E_const (Int (abs n,tz)), r
      | ("Int.neg"),  E_const (Int (n,tz)) -> E_const (Int (- n,tz)), r
      | ("Vect.infos", E_const(C_vector cs)) -> E_const(C_tuple[Int(List.length cs,Types.new_size_unknown()); List.hd cs]), r
      | ("Vect.nth"),  E_tuple [E_const (C_vector cs); E_const (Int (i,tz))] -> 
          let len = List.length cs in
          if i < 0 || i > len then check_bounds ~index:i ~size:len;
          E_const (List.nth cs i), r
      | ("Vect.copy_with"),  E_tuple [E_const (C_vector cs); E_const (Int (i,tz));E_const ci] -> 
          let len = List.length cs in
          if i < 0 || i > len then check_bounds ~index:i ~size:len;
          let rec update n i cs =
            match cs with
            | [] -> assert false
            | c::cs' -> if n = i then ci::cs' else c::update (n+1) i cs
          in 
          let cs' = update 0 i cs in
          E_const (C_vector cs'), r
      | x,_ -> Printf.printf "===> %s\n" x; assert false (* unknown pritmitive *)
      end
  | Op op -> begin
      match op,e2 with
      | Runtime(Vector_create sz), E_const(c) ->
          (match sz with
          | Sz_lit k ->
              E_const(C_vector(List.init k (fun _ -> c))),r
          | _ -> assert false)
      | Runtime(Tuple_of_int n),  E_const (Int (l,tz)) ->
          (* https://stackoverflow.com/questions/14328600/ocaml-looking-at-a-specific-digit-of-an-int *)
          let digits n = (* Assuming n is a non-negative number *)
              let rec loop n acc =
                  if n = 0 then acc
                  else loop (n/10) (Bool (n mod 10 = 1)::acc) in
              match n with
              | 0 -> [Bool false]
              | _ -> loop n []
          in
          let ls = digits l in
          let rec loop acc l n =
            match l,n with
            | _,0 -> List.rev acc 
            | x::xs,n -> loop (x::acc) xs (n-1)
            | [],n -> loop (Bool false::acc) l (n-1)
          in 
          let vs = loop [] ls n in
          E_tuple (List.map (fun c -> E_const c) vs), r
      | Runtime(Resize_int sz),  E_const (Int (l,tz)) -> 
          let return_c k =
            E_const (Int (l mod (1 lsl (k-1)) ,Sz_lit k))
          in
          (match Types.canon_size sz with
          | Sz_lit k ->
              return_c k
          | _ -> 
              return_c 32), r
      | Runtime(Print_string),E_const (String s) ->
         assert (evaluated e);
         Format.fprintf Format.std_formatter "%s" s; flush stdout; E_const Unit, r
      | Runtime(Print_int),E_const (Int(n,_)) ->
         assert (evaluated e);
         Format.fprintf Format.std_formatter "%d" n; flush stdout; E_const Unit, r
      | Runtime(Print_newline),E_const Unit ->
         assert (evaluated e);
         Format.fprintf Format.std_formatter "\n"; flush stdout; E_const Unit, r
  
      | Runtime(Print),e ->
         assert (evaluated e);
         Format.fprintf Format.std_formatter "==> %a\n" Ast_pprint.pp_exp e; flush stdout; E_const Unit, r
      | GetTuple{pos=i;arity=n}, E_tuple vs ->
          check_bounds ~index:i ~size:n;
          List.nth vs i, r
      | GetTuple{pos=i;arity=n}, E_const(C_tuple cs) ->
          check_bounds ~index:i ~size:n;
          E_const(List.nth cs i), r
      | Runtime(Assert),E_const (Bool b) -> assert b; E_const Unit, r
      | Wait 0,v ->
          v, r
      | Wait n,v ->
          E_app(E_const(Op(Wait (n-1))),v), r
      | Runtime(String_length),E_const(String s) -> E_const(Int (String.length s,Sz_lit 32)), r
      | TyConstr _, v -> v, r
      | _ -> error_cannot_be_reduced (E_app(e,e2))
    end
  | Inj _ -> E_app(e,e2),r
  | (Unit|Bool _|Int _|String _|V_loc _|C_tuple _) ->
      error_cannot_be_reduced (E_app(e,e2))

let rec find_case c hs =
  match hs with
  | [] -> None
  | (cs,e)::hs' -> if List.exists (eq_const c) cs then Some (e) else
                   find_case c hs'


open Format
let fmt = Format.std_formatter

let rec red (e,r) =
   (* fprintf fmt "%a\n" Ast_pprint.pp_exp  e ;
   (fprintf fmt "i: ";
             SMap.iter (fun x e -> fprintf fmt "    (%s %a)\n" x Ast_pprint.pp_exp e) r.mu;
             fprintf fmt "@,"); *)
  let rec red_list es r =
    let rec aux acc es r =
      match es with
      | [] -> List.rev acc,r
      | e1::es' -> let e1',r1 = red (e1,r) in
                   aux (e1' :: acc) es' r1
    in aux [] es r
  in

  if evaluated e then (
    (* [REDUCE-VAL] *) 
    (e,r)
  ) 
  else
  match e with
  | E_deco(e1,_) -> red (e1,r)
  | E_const _ | E_fun (_,_, _) | E_fix (_, _) ->
      assert false (* see [REDUCE-VAL] *)
  | E_var x -> 
      (* we reduce only close expressions *)
      Prelude.Errors.raise_error ~msg:("unbound variable "^x) ()
  | E_if(e,e1,e2) ->
     (match red (e,r) with
     | E_const (Bool true),r' ->
         (* [REDUCE-IF-TRUE] *)
        red (e1,r')
     | E_const (Bool false),r' ->
        (* [REDUCE-IF-FALSE] *)
        red (e2,r')
     | (e',r') ->
        (* [REDUCE-IF-PAUSE] *)
        E_if(e',e1,e2),r')
  | E_case(e,hs,e_els) ->
     (match red (e,r) with
      | (E_const c,r') ->
         (* [REDUCE-CASE-SELECT] *)
         (match find_case c hs with
          | None -> red (e_els,r')
          | Some ei -> red (ei,r'))
      | (e',r') ->
         (* [REDUCE-CASE-PAUSE] *)
         E_case(e',hs,e_els),r')
  | E_match(e,hs,eo) ->
     (match red (e,r) with
      | (E_app(E_const (Inj ctor),(E_const _ as v)),r') ->
         (* [REDUCE-MATCH-SELECT] *)
         (match List.assoc_opt ctor hs with
          | None -> (match eo with
                     | None -> Prelude.Errors.raise_error ~msg:"match failure" ()
                     | Some e' -> (e',r))
          | Some (p,ei) -> red ((subst_p_e p v ei),r'))
      | (e',r') ->
         (* [REDUCE-CASE-PAUSE] *)
         E_match(e',hs,eo),r')
  | E_letIn(p,ty,e1,e2) ->
    let e1',r' = red (e1,r) in
    if evaluated e1'
    then (* [REDUCE-LET-VAL] *)
      red (subst_p_e p e1' e2,  r')
    else (* [REDUCE-LET-PAUSE] *)
      (E_letIn(p,ty,e1',e2),r')
  | E_tuple es ->
      (* [REDUCE-TUPLE] *)
      let rec loop acc es r =
        match es with
      | [] -> (* [REDUCE-TUPLE-VAL] *)
              let es' = List.rev acc in 
              (E_tuple es',r)
      | e1::es' ->
        let e1',r' = red (e1,r) in
        if evaluated e1' then
          loop (e1'::acc) es' r' 
        else
          (* [REDUCE-TUPLE-PAUSE] *)
          let es' = List.rev_append acc (e1'::es') in
          (E_tuple es', r')
      in
      loop [] es r
  | E_app(E_const (Op (TyConstr ty)),e2) ->
      (* let e2 = instance t e2 in *)
      let e',mu' = red (e2(*id_tau e2 t*),r) in
      if evaluated e' then (e',mu') else (E_app(E_const (Op (TyConstr ty)),e'),mu')
  | E_app(e1,e2) ->
      let e2',r' = red (e2,r) in
      if not (evaluated e2')
      then (* [REDUCE-APP-PAUSE] *)
        (E_app(e1,e2'), r')
      else
        let v = e2' in
        let e1',r'' = red (e1,r') in
        (match e1' with
         | E_const c -> app_const e1' v r''
         | E_fun(p,(ty,_),e) ->
             let e1 = instance ty v e in
             let e' = subst_p_e p v e1 in
             red (e',r'')
         | (E_fix(g,(p,(ty,_),e))) as w ->
             let e = instance ty v e in
             (E_letIn(p,ty,v,subst_e g w e)),r''  
            (** we do not substituate [p] by [v] in [e] directly as the redex must not be a value *)
         | _ -> assert false)
  | E_reg((p,tyB,e1),e0,l) ->
      (* [Reg] *)
      let v0,r = red (e0,r) in
      let v = match SMap.find_opt l r.mu with
              | None -> v0
              | Some v -> v in
      let v',r = red (E_letIn(p,Ty_base tyB,v,e1),r) in
      v', add_r l v' r
  (* | E_set(e1,e2) ->
     (* [Set] *)
     let v1,r1 = red (e1,r) in
     let v2,r2 = red (e2,r1) in
     (E_const Unit, add_r x v r') *)
  | E_exec (e1,e2,eo3,k) ->
      let r = match eo3 with
              | None -> r
              | Some(e3) ->
                 let v3,r = red (e3,r) in
                 assert (evaluated v3);
                 (match v3 with
                  | E_const (Bool false) ->
                     (* [REDUCE-EXEC-RESET] *)
                     (add_r k e1 r)
                  | _ -> r)
      in
      if not (SMap.mem k r.mu) then
        (* [REDUCE-EXEC-INIT] *)
        let r' = add_r k e1 r in
        red (E_exec (e1,e2,eo3,k),r')
      else
        let e = SMap.find k r.mu in
        let e',r' = red (e,r) in
        if evaluated e' then
          (* [REDUCE-EXEC-VAL] *)
          (* let e1',r' = red (e1,r') in*)
          E_tuple[e';E_const(Bool true)], (remove_r k r')
        else
          (* [REDUCE-EXEC-DEFAULT] *)
          let v2,r'' = red (e2,r') in
          assert (evaluated v2);
          E_tuple[v2;E_const(Bool false)], (add_r k e' r'')
  | E_par(es) ->
      (* [Par] *)
      let es',r' = red_list es r in
      let e' = if List.for_all evaluated es'
               then E_tuple es'
               else E_par es' in
      e',r'
  | E_array_create(sz,_) ->
      let l = Ast.gensym () in
      let n = match sz with 
              | Sz_lit n -> n 
              | _ -> assert false (*error *)
      in
      let any_constant = Unit in
      E_const (V_loc l), {r with statics = SMap.add l (Array.make n any_constant) r.statics}
  | E_array_get (x,e1) ->
      let e1',r1 = red (e1,r) in
      if evaluated e1' then E_const (buffer_get x e1' r),r else
      E_array_get (x,e1'),r1
  | E_array_length(x) ->
      E_const(buffer_length x r),r
  | E_array_set (x,e1,e2) ->
      let e1',r1 = red (e1,r) in
      let e2',r2 = red (e2,r1) in
      if evaluated e1' && evaluated e2' then E_const(Unit), set_buffer x e1' e2' r else
      E_array_set (x,e1',e2'),r2
  | E_generate _ 
  | E_for _ -> assert false (* todo *)



let eval_prog mu main v =
  (* fprintf fmt "%a\n" Ast_pprint.pp_exp  main ;*)
  let e',mu' = red ((E_app(main,v)), mu) in
  (e',mu') ;;

let eval n mu main vs ty =
  if vs = [] then (
    Prelude.Errors.error (fun fmt ->
       Format.fprintf fmt "@[<v>Arguments needed to evaluate the program (see option -arg)@]")
  );
  let rec eval_aux i n mu ef vs =
    if i >= n then () else (
    match vs with
    | [] -> ()
    | [v] -> let len = (n - i + 1) in 
             assert (len > 1);
             let vs' = List.init len (fun _ -> v) in

             eval_aux i n mu ef vs'
    | v::vs' ->
       let open Prelude.Errors in
       let open Format in
       fprintf std_formatter "@[<v 0>%a : " 
                  (emph_pp blue (fun fmt () -> fprintf fmt "cycle %d" i)) ();
       fprintf std_formatter "%a --> " Ast_pprint.pp_exp v ;
       (let e',mu' = eval_prog mu ef v in
        if evaluated e' then (
           let v' = e' in
           fprintf std_formatter "%a@,"
                 Ast_pprint.pp_exp v';
           eval_aux (i+1) n mu' main vs'
        ) else (
          fprintf std_formatter "%a@," (emph red) "(running)";
          eval_aux (i+1) n mu' (E_fun(P_var "_",(ty,Types.new_tyB_unknown()),e')) vs'
        );
        fprintf std_formatter "@]" 
    )) in 
    eval_aux 0 n mu (* (Ast_undecorated.remove_deco*) main vs

let prepare_statics (statics: (x * static) list) : c array SMap.t =
  smap_of_list statics |>
  SMap.map (function 
            | Static_array(c,n) -> Array.make n c
            | Static_const c -> Array.make 1 c)

let interp_pi ~nb_iterations (pi : pi) (value_list : e list) ty : (e * r) =
  let r = { r_init with statics = prepare_statics pi.statics } in
  let targ = Types.new_ty_unknown () in
  Typing.unify_ty ~loc:Prelude.dloc ty (Ty_fun(targ,Types.new_dur_unknown(), Types.new_tyB_unknown ()));
  eval nb_iterations r (* (ty_annot ~ty*) pi.main value_list targ;
  (E_const Unit, r_init) ;;

