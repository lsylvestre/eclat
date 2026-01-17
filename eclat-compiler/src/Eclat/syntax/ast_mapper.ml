open Ast

(** traversal order of sub-expressions is unspecified *)

let rec map f e =
  match e with
  | E_deco(e, ty) ->
      let e' = f e in
      E_deco(e', ty)
  | E_const _ | E_var _ -> e
  | E_fun(p, ty, e) ->
      E_fun(p, ty, f e)
  | E_fix(x, (p, ty, e)) ->
      let e' = f e in
      E_fix(x, (p, ty, e'))
  | E_tuple es ->
      let es' = List.map f es in
      E_tuple es'
  | E_app(e1, e2) ->
      let e1' = f e1 in
      let e2' = f e2 in
      E_app(e1', e2')
  | E_if(e1, e2, e3) ->
      let e1' = f e1 in
      let e2' = f e2 in
      let e3' = f e3 in
      E_if(e1', e2', e3')
  | E_case(e1, hs, e_els) ->
      let e1' = f e1 in
      let hs' = List.map (fun (c, e) -> c, f e) hs in
      let e_els' = f e_els in
      E_case(e1',hs',e_els')
  | E_match(e1, hs, eo) ->
      let e1' = f e1 in
      let hs' = List.map (fun (c, (p, e)) -> c, (p, f e)) hs in
      let eo' = Option.map f eo in
      E_match(e1', hs', eo')
  | E_letIn(p, ty, e1, e2) ->
      let e1' = f e1 in
      let e2' = f e2 in
      E_letIn(p, ty, e1', e2')
  | E_ref(e1) ->
      let e1' = f e1 in
      E_ref(e1')
  | E_get(e1) ->
      let e1' = f e1 in
      E_get(e1')
  | E_set(e1,e2) ->
      let e1' = f e1 in
      let e2' = f e2 in
      E_set(e1',e2')
  | E_array_make(sz,e1,loc) ->
      let e1' = f e1 in
      E_array_make(sz,e1',loc)
  | E_array_create _ ->
      e
  | E_array_length _ ->
      e
  | E_array_get(x_annot, e1) ->
      let e1' = f e1 in
      E_array_get(x_annot,e1')
  | E_array_set(x_annot,e1,e2) ->
      let e1' = f e1 in
      let e2' = f e2 in
      E_array_set(x_annot, e1', e2')
  | E_array_get_start(x_annot,e1) ->
      let e1' = f e1 in
      E_array_get_start(x_annot,e1')
  | E_array_get_end _ -> e
  | E_array_set_immediate(x_annot,e1,e2) ->
      let e1' = f e1 in
      let e2' = f e2 in
      E_array_set_immediate(x_annot, e1', e2')
  | E_array_from_file(x,e1) ->
      let e1' = f e1 in
      E_array_from_file(x,e1')
  | E_par(es) ->
      let es' = List.map f es in
      E_par es'
  | E_reg((p,tyB,e1),e0,l) ->
      let e0' = f e0 in
      let e1' = f e1 in
      E_reg((p, tyB, e1'), e0', l)
  | E_exec(e1,e2,e3,l) ->
      let e1' = f e1 in
      let e2' = f e2 in
      let e3' = Option.map f e3 in
      E_exec(e1', e2', e3', l)
  | E_vector es ->
      let es' = List.map f es in
      E_vector es'
  | E_vector_mapi (is_par, (p, typ, e1), e2, ty) ->
      let e1' = f e1 in
      let e2' = f e2 in
      E_vector_mapi (is_par, (p, typ, e1'), e2', ty)
  | E_run(i,e,l) ->
      E_run(i, f e, l)
  | E_for(x,sz1,sz2,e,loc) ->
      E_for(x,sz1,sz2,f e,loc)
  | E_generate((p, ty, e1), e2, sz3, sz4, loc) ->
      E_generate((p, ty, f e1), f e2, sz3, sz4, loc)
  | E_pause (l,e1) -> E_pause (l,f e1)
  | E_sig_get(x) -> 
      E_sig_get(x)
  | E_emit(x,e1) ->
      let e1' = f e1 in
      E_emit(x,e1')
  | E_sig_create(e1) ->
      let e1' = f e1 in
      E_sig_create(e1')
  | E_loop(e1) ->
      let e1' = f e1 in
      E_loop(e1')
  | E_trap _ -> e
  | E_exit(x,e1) ->
      let e1' = f e1 in
      E_exit(x,e1')
  | E_suspend(e1,x) ->
      let e1' = f e1 in
      E_suspend(e1',x)


(** traversal order of sub-expressions is unspecified *)
let rec iter f (e:e) : unit =
  match e with
  | E_deco (e,_) ->
      f e
  | E_var _ ->
      ()
  | E_const _ ->
      ()
  | E_if(e1,e2,e3) ->
      f e1; f e2; f e3
  | E_case(e1,hs,e_els) ->
      f e1; List.iter (fun (_,ei) -> f ei) hs; f e_els
  | E_match(e1,hs,eo) ->
      f e1; List.iter (fun (_,(_,ei)) -> f ei) hs; Option.iter f eo
  | E_app(e1,e2) ->
      f e1; f e2
  | E_letIn(_,_,e1,e2) ->
      f e1; f e2
  | E_tuple es ->
      List.iter f es
  | E_fun(_,_,e) | E_fix(_,(_,_,e)) ->
      f e
  | E_ref(e1) ->
      f e1
  | E_get(e1) -> f e1
  | E_set(e1,e2) ->
      f e1; f e2
  | E_par(es) ->
      List.iter f es
   | E_reg((_,_,e1),e0,_) ->
      f e1; f e0
  | E_exec(e1,e2,e3,_) ->
      f e1; f e2; Option.iter f e3
  | E_array_make(_,e1,_) ->
      f e1
  | E_array_create _ ->
      ()
  | E_array_length _ ->
      ()
  | E_array_get(_,e1) ->
      f e1
  | E_array_set(_,e1,e2) ->
      f e1; f e2
  | E_array_get_start(_,e1) ->
      f e1
  | E_array_get_end _ ->
      ()
  | E_array_set_immediate(_,e1,e2) ->
      f e1; f e2
  | E_array_from_file(_,e1) -> f e1
  | E_for(_,_,_,e,_) ->
      f e
  | E_generate((_,_,e1),e2,_,_,_) ->
      f e1; f e2
  | E_vector es ->
      List.iter f es
  | E_vector_mapi(_,(_,_,e1),e2,_) ->
      f e1; f e2
  | E_run(_,e1,_) ->
      f e1
  | E_pause (_,e1) -> f e1
 
(*

type clock1 ;;
operator Lustre.to_bool : bool => 'a ;;
operator Lustre.as_bool : 'a => bool ;;

let when(f,c) =
  if as_bool c then f() else absent ();;

let whenot(f,c) =
  if as_bool c then absent() else f();;

let merge(c,x,y) =
  if as_bool c then
  *)
(****************



operator Lustre.when_on : ('a * 'ck clock<1>) => ('a * 'ck clock<1>) on<1> ;;
operator Lustre.when_not_on : ('a * 'ck clock<1>) => ('a * 'ck clock<1>) not_on<1> ;;

type 'a clock<1> ;;

let c1 : clock1 ;;
let c2 : clock2 ;;


let when(f,c) =
  if as_bool Lustre.when_on(,c)

let whenot(s,f) =
  if as_clock(s) then absent() else f() ;; 



let x = clock<> ;;
let y = signal<> ;;

node main i returns o =
  o1 = when(c1,i);
  o2 = whenot(c1,i);
  o = merge(c1,o1,o2) ;;


let f () = print_int 3 ;;
let g () = print_int 4 ;;
let main : int => unit = 
  fun n ->
    let s = signal<> in
    merge (s,f,g) ;;


(*
type 'a not_on<1> ;;
type 'a on<1> ;;
type 'a clock<1> ;;

operator%with_sizes Lustre.as_clock : (bool * 'ck) => 'ck clock<1> ;; 
operator%with_sizes Lustre.as_bool : 'a => bool ;; 



operator Lustre.when_on : ('a * 'ck clock<1>) => ('a * 'ck clock<1>) on<1> ;;
operator Lustre.when_not_on : ('a * 'ck clock<1>) => ('a * 'ck clock<1>) not_on<1> ;;
operator Lustre.merge : ('a clock<1> * 
                  ('b * 'a clock<1>) on<1> * 
                  ('b * 'a clock<1>) not_on<1>) => 'b ;;


let as_bool ck = Lustre.as_bool ck ;;

let when_on (f,ck) =
  let x = if as_bool ck then f () else absent() in
  Lustre.when_on(x,ck) ;;

let whenot_on (f,ck) =
  let x = if as_bool ck then absent() else f () in
  Lustre.when_not_on(x,ck) ;;

let merge = Lustre.merge ;;

*)

(*

let main i = 
  let x = fby(true,false) in
  let c = abstype in
  let y = when_on ((fun () -> print_int 3; 3),c) in
  let z = whenot_on ((fun () -> print_int 4; 4),c) in 
  let u = merge(c,y,z) in print_int u ;;

let main v = 
  let x = fby(true,false) in
  let c = as_clock(x) in
  let i = fby(false,true) in
  let c2 = as_clock(i) in
  let y = when_on ((fun () -> print_int 3; 3),c) in
  let z = whenot_on ((fun () -> print_int 4; 4),c2) in 
  let u = merge(c,y,z) in print_int u ;;


let main i = 
  let x = fby(true,false) in
  let c = as_clock(x) in
  let c2 = as_clock(x) in
  let y = when_on (4,c) in
  let z = whenot_on (3,c2) in 
  let u = merge(c,y,z) in u ;;


let main i = 
  let x = fby(true,false) in
  let c = as_clock(x) in let y = Lustre.when (4,c) in let z = when_not (3,c) in let u = Lustre.merge(c,y,z) in u ;;

let main i = 
  let x = fby(true,false) in
  let c = Lustre.as_clock(x) in let y = Lustre.when_on (4,c) in let z = Lustre.when_not_on (3,c) in let u = Lustre.merge(c,y,z) in u ;;


let main i = 
  let x = fby(true,false) in
  let c = Lustre.as_clock (x) in let y = Lustre.when_on (4,c) in let z = Lustre.when_not_on (3,c) in let u = Lustre.merge(c,y,y) in u ;;



let fby2 (x, y) =
  (x,y) ;;

*)
  (* let main _ = (fby(1,2), fby(true,false));;








let foby(x,y) =
  let pre_sy = signal<> in
  let sy = signal<> in
  emit sy(y);
  let _ = exec emit pre_sy(x); %loop let z = ?sy in pause(); emit pre_sy(z) end default () in
  ?pre_sy ;;

let main (x,y) =
  let z = foby(x,y) in
  print_int z;print_string ";" ;;






let foby(x,y) =
  let pre_sy = signal<> in
  let sy = signal<> in
  emit sy(y);
  let _ = exec emit pre_sy(x); %loop let z = ?sy in pause(); emit pre_sy(z) end default () in
  ?pre_sy ;;

let main (x,y) =
  let z = foby(x,y) in
  print_int z;print_string ";" ;;



   *)*******)
 | E_sig_get _ -> 
      ()
  | E_emit(_,e1) ->
      f e1
  | E_sig_create(e1) ->
      f e1
  | E_loop(e1) ->
      f e1
  | E_trap _ -> ()
  | E_exit(_,e1) ->
      f e1
  | E_suspend(e1,_) ->
      f e1

let declare ds ts e =
  List.fold_right2 (fun (x,v) t e -> E_letIn(P_var x,t,v,e)) ds ts e

let declare' ds e =
  List.fold_right (fun (x,(t,v)) e -> E_letIn(P_var x,t,v,e)) ds e

let accum f (e:e) =   (* : ((x * ty * e) list * e)*)
  let rec aux e =
    let open Ast in
      let aux_list es =
        let rec loop dss es_acc es =
          match es with
          | [] -> List.concat (List.rev dss), List.rev es_acc
          | ei::es' ->
             let (dsi,ei') = aux ei in
             loop (dsi::dss) (ei'::es_acc) es'
      in loop [] [] es
    in
    match f aux e with
    | Some (ds,e') -> (ds,e')
    | None ->
        (match e with
        | E_deco(e1,deco) ->
            let ds1,e1' = aux e1 in
            ds1, E_deco(e1',deco)
        | E_const _ | E_var _ -> [],e
        | E_tuple es ->
            let ds,es' = aux_list es in
            ds,E_tuple(es')
        | E_app(e1,e2) ->
            let ds1,e1' = aux e1 in
            let ds2,e2' = aux e2 in
            ds1@ds2,E_app(e1',e2')
        | E_letIn(p,ty,e1,e2) ->
            let ds1,e1' = aux e1 in
            let ds2,e2' = aux e2 in
            ds1@ds2,E_letIn(p,ty,e1',e2')
        | E_fix(f,(p,ty,e1)) ->
            let ds1,e1' = aux e1 in
            let v = E_fix(f,(p,ty,e1')) in
            ds1,v
        | E_fun(p,ty,e1) ->
            let ds1,e1' = aux e1 in
            ds1,E_fun(p,ty,e1')
        | E_if(e1,e2,e3) ->
            let ds1,e1' = aux e1 in
            let ds2,e2' = aux e2 in
            let ds3,e3' = aux e3 in
            ds1@ds2@ds3,E_if(e1',e2',e3')
        | E_case(e1,hs,e_els) ->
          let ds1,e1' = aux e1 in
          let dss,hs' = List.split @@ List.map (fun (c,e) -> let ds,e' = aux e in ds,(c,e')) hs in
          let ds,e_els' = aux e_els in
          ds1@List.concat dss@ds, E_case(e1',hs',e_els')
        | E_match(e1,hs,eo) ->
          let ds1,e1' = aux e1 in
          let dss,hs' = List.split @@ List.map (fun (x,(p,e)) -> let ds,e' = aux e in ds,(x,(p,e'))) hs in
          let dsw,eo' = match eo with
                        | None -> [],eo
                        | Some ew -> let dsw,ew' = aux ew in
                                     (dsw,Some ew')
          in
          ds1@List.concat dss@dsw, E_match(e1',hs',eo')
        | E_ref(e1) ->
            let ds1,e1' = aux e1 in
            ds1,E_ref(e1')
        | E_get _ ->
            [], e
        | E_set(e1,e2) ->
            let ds1,e1' = aux e1 in
            let ds2,e2' = aux e2 in
            ds1@ds2,E_set(e1',e2')
        | E_array_make(sz,e1,loc) ->
            let ds1,e1' = aux e1 in
            ds1,E_array_make(sz,e1',loc)
        | E_array_create _ ->
            [],e
        | E_array_length _ ->
            [],e
        | E_array_get(x,e1) ->
            let ds1,e1' = aux e1 in
            ds1,E_array_get(x,e1')
        | E_array_set(x,e1,e2) ->
            let ds1,e1' = aux e1 in
            let ds2,e2' = aux e2 in
            ds1@ds2,E_array_set(x,e1',e2')
        | E_array_get_start(x,e1) ->
            let ds1,e1' = aux e1 in
            ds1,E_array_get_start(x,e1')
        | E_array_get_end _ -> [],e
        | E_array_set_immediate(x,e1,e2) ->
            let ds1,e1' = aux e1 in
            let ds2,e2' = aux e2 in
            ds1@ds2,E_array_set_immediate(x,e1',e2')
        | E_array_from_file(x,e1) ->
            let ds1,e1' = aux e1 in
            ds1,E_array_from_file(x,e1')
        | E_par(es) ->
            let ds,es' = aux_list es in
            ds,E_par(es')
        | E_reg((p,tyB,e1),e0,l) ->
            let ds1,e1' = aux e1 in
            let ds0,e0' = aux e0 in
            ds1@ds0,E_reg((p,tyB,e1'),e0',l)
        | E_exec(e1,e2,eo,l) ->
            let ds1,e1' = aux e1 in
            let ds2,e2' = aux e2 in
            let ds3,eo' = match eo with
                          | None -> [],None 
                          | Some e3 -> let ds,e3' = aux e3 in
                                       ds,Some e3' in
            ds2@ds1@ds3,E_exec(e1',e2',eo',l)
        | E_for(x,sz1,sz2,e3,loc) ->
            let ds3,e3' = aux e3 in
            ds3,E_for(x,sz1,sz2,e3,loc)
             (* NB: definitions in [e_st1] and [e_st2] and [e3]
                are *not* globalized *)
        | E_generate((p,ty,e1),e2,sz3,sz4,loc) ->
          let ds1,e1' = aux e1 in
          let ds2,e2' = aux e2 in
          ds1@ds2,E_generate((p,ty,e1'),e2',sz3,sz4,loc)
          (* NB: definitions in [e_st1] are *not* globalized *)
        | E_vector es ->
            let ds,es' = aux_list es in
            ds,E_vector(es')
        | E_vector_mapi(is_par,(p,typ,e1),e2,ty) ->
            let ds1,e1' = aux e1 in
            let ds2,e2' = aux e2 in
            ds1@ds2,E_vector_mapi(is_par,(p,typ,e1'),e2',ty)
        | E_run(x,e1,l) ->
            let ds1,e1' = aux e1 in
            ds1,E_run(x,e1',l)
        | E_pause (l,e1) -> 
            let ds1,e1' = aux e1 in
            [],E_pause (l,declare' ds1 e1')
        | E_sig_get _ -> 
            [],e
        | E_emit(x,e1) ->
            let ds1,e1' = aux e1 in
            ds1, E_emit(x,e1')
        | E_sig_create e1 -> 
            let ds1,e1' = aux e1 in
            ds1, E_sig_create(e1')
        | E_loop(e1) ->
            let ds1,e1' = aux e1 in
            [],E_loop(declare' ds1 e1')
        | E_trap _ -> [],e
        | E_exit(x,e1) ->
            let ds1,e1' = aux e1 in
            ds1,E_exit(x,e1')
        | E_suspend(e1,x) ->
            let ds1,e1' = aux e1 in
            [],E_suspend(declare' ds1 e1',x)
    ) 
  in 
  aux e
   

let map_pi f pi =
  Map_pi.map (map f) pi

