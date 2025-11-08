
type others = bool * bool ;;

let fst (a,_) = a;;

type state = (short * value * short * (value * char * short) * others) ;;

let caml_set_finish (_,led) =
  (true,led) ;;

let others_default = (false, false) ;;

type st = DECOD of unit | EXE of (opcode * value * short) * short ;;

let update_acc (next_acc,s) =
  let (pc, acc, sp, other_regs, others) = s in
  (pc, next_acc, sp, other_regs, others) ;;

let update_sp (next_sp,s) =
  let (pc, acc, sp, other_regs, others) = s in
  (pc, acc, next_sp, other_regs, others) ;;

(* ********************************************** *)


let interp_vm (s: state) : state =
  let (pc, acc, sp, other_regs, others) = s in
  let (env, extra_args, trap_sp) = other_regs in
  let zzz = reg (fun c -> c + 1) init 0 in
  let ((_,out),_) = reg (fun ((st, s),rdy_in) ->
     print_int zzz; print_string "---------["; print_int (if rdy_in then 1 else 0); print_string "]";
     let (r,rdy) = if not(rdy_in) then (st,true) else (print_string "DECOD";
          exec 
                      let x = code.(pc)
                      and (popped,sp_minus_1) = imm_pop_stack(sp) in
                      let pc_plus_1 = pc + 1 in
                       ((x,popped,sp_minus_1),pc_plus_1)
                    default st )
   in print_newline ();
     if not(rdy) then ((r, s), true) else (
    let ((x,popped,sp_minus_1),pc_plus_1) = r in
    print_string "---------[GO]!"; print_string "---------["; print_int (if rdy then 1 else 0); 
          print_string "pc:";   print_int pc;
          (* print_string "(opcode:";   print_int (code[pc]); print_string ")"; *)
          print_string "|acc:"; print_val acc;
          print_string "|sp:";  print_int sp;
          print_string "|env:";  print_val env;
          (*print_string "stack:"; print_stack(sp);*)
          print_newline ();
       (match x with
        | POP(n) ->
            let sp_minus_n = sp - n in
            ((r, (pc_plus_1, acc, sp_minus_n, other_regs, others)),true)
        | CONST(n) ->
            let next_acc = val_long n in
            ((r, (pc_plus_1, next_acc, sp, other_regs, others)),true)
        | UNOP(op) ->
            let a1 = long_val acc in
            let res =
              match op with
              | NOT() -> bnot a1
              | NEG() -> 0 - a1
              | ISINT() -> int_of_bool(is_int(acc))
            in
            ((r,(pc_plus_1, val_long(res), sp, other_regs, others)),true)
        | ADD(b,ofs) ->
            let a1 = long_val acc in
            print_string "~~~~|acc:"; print_val acc; 
             print_newline ();
            let a2 = if b then ofs else long_val popped in
            let res = a1 + a2 in
            let next_sp = if b then sp else sp_minus_1 in
            ((r,(pc_plus_1, val_long(res), next_sp, other_regs, others)),true)
        | STACK_MINUS_1(op,n) ->
            let sp_minus_n = sp - n in
            let x = match op with 
                    | ACC() -> ACC0(sp_minus_n)
                    | ASSIGN() -> ASSIGN0(sp_minus_n,sp,false) in
            ((((x,popped,sp_minus_1),pc_plus_1),s),false)
        | ACC0(sp_minus_n) ->  (exec let v = imm_stack_get(sp_minus_n) in
                                        (r,(pc_plus_1, v, sp, other_regs, others))
                                   default (st,s))
        | ASSIGN0(i,next_sp,b) ->  (exec imm_stack_set(i,acc);
            (* print_string "~~~~|acc:"; print_val acc; 
          *) print_string "::::"; print_val acc;
             print_newline ();
                                           (r,(pc_plus_1, if b then acc else val_unit, next_sp, other_regs, others))
                                     default (st,s))
        | PUSH() -> let x = ASSIGN0(sp,sp+1,true) in
                    ((((x,popped,sp_minus_1),pc_plus_1),s),false)
        | GETVECTITEM() ->
            let x = GET(GETFIELD(),as_short(long_val(popped))) in
            ((((x,popped,sp_minus_1),pc_plus_1),(pc_plus_1, acc, sp_minus_1, other_regs, others)), false)
        | GET(i,n) ->
            let v = match i with
                    | ENVACC() -> env
                    | GETFIELD() -> acc
                    | GETGLOBAL() -> val_ptr global_start in 
             (exec let v2 = imm_get_field(v,n) in
                      (r,(pc_plus_1, v2, sp, other_regs, others))
                 default (st,s))
      | SET(i,n) ->
             (exec
                   let (v,w) = 
                     match i with
                     | SETFIELD() -> (acc,popped)
                     | SETGLOBAL() -> (val_ptr global_start, acc)
                   in
                   imm_set_field(v,n,w);
                   (r,(pc_plus_1, val_unit, sp, other_regs, others))
                default (st,s))
        | GROUP_COMPARE(op,g) ->
               exec
                  let (a1,a2) = 
                      match g with
                      | COMPARE() -> (* 0 cycle *)
                          let a1 = long_val acc in
                          let v2 = popped in
                          let a2 = long_val(v2) in
                          (a1,a2)
                      | BCOMPARE(n,l) -> (* 0 cycle *)
                          let v = long_val acc in
                          (n,v)
                   in
                   let v1 = a1 < a2 in
                   let v2 = a1 = a2 in
                   pause();
                   let res = match op with
                             | LT() -> v1
                             | LE() -> v1 or v2
                             | EQ() -> v2
                             | GE() -> not(v1)
                             | GT() -> not(v1 or v2)
                             | NEQ() -> not(v2)
                             | _ -> false in
                    let next_s = 
                      match g with
                      | COMPARE() ->
                          let (next_acc,next_sp) = (val_long (int_of_bool res), sp_minus_1) in
                          (pc_plus_1,next_acc, next_sp, other_regs, others)
                      | BCOMPARE(n,l) ->
                          let l2 = if res then l else pc_plus_1 in
                          let (next_pc,others) = (l2,others) in
                          (next_pc, acc, sp, other_regs, others)
                    in (r,next_s)
               default (st,s)
          | GROUP3(i) ->
              let (next_pc,others) =
                match i with
                | BRANCH(l) -> (* 0 cycle *)
                    (l,others)
                | BRANCHIF(b,l) -> (* 0 cycle *) 
                    let l2 = if bxor(b, (is_long acc & long_val(acc) = 0)) then l else pc_plus_1 in
                    (l2,others)
                | STOP() -> (* 0 cycle *)
                    print_string "STOP : ";
                    (pc, others) (* caml_set_finish(others) *)
                | SWITCH(l1,l2) -> 
                    let ofs = as_short(long_val acc) in
                    let pc2 = (if (is_int acc) then l1 else l2) + ofs in
                    (pc2,others)
                | CHECK_SIGNALS() -> (pc_plus_1,others)
            in
            let next_s = (next_pc, acc, sp, other_regs, others) in
            ((r,next_s),true)
    | OFFSETCLOSURE(n) -> (* 0 cycle *)    
       (* print_block (env);*) print_string "===============%%>\n"; print_int n;
       let v = val_ptr (ptr_val(env) + n*2) in
       ((r,(pc_plus_1,v, sp, other_regs, others)),true)
    | GROUP4(i) ->
        let (a,rdy2) = (exec
         let next_s = 
           match i with
           | CALL(p,gt1,gt2,gt3,gt4) -> (* N cycles where N is the duration of p *)
                let (a1,sp) = if gt1 then (popped,sp_minus_1) else (val_unit,sp) in
                let (a2,sp) = if gt2 then pop_stack(sp) else (val_unit,sp) in
                let (a3,sp) = if gt3 then pop_stack(sp) else (val_unit,sp) in
                let (a4,sp) = if gt4 then pop_stack(sp) else (val_unit,sp) in
                let st = (pc,acc,sp, (env, extra_args, trap_sp), others) in
                let (res,st) = external_call (p,(acc,(a1,(a2,(a3,(a4))))),st) in
                let (pc,acc,sp, (_, extra_args, trap_sp), others) = st in (* must not modify env *)
                let acc = res in print_string "YOUPi!";
                (pc_plus_1,acc,sp, (env, extra_args, trap_sp), others)

           | POPTRAP() -> (* 1 cycle *)
               let (v,sp) = pop_stack(sp_minus_1) in
               let next_trap_sp = as_short(long_val(v)) in
               let sp = sp - 2 in
               (pc_plus_1,acc,sp,(env, extra_args, next_trap_sp), others)
           | RAISE() -> (* 4 cycle *)
               (* if trap_sp = (-1)   (* i.e., uncaught_exception *)
                  then (143,acc,sp, other_regs, others) else *)
               let sp = trap_sp in
               let (v0,sp) = pop_stack(sp) in
               let next_pc = as_short(long_val(v0)) in
               let (v1,sp) = pop_stack(sp) in
               let next_trap_sp = as_short(long_val(v1)) in
               let (v2,sp) = pop_stack(sp) in
               let next_env = v2 in
               let (v3,sp) = pop_stack(sp) in
               let next_extra_args = char_of_long(long_val(v3)) in
               (next_pc,acc,sp, (next_env, next_extra_args, next_trap_sp), others)
           | RESTART() -> (* (nbargs+1)+1 cycles *)
               let nbargs = char_of_short(size_val(env)) - 2 in
               print_string "=======GGGGGGG;"; print_int (size_val(env));
               let rec loop_push(sp,i) =
                 if pause (i >= nbargs) then sp else
                 let sp = push_stack(get_field(env,short_of_char(i+2)),sp) in
                 loop_push(sp,i+1)
               in
               let sp = loop_push(sp,0) in
               let next_env = get_field1(env) in
               let next_extra_args = extra_args + nbargs in
               (pc_plus_1,acc,sp, (next_env, next_extra_args, trap_sp), others)
           | MAKEBLOCK(atom,closure,sz,tag,l) ->
            (* ok ? *)
               let u = closure & sz > 0 in
               (* print_string "CLOSURE: "; print_int (if u then 1 else 0); *)
               let sp = pause(if u then push_stack(acc,sp) else sp) in
               let sz_block = if closure then sz+1 else sz in
               let (acc,env,blk) = make_block(sp,acc,env,tag,sz_block) in
               let v = if closure then val_int (as_long(l)) else acc in
               set_field0(blk,v);
               if atom
               then (pc_plus_1,blk,sp,(env, extra_args, trap_sp), others)
               else (let rec fill(i,sp) = 
                        if i >= (if closure then sz+1 else sz) then sp else
                          (let (v,sp) = pop_stack(sp) in
                           set_field(blk,i,v); fill(i+1,sp))
                     in
                     let sp = fill(1,sp) in
                     print_block(blk);
                     (pc_plus_1,blk,sp,(env, extra_args, trap_sp), others))

           | RETURN(n) -> (* 3 cycles if (extra_args = 0) else 1 cycle *)
               let sp = sp - n in
               print_int (sp);
               let u = pause (extra_args = 0) in 
               if u then (
                 let then_next_pc = as_short(long_val(stack_get(sp-1))) in
                 let next_env =                              stack_get(sp-2)   in
                 let next_extra_args = char_of_long(long_val(stack_get(sp-3))) in
                 (then_next_pc,acc,sp-3,(next_env, next_extra_args, trap_sp), others))
               else (
                 (* 1 cycle *) 
                 let next_env = acc in
                 let next_extra_args = extra_args - 1 in
                 let next_pc = as_short(long_val(get_field0(acc))) in
                 (next_pc, acc, sp,(next_env, next_extra_args, trap_sp), others))

           | APPTERM(n,s) -> 
                 let n8 = char_of_short(n) in
                 let next_sp = sp - s + n in
                 let rec w i =
                    if i = 0 then () else (
                      stack_set(next_sp-i, stack_get(sp-i)); w(i-1)
                    )
                 in w(n);
                 let next_pc = as_short(long_val(get_field0(acc))) in
                 let next_env = acc in
                 let next_extra_args = extra_args + n8 - 1 in
                 (next_pc,acc,next_sp,(next_env, next_extra_args, trap_sp), others)

           | APPLY(special,c,n) -> (* 1 cycle *)
                 let apply((i1,i2,i3),next_extra_args,appterm,n,ofs) =
                    let dont_care = val_unit in
                    let (arg1,sp) = if i1 then pop_stack(sp) else (dont_care,sp) in
                    let (arg2,sp) = if i2 then pop_stack(sp) else (dont_care,sp) in
                    let (arg3,sp) =  (if i3 then pop_stack(sp) else (dont_care,sp)) in

                    let sp = push_stack(val_long(long_of_char(extra_args)),sp) in
                    let sp = push_stack(env,sp) in
                    let sp = push_stack(val_long(as_long(pc_plus_1)),sp) in

                    let sp = if i3 then push_stack(arg3,sp) else sp in
                    let sp = if i2 then push_stack(arg2,sp) else sp in
                    let sp =  (if i1 then push_stack(arg1,sp) else sp) in
                    let next_pc = as_short(long_val (get_field(acc,0))) in
                    let next_env = acc in
                    (next_pc,acc,sp,(next_env, next_extra_args, trap_sp), others)
                  in
                  if special then apply (
                         match c with 
                         | 1 -> ((true,false,false),0,false,0,0)
                         | 2 -> ((true,true,false),1,false,0,0)
                         | _ (* 3 *) -> ((true,true,true),2,false,0,0) 
                       )
                  else
                    let next_extra_args = c - 1 in
                    let next_env = acc in
                    let next_pc_apply = as_short(long_val(get_field0(acc))) in
                    (next_pc_apply,acc,sp,(next_env, next_extra_args, trap_sp), others)

           | PUSHTRAP(l) ->
               let sp = push_stack(val_long(long_of_char(extra_args)),sp) in
               let sp = push_stack(env,sp) in
               let c = val_long(as_long(l)) in
               let sp = push_stack(val_long(as_long(trap_sp)),sp) in
               let sp = push_stack(c,sp) in
               let next_trap_sp = sp in
               (pc_plus_1, acc, sp,(env, extra_args, next_trap_sp), others)

           | GRAB(c) ->     
               let x = extra_args in
               let u = pause (x >= c) in
               if u
               then let next_extra_args = x - c in
                    (pc_plus_1,acc,sp, (env, next_extra_args, trap_sp), others)
               else (
                 let (_,env,next_acc) = make_block(sp,acc,env,closure_tag,short_of_char(x + 3)) in
                 (* print_string "%%%%%%%%%%%%%%%%%%%%%%%%%%%%=>"; print_int (pc - 1);*) (* minus 2 ? *)
                 set_field0(next_acc, val_long (as_long(pc - 1)));
                 set_field1(next_acc, env);
                 let rec loop(i,sp) =       (* todo *)
                   if i > x then sp else
                   let (v,sp) = pop_stack(sp) in
                   set_field(next_acc,short_of_char(i+2),v);
                   loop(i+1,sp)
                 in
                 let sp = loop(0,sp) in
                 let v = stack_get(sp-1) in
                 let next_pc = as_short(long_val v) in
                 let w = stack_get(sp-2) in
                 let next_env = w in
                 let u = stack_get(sp-3) in
                 let next_extra_args = char_of_long(long_val(u)) in
                 (next_pc,next_acc,sp-3, (next_env, next_extra_args, trap_sp), others))

           | CLOSUREREC(f,v,o,l) ->
                 let f = short_of_char(f) in
                 let sp = if v > 0 then push_stack(acc,sp) else sp in
                 let closure_size = (2 * f) - 1 + v in
                 let (_,env,next_acc) = make_block(sp,acc,env,closure_tag,closure_size) in
                 set_field(next_acc, 0, val_long (as_long(l)));
                 let rec w0(i,sp) =
                    if i = v (* >= *) then sp else
                    let (x,sp) = pop_stack(sp) in
                    (set_field(next_acc, i + 2 * f - 1, x); w0(i+1,sp))
                 in 
                 let sp = w0(0,sp) in
                 let rec w1(i) =
                   if i >= f then () else
                    (((* if i <> f-1 then *)
                   set_field(next_acc,2*i-1,val_long(make_header(infix_tag,closure_size-2*i)));
                   set_field(next_acc,2*i,val_long(as_long(o+i))); w1(i+1)))
                 in
                 w1(1);
                 (* print_block next_acc; *)
                 let sp = push_stack(next_acc,sp) in
                 let rec w3(i,sp) =
                    if i >= f then sp else
                    let sp = push_stack(val_long (as_long (ptr_val next_acc + (2 * i))),sp) in
                    w3(i+1,sp)
                 in
                 print_block(next_acc);
                 let sp = w3(1,sp) in
                 (pc_plus_1, next_acc, sp,(env, extra_args, trap_sp), others)
          | PUSHRETADDR(n) -> (* 3 cycles *)
              let sp = push_stack(val_long(long_of_char(extra_args)),sp) in
              let c = val_long (as_long n) in
              let sp = push_stack(env,sp) in
              let sp = push_stack(c,sp) in
              (pc_plus_1,acc, sp, other_regs, others)
         in 
         (r,next_s)
        default (st,s)
      )     in print_string "~~~~~["; print_int (if rdy2 then 44 else 45); print_string "]";(a,rdy2)   
    | LABEL(l) -> (* 0 cycle *)
        ((r,(l, acc, sp, other_regs, others)),true)
    | _ -> 
        ((r,s),true))))
    init ((((LABEL(0),val_unit,0),0),s),true) in out;;