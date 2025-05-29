
type others = bool * bool ;;


type state = (short * value * short * (value * char * short) * others) ;;

let caml_set_finish (_,led) =
  (true,led) ;;

let others_default = (false, false) ;;


(* ********************************************** *)


let step_vm (s : state) : state =
  let (pc, acc, sp, other_regs, others) = s in
  let (env, extra_args, trap_sp) = other_regs in
  print_string "pc:";   print_int pc;
  (* print_string "(opcode:";   print_int (code[pc]); print_string ")"; *)
  print_string "|acc:"; print_val acc;
  print_string "|sp:";  print_int sp;
  print_string "|env:";  print_val env;
  (*print_string "stack:"; print_stack(sp);*)
  print_newline ();

  (*  ************* precomputed values  ************* *)

  let pc_plus_1 : short = pc + 1 in

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


  (* ************* DECOD: 1 cycle **************** *)
  (* assert (pc < code.length);*)
  let x = code.(pc) 
  and (popped,sp_minus_1) = pop_stack(sp) in
  (* ************* EXEC ************************** *)
  match x with
  | GROUP1(i,n) ->
      let sp_minus_n = sp - n in
      let sp_minus_n_minus_1 = sp_minus_n - 1 in
      let (next_acc,next_sp) =
        match i with
        | ACC() -> (* 1 cycle *)
            let v = stack_get(sp_minus_n_minus_1) in
            (v,sp)
        | PUSH() -> (* 1 cycle *)
            let sp = push_stack(acc,sp) in
            (acc, sp)
        | POP() -> (* 0 cycle *)
            (acc, sp_minus_n)
        | ASSIGN() -> (* 1 cycle *)
              stack_set(sp_minus_n_minus_1, acc);
              (val_unit,sp)
        | ENVACC() -> (* 1 cycle *)
            let v = get_field(env,n) in
            (v, sp)
        | CONST(n) -> (* 0 cycle *) 
            (val_long n, sp)
        | OFFSET(ofs) -> (* 0 cycle *) 
            let res = addint(long_val acc, ofs) in 
            (val_long(res), sp)
        | OFFSETREF(ofs) -> 
            let f0 = get_field0(acc) in (* 1 cycle *) 
            let v = val_long((long_val f0) + ofs) in
            set_field0(acc,v);
            (val_unit,sp)
        | GETFIELD() -> (* 1 cycle *) 
            assert (is_ptr(acc));
            let v = get_field(acc,n) in
            (v,sp)
        (*| GETGLOBALFIELD(p) -> (* 2 cycle *) 
            let u = global_get(n) in 
            let v = get_field(u,p) in
            (v,sp)*)
        | OFFSETCLOSURE() -> (* 0 cycle *)    
             print_block (env); print_string "===============%%>\n"; print_int n;
            let v = val_ptr (ptr_val(env) + n*2) in
            (v,sp)
        | PUSHRETADDR() -> (* 3 cycles *)
            let sp = push_stack(val_long(long_of_char(extra_args)),sp) in
            let c = val_long (as_long n) in
            let sp = push_stack(env,sp) in
            let sp = push_stack(c,sp) in
            (acc,sp)
        | GETGLOBAL() -> (* 1 cycles *)
            let v = global_get n in
            (v, sp)
        | SETGLOBAL(v) -> (* 1 cycles *)
            global_set(n,acc);
            (val_unit, sp)
         | UNOP(op) ->
            let res = match op with 
                      | NOT() ->  bnot (long_val (acc))
                      | NEG() ->  - (long_val (acc))
                      (*| OFFSET(ofs) -> addint(long_val acc, ofs)*)
                      | VECTLENGTH() -> as_long(size_val acc)
                      | ISINT() -> int_of_bool(is_int(acc))
                      | _ -> 0 
                       in
            (val_long(res), sp)

        | _ -> print_string "unknown opcode : ";
           (* print_int (code[pc]);*)
           print_newline ();
           (acc,sp)
      
    in
    (pc_plus_1,next_acc, next_sp, other_regs, others)
  | GROUP2(i) ->
      let binop(op) = 
        let a1 = long_val acc in
        let a2 = long_val(popped) in
        let res = op(a1,a2) in
        let v = val_long res in
        (v, sp_minus_1)
      in
      let (next_acc,next_sp)= 
       (match i with
        | ADD() -> (* 0 cycle *)
            binop(addint)
        | SUB() -> (* 0 cycle *)
            binop(subint)
        | MUL() ->
            binop(mulint)
        | DIV() ->
            binop(divint)
        | MOD() ->
            binop(modint)
        | OR() -> (* 0 cycle *)
            binop(orint)
        | AND() -> (* 0 cycle *)
            binop(andint)
        | LSL() -> 
            binop(lslint)
        | ASR() -> 
            binop(lsrint)
          (*| MOD() -> binop(modint)
         | DIV() -> binop(modint)*)
        | GETVECTITEM() -> (* 1 cycle *) 
            let (n,sp) = (popped,sp_minus_1) in
            let v = get_field(acc,as_short(long_val(n))) in
            (v, sp)
        | SETVECTITEM() -> (* 2 cycles *) 
            let (n,sp) = (popped,sp_minus_1) in
            let (v,sp) = pop_stack(sp) in
            set_field(acc,as_short(long_val(n)),v);
            let next_acc = val_unit in
            (next_acc,sp)

        | COMPARE(op) -> (* 0 cycle *)
            let a1 = long_val acc in
            let v2 = popped in
            let a2 = long_val(v2) in
            let v1 = a1 < a2 in
            let v2 = a1 = a2 in
            let res = match op with
                      | LT() -> v1
                      | LE() -> v1 or v2
                      | EQ() -> v2
                      | GE() -> not(v1)
                      | GT() -> not(v1 or v2)
                      | NEQ() -> not(v2)
                      | _ -> false
                       in
            let v = val_long (int_of_bool res) in
            (v,sp_minus_1)

        | SETFIELD(n) -> (* 1 cycle *)    
            set_field(acc,n,popped);
            (val_unit, sp_minus_1)

        | _ -> 
            print_string "unknown opcode : ";
            (* print_int (code[pc]);*)
            print_newline ();
            (acc,sp)
    
        ) 
      in
      (pc_plus_1,next_acc, next_sp, other_regs, others)

  | GROUP3(i) ->  
      let (next_pc,others) = 
       (match i with
        | BRANCH(l) -> (* 0 cycle *)
            (l,others)
        | BRANCHIF(b,l) -> (* 0 cycle *) 
            let l2 = if bxor(b, (is_long acc & long_val(acc) = 0)) then l else pc_plus_1 in
            (l2,others)
  
        | BCOMPARE(op,n,l) -> (* 0 cycle *)
            let v = long_val acc in
            let v1 = n < v in
            let v2 = n = v in
            let res = match op with
                      | LT() -> v1
                      | LE() -> v1 or v2
                      | EQ() -> v2
                      | GE() -> not(v1)
                      | GT() -> not(v1 or v2)
                      | NEQ() -> not(v2) in
            let l2 = if res then l else pc_plus_1 in
            (l2,others)
        | STOP() -> (* 0 cycle *)
            print_string "STOP : "; 
            (pc, caml_set_finish(others))
        | SWITCH(l1,l2) -> 
            let ofs = as_short(long_val acc) in
            let pc2 = (if (is_int acc) then l1 else l2) + ofs in
            (pc2,others)
        | CHECK_SIGNALS() -> (pc_plus_1,others)
        )
    in
    (next_pc, acc, sp, other_regs, others)   

         | LABEL(l) -> (* 0 cycle *)
            (l, acc, sp, other_regs, others)   


    | CALL(p,gt1,gt2,gt3,gt4) -> (* N cycles where N is the duration of p *)
        let (a1,sp) = if gt1 then (popped,sp_minus_1) else (val_unit,sp) in
        let (a2,sp) = if gt2 then pop_stack(sp) else (val_unit,sp) in
        let (a3,sp) = if gt3 then pop_stack(sp) else (val_unit,sp) in
        let (a4,sp) = if gt4 then pop_stack(sp) else (val_unit,sp) in
        let st = (pc,acc,sp, (env, extra_args, trap_sp), others) in
        let (res,st) = external_call (p,(acc,(a1,(a2,(a3,(a4))))),st) in
        let (pc,acc,sp, (_, extra_args, trap_sp), others) = st in (* must not modify env *)
        let acc = res in
        (pc_plus_1,acc,sp, (env, extra_args, trap_sp), others)

    | GROUP4(i) -> 
       (match i with
        | POPTRAP() -> (* 1 cycle *)
            let sp = sp - 1 in
            let (v,sp) = pop_stack(sp) in
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
      )

        | MAKEBLOCK(atom,closure,sz,tag,l) ->
                  (*let (acc,env,blk) = make_block(sp,acc,env,tag,sz) in
                  (* if atom then (pc_plus_1,blk,sp,(env, extra_args, trap_sp), others) else *)(
                  set_field0(blk,acc);
                  let rec fill(i,sp) =
                     if i = 0 then sp else
                     let (v,sp) = pop_stack(sp) in
                     (set_field(blk,i,v); fill(i-1,sp))
                  in
                  let sp = fill(sz,sp) in
                  (pc_plus_1,blk,sp,(env, extra_args, trap_sp), others))*)
            let u = closure & sz > 0 in
            (* print_string "CLOSURE: "; print_int (if u then 1 else 0); *)
            let sp = pause(if u then push_stack(acc,sp) else sp) in
            let sz_block = if closure then sz+1 else sz in
            let (acc,env,blk) = make_block(sp,acc,env,tag,sz_block) in
            let v = if closure then val_int (as_long(l)) else acc in
            set_field0(blk,v);
            if atom then (pc_plus_1,blk,sp,(env, extra_args, trap_sp), others) else (
                            let rec fill(i,sp) = 
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
         (then_next_pc,acc,sp-3,(next_env, next_extra_args, trap_sp), others)
         )
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
      if special then apply ( (
             match c with 
             | 1 -> ((true,false,false),0,false,0,0)
             | 2 -> ((true,true,false),1,false,0,0)
             | _ (* 3 *) -> ((true,true,true),2,false,0,0) 
           ))
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
    if u then let next_extra_args = x - c in
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


| _ -> s  ;;
