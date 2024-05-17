
type others = bool * bool ;;


type state = (short * value * short * (value * char * short) * others) ;;

let caml_set_finish (_,led) =
  (true,led) ;;

let others_default = (false, false) ;;


(* ********************************************** *)

let interp (run) =
  let step (s : state) : state =
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
    let pc_plus_2 : short = pc + 2 in
    let sp_plus_1 : short = sp + 1 in
    let sp_minus_1 : short = sp - 1 in

    (*  ************* instructions ************* *)

    let acc_n ((next_pc,sp,n) : (short * short * short)) : state =
      let v = get_stack_ofs(sp,n) in
      (next_pc, v, sp, other_regs, others) in

    let push() : state =
      let sp = push_stack(acc,sp) in
      (pc_plus_1, acc, sp, other_regs, others) in

    let env_acc_n (next_pc,sp,n) =
      (next_pc,get_field(env,n-1),sp, other_regs, others) in

    let const_n (next_pc,sp,n) =
      (next_pc,val_long n,sp, other_regs, others) in

    let push_acc_n (next_pc,n) =
      let sp = push_stack(acc,sp) in
      acc_n(next_pc,sp,n) in

    let push_env_acc_n (next_pc,n) =
      let sp = push_stack(acc,sp) in
      env_acc_n(next_pc,sp,n) in

    let pushconst_n (next_pc,n) =
      let sp = push_stack(acc,sp) in
      const_n(next_pc,sp,n) in

    let rec apply((i1,i2,i3),next_extra_args,appterm,n,ofs) =
      print_string "ENV:"; print_val env; print_newline ();
      let dont_care = val_unit in
      let (arg1,sp) = if i1 then pop_stack(sp) else (dont_care,sp) in
      let (arg2,sp) = if i2 then pop_stack(sp) else (dont_care,sp) in
      let (arg3,sp) = if i3 then pop_stack(sp) else (dont_care,sp) in

      let sp = if appterm then
                 let sp = sp - n + ofs in sp
               else
                (* assume next pc is pc_plus_1 *)
                let sp = push_stack(val_long(long_of_char(extra_args)),sp) in
                let sp = push_stack(env,sp) in
                let sp = push_stack(val_long(as_long(pc_plus_1)),sp) in
                sp
      in

      let sp = if i3 then push_stack(arg3,sp) else sp in
      let sp = if i2 then push_stack(arg2,sp) else sp in
      let sp = if i1 then push_stack(arg1,sp) else sp in
      let next_pc = as_short(long_val (get_field(acc,0))) in
      let next_env = acc in
      (next_pc,acc,sp,(next_env, next_extra_args, trap_sp), others)
    in

    let rec offsetclosure_n (next_pc, sp, n) =
      let v = val_ptr (ptr_val(env) + n) in
      (next_pc, v, sp, other_regs, others) in

    let pushoffsetclosure_n (next_pc,n) =
      let sp = push_stack(acc,sp) in
      offsetclosure_n (next_pc, sp, n) in

    let get_field_n ((next_pc,n) : (short * short)) : state =
      assert (is_ptr(acc));
      let v = get_field(acc,n) in
      (next_pc,v,sp,other_regs, others) in

    let set_field_n ((next_pc,n) : (short * short)) : state =
      let (v,sp) = pop_stack(sp) in
      set_field(acc,n,v);
      (next_pc,val_unit, sp, other_regs, others) in

    let rec binop_int(op) =
      let a1 = long_val acc in
      let (v2,sp) = pop_stack(sp) in
      let a2 = long_val(v2) in
      let res = match op with
                | 0 -> addint(a1,a2)
                | 1 -> subint(a1,a2)
                | 2 -> mulint(a1,a2)
                | 3 -> modint(a1,a2)
                | 4 -> modint(a1,a2)
                | 5 -> andint(a1,a2)
                | 6 -> orint(a1,a2)
                | 7 -> xorint(a1,a2)
                | 8 -> lslint(a1,a2)
                | 9 -> lsrint(a1,a2)
                | 10 -> asrint(a1,a2)
                | 11 -> ultint(a1,a2)
                | 12 -> ugeint(a1,a2)
                | _ -> 0 end
      in
      let v = val_long res in
      (pc_plus_1,v,sp, other_regs, others) in

    let rec compare (op,a1,a2) : bool =
      let eq = a1 == a2 in
      let lt = a1 < a2 in
      match op with
      | 0 (* EQ *) -> eq
      | 1 (* NEQ *) -> not(eq)
      | 2 (* LTINT *) -> lt
      | 3 (* LEINT *) -> lt or eq
      | 4 (* GTINT *) -> not(lt or eq)
      | 5 (* GEINT *) -> not(lt)
      | _ -> false end
    in

    let rec binop_compare(op) =
      let a1 = long_val acc in
      let (v2,sp) = pop_stack(sp) in
      let a2 = long_val v2 in
      let res = compare(op,a1,a2) in
      let v = val_long (int_of_bool(res)) in
      (pc_plus_1,v,sp, other_regs, others) in

    let rec make_block_n (next_pc,sp,doSetAcc,i1,i2,tag,sz) =
         let (acc,env,blk) = make_block(sp,acc,env,char_of_long(tag),sz) in
         let () = if doSetAcc then set_field(blk,0,acc) else () in
         let sp = if i1 then (let (v1,sp) = pop_stack(sp) in
                              set_field(blk,1,v1);
                              sp) else sp in
         let sp = if i2 then (let (v2,sp) = pop_stack(sp) in
                              set_field(blk,2,v2);
                              sp) else sp in
         (next_pc,blk,sp,(env, extra_args, trap_sp), others) in

    let branch(pc,argument1) =
      let c = as_short(argument1) in
      (pc_plus_1 + c, acc, sp, other_regs, others) in

    let rec branch_if (doInv) =
      let b = (long_val(acc) <> 0) in
      let b = if doInv then not(b) else b in
      if b
      then let arg = code[pc_plus_1] in branch(pc_plus_1,arg)
      else (pc_plus_2, acc, sp, other_regs, others) 
    in

    (* ************************************)
    assert (pc < code.length);
    let x = resize_int<8> (code[pc]) in
    match x with
    | 0 (* ACC0 *) -> acc_n(pc_plus_1, sp, 0)
    | 1 (* ACC1 *) -> acc_n(pc_plus_1, sp, 1)
    | 2 (* ACC2 *) -> acc_n(pc_plus_1, sp, 2)
    | 3 (* ACC3 *) -> acc_n(pc_plus_1, sp, 3)
    | 4 (* ACC4 *) -> acc_n(pc_plus_1, sp, 4)
    | 5 (* ACC5 *) -> acc_n(pc_plus_1, sp, 5)
    | 6 (* ACC6 *) -> acc_n(pc_plus_1, sp, 6)
    | 7 (* ACC7 *) -> acc_n(pc_plus_1, sp, 7)
    | 9 (* PUSH *) -> push()
    | 10 (* PUSHACC0 *) -> push()
    | 11 (* PUSHACC1 *) -> push_acc_n(pc_plus_1, 1)
    | 12 (* PUSHACC2 *) -> push_acc_n(pc_plus_1, 2)
    | 13 (* PUSHACC3 *) -> push_acc_n(pc_plus_1, 3)
    | 14 (* PUSHACC4 *) -> push_acc_n(pc_plus_1, 4)
    | 15 (* PUSHACC5 *) -> push_acc_n(pc_plus_1, 5)
    | 16 (* PUSHACC6 *) -> push_acc_n(pc_plus_1, 6)
    | 17 (* PUSHACC7 *) -> push_acc_n(pc_plus_1, 7)
    | 21 (* ENVACC1 *) -> env_acc_n(pc_plus_1, sp, 1)
    | 22 (* ENVACC2 *) -> env_acc_n(pc_plus_1, sp, 2)
    | 23 (* ENVACC3 *) -> env_acc_n(pc_plus_1, sp, 3)
    | 24 (* ENVACC4 *) -> env_acc_n(pc_plus_1, sp, 4)
    | 26 (* PUSHENVACC1 *) -> push_env_acc_n(pc_plus_1, 1)
    | 27 (* PUSHENVACC2 *) -> push_env_acc_n(pc_plus_1, 2)
    | 28 (* PUSHENVACC3 *) -> push_env_acc_n(pc_plus_1, 3)
    | 29 (* PUSHENVACC4 *) -> push_env_acc_n(pc_plus_1, 4)
    | 33 (* APPLY1 *) -> apply((true,false,false),0,false,0,0)
    | 34 (* APPLY2 *) -> apply((true,true,false),1,false,0,0)
    | 35 (* APPLY3 *) -> apply((true,true,true),2,false,0,0)
    | 41 (* RESTART *) -> let nbargs = char_of_short(size_val(env)) - 2 in
                          let rec loop_push(sp,i) =
                            if i >= nbargs then sp else
                            let sp = push_stack(get_field(env,short_of_char(i+2)),sp) in
                            loop_push(sp,i+1)
                          in
                          let sp = loop_push(sp,0) in
                          let next_env = get_field(env,1) in
                          let next_extra_args = extra_args + nbargs in
                          (pc_plus_1,acc,sp, (next_env, next_extra_args, trap_sp), others)
    | 45 (* OFFSETCLOSUREM2 *) -> offsetclosure_n(pc_plus_1, sp, -2)
    | 46 (* OFFSETCLOSURE0 *) -> offsetclosure_n(pc_plus_1,sp,  0)
    | 47 (* OFFSETCLOSURE2 *) -> offsetclosure_n(pc_plus_1, sp, 2)
    | 49 (* PUSHOFFSETCLOSUREM2 *) -> pushoffsetclosure_n (pc_plus_1, -2)
    | 50 (* PUSHOFFSETCLOSURE0 *) -> pushoffsetclosure_n (pc_plus_1, 0)
    | 51 (* PUSHOFFSETCLOSURE2 *) -> pushoffsetclosure_n (pc_plus_1, 2)
    | 58 (* ATOM0 *) -> make_block_n(pc_plus_1,sp,false,false,false,0,0)
    | 60 (* PUSHATOM0 *) -> let sp = push_stack(acc,sp) in
                            make_block_n(pc_plus_1,sp,false,false,false,0,0)
    | 67 (* GETFIELD0 *) -> get_field_n(pc_plus_1, 0)
    | 68 (* GETFIELD1 *) -> get_field_n(pc_plus_1, 1)
    | 69 (* GETFIELD2 *) -> get_field_n(pc_plus_1, 2)
    | 70 (* GETFIELD3 *) -> get_field_n(pc_plus_1, 3)

    (* 72 GETFLOATFIELD *)

    | 73 (* SETFIELD0 *) -> set_field_n(pc_plus_1, 0)
    | 74 (* SETFIELD1 *) -> set_field_n(pc_plus_1, 1)
    | 75 (* SETFIELD2 *) -> set_field_n(pc_plus_1, 2)
    | 76 (* SETFIELD3 *) -> set_field_n(pc_plus_1, 3)

    | 79 (* VECTLENGTH *) -> let nex_acc = val_long(as_long(size_val acc)) in
                            (pc_plus_1,nex_acc,sp, other_regs, others)
    | 80 (* GETVECTITEM *) -> let (n,sp) = pop_stack(sp) in
                              let v = get_field(acc,as_short(long_val(n))) in
                              (pc_plus_1, v, sp, other_regs, others)
    | 81 (* SETVECTITEM *) -> let (n,sp) = pop_stack(sp) in
                              let (v,sp) = pop_stack(sp) in
                              set_field(acc,as_short(long_val(n)),v);
                              let next_acc = val_unit in
                              (pc_plus_1,next_acc,sp, other_regs, others)

    | 82 (* GETSTRINGCHAR *) -> (* currently, string are implemented as char array,
                                 since char cannot be easely packed in custom-sized `long` value *)
                                let (n,sp) = pop_stack(sp) in
                                let next_acc = get_field(acc,as_short(long_val(n))) in
                                (pc_plus_1,next_acc,sp, other_regs, others)
    | 83 (* SETSTRINGCHAR *) -> (* currently, string are implemented as char array,
                                 since char cannot be easely packed in custom-sized `long` value *)
                                let (n,sp) = pop_stack(sp) in
                                let (v,sp) = pop_stack(sp) in
                                set_field(acc,as_short(long_val(n)),v);
                                (pc_plus_1,val_unit,sp, other_regs, others)

    | 85 (* BRANCHIF *) -> branch_if(false)
    | 86 (* BRANCHIFNOT *) -> branch_if(true)
    | 88 (* BOOLNOT *) -> (pc_plus_1,val_long (bnot (long_val (acc))), sp, other_regs, others)
    | 90 (* POPTRAP *) -> let sp = sp_minus_1 in
                          let (v,sp) = pop_stack(sp) in
                          let next_trap_sp = as_short(long_val(v)) in
                          let sp = sp - 2 in
                          (pc_plus_1,acc,sp,(env, extra_args, next_trap_sp), others)
    | 91 (* RAISE *) -> (* if trap_sp = (-1)   (* i.e., uncaught_exception *)
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
    | 99  (* CONST0 *) -> const_n(pc_plus_1, sp, 0)
    | 100 (* CONST1 *) -> const_n(pc_plus_1, sp, 1)
    | 101 (* CONST2 *) -> const_n(pc_plus_1, sp, 2)
    | 102 (* CONST3 *) -> const_n(pc_plus_1, sp, 3)
    | 104 (* PUSHCONST0 *) -> pushconst_n(pc_plus_1, 0)
    | 105 (* PUSHCONST1 *) -> pushconst_n(pc_plus_1, 1)
    | 106 (* PUSHCONST2 *) -> pushconst_n(pc_plus_1, 2)
    | 107 (* PUSHCONST3 *) -> pushconst_n(pc_plus_1, 3)

    | 110 (* ADDINT *) -> binop_int(0)
    | 111 (* SUBINT *) -> binop_int(1)
    | 112 (* MULINT *) -> binop_int(2)
    | 113 (* DIVINT *) -> binop_int(3)
    | 114 (* MODINT *) -> binop_int(4)
    | 115 (* ANDINT *) -> binop_int(5)
    | 116 (* ORINT *) -> binop_int(6)
    | 117 (* XORINT *) -> binop_int(7)
    | 118 (* LSLINT *) -> binop_int(8)
    | 119 (* LSRINT *) -> binop_int(9)
    | 120 (* ASRINT *) -> binop_int(10)

    | 130 (* GETMETHOD *) -> fatal_error "unsupported instruction GETMETHOD"

    | 137 (* ULTINT *) -> binop_int(11)
    | 138 (* UGEINT *) -> binop_int(12)

    | 141 (* GETPUBMET *) -> fatal_error "unsupported instruction GETPUBMET"
    | 142 (* GETDYNMET *) -> fatal_error "unsupported instruction GETDYNMET"

    | 121 (* EQ *) -> binop_compare(0)
    | 122 (* NEQ *) -> binop_compare(1)
    | 123 (* LTINT *) -> binop_compare(2)
    | 124 (* LEINT *) -> binop_compare(3)
    | 125 (* GTINT *) -> binop_compare(4)
    | 126 (* GEINT *) -> binop_compare(5)
    | 129 (* ISINT *) -> (pc_plus_1, val_long(int_of_bool(is_int(acc))), sp, other_regs, others)
    | 143 (* STOP *) -> print_string "STOP : "; (pc, acc, sp, other_regs, caml_set_finish(others))
     | _ ->

      let pc_plus_3 : short = pc + 3 in
      let extra_args_gt_0 : bool = extra_args > 0 in

      let rec compbranch(op,n,ofs) =
        let v = long_val acc in
        let b = compare(op,n,v) in
        if b
        then (let c = pc_plus_2 + as_short(ofs) in
             (c,acc,sp, other_regs, others))
        else (pc_plus_3,acc,sp, other_regs, others) in

      (* ============================================================================= *)
      (let argument1 = code[pc_plus_1] in
      match x with
      | 8 (* ACC *) -> acc_n(pc_plus_1, sp, as_short(argument1))
      | 18 (* PUSHACC *) -> push_acc_n (pc_plus_2, as_short(argument1))
      | 19 (* POP *) -> (pc_plus_2, acc, sp - as_short(argument1), other_regs, others)
      | 20 (* ASSIGN *) -> ram_set(sp_minus_1 - as_short(argument1), acc);
                           (pc_plus_2,val_unit,sp, other_regs, others)
      | 25 (* ENVACC *) -> env_acc_n(pc_plus_2, sp, as_short(argument1))
      | 30 (* PUSHENVACC *) -> push_env_acc_n(pc_plus_2, as_short(argument1))
      | 31 (* PUSH-RETADDR *) -> let sp = push_stack(val_long(long_of_char(extra_args)),sp) in
                                 let c = val_long (as_long (pc_plus_1 + as_short(argument1))) in
                                 let sp = push_stack(env,sp) in
                                 let sp = push_stack(c,sp) in
                                 (pc_plus_2,acc,sp,other_regs, others)
      | 32 (* APPLY *) -> let next_pc = as_short(long_val(get_field(acc,0))) in
                          let next_env = acc in
                          let next_extra_args = char_of_long(argument1) - 1 in
                          (next_pc,acc,sp,(next_env, next_extra_args, trap_sp), others)
      | 37 (* APPTERM1 *) -> apply((true,false,false),extra_args,true,as_short(argument1),1)
      | 38 (* APPTERM2 *) -> apply((true,true,false),extra_args+1,true,as_short(argument1),2)
      | 39 (* APPTERM3 *) -> apply((true,true,true),extra_args+2,true,as_short(argument1),3)
      | 40 (* RETURN *) -> let sp = sp - as_short(argument1) in
                           if extra_args_gt_0 then (
                             let next_env = acc in
                             let next_extra_args = extra_args - 1 in
                             let next_pc = as_short(long_val (get_field(acc,0))) in
                             (next_pc, acc, sp,(next_env, next_extra_args, trap_sp), others))
                           else (
                             let (v0,sp) = pop_stack(sp) in
                             let next_pc = as_short(long_val v0) in
                             let (v1,sp) = pop_stack(sp) in
                             let next_env = v1 in
                             let (u,sp) = pop_stack(sp) in
                             let next_extra_args = char_of_long(long_val(u)) in
                             (next_pc,acc,sp,(next_env, next_extra_args, trap_sp), others)
                           )
     | 42 (* GRAB *) ->  let n = char_of_long(argument1) in
                         let x = extra_args in
                         if x >= n then ( let next_extra_args = x - n in
                                          (pc_plus_2,acc,sp, (env, next_extra_args, trap_sp), others) )
                         else (
                           let (_,env,next_acc) = make_block(sp,acc,env,closure_tag,short_of_char(x + 3)) in
                           set_field(next_acc, 0, val_long (as_long(pc_plus_2 - 3)));
                           set_field(next_acc, 1, env);
                           let rec w(i,sp) =
                             if i > x then sp else
                             let (v,sp) = pop_stack(sp) in
                             set_field(next_acc,short_of_char(i+2),v);
                             w(i+1,sp)
                           in
                           let sp = w(0,sp) in
                           let (v,sp) = pop_stack(sp) in
                           let next_pc = as_short(long_val v) in
                           let (w,sp) = pop_stack(sp) in
                           let next_env = w in
                           let (u,sp) = pop_stack(sp) in
                           let next_extra_args = char_of_long(long_val(u)) in
                           (next_pc,next_acc,sp, (next_env, next_extra_args, trap_sp), others))
      | 48 (* OFFSETCLOSURE *) -> offsetclosure_n(pc_plus_2, sp, as_short(argument1))
      | 52 (* PUSHOFFSETCLOSURE *) -> pushoffsetclosure_n (pc_plus_2, as_short(argument1))
      | 53 (* GETGLOBAL *) -> let v = global_get (as_short(argument1)) in
                             (pc_plus_2, v, sp, other_regs, others)
      | 54 (* PUSHGETGLOBAL *) ->  let sp = push_stack(acc,sp) in
                                    let v = global_get (as_short(argument1)) in
                                    (pc_plus_2, v, sp_plus_1, other_regs, others)
      | 57 (* SETGLOBAL *) ->  global_set(as_short(argument1),acc);
                                (pc_plus_2,val_unit,sp, other_regs, others)

      | 59 (* ATOM *) -> make_block_n(pc_plus_2,sp,false,false,false,argument1,0)
      | 61 (* PUSHATOM *) -> let sp = push_stack(acc,sp) in
                             make_block_n(pc_plus_2,sp,false,false,false,argument1,0)
      | 63 (* MAKEBLOCK1 *) -> make_block_n(pc_plus_2,sp,true,false,false,argument1,1)
      | 64 (* MAKEBLOCK2 *) -> make_block_n(pc_plus_2,sp,true,true,false,argument1,2)
      | 65 (* MAKEBLOCK3 *) -> make_block_n(pc_plus_2,sp,true,true,true,argument1,3)

      | 66 (* MAKEFLOATBLOCK *) -> fatal_error "unsupported instruction SETFLOATFIELD"

      | 71 (* GETFIELD *) -> get_field_n (pc_plus_2, as_short(argument1))
      | 77 (* SETFIELD *) -> set_field_n (pc_plus_2, as_short(argument1))

      | 78 (* SETFLOATFIELD *) -> fatal_error "unsupported instruction SETFLOATFIELD"

      | 87 (* SWITCH *) -> let n = argument1 in
                           let ofs =
                             if (is_int acc)
                             then as_short(long_val acc)
                             else let idx : short = tag_val acc in
                                  as_short(n) + idx
                           in
                           (pc_plus_2 + as_short(code[pc_plus_2+ofs]), acc, sp, other_regs, others)

      | 84 (* BRANCH *) -> branch(pc,argument1)
      | 89 (* PUSHTRAP *) -> let ofs = argument1 in
                             let sp = push_stack(val_long(long_of_char(extra_args)),sp) in
                             let sp = push_stack(env,sp) in
                             let c = val_long(as_long(pc_plus_1+as_short(ofs))) in
                             let sp = push_stack(val_long(as_long(trap_sp)),sp) in
                             let sp = push_stack(c,sp) in
                             let next_trap_sp = sp in
                             (pc_plus_2, acc, sp,(env, extra_args, next_trap_sp), others)

      | 92 (* CHECK-SIGNALS : unsupported *) ->
        (pc_plus_1,acc,sp, (env, extra_args, trap_sp), others)

      | 93 (* C-CALL1 *) -> let p = argument1 in
                            let sp = push_stack(env,sp) in
                            let st = (pc,acc,sp, (env, extra_args, trap_sp), others) in
                            let (res,st) = external_call (p,caml_prepare_args1(acc),st) in
                            let (pc,acc,sp, (env, extra_args, trap_sp), others) = st in
                            let acc = res in
                            let (v,sp) = pop_stack(sp) in
                            let next_env = v in
                            (pc_plus_2,acc,sp, (next_env, extra_args, trap_sp), others)
      | 94 (* C-CALL2 *) -> let p = argument1 in
                            let (a,sp) = pop_stack(sp) in
                            let sp = push_stack(env,sp) in
                            let st = (pc,acc,sp, (env, extra_args, trap_sp), others) in
                            let (res,st) = external_call (p,caml_prepare_args2(acc,a),st) in
                            let (pc,acc,sp, (env, extra_args, trap_sp), others) = st in
                            let acc = res in
                            let (v,sp) = pop_stack(sp) in
                            let next_env = v in
                            (pc_plus_2,acc,sp, (next_env, extra_args, trap_sp), others)
      | 95 (* C-CALL3 *) -> let p = argument1 in
                            let (a1,sp) = pop_stack(sp) in
                            let (a2,sp) = pop_stack(sp) in
                            let sp = push_stack(env,sp) in
                            let st = (pc,acc,sp, (env, extra_args, trap_sp), others) in
                            let (res,st) = external_call (p,caml_prepare_args3(acc,a1,a2),st) in
                            let (pc,acc,sp, (env, extra_args, trap_sp), others) = st in
                            let acc = res in
                            let (v,sp) = pop_stack(sp) in
                            let next_env = v in
                            (pc_plus_2,acc,sp, (next_env, extra_args, trap_sp), others)
      | 96 (* C-CALL4 *) -> let p = argument1 in
                            let (a1,sp) = pop_stack(sp) in
                            let (a2,sp) = pop_stack(sp) in
                            let (a3,sp) = pop_stack(sp) in
                            let sp = push_stack(env,sp) in
                            let st = (pc,acc,sp, (env, extra_args, trap_sp), others) in
                            let (res,st) = external_call (p,caml_prepare_args4(acc,a1,a2,a3),st) in
                            let (pc,acc,sp, (env, extra_args, trap_sp), others) = st in
                            let acc = res in
                            let (v,sp) = pop_stack(sp) in
                            let next_env = v in
                            (pc_plus_2,acc,sp, (next_env, extra_args, trap_sp), others)
      | 97 (* C-CALL5 *) -> let p = argument1 in
                            let (a1,sp) = pop_stack(sp) in
                            let (a2,sp) = pop_stack(sp) in
                            let (a3,sp) = pop_stack(sp) in
                            let (a4,sp) = pop_stack(sp) in
                            let sp = push_stack(env,sp) in
                            let st = (pc,acc,sp, (env, extra_args, trap_sp), others) in
                            let (res,st) = external_call (p,caml_prepare_args5(acc,a1,a2,a3,a4),st) in
                            let (pc,acc,sp, (env, extra_args, trap_sp), others) = st in
                            let acc = res in
                            let (v,sp) = pop_stack(sp) in
                            let next_env = v in
                            (pc_plus_2,acc,sp, (next_env, extra_args, trap_sp), others)

      | 98 (*  C-CALLN, unsupported *) -> fatal_error "unsupported instruction CALLN"

      | 103 (* CONSTINT *) -> const_n(pc_plus_2, sp, argument1)
      | 108 (* PUSHCONSTINT *) -> pushconst_n(pc_plus_2, argument1)


      | 127 (* OFFSETINT *) -> let ofs = argument1 in
                               let v = val_long(addint(long_val acc, ofs)) in
                               (pc_plus_2, v, sp, other_regs, others)
      | 128 (* OFFSETREF *) -> let ofs = argument1 in
                               let f0 = get_field(acc,0) in
                               let v = val_long((long_val f0) + ofs) in
                               set_field(acc,0,v);
                               (pc_plus_2,val_unit,sp, other_regs, others)
      | 139 (* BULTINT *) -> compbranch(2,argument1,long_val(acc))
      | 140 (* BUGEINT *) -> compbranch(5,argument1,long_val(acc))
      | _ ->
        (* ============================================================================= *)
        (let argument2 = code[pc_plus_2] in
         match x with
        | 36 (* APPTERM *) -> let n = argument1 in
                              let s = argument2 in (* not yet checked *)
                              let n8 = char_of_long(n) in
                              let n = as_short(n) in
                              let s = as_short(s) in
                              let next_sp = sp - n - s in
                              let rec w i =
                                if i > n then () else (
                                  ram_set(next_sp-i, ram_get(sp-i)); w(i+1)
                                )
                              in w(1);
                              let next_pc = as_short(long_val(get_field(acc,0))) in
                              let next_env = acc in
                              let next_extra_args = extra_args + n8 - 1 in
                              (next_pc,acc,sp,(next_env, next_extra_args, trap_sp), others)
        | 43 (* CLOSURE *) -> let n = argument1 in
                              let ofs = argument2 in
                              let n = as_short(n) in
                              let sp = if n > 0 then push_stack(acc,sp) else sp in
                              let (_,next_env,next_acc) = make_closure(sp,acc,env,pc_plus_2+as_short(ofs),n+1) in
                              let rec fill(i,sp) =
                                if i > n then sp else
                                (let (v,sp) = pop_stack(sp) in
                                 set_field(next_acc,i,v); fill(i+1,sp))
                              in
                              let sp = fill(1,sp) in
                              (pc_plus_3, next_acc, sp,  (next_env, extra_args, trap_sp), others)
        | 55 (* GETGLOBALFIELD *) -> let n = argument1 in
                                     let p = argument2 in
                                     let v = get_field(global_get (as_short(n)),as_short(p)) in
                                     (pc_plus_3,v,sp, other_regs, others)
        | 56 (* PUSHGETGLOBALFIELD *) -> let n = argument1 in
                                         let p = argument2 in
                                         let sp = push_stack(acc,sp) in
                                         let v = get_field(global_get (as_short(n)),as_short(p)) in
                                         (pc_plus_3,v,sp_plus_1, other_regs, others)

        | 62 (* MAKEBLOCK *) -> let sz = as_short(argument1) in
                                let tag = char_of_long(argument2) in
                                let (acc,env,blk) = make_block(sp,acc,env,tag,sz) in
                                set_field(blk,0,acc);
                                let rec fill(i,sp) =
                                   if i >= sz then sp else
                                   let (v,sp) = pop_stack(sp) in
                                   (set_field(blk,i,v); fill(i+1,sp))
                                in
                                let sp = fill(1,sp) in
                                (pc_plus_3,blk,sp,(env, extra_args, trap_sp), others)
        | 131 (* BEQ *) -> compbranch(0,argument1,argument2)
        | 132 (* BNEQ *) -> compbranch(1,argument1,argument2)
        | 133 (* BLTINT *) -> compbranch(2,argument1,argument2)
        | 134 (* BLEINT *) -> compbranch(3,argument1,argument2)
        | 135 (* BGTINT *) -> compbranch(4,argument1,argument2)
        | 136 (* BGEINT *) -> compbranch(5,argument1,argument2)
        | _ ->
          (* ============================================================================= *)
          (let argument3 = code[pc_plus_3] in
           match x with
           | 44 (* CLOSUREREC *) ->  let f = as_short(argument1) in
                                     let v = as_short(argument2) in
                                     let o = as_short(argument3) in
                                     let sp = if v > 0 then push_stack(acc,sp) else sp in
                                     let closure_size = (2 * f) - 1 + v in
                                     let (_,env,next_acc) = make_block(sp,acc,env,closure_tag,closure_size) in
                                     set_field(next_acc, 0, val_long (as_long(pc_plus_3+o)));
                                     let rec w0(i,sp) =
                                       if i >= v then sp else
                                       let (x,sp) = pop_stack(sp) in
                                       (set_field(next_acc, i + 2 * f - 1, x); w0(i+1,sp))
                                     in
                                     let sp = w0(0,sp) in
                                     let rec w1(i) =
                                       if i >= f then () else
                                       (set_field(next_acc,2*i-1,val_long(make_header(infix_tag,2*i)));
                                        set_field(next_acc,2*i,val_long(as_long(pc_plus_2+as_short(code[pc_plus_3 + i]))));
                                        w1(i+1))
                                     in
                                     w1(1);
                                     let sp = push_stack(next_acc,sp) in
                                     let rec w3(i,sp) =
                                       if i >= f then sp else
                                       let sp = push_stack(val_long (as_long (ptr_val next_acc + (2 * i))),sp) in
                                       w3(i+1,sp)
                                     in
                                     let sp = w3(1,sp) in
                                     (pc_plus_3+f, next_acc, sp,(env, extra_args, trap_sp), others)

          | _ -> print_string "unknown opcode : ";
                 print_int (code[pc]);
                 print_newline ();
                 s
          end)
        end)
      end)
    end
  in
  let exec_step (s,_) =
    if not(run) then (s,true)
    else (let (s2,rdy) = exec
                           step s
                         default s
          in (s2,rdy))
  in
  reg exec_step last ((0,val_unit,stack_start,(val_unit, 0, 0),others_default),true) ;;

let config1 () =
  init_data ();
  load_code () ;;


let print_cy = true ;;


(** [await(i,rst)] sustains value true as soon as
    input [i] is true until [rst] is false *)
let await (i,rst) : bool =
  let step(s) = (s or i) & not rst in
  reg step last false ;;

let load_bytecode (rst) =
  let ((),rdy) = exec config1 () default () in
  await(rdy,rst) ;;

let ocaml_vm (button) =
  let step (_,_,init_done,led) =
    if not(init_done)
    then
      let rdy = load_bytecode(false) in
      (false,true,rdy,led)
    else
      let (s,rdy) = interp(init_done) in
      let (pc,acc,sp,(env, extra_args, trap_sp),(finished,led)) = s in
      (finished,rdy,init_done,led)
  in
  let (stop,rdy,_,o) = reg step last (false,false,false,false) in
  (stop,not(rdy),o)
;;
