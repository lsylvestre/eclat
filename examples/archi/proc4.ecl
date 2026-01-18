(* 
  
    $ ./eclat ../proc/alu.ecl ../proc/proc1.ecl
    $ make simul NS=100

*)

type op = Alu of aluop 
        | Div of unit
        | Beq of int ;;


(** naive, multi-cycle division **)
let div(a,b) = 
  let rec div_aux (a,b,q) = 
    if a < b then q else div_aux ((a-b), b, (q+1))
  in div_aux (a, b, 0) ;;


type rs2_type = Rs2Imm of unit      (* operand 2 is an immediate value *)
              | Rs2Reg of unit ;;   (* operand 2 is retreived from register file *)

let code = [ ((Alu(Add()),Rs2Imm()),4,0,200),
             ((Alu(Sub()),Rs2Imm()),5,0,10),
             ((Alu(Add()),Rs2Imm()),6,1,55),
             ((Alu(Add()),Rs2Imm()),7,0,66),
             ((Div(),Rs2Imm()),13,4,8),
             ((Alu(Add()),Rs2Imm()),8,4,1),
             ((Beq(-4),Rs2Reg()),0,12,12),
             ((Alu(Add()),Rs2Imm()),9,10,10),
             ((Alu(Lsl()),Rs2Imm()),10,4,2),
             ((Alu(Add()),Rs2Imm()),8,4,1) ] ;;

(* ============== Write Back ============== *)
let write_back(mem_wb, reg_file) =
  let next_reg_file =
    let (valid, mem_wb_rd, mem_wb_res) = mem_wb in
    if valid 
    then vect_update (reg_file, mem_wb_rd, mem_wb_res)
    else reg_file
  in
  let rdy_wb = true in
  (next_reg_file, rdy_wb) ;;

(* ============== Mem ============== *)
let mem(rdy_wb, ex_mem, mem_wb) =
  let (valid, ex_mem_rd, ex_mem_res) = ex_mem in
  if rdy_wb & not(valid) then
    let mem_wb_rd = ex_mem_rd in
    let mem_wb_res = ex_mem_res in
    (true, mem_wb_rd, mem_wb_res)
  else mem_wb ;;

(* ============== Exec ============== *)
let execute(de_ex, pc, ex_mem) =
  let (valid_de, de_ex_op, de_ex_rd, de_ex_a, de_ex_b) = de_ex in
  if valid_de then
    let next_ex_mem_rd = de_ex_rd in
    let ((next_ex_mem_res, pc, br_taken), rdy) : (int<32> * int<32> * bool) * bool = 
      exec
        match de_ex_op with
        | Alu cmd -> (alu(cmd, de_ex_a, de_ex_b), pc, false)
        | Div() -> (div (de_ex_a, de_ex_b), pc, false)
        | Beq(l) -> if de_ex_a = de_ex_b then (0, pc + l, true) else (0,pc,false)
      default (0,0,false)
    in
    if rdy then print_string "---> finished!";
    if br_taken then (print_string "---> branch_to:"; print_int pc; print_newline ());
    print_string "ALU_OUT:"; print_int next_ex_mem_res; print_newline ();
    let valid = rdy in
    ((valid, next_ex_mem_rd, next_ex_mem_res), pc, br_taken)
  else (ex_mem, pc, false) ;;

(* ============== Decod ============== *)
let decod((if_de, reg_file), de_ex) =
  let (valid, if_de_instr) = if_de in
  if valid then
    let ((op, rs2_type), rd, rs1, rs2) = if_de_instr in
    let next_de_ex_op = op in
    let next_de_ex_rd = rd in
    let next_de_ex_a = vect_nth(reg_file,rs1) in
    let next_de_ex_b = match rs2_type with
                       | Rs2Imm() -> rs2
                       | Rs2Reg() -> vect_nth(reg_file,rs2) in
    (true, next_de_ex_op, next_de_ex_rd, next_de_ex_a, next_de_ex_b)
  else de_ex ;;

(* ============== Instruction Fetch ============== *)
let ifetch(next_br_taken, next_pc, (code,pc)) =
  let next_if_de_instr = vect_nth(code,pc) in
  let valid = true in
  ((valid, next_if_de_instr), (if next_br_taken then next_pc else pc + 1)) ;;


let config0 = (vect_create<32>(0), (true,0,0), (true,0,0),
               (true,Alu(Add()),0,0,0), (true,((Alu(Add()),Rs2Imm()),0,0,0)), 0,false) ;;

let vidage(reg_file, pc) = 
  (reg_file, (true,0,0), (true,0,0), (true,Alu(Add()),0,0,0), (true,((Alu(Add()),Rs2Imm()),0,0,0)), pc, false) ;;

(* ================================ *
 * minimalistic 5-stage pipeline,   *
 * with multi-cycle exec unit,      *
 * computes outputs and next state  *
 * ================================ *)
let cpu_step (reg_file, mem_wb, ex_mem, de_ex, if_de, pc, br_taken) =
     if br_taken then vidage(reg_file, pc) else
     let (next_reg_file,rdy_wb)  = write_back (mem_wb, reg_file) in
     let (next_mem_wb, rdy_ex)   = mem        (rdy_wb, ex_mem, mem_wb) in
     let (next_ex_mem, next_pc,
          next_br_taken)         = execute    (de_ex, pc, ex_mem) in
     let next_de_ex              = decod      ((if_de, reg_file), de_ex)  in
     let (next_if_de, next_pc)   = ifetch     ((next_br_taken, next_pc, (code,pc))) in
     (next_reg_file, next_mem_wb, next_ex_mem, next_de_ex, next_if_de, next_pc, next_br_taken)
;;





let main () =
  reg cpu_step init config0 ;;

