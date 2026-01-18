(* 
  
    $ ./eclat ../examples/archi/alu.ecl ../examples/archi/proc1.ecl
    $ make simul NS=100

*)

type rs2_type = Rs2Imm of unit      (* operand 2 is an immediate value *)
              | Rs2Reg of unit ;;   (* operand 2 is retreived from register file *)

let code = [ ((Add(),Rs2Imm()),4,0,42),
             ((Sub(),Rs2Imm()),5,0,43),
             ((Add(),Rs2Imm()),6,1,55),
             ((Add(),Rs2Imm()),7,0,66),
             ((Add(),Rs2Reg()),8,4,4),
             ((Add(),Rs2Imm()),9,10,10),
             ((Lsl(),Rs2Imm()),10,4,2),
             ((Add(),Rs2Imm()),8,4,1),
             ((Add(),Rs2Imm()),8,4,1) ] ;;

(* ============== Write Back ============== *)
let write_back(mem_wb, reg_file) =
  let (mem_wb_rd, mem_wb_res) = mem_wb in
  vect_update(reg_file, mem_wb_rd, mem_wb_res) ;;

(* ============== Mem ============== *)
let mem(ex_mem) =
  let (ex_mem_rd, ex_mem_res) = ex_mem in
  let mem_wb_rd = ex_mem_rd in
  let mem_wb_res = ex_mem_res in
  (mem_wb_rd, mem_wb_res) ;;

(* ============== Exec ============== *)
let execute(de_ex) =
  let (de_ex_op, de_ex_rd, de_ex_a, de_ex_b) = de_ex in
  let next_ex_mem_rd = de_ex_rd in
  let next_ex_mem_res : int<32> = alu(de_ex_op, de_ex_a, de_ex_b) in
  print_string "ALU_OUT:"; print_int next_ex_mem_res; print_newline ();
  (next_ex_mem_rd, next_ex_mem_res) ;;

(* ============== Decod ============== *)
let decod(if_de, reg_file) = 
  let (if_de_instr,()) = if_de in
  let ((op, rs2_type), rd, rs1, rs2) = if_de_instr in
  let next_de_ex_op = op in
  let next_de_ex_rd = rd in
  let next_de_ex_a = vect_nth(reg_file,rs1) in
  let next_de_ex_b = match rs2_type with
                     | Rs2Imm() -> rs2
                     | Rs2Reg() -> vect_nth(reg_file,rs2) in
  (next_de_ex_op, next_de_ex_rd, next_de_ex_a, next_de_ex_b) ;;

(* ============== Instruction Fetch ============== *)
let ifetch(code,pc) =
  let next_if_de_instr = vect_nth(code,pc) in
  ((next_if_de_instr, ()), pc + 1) ;;


(* ================================ *
 * minimalistic 4-stage pipeline,   *
 * computes outputs and next state  *
 * ================================ *)
let cpu_step (reg_file, mem_wb, ex_mem, de_ex, if_de, pc) =  
     let next_reg_file = write_back (mem_wb, reg_file) in
     let next_mem_wb   = mem        (ex_mem)           in
     let next_ex_mem   = execute    (de_ex)            in
     let next_de_ex    = decod      (if_de, reg_file)  in
     let (next_if_de, 
          next_pc)     = ifetch(code,pc) in
     (next_reg_file, next_mem_wb, next_ex_mem, next_de_ex, next_if_de, next_pc)
;;



let config0 = (vect_create<32>(0), (0,0), (0,0),
               (Add(),0,0,0), (((Add(),Rs2Imm()),0,0,0),()), 0) ;;



let main () =
  reg cpu_step init config0 ;;
