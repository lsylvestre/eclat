(* 
  
    $ ./eclat ../examples/archi/alu.ecl ../examples/archi/proc2.ecl
    $ make simul NS=100

*)

type op = Alu of aluop 
        | Div of unit ;;


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
             ((Div(),Rs2Imm()),8,4,8),
             ((Alu(Add()),Rs2Imm()),9,10,10),
             ((Alu(Lsl()),Rs2Imm()),10,4,2),
             ((Alu(Add()),Rs2Imm()),8,4,1),
             ((Alu(Add()),Rs2Imm()),8,4,1) ] ;;

(* ============== Write Back ============== *)
let write_back(rdy, mem_wb, reg_file) =
  if rdy then
    let (mem_wb_rd, mem_wb_res) = mem_wb in
    vect_update (reg_file, mem_wb_rd, mem_wb_res)
  else reg_file ;;

(* ============== Mem ============== *)
let mem(rdy, ex_mem, mem_wb) =
  if rdy then 
    let (ex_mem_rd, ex_mem_res) = ex_mem in
    let mem_wb_rd = ex_mem_rd in
    let mem_wb_res = ex_mem_res in
    (mem_wb_rd, mem_wb_res)
  else mem_wb ;;

(* ============== Exec ============== *)
let execute(de_ex) =
  let (de_ex_op, de_ex_rd, de_ex_a, de_ex_b) = de_ex in
  let next_ex_mem_rd = de_ex_rd in
  let (next_ex_mem_res, rdy) : int<32> * bool = 
    exec
      match de_ex_op with
      | Alu cmd -> alu(cmd, de_ex_a, de_ex_b)
      | Div() -> div (de_ex_a, de_ex_b)  
    default 0
  in
  if rdy then print_string "---> finished!";
  print_string "ALU_OUT:"; print_int next_ex_mem_res; print_newline ();
  ((next_ex_mem_rd, next_ex_mem_res), rdy) ;;

(* ============== Decod ============== *)
let decod(rdy, (if_de, reg_file), de_ex) =
  if rdy then
    let (if_de_instr,()) = if_de in
    let ((op, rs2_type), rd, rs1, rs2) = if_de_instr in
    let next_de_ex_op = op in
    let next_de_ex_rd = rd in
    let next_de_ex_a = vect_nth(reg_file,rs1) in
    let next_de_ex_b = match rs2_type with
                       | Rs2Imm() -> rs2
                       | Rs2Reg() -> vect_nth(reg_file,rs2) in
    (next_de_ex_op, next_de_ex_rd, next_de_ex_a, next_de_ex_b)
  else de_ex ;;

(* ============== Instruction Fetch ============== *)
let ifetch(rdy,(code,pc),if_de) =
  if rdy then
    let next_if_de_instr = vect_nth(code,pc) in
    ((next_if_de_instr, ()), pc + 1)
  else (if_de, pc) ;;

(* ================================ *
 * minimalistic 5-stage pipeline,   *
 * with multi-cycle exec unit,      *
 * computes outputs and next state  *
 * ================================ *)
let cpu_step (reg_file, mem_wb, ex_mem, de_ex, if_de, pc, rdy) =
     let next_reg_file           = write_back (rdy, mem_wb, reg_file) in
     let next_mem_wb             = mem        (rdy, ex_mem, mem_wb)   in
     let (next_ex_mem, next_rdy) = execute    (de_ex)          in
     let next_de_ex              = decod      (next_rdy, (if_de, reg_file), de_ex)  in
     let (next_if_de, next_pc)   = ifetch     (next_rdy,(code,pc),if_de) in
     (next_reg_file, next_mem_wb, next_ex_mem, next_de_ex, next_if_de, next_pc, next_rdy)
;;



let config0 = (vect_create<32>(0), (0,0), (0,0),
               (Alu(Add()),0,0,0), (((Alu(Add()),Rs2Imm()),0,0,0),()), 0, true) ;;



let main () =
  reg cpu_step init config0 ;;

