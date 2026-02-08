type compare_op =
  LT of unit 
  | GT of unit 
  | EQ of unit 
  | NEQ of unit 
  | LE of unit
  | GE of unit ;;

type unop = NOT of unit 
          | NEG of unit
          | ISINT of unit ;;


type group3 =
             BRANCH         of short
            | BRANCHIF      of bool * short
            
            | STOP          of unit
            | SWITCH        of short * short
            | CHECK_SIGNALS of unit

type group_compare =
   COMPARE of unit 
 | BCOMPARE of long * short

type group4 =
   CALL          of short * bool * bool * bool * bool
  | POPTRAP of unit
  | RAISE of unit 

  | RESTART of unit
  | RETURN of short
  | APPTERM of short * short
  | PUSHTRAP of byte 
  | PUSHRETADDR of short
  | APPLY of bool * byte * short 
  | MAKEBLOCK     of bool * bool * short * byte * short(* is_atom * size * tag *) 
  | GRAB of byte 
  | CLOSUREREC of byte * short * short * short ;; 


type get_seg = ENVACC of unit | GETFIELD of unit | GETGLOBAL of unit
type set_seg = SETFIELD of unit | SETGLOBAL of unit (* | SETVECTITEM of unit *)
(* type stack_op = ACC of unit | ASSIGN of unit | PUSH of unit*)
type opcode =
    LABEL         of short
  | POP           of short
  | CONST         of long 
  | ADD           of bool * long     (* include offsetint *)
  | GETVECTITEM of unit
  | SETVECTITEM of unit 
  | GET of get_seg * short
  | SET of set_seg * short
  | ACC of short 
  | ASSIGN of short
  | PUSH of unit 
  (* | STACK_N of stack_op * short*)
  | GROUP3        of group3 
  | OFFSETCLOSURE of short
  | GROUP4        of group4
  | GROUP_COMPARE of compare_op * group_compare
  