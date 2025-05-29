type unop = NOT of unit 
          | NEG of unit
          | VECTLENGTH of unit
          | ISINT of unit

type compare_op = LT of unit   | GT of unit   | EQ  of unit  | NEQ  of unit  | LE of unit   | GE of unit  

type group1 =  
              ACC           of unit
            | PUSH          of unit
            | POP           of unit
            | ASSIGN        of unit
            | MAKEFLOATBLOCK of long (* todo*)
            | ENVACC        of unit
            | CONST         of long            
            | OFFSET        of long
            | OFFSETREF     of long
            | UNOP          of unop
            | GETMETHOD     of unit
            | GETPUBMET     of unit
            | GETDYNMET     of unit
            | GETFIELD      of unit
            | SETFLOATFIELD of unit (* todo*)
            | BUCOMPARE     of long * bool
             (* BCOMPARE : todo *)
            | OFFSETCLOSURE of unit (* ptr *)
            | PUSHRETADDR   of unit
            | GETGLOBAL     of unit
            | SETGLOBAL     of unit
             ;;
type group2 = 
              GETVECTITEM   of unit
            | SETVECTITEM   of unit
            | GETSTRINGCHAR of unit
            | SETSTRINGCHAR of unit
            | ADD           of unit
            | SUB           of unit
            | MUL           of unit
            | OR            of unit
            | AND           of unit
            | MOD           of unit
            | DIV           of unit
            | LSL           of unit
            | ASR           of unit
            | XOR           of unit
            | COMPARE       of compare_op
            | SETFIELD      of short

type group3 =
             BRANCH         of short
            | BRANCHIF      of bool * short
            | BCOMPARE      of compare_op * long * short
            | STOP          of unit
            | SWITCH        of short * short
            | CHECK_SIGNALS of unit

type group4 =
   POPTRAP       of unit
             | RAISE         of unit
            | RESTART       of unit 


type opcode =
    LABEL         of short
  | GROUP1        of group1 * short
  | GROUP2        of group2 
  | GROUP3        of group3 
  | GROUP4        of group4
  | MAKEBLOCK     of bool * bool * short * char * short(* is_atom * size * tag *)
  | RETURN        of short
  | APPTERM       of short * short
  | APPLY         of bool * char * short
  | PUSHTRAP      of short
  | CLOSURE       of short * short
  | GRAB          of char
  | CALL          of short * bool * bool * bool * bool
  | CLOSUREREC    of char * short * short * short       (* char: up to 256 mutual recursive values *)                  
