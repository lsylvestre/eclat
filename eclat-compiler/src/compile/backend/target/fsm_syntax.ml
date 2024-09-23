type x = string

type tvar = V of string | T of ty
and ty = TInt of ty | TBool | TUnit
       | TTuple of ty list
       | TVar of tvar ref
       | TString of ty
       | TStatic of {elem:ty ; size: ty}
       | TVector of {elem:size ; size: ty}
       | TVect of int
       | TAbstract of x * size list * ty list
       | TSize of int
and size = ty

let new_tvar = let c = ref 0 in fun () -> incr c; TVar (ref (V ("'a"^string_of_int !c)))


type c = Unit
       | Int of {value:int;tsize:ty}
       | Bool of bool
       | Enum of x
       | CTuple of c list
       | CVector of c list
       | String of string (* non synthesizable *)
       | CSize of int
       | C_encode of c * int
  
type op = If (* i.e., a multiplexer *)
        | Runtime of Operators.op
        | GetTuple of
            (* (pos,arity,ty) *)
            (* ty is the type of the value from which a field is extracted  *)
             int * int * ty
        | TyConstr of ty

type global =
  | Static_array_of of ty
  | Static_array of c * int

type a = A_letIn of x * a * a
  | A_tuple of a list
  | A_vector of a list
  | A_const of c
  | A_var of x
  | A_call of op * a
  | A_string_get of x * x
  | A_ptr_taken of x
  | A_ptr_write_taken of x
  | A_buffer_get of x
  | A_buffer_length of x * ty (* [ty] is the size of the resulting integer *)
  | A_decode of x * ty
  | A_encode of x * ty * int

type write = Delayed | Immediate

type q = x
type l = x

type s = (* all instructions terminates in one clock cycle *)
  | S_skip
  | S_continue of q
  | S_if of x * s * s option
  | S_case of x * (c list * s) list * s option
  | S_set of x * a
  | S_acquire_lock of l
  | S_release_lock of l
  | S_read_start of l * a
  | S_read_stop of x * l
  | S_write_start of x * a * a
  | S_write_stop of x
  | S_seq of s * s
  | S_letIn of x * a * s
  | S_fsm of id * x * x * q * t list * s (* id * rdy * result * compute * transition * start instruction *)
  | S_in_fsm of id * s
  | S_call of Operators.op * a
  | S_external_run of x * int * x * x * a (* (f,id,result,rdy,a) *)


and t = (x * s)

and id = string
  (* // ... *)
  (* case *)

let set_ x a = S_set(x,a)
let seq_ s s' = S_seq(s,s')


module Debug = struct
  open Format

  let rec pp_ty fmt = function
  | TInt tz -> fprintf fmt "int<%a>" pp_ty tz
  | TBool -> fprintf fmt "bool"
  | TUnit ->  fprintf fmt "unit"
  | TTuple ts ->
      fprintf fmt "@[<v>(";
      pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " * ") pp_ty fmt ts;
      fprintf fmt ")@]"
  | TVar{contents=V n} -> fprintf fmt "'a%s" n
  | TVar{contents=T t} -> pp_ty fmt t
  | TString ty -> fprintf fmt "string<%a>" pp_ty ty
  | TVector {elem ; size} -> fprintf fmt "%a vector<%a>" pp_ty elem pp_ty size
  | TVect n -> fprintf fmt "vect<%d>" n
  | TAbstract(x,ns,ts) -> 
      fprintf fmt "%s<" x;
      pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_ty fmt ns; 
      fprintf fmt ">";
      fprintf fmt "@[<v>[";
      pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_ty fmt ts;
      fprintf fmt "]@]"
  | TSize n -> fprintf fmt "size<%d>" n
  | TStatic {elem ; size} -> fprintf fmt "%a buffer<%a>" pp_ty elem pp_ty size

let pp_tuple = Ast_pprint.pp_tuple
let pp_vector = Ast_pprint.pp_vector

  let rec pp_c fmt = function
    | Unit -> fprintf fmt "()"
    | Int{value=n;tsize=t} -> fprintf fmt "%d'%ab" n pp_ty t
    | Bool b -> fprintf fmt "\"%d\"" (if b then 1 else 0)
    | Enum x -> fprintf fmt "%s" x
    | CTuple(cs) ->
        pp_tuple fmt pp_c cs
    | CVector(cs) ->
        pp_vector fmt pp_c cs
    | CSize n ->
        fprintf fmt "size%d" n
    | String s -> fprintf fmt "\"%s\"" s
    | C_encode (c,n) ->
        fprintf fmt "encode<%d>(%a)" n pp_c c

  let pp_op fmt = function
  | If -> fprintf fmt "eclat_if"
  | Runtime p -> Operators.pp_op fmt p
  | GetTuple (i,_,_) -> fprintf fmt "eclat_get_%d" i
  | TyConstr _ -> fprintf fmt "eclat_id"


  let rec pp_a fmt = function
  | A_const c -> pp_c fmt c
  | A_var x -> fprintf fmt "%s" x
  | A_call(op,a) ->
     fprintf fmt "@[%a(%a)@]" pp_op op pp_a a
  | A_letIn(x,a1,a2) ->
     fprintf fmt "@[<v>let %s = %a in@,%a@]" x pp_a a1 pp_a a2
  | A_tuple aas ->
      pp_tuple fmt pp_a aas
  | A_vector aas ->
      pp_vector fmt pp_a aas
  | A_string_get(sx,ix) -> fprintf fmt "%s[%s]" sx ix
  | A_ptr_taken(x) -> fprintf fmt "ptr_taken<%s>" x
  | A_ptr_write_taken(x) -> fprintf fmt "ptr_write_taken<%s>" x
  | A_buffer_get(x) -> fprintf fmt "static_get_value(%s)" x
  | A_buffer_length(x,_) -> fprintf fmt "%s.length" x
  | A_decode(x,_) -> fprintf fmt "decode(%s)" x
  | A_encode(x,_,n) -> fprintf fmt "encode(%s,%d)" x n

  let rec pp_s fmt = function
  | S_skip -> fprintf fmt "skip"
  | S_continue q ->
    fprintf fmt "continue %s" q
  | S_if(x,s,so) ->
      fprintf fmt "@[<v 2>if %s = '1' then@,%a@]@," x pp_s s;
      Option.iter (fun s' ->
        fprintf fmt "@[<v 2>else@,%a@]@," pp_s s') so;
      fprintf fmt "end if;"
  | S_case(x,hs,so) ->
      fprintf fmt "@[<v>case %s is@," x;
      let pp_cs fmt cs = 
        pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " | ") pp_c fmt cs in
      List.iter (fun (cs,s) -> 
        fprintf fmt "@[<v 2>when %a =>@,%a@]@," pp_cs cs pp_s s) hs;
      Option.iter (fun s -> fprintf fmt "@[<v 2>when others => %a@]@," pp_s s) so;
      fprintf fmt "@]end case;";
  | S_set(x,a) ->
      fprintf fmt "@[<v>%s := %a;@]" x pp_a a
  | S_acquire_lock(l) -> fprintf fmt "@[<v>acquire_lock(%s);@]" l
  | S_release_lock(l) -> fprintf fmt "@[<v>release_lock(%s);@]" l
  | S_read_start(x,idx) ->
      fprintf fmt "@[<v>read_start<%s>(%a);@]" x pp_a idx
  | S_read_stop(x,l) ->
      fprintf fmt "@[<v>%s <- read_stop<%s>();@]" x l
  | S_write_start(x,idx,a) ->
      fprintf fmt "@[<v>write_start<%s>[%a] <- %a;@]" x pp_a idx pp_a a
  | S_write_stop(x) ->
      fprintf fmt "@[<v>write_stop<%s>_end;@]" x
  | S_seq(s1,s2) ->
      fprintf fmt "@[<v>%a@,%a@]" pp_s s1 pp_s s2
  | S_letIn(x,a,s) ->
      fprintf fmt "@[<v>let %s = %a in@,%a@]" x pp_a a pp_s s
  | S_fsm(_,_,result,_,ts,s) ->
      fprintf fmt "@[<v>((%s) -> %a)@]" result pp_fsm (ts,s)
  | S_in_fsm(id,s) ->
      fprintf fmt "@[<v>%a in fsm %s@]" pp_s s id
  | S_call(op,a) ->
     fprintf fmt "@[%a(%a)@]" Operators.pp_op op pp_a a
  | S_external_run(f,i,res,rdy,a) ->
    fprintf fmt "@[(%s,%s) := run_%d %s %a;@]" res rdy i f pp_a a
  and pp_fsm fmt (ts,s) =
    let pp_t fmt (x,s) = fprintf fmt "@[%s = %a@]@," x pp_s s in
    fprintf fmt "@[<v>let rec ";
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,and ") pp_t fmt ts;
    fprintf fmt "@,@]in %a" pp_s s

end
