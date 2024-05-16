(** Pretty printer for source programs *)

open Types
open Ast

(** flag [hexa_int_pp_flag]:
    - when true, integers displayed in hexadecimal
    - when false, integers displayed in decimal
 *)
let hexa_int_pp_flag = ref false

open Format ;;
set_ellipsis_text "<...>";; (* Format customization *)
set_max_boxes 500 ;;

type fmt = formatter
type 'a pp = fmt -> 'a -> unit

(** auxilliary function for parenthesizing *)
let parenthesize ~(paren:bool) (pp:'a pp) (fmt:fmt) (a:'a) : unit =
 if paren then fprintf fmt "(%a)" pp a else pp fmt a


(** [size_is_not_zero ty] returns true iff [ty] a size type of size 0,
    e.g., [max(0,0+0)]. *)
let rec size_is_not_zero (ty:ty) : bool =
  match ty with
  | T_size n -> n > 0
  | T_add(t1,t2) | T_max(t1,t2) -> size_is_not_zero t1 || size_is_not_zero t2
  | T_var _ -> false
  | _ -> false


(** pretty printer for types *)
let pp_ty (fmt:fmt) (ty:ty) : unit =
  let h_assoc_tvars = Hashtbl.create 10 in
  let tvars_cur = ref 0 in
  let assoc_tvars n =
    match Hashtbl.find_opt h_assoc_tvars n with
    | Some v -> v
    | None -> let v = !tvars_cur in incr tvars_cur; Hashtbl.add h_assoc_tvars n v; v in
  let rec pp_type ~paren fmt ty =
  let open Format in
  match ty with
  | T_const tc ->
      (match tc with
       | TInt tz -> fprintf fmt "int<%a>" (pp_type ~paren:false) tz
       | TBool -> fprintf fmt "%s" "bool"
       | TUnit -> fprintf fmt "%s" "unit")
  | T_var{contents=Unknown n} ->
     (match (assoc_tvars n) with
      | 0 -> fprintf fmt "'a"
      | 1 -> fprintf fmt "'b"
      | 2 -> fprintf fmt "'c"
      | 3 -> fprintf fmt "'d"
      | v -> fprintf fmt "'a%d" v)
  | T_var{contents=Ty t} ->
      fprintf fmt "%a" (pp_type ~paren) t
  | T_tuple ts ->
      (* Parentheses are needed to avoid confusion between tuples and pair nesting *)
      fprintf fmt "(";
      pp_print_list
            ~pp_sep:(fun fmt () -> fprintf fmt " * ")
            (pp_type ~paren:true) fmt ts;
      fprintf fmt ")"
  | T_fun{arg;dur;ret} ->
      parenthesize ~paren (fun fmt () ->
      match dur with
      | T_size 0 ->
          fprintf fmt "%a => %a"
            (pp_type ~paren:true) arg
            (pp_type ~paren:true) ret
      | t -> if size_is_not_zero t then (
                fprintf fmt "%a -> %a"
                   (pp_type ~paren:true) arg
                   (pp_type ~paren:true) ret
              ) else (
                fprintf fmt "%a -[%a]-> %a"
                  (pp_type ~paren:true) arg
                  (pp_type ~paren:false) dur
                  (pp_type ~paren:true) ret)
        ) fmt ()
  | T_sum(cs) ->
      fprintf fmt "(";
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt " | ")
      (fun fmt (x,t) ->
         fprintf fmt "%s of %a" x (pp_type ~paren:false) t) fmt cs;
      fprintf fmt ")"
  | T_string tz ->
      fprintf fmt "string<%a>"
        (pp_type ~paren:false) tz
  | T_static tz ->
      fprintf fmt "static<%a>"
        (pp_type ~paren:false) tz
  | T_ref telem ->
      fprintf fmt "ref<%a>"
        (pp_type ~paren:false) telem
  | T_array{elem=t;size=tz} ->
      fprintf fmt "%a array<%a>"
        (pp_type ~paren:true) t
        (pp_type ~paren:false) tz
  | T_matrix{elem=t;size=tz} ->
      fprintf fmt "%a matrix<%a>"
        (pp_type ~paren:true) t
        (pp_type ~paren:false) tz
  | T_vector{elem=t;size=tz} ->
      fprintf fmt "%a vector<%a>"
        (pp_type ~paren:true) t
        (pp_type ~paren:false) tz
  | T_size n ->
      fprintf fmt "%d" n
  | T_response_time n ->
      fprintf fmt "%d" n
  | T_infinity ->
      fprintf fmt "+∞"
  | T_add (tz1,tz2) ->
      parenthesize ~paren (fun fmt () ->
        fprintf fmt "%a + %a"
          (pp_type ~paren:true) tz1
          (pp_type ~paren:true) tz2
        ) fmt ()
  | T_max (tz1,tz2) ->
      fprintf fmt "max(%a,%a)"
      (pp_type ~paren:false) tz1
      (pp_type ~paren:false) tz2
  | T_le (tz1,tz2) ->
      parenthesize ~paren (fun fmt () ->
        fprintf fmt "t where t = %a & t <= %a"
          (pp_type ~paren:true) tz1
          (pp_type ~paren:true) tz2
      )fmt ()
  in
  pp_type ~paren:false fmt ty


(** pretty printer for identifiers *)
let pp_ident (fmt:fmt) (x:x) : unit =
  fprintf fmt "%s" x


(** pretty printer for operators *)
let pp_op (fmt:fmt) (op:op) : unit =
    let pp_str (s:string) : unit =
      fprintf fmt "%s" s
    in
    match op with
    | Runtime p ->
        Operators.pp_op fmt p
    | Wait n ->
        fprintf fmt "wait<%d>" n
    | GetTuple {pos=0;arity=2} ->
        pp_str "fst"
    | GetTuple {pos=1;arity=2} ->
        pp_str "snd"
    | GetTuple {pos=i;arity=_} ->
        fprintf fmt "get_tuple<%d>" i
    | TyConstr ty ->
        fprintf fmt "(as_type %a)@,"  pp_ty ty

let pp_tuple (fmt:fmt) pp vs =
  fprintf fmt "(";
  pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt ", ")
        pp fmt vs;
  fprintf fmt ")"

let pp_vector (fmt:fmt) pp vs =
  fprintf fmt "[|";
  pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt "; ")
        pp fmt vs;
  fprintf fmt "|]"

(** pretty printer for constants *)
let rec pp_const (fmt:fmt) (c:c) : unit =
  match c with
  | Int (n,tz) ->
      fprintf fmt "(";
      if !hexa_int_pp_flag then fprintf fmt "0x%x" n else fprintf fmt "%d" n;
      fprintf fmt " : int<%a>)" pp_ty tz
  | Bool b -> fprintf fmt "%b" b
  | Unit -> fprintf fmt "()"
  | String s -> fprintf fmt "\"%s\"" s
  | Op op ->
     fprintf fmt "(%a)"
        pp_op op
  | V_loc l ->
      fprintf fmt "#%a" pp_ident l
  | C_tuple(cs) ->
      pp_tuple fmt pp_const cs
  | C_vector(cs) ->
      pp_vector fmt pp_const cs
  | C_size n -> fprintf fmt "size<%d>" n
  | Inj x ->
      fprintf fmt "%s" x

(** pretty printer for patterns *)
let rec pp_pat (fmt:fmt) (p:p) : unit =
  match p with
  | P_unit ->
      fprintf fmt "()"
  | P_var x ->
      pp_ident fmt x
  | P_tuple ps ->
      pp_tuple fmt pp_pat ps

(** pretty printer for expressions *)
let pp_exp (fmt:fmt) (e:e) : unit =
  let rec pp_e ~paren fmt e =
  match e with
  | E_deco(e,loc) ->
      pp_e ~paren fmt e
  | E_const c ->
      pp_const fmt c
  | E_var x ->
      pp_ident fmt x
  | E_fun (p,e) ->
       fprintf fmt "(fun %a ->@,%a)"
          pp_pat p
          (pp_e ~paren:false) e
  | E_fix (f,(p,e)) ->
       fprintf fmt "(fix %a (fun %a ->@,%a))"
          pp_ident f
          pp_pat p
          (pp_e ~paren:false) e
  | E_if(e,e1,e2) ->
      parenthesize ~paren (fun fmt () ->
        fprintf fmt "@[<v>if %a@,then %a@,else %a@]"
          (pp_e ~paren:true) e
          (pp_e ~paren:false) e1
          (pp_e ~paren:false) e2) fmt ()
  | E_case(e,hs,e_els) ->
      fprintf fmt "(@[<v>match %a with@,%a@,| _ -> %a@])"
        (pp_e ~paren:false) e
        (pp_print_list
            (fun fmt (c,e) -> fprintf fmt "| %a -> %a" pp_const c (pp_e ~paren:false) e))
         hs
        (pp_e ~paren:false) e_els
  | E_match(e,hs,eo) ->
      fprintf fmt "(@[<v>match %a with@,%a%a@])"
        (pp_e ~paren:true) e
        (pp_print_list
            (fun fmt (x,(p,e)) -> fprintf fmt "| %s %a -> %a" x pp_pat p (pp_e ~paren:false) e))
         hs
        (fun fmt eo ->
            match eo with
            | None -> ()
            | Some e -> fprintf fmt "@,| _ -> %a" (pp_e ~paren:false) e) eo
  | E_letIn(p,e1,e2) ->
      parenthesize ~paren (fun fmt () ->
        fprintf fmt "@[<v>@[<v 2>let %a =@,%a in@]@,%a@]"
          pp_pat p
          (pp_e ~paren:false) e1
          (pp_e ~paren:false) e2) fmt ()
  | E_app(e1,e2) ->
      parenthesize ~paren (fun fmt () ->
        fprintf fmt "@[<v>%a %a@]"
          (pp_e ~paren:true) e1
          (pp_e ~paren:true) e2) fmt ()
  | E_tuple es ->
        pp_tuple fmt (pp_e ~paren:false) es
  | E_reg((p,e1), e0,l) ->
      parenthesize ~paren (fun fmt () ->
        fprintf fmt "@[<v>reg[%s] (fun %a -> %a) last %a@]" l
          pp_pat p
          (pp_e ~paren:false) e1 (pp_e ~paren:false) e0) fmt ()
  | E_exec(e1,e2,e3,x) ->
      fprintf fmt "(@[<v>exec[%s] %a default %a@])"
        x (pp_e ~paren:false) e1 (pp_e ~paren:false) e2
  | E_ref(e1) ->
      parenthesize ~paren (fun fmt () ->
        fprintf fmt "ref %a" (pp_e ~paren:true) e1) fmt ()
  | E_get(e1) ->
        fprintf fmt "!%a" (pp_e ~paren:true) e1
  | E_set(e1,e2) ->
      parenthesize ~paren (fun fmt () ->
        fprintf fmt "@[<v>%a := %a@]"
          (pp_e ~paren:true) e1
          (pp_e ~paren:true) e2) fmt ()
  | E_local_static_array(e1,_) ->
     fprintf fmt "array_create %a"
        (pp_e ~paren:true) e1
  | E_array_length(x) ->
      fprintf fmt "@[<v>%a.length@]"
        pp_ident x
  | E_array_get(x,e1) ->
      fprintf fmt "@[<v>%a.(%a)@]"
        pp_ident x
        (pp_e ~paren:false) e1
  | E_array_set(x,e1,e2) ->
      parenthesize ~paren (fun fmt () ->
        fprintf fmt "@[<v>%a.(%a) <- %a@]"
          pp_ident x
          (pp_e ~paren:false) e1
          (pp_e ~paren:false) e2) fmt ()
  | E_local_static_matrix(e1,es,_) ->
     fprintf fmt "%a" (pp_e ~paren:true) e1;
     List.iter (fun e -> fprintf fmt "^%a" (pp_e ~paren:true) e) es
  | E_matrix_size(x,n) ->
      fprintf fmt "@[<v>%a.(%d).size@]"
        pp_ident x n
  | E_matrix_get(x,es) ->
      fprintf fmt "@[<v>";
      pp_ident fmt x;
      List.iter (fun e -> 
        fprintf fmt ".(%a)" (pp_e ~paren:false) e
      ) es;
      fprintf fmt "@]";
  | E_matrix_set(x,es,e2) ->
      fprintf fmt "@[<v>";
      pp_ident fmt x;
      List.iter (fun e -> 
        fprintf fmt ".(%a)" (pp_e ~paren:false) e
      ) es;
      fprintf fmt " <- %a@]" (pp_e ~paren:false) e2;
  | E_par(es) ->
      fprintf fmt "(";
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt " || ")
          (pp_e ~paren:false) fmt es;
      fprintf fmt ")"
  | E_for(x,e_st1,e_st2,e3,_) ->
      fprintf fmt "for %s = %a to %a do %a done" x 
        (pp_e ~paren:false) e_st1
        (pp_e ~paren:false) e_st2
        (pp_e ~paren:false) e3
  | E_generate((p,e1),e2,e_st3,_) ->
      fprintf fmt "generate %a %a ~depth:%a"
        (pp_e ~paren:true) (E_fun(p,e1))
        (pp_e ~paren:true) e2
        (pp_e ~paren:true) e_st3
  | E_vector(es) ->
      pp_vector fmt (pp_e ~paren:false) es
  | E_vector_mapi(is_par,(p,e1),e2,_) ->
      fprintf fmt "mapi%s" (if is_par then "_par" else "");
      fprintf fmt "%a %a" (pp_e ~paren:true) (E_fun(p,e1))
                          (pp_e ~paren:true) e2
  | E_int_mapi(is_par,(p,e1),e2,_) ->
      fprintf fmt "mapi%s" (if is_par then "_par" else "");
      fprintf fmt "%a %a" (pp_e ~paren:true) (E_fun(p,e1))
                          (pp_e ~paren:true) e2
  in
  fprintf fmt "@[<v 0>%a@]" (pp_e ~paren:false) e

(** pretty printer for static declarations *)
let pp_static (fmt:fmt) (g:static) : unit =
  match g with
  | Static_array_of (t,_) ->
      fprintf fmt "array of %a" pp_ty t
  | Static_array(c,n) ->
      fprintf fmt "(%a)^%d" pp_const c n
  | Static_matrix(c,n_list) ->
      fprintf fmt "(%a)" pp_const c;
      List.iter (fun n -> fprintf fmt "^%d" n) n_list 
  | Static_const(c) ->
      fprintf fmt "(%a)" pp_const c

(** pretty printer for programs *)
let pp_pi (fmt:fmt) (pi:pi) : unit =
  let {statics;main} = pi in
  fprintf fmt "@[<v>";
  List.iter (fun (x,g) -> fprintf fmt "let %s = %a;;@," x pp_static g) statics;
  fprintf fmt "%a@]" pp_exp main
