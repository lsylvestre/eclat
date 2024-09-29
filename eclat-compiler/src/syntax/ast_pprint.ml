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
      fprintf fmt " : int<%a>)" pp_size tz
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
  | C_appInj(x,c,_) ->
      fprintf fmt "%s(%a)" x pp_const c
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
  | E_fun (p,(ty,tyB),e) ->
       fprintf fmt "(fun (%a:%a) : %a ->@,  @[%a@])"
          pp_pat p
          Types.pp_ty ty
          Types.pp_tyB tyB
          (pp_e ~paren:false) e
  | E_fix (f,(p,(ty,tyB),e)) ->
       fprintf fmt "(fix %a (fun (%a:%a) : %a ->@,  @[%a@]))"
          pp_ident f
          pp_pat p
          Types.pp_ty ty
          Types.pp_tyB tyB
          (pp_e ~paren:false) e
  | E_if(e,e1,e2) ->
      parenthesize ~paren (fun fmt () ->
        fprintf fmt "@[<v>if %a@,then %a@,else %a@]"
          (pp_e ~paren:true) e
          (pp_e ~paren:false) e1
          (pp_e ~paren:false) e2) fmt ()
  | E_case(e,hs,e_els) ->
      let pp_cs fmt cs =
         pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt " | ")
          pp_const fmt cs
      in 
      fprintf fmt "(@[<v>match %a with@,%a@,| _ -> %a@])"
        (pp_e ~paren:false) e
        (pp_print_list
            (fun fmt (cs,e) -> fprintf fmt "| %a -> %a" pp_cs cs (pp_e ~paren:false) e))
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
  | E_letIn(p,ty,e1,e2) ->
      parenthesize ~paren (fun fmt () ->
        fprintf fmt "@[<v>@[<v 2>let %a : %a =@,%a in@]@,%a@]"
          pp_pat p
          Types.pp_ty ty
          (pp_e ~paren:false) e1
          (pp_e ~paren:false) e2) fmt ()
  | E_app(e1,e2) ->
      parenthesize ~paren (fun fmt () ->
        fprintf fmt "@[<v>%a %a@]"
          (pp_e ~paren:true) e1
          (pp_e ~paren:true) e2) fmt ()
  | E_tuple es ->
        pp_tuple fmt (pp_e ~paren:false) es
  | E_reg((p,tyB,e1), e0,l) ->
      parenthesize ~paren (fun fmt () ->
        fprintf fmt "@[<v>reg[%s] (fun %a : %a -> %a) last %a@]" l
          pp_pat p
          Types.pp_tyB tyB
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
  | E_array_create(sz,_) ->
     fprintf fmt "create<%a>"
       pp_size sz
  | E_array_make(sz,e,_) ->
     fprintf fmt "make<%a>(%a)" 
       pp_size sz (pp_e ~paren:false) e
  | E_array_length(x) ->
      parenthesize ~paren (fun fmt () ->
        fprintf fmt "@[<v>length %a@]"
          pp_ident x) fmt ()
  | E_array_get(x,e1) ->
      parenthesize ~paren (fun fmt () ->
      fprintf fmt "@[<v>get(%a,%a)@]"
        pp_ident x
        (pp_e ~paren:false) e1) fmt ()
  | E_array_set(x,e1,e2) ->
      parenthesize ~paren (fun fmt () ->
        fprintf fmt "@[<v>set(%a,%a,%a)@]"
          pp_ident x
          (pp_e ~paren:false) e1
          (pp_e ~paren:false) e2) fmt ()
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
  | E_generate((p,ty,e1),e2,e_st3,_) ->
      fprintf fmt "generate %a %a ~depth:%a"
        (pp_e ~paren:true) (E_fun(p,ty,e1))
        (pp_e ~paren:true) e2
        (pp_e ~paren:true) e_st3
  | E_vector(es) ->
      pp_vector fmt (pp_e ~paren:false) es
  | E_vector_mapi(is_par,(p,(tyB1,tyB2),e1),e2,_) ->
      fprintf fmt "mapi%s" (if is_par then "_par" else "");
      fprintf fmt "%a %a" (pp_e ~paren:true) 
                            (E_fun(p,(Types.Ty_base tyB1,tyB2),e1))
                          (pp_e ~paren:true) e2
  | E_run(i,e) ->
      fprintf fmt "run %s(%a)" i (pp_e ~paren:false) e
  in
  fprintf fmt "@[<v 0>%a@]" (pp_e ~paren:false) e

(** pretty printer for static declarations *)
let pp_static (fmt:fmt) (g:static) : unit =
  match g with
  | Static_array_of (t,_) ->
      fprintf fmt "static_array : %a" pp_ty t
  | Static_array(c,n) ->
      fprintf fmt "(%a)^%d" pp_const c n
  | Static_const(c) ->
      fprintf fmt "(%a)" pp_const c

(** pretty printer for programs *)
let pp_pi (fmt:fmt) (pi:pi) : unit =
  let {statics;main} = pi in
  fprintf fmt "@[<v>";
  List.iter (fun (x,g) -> fprintf fmt "let %s = %a;;@," x pp_static g) statics;
  fprintf fmt "%a@]" pp_exp main
