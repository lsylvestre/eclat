(** Generating sequential (OCaml code) from Eclat programs.
  
  The purpose is to simulate the generated circuit expressed 
  in the intermediate representation of the compiler, which is
  an imperative dataflow hardware language close to sequential code.
  The circuit is optimized for synthesis and not for software excution,
  therefore it is not an idiomatic software implementation
  (no modularity, no function definitions). 

*)
open Fsm_syntax
open Format

module SMap = Ast.SMap
let gensym =
  let x = ref 0 in
  (fun () -> incr x; "v"^string_of_int !x)

(** each Eclat program [f] is translated to a step function [f_step] *)
let pp_prog_name fmt x =
  fprintf fmt "%s_step" x

(** [norm_ident x] convert [x] to a valid OCaml identifier *)
let norm_ident x = String.map (function '$' | '.' -> '_' | c -> c) x

(** code generator for identifiers *)
let pp_ident fmt (x:x) : unit =
    fprintf fmt "%s" (norm_ident x)

let pp_state fmt (x:x) : unit =
    let x = norm_ident x in
    let x = if x.[0] = '_' then "Q"^x else x in
    fprintf fmt "%s" (String.uppercase_ascii x)

(* lock-based support for concurrent memory accesses *)
let ptr_taken x = x^"_lock" 

let pp_tuple pp fmt vs =
  fprintf fmt "(";
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp fmt vs;
  fprintf fmt ")"

let pp_vector fmt pp vs =
  fprintf fmt "[|";
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp fmt vs;
  fprintf fmt "|]"


let size_ty t =
  (* we must canonize [t] to prevent it from being considered as a type variable *)
  Fsm_typing.(size_ty (canon t))


(** code generator for constants *)
let rec pp_c fmt c =
  match c with
  | Unit -> fprintf fmt "()"
  | Int {value=n;tsize} ->
      (* todo: size *)
      fprintf fmt "%dL" n
  | Bool b ->
      fprintf fmt "%s" (if b then "true" else "false")
  | Enum x -> pp_state fmt x
  | CTuple(cs) ->
       pp_tuple pp_c fmt cs
  | CVector(cs) ->
      pp_vector fmt pp_c cs
  | String s -> fprintf fmt "\"%s\"" s
  | CSize n -> fprintf fmt "%d" n
  | C_encode(c,_) ->
      fprintf fmt "Bitvector.encode(%a)" pp_c c  (* ok ? *)

(** code generator for call of operator *)
let rec pp_call externals typing_env res fmt (op,a) =
  (match res with None -> () | Some x -> fprintf fmt "%a := " pp_ident x);
  match op with
  | GetTuple(i,_,ty) ->
      let ts = match Fsm_typing.canon ty with TTuple ts -> ts | _ -> assert false in
      (match a with
      | A_tuple aa -> (pp_a typing_env externals) fmt (List.nth aa i)
      | _ -> let z = gensym () in
             fprintf fmt "(let (";
             let ps = List.mapi (fun j _ -> if i = j then z else "_") ts in
             pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",")
               pp_ident fmt ps;
             fprintf fmt ") = %a in %a)" (pp_a typing_env externals) a pp_ident z)
  | Runtime(Int_of_tuple n) ->
      let xs = List.init n (fun _ -> norm_ident @@ gensym ()) in
      fprintf fmt "(let %a =" (pp_tuple (fun fmt x -> fprintf fmt "%s" x)) xs;
      fprintf fmt "%a" (pp_a typing_env externals) a;
      fprintf fmt "in\n";
      let rec loop acc xs =
        match xs with
        | [] -> fprintf fmt "0L"
        | x::xs' -> fprintf fmt "(Int.add_ ((if %s then %LdL else 0L)," x acc;
                    loop (Int64.mul acc 2L) xs';
                    fprintf fmt "))"
      in
      loop 1L (List.rev xs);
      fprintf fmt ")"
  | Runtime(Tuple_of_int n) ->
     let z = norm_ident @@ gensym () in
     fprintf fmt "(let %s = %a in\n" z 
       (pp_a typing_env externals) a;
     fprintf fmt "(";
     let ns = (List.init n (fun i -> i)) in
     pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")
        (fun fmt i -> fprintf fmt "Int.get_bit_(%s,%dL)" z i) 
         fmt ns;
     fprintf fmt "))"
  | Runtime(Size_of_val(ty,size_int)) -> 
     let n = size_ty (Fsm_typing.translate_ty ty) in
     pp_c fmt (Int{value=n;tsize=Fsm_typing.translate_size size_int})
  | Runtime(Resize_int sz) ->
      let n = size_ty @@ Fsm_typing.translate_size sz in
      fprintf fmt "Int.resize_(%a,%d)" (pp_a typing_env externals) a n
  | Runtime(Vector_create sz) ->
      let n = size_ty @@ Fsm_typing.translate_size sz in
      fprintf fmt "Vector.make_ (%d,%a)" n (pp_a typing_env externals) a
  | Runtime(External_fun (x,ty)) ->
      (** no normalization of ident [x], 
          but add caracter '_' for ensuring [x_] is a valid 
          qualified identifier in OCaml
          e.g. VHDL function Int.lor becomes OCaml value Int.lor_ *)
      fprintf fmt "%s_ (%a)" x
        (pp_a typing_env externals) a
  | Runtime(Assert) -> 
      fprintf fmt "assert (%a)"
        (pp_a typing_env externals) a
  | Runtime(Print_newline) -> 
      fprintf fmt "Print.newline_ (%a)"
        (pp_a typing_env externals) a
  | Runtime(Print_string) -> 
      fprintf fmt "Print.string_ (%a)"
        (pp_a typing_env externals) a
  | Runtime(Print_int) -> 
      fprintf fmt "Print.int_ (%a)"
        (pp_a typing_env externals) a
  | Runtime p ->
      Operators.gen_op fmt p (fun fmt a -> 
        fprintf fmt "(%a)" 
          (pp_a typing_env externals) a
        ) a
  | _ -> fprintf fmt "@[%a(%a)@]" pp_op op (pp_a typing_env externals) a

(** code generator for operator *)
and pp_op fmt = function
| If -> fprintf fmt "Bool.if_"
| Runtime p -> assert false (* deal with in pp_call*)
| TyConstr _ -> fprintf fmt "id_"
| GetTuple (i,_,_) -> assert false (* special case, defined below (see tuple_access) *)

(** code generator for atoms (i.e. combinatorial expression) *)
(* assumes that the let-bindings of atoms are not nested *)
and pp_a typing_env externals fmt = function
| A_const c -> pp_c fmt c
| A_var x -> fprintf fmt "!%a" pp_ident x
| A_call(op,a) ->
   pp_call externals typing_env None fmt (op,a)
| A_letIn _ -> assert false (* flattening needed before *)
| A_tuple aas -> pp_tuple (pp_a typing_env externals) fmt aas
| A_vector aas -> pp_vector fmt (pp_a typing_env externals) aas
| A_string_get(s,i) ->
    fprintf fmt "%a.(%a)" pp_ident s pp_ident i
| A_buffer_get(xb) ->
    pp_ident fmt ("$"^xb^"_value") (** A_buffer_get : deacode to be removed *)
| A_ptr_taken(x)
| A_ptr_write_taken(x) ->
    fprintf fmt "Lock.is_taken(%a)" pp_ident (ptr_taken x)
| A_buffer_length(x,tz) ->
    fprintf fmt "Int64.of_int (Array.length %a)" pp_ident x
| A_encode(y,ty,n) ->
   fprintf fmt "Bitvector.encode !%a"
     pp_ident y
| A_decode(y,ty) ->
   fprintf fmt "Bitvector.decode !%a"
     pp_ident y

let pp_a2 typing_env x externals fmt a =
  fprintf fmt "%a := %a" 
    pp_ident x 
    (pp_a typing_env externals) a


(** code generator for statements *)
let rec pp_s typing_env externals ~st fmt = function
| S_skip -> fprintf fmt "()"
| S_continue q -> fprintf fmt "%a := %a" pp_ident st pp_state q
| S_if(z,s1,so) ->
    fprintf fmt "(@[<v 2>if !%a then@,(%a)@]"
      pp_ident z 
      (pp_s typing_env externals ~st) s1;
    Option.iter (fun s2 -> fprintf fmt "@,@[<v 2>else@,(%a)@,@]" 
                             (pp_s typing_env externals ~st) s2) so;
    fprintf fmt ")"
| S_case(y,hs,so) ->
    fprintf fmt "@[<v>(match !%a with@," pp_ident y;
    let pp_cs fmt cs = 
      pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " | ")
        pp_c fmt cs
    in
    List.iter (fun (cs,s) ->
      fprintf fmt "@[<v 2>| %a -> @[<v 0>%a@]@]@," 
        pp_cs cs 
        (pp_s typing_env externals ~st) s) hs;
    Option.iter (fun s ->
      fprintf fmt "@[<v 2>| _ -> @,%a@]@," 
        (pp_s typing_env externals ~st) s) so;
    fprintf fmt ")@]";
| S_set(x,a) -> 
    fprintf fmt "%a" 
      (pp_a2 typing_env x externals) a
| S_acquire_lock(l) ->
      fprintf fmt "@[Lock.acquire(%a)@]" 
        pp_ident (ptr_taken l)
 | S_release_lock l ->
      fprintf fmt "@[Lock.release(%a)@]" 
        pp_ident (ptr_taken l)
| S_read_start(x,idx) -> 
      (* todo: avoid code duplication between S_setptr & S_setptr_write *)
      fprintf fmt "@[%a_value := %a.(Int64.to_int (%a))@]" 
        pp_ident x 
        pp_ident x 
        (pp_a typing_env externals) idx;
| S_read_stop(x,l) ->
        fprintf fmt "@[%a := !%a@]" 
            pp_ident x
            pp_ident (l^"_value") 
  | S_write_start(x,idx,a) ->
      fprintf fmt "@[(%a.(Int64.to_int (%a)) <- %a)@]"
        pp_ident x
        (pp_a typing_env externals) idx
        (pp_a typing_env externals) a;
  | S_write_stop _ ->
      fprintf fmt "()"
| ( S_seq(S_skip,s) 
  | S_seq(s,S_skip) ) -> 
      pp_s typing_env externals ~st fmt s
| S_seq(s1,s2) ->
    fprintf fmt "@[<v>%a;@,%a@]" 
      (pp_s typing_env externals ~st) s1 
      (pp_s typing_env externals ~st) s2
| S_letIn(x,a,s) ->
    fprintf fmt "@[<v>%a;@,%a@]"
      (pp_a2 typing_env x externals) a 
      (pp_s typing_env externals ~st) s
| S_fsm(id,rdy,x,cp,ts,s) ->
     let (st2,_,_) = List.assoc id !List_machines.extra_machines in
     pp_fsm typing_env externals fmt
        ~state_var:st2 ~idle:cp ~rdy (id,ts,s)
| S_in_fsm(id,s) ->
     let (st2,_,_) = List.assoc id !List_machines.extra_machines in
     pp_s typing_env externals ~st:st2 fmt s
| S_call(op,a) ->
   fprintf fmt "%a" (pp_call externals typing_env None) (Runtime(op),a)
| S_external_run(f,id,res,rdy,a) ->
     fprintf fmt "%a := %a(true,%a);@,"
       pp_ident res
       pp_prog_name f
       (pp_a typing_env externals) a;
     fprintf fmt "%a := snd %a" pp_ident rdy pp_ident res

(** code generator for FSMs *)
and pp_fsm typing_env externals fmt ~state_var:st ~idle ~rdy (id,ts,s) =
  fprintf fmt "(match !%a with@," pp_ident st;
  List.iter (fun (x,s) ->
      fprintf fmt "@[<v 2>| %a ->@,@[<v 2> %a@,@]@]@,"
        pp_state x 
        (pp_s typing_env externals ~st) s) ts;
  fprintf fmt "@[<v 2>| %a -> @,@[<v 2>%a@,@]@]@,"
    pp_state idle 
    (pp_s typing_env externals ~st) s;
  fprintf fmt ")@]"

(* default value as bitvector where each bit is at '0' *)
let default_zero_value nbits =
  "\""^String.make nbits '0' ^"\"" ;;

let chop_size = function
| TSize n -> n
| _ -> 32 (* default size *)

(* default value according to the given type. *)
let rec default_zero t =
  match Fsm_typing.canon t with
  | TStatic{size;elem} -> 
      Printf.sprintf "(Array.make %d %s)"
        (chop_size size)
        (default_zero elem)
  | TVector{size;elem} -> 
      Printf.sprintf "(Array.make %d %s)"
        (chop_size size)
        (default_zero elem)
  | TBool -> "false"
  | TUnit -> "()"
  | TInt size -> 
     let n = chop_size size in
     if n > 63 then Prelude.Errors.error (fun fmt -> 
                      Format.fprintf fmt 
                        "@[<v>integer sizes in Eclat programs should be less than 64 @,\
                        \ for OCaml code generation (-ocaml). Find integer size %d. @,\
                        \ Note: it is a current limitation of the compiler.@]" n)
     else "0L"
  | TTuple ts -> 
      "("^ String.concat "," (List.map default_zero ts) ^")"
  | TString size ->
      Printf.sprintf "(String.make %d '0')"
        (chop_size size)
  | TAbstract(_,[],[]) -> "0L"
  | TAbstract(x,[size],[t]) -> 
      Printf.sprintf "(Array.make %d %s)"
        (chop_size size)
        (default_zero t)
  | TAbstract(x,_,_) ->
      (** assume the user provides a function [default_x : unit -> x] 
          for the given abstract type [x] *)
      Printf.sprintf "default_%s ()" x
  | TSize _ -> assert false (* already removed *)
  | TVar _ -> 
       (** replace unknown by a dummy constant 
           of the given type unknown (no polymorphism). *)
       Obj.magic "()"
  | TVect _ ->
      "Bitvector.dummy"

let type_state_var fmt state_var idle xs =
  fprintf fmt "type %s = %a " state_var pp_state idle;
  List.iter (fun q -> fprintf fmt " | %a" pp_state q) xs;
  fprintf fmt "\n"

let declare_state_var fmt state_var idle xs =
  fprintf fmt "let %s = ref %a in@," state_var pp_state idle

let pp_ty fmt t =
  match Fsm_typing.canon t with
  | TStatic{elem;size=TTuple ts} -> 
      fprintf fmt "array_value_%d" (size_ty elem);
      List.iter (fun tsize -> fprintf fmt "(0 to %d)" (size_ty tsize - 1)) ts;
  | TStatic{elem;size} -> fprintf fmt "array_value_%d(0 to %d)" (size_ty elem) (size_ty size - 1);
  | _ ->
      fprintf fmt "value(0 to %d)" (size_ty t-1)




module ArrayType = Map.Make(struct
    type t = int let compare = Stdlib.compare
  end)

let rec ocaml_type t =
  let gensym () = "v" ^ norm_ident @@ gensym () in
  match t with
  | TStatic{size=_;elem} -> "(" ^ ocaml_type elem ^ " array)"
  | TVector{size=_;elem} -> "(" ^ ocaml_type elem ^ " array)"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TInt _size -> "Int.t"
  | TTuple ts ->"("^ String.concat "*" (List.map ocaml_type ts) ^")"
  | TString _ -> "string"
  | TAbstract _ -> "'" ^ gensym ()
  | TSize _ -> assert false (* already removed *)
  | TVect _ -> "Bitvector.t"
  | _ -> "'" ^ gensym ()

let array_decl fmt x n default_value_pp =
  fprintf fmt "let %a = Array.make %d %a in\n" pp_ident x n default_value_pp ();
  fprintf fmt "let %a_lock : Lock.t = (Lock.init ()) in\n" pp_ident x;
  fprintf fmt "let %a_value = ref %a in\n" pp_ident x default_value_pp ()

let pp_component fmt ~vhdl_comment ~name ~externals ~state_var 
                     ~argument ~result ~idle ~rdy ~statics 
                     typing_env infos (ts,s) arg_list =

  fprintf fmt "open Runtime\n\n";
  fprintf fmt "type %s_t = " state_var;
  fprintf fmt "%a" pp_state idle;
  List.iter (fun (q,_) -> fprintf fmt " | %a" pp_state q) ts;
  fprintf fmt "\n\n";

  List.iter (fun (_,(sv,cp,xs)) -> type_state_var fmt sv cp xs) !List_machines.extra_machines;

  (* fprintf fmt "let %s = ref \"0\" ;;@," rdy;*)
  let hash_struct_decl = Hashtbl.create 10 in
  Hashtbl.iter (fun x t ->
    let t = Fsm_typing.canon t in
    match Hashtbl.find_opt hash_struct_decl t with
    | None -> Hashtbl.add hash_struct_decl t (SMap.singleton x (), "t_"^gensym ())
    | Some (xs,name) -> Hashtbl.replace hash_struct_decl t (SMap.add x () xs,name)
    ) typing_env;
  
  let argument = fst argument in
  let result = fst result in

  fprintf fmt "@,@[<v 2>let %a =@," pp_prog_name name;

  List.iter (fun (x,st) ->
    match st with
    | Static_array_of ty ->
          let ty_elem,sz = match Fsm_typing.canon ty with
                          | TStatic {elem;size=sz} -> elem,sz
                          | _ -> assert false (* error *) in
          let n = match Fsm_typing.canon sz with
                  | TSize n -> n
                  | _ ->  Prelude.Errors.error (fun fmt -> 
                            Format.fprintf fmt "unspecified size for array %s" x) in
          array_decl fmt x n ((fun fmt () -> fprintf fmt "%s" (default_zero ty_elem)))  
    | Static_array(c,n) ->
          array_decl fmt x n (fun fmt () -> pp_c fmt c)

    ) statics;


  declare_state_var fmt state_var idle (List.map fst ts);
  List.iter (fun (_,(sv,cp,xs)) -> declare_state_var fmt sv cp xs) !List_machines.extra_machines;


  Hashtbl.iter (fun t (xs,y) ->
    
       SMap.iter (fun x () -> 
          let ty = (Hashtbl.find typing_env x) in
          match Fsm_typing.canon ty with
          | TStatic _ -> ()
          | _ -> 
              if x <> argument 
              then fprintf fmt "@[let %a : %s ref = ref %s in@]@," 
                  pp_ident x 
                  (ocaml_type ty)
                 (default_zero ty)) xs
  ) hash_struct_decl;

  fprintf fmt "@[<v 2>fun arg ->@,let %a = ref arg in@," pp_ident argument;

  pp_fsm typing_env externals fmt ~state_var ~idle ~rdy (name,ts,s);

  fprintf fmt ";@,!%s@,@]@]@]" result;

  fprintf fmt "

let run n =
  let args_list = [|%a|] in
  let k = Array.length args_list - 1 in
  for i = 0 to n - 1 do
    ignore (%a (args_list.(min i k)))
  done;;
  " 
  (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") 
      (pp_a typing_env externals)) arg_list
  pp_prog_name name;

  fprintf fmt "

let () =
  Arg.parse [
    (\"-run\", Arg.Int (fun n -> run n), \"execute the given number of cycles\")
  ]
  (fun _ -> ()) \"Usage:\n  ./eclat file\"
;;
@."