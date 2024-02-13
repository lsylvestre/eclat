(** error handling *)

type ident = string

type loc = filename * (Lexing.position * Lexing.position)
and filename = string

let dloc : loc = "",(Lexing.dummy_pos, Lexing.dummy_pos)

module Errors = struct

  exception Caml_error (** used to exit after dislaying error messages *)

  let caml_error_handler ?(on_error=fun _ -> exit 0) (f : 'a -> 'b) () : 'b =
    try f () with
    | Caml_error -> on_error ()

  open Format

  (** colors for pretty-printing *)

  let reset = "\027[m"
  let bold = "\027[1m"
  let red = "\027[31m"
  let green = "\027[33m"
  let blue = "\027[34m"
  let purple = "\027[35m"

  let emph_pp color pp fmt a =
       fprintf fmt "%s%s%a%s" bold color pp a reset

  let emph color fmt s =
     emph_pp color (fun fmt s -> fprintf fmt "%s" s) fmt s

  let pp_loc fmt (filename,(Lexing.{pos_lnum=l1;pos_cnum=c1;pos_bol=b1},
                            Lexing.{pos_lnum=l2;pos_cnum=c2;pos_bol=b2})) =
    if l1 = l2
    then fprintf fmt "file %s, line %d, characters %d-%d" filename l1 (c1-b1) (c2-b2)
    else fprintf fmt "file %s, from line %d, characters %d, to line %d characters %d"
           filename l1 (c1-b1) l2 (c2-b2)

  let error ?(error_kw="Error") ?loc pp =
    let fmt = err_formatter in
    (match loc with
    | None -> ()
    | Some loc -> fprintf fmt "%s%a%s:\n" bold pp_loc loc reset);
    fprintf fmt "%s%s%s%s: " bold red error_kw reset;
    pp fmt;
    fprintf fmt "@.";
    raise Caml_error

  (** emit a warning *)

  let warning ?(warning_kw="Warning") ?loc pp : unit =
    let fmt = err_formatter in
    (match loc with
    | None -> ()
    | Some loc -> fprintf fmt "%s%a%s:\n" bold pp_loc loc reset);
    fprintf fmt "%s%s%s%s: " bold purple warning_kw reset;
    pp fmt


  (** emit syntax-error and raise [Caml_error] *)

  let syntax_error ?msg loc =
    let pp fmt =
      fprintf fmt "syntax error";
      match msg with
      | None -> ()
      | Some s ->
         fprintf fmt " %s%s(%s)%s" bold blue s reset in
    error ~loc pp

  (** emit an error message and raise [Caml_error] *)

  let raise_error ?loc ?msg () =
    let pp fmt =
      match msg with
      | None -> ()
      | Some s ->
         fprintf fmt " %s(%s)%s" bold s reset in
    error ?loc pp

end

let map_split3 f l =
  let rec aux l1 l2 l3 l =
  match l with
  | [] -> (List.rev l1, List.rev l2, List.rev l3)
  | x::xs -> let (x1,x2,x3) = f x in aux (x1::l1) (x2::l2) (x3::l3) xs
in aux [] [] [] l
