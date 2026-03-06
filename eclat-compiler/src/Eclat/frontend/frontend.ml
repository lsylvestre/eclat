open Ast
open Prelude.Errors

let no_stdlib_flag = ref false

let main_function_undefined fmt main_name =
  Format.fprintf fmt "@[<v>The main function (%a) is not defined.@]"
    (emph_pp blue (fun fmt () -> Format.fprintf fmt "%s" main_name)) ()

(* read a sequence of phrases (each phrase finishes with `;;`)
   upto the character '#' marking end of the program 
 *)
let read_phrase () =
  (* phrases terminate with ";;" *)
  let rec loop ~first_call acc =
    let s = read_line () in
    let new_acc = if first_call then s 
                  else acc^"\n"^s in (* todo (improvment): use a buffer to avoid multiple concatenations *)
    if String.contains s '#' 
    then new_acc 
    else
    (* since the "end of phrase" marker is ";;", we detect the last occurring ';'
       of each line and we test if the previous caracter is also a ';' *) 
    match String.rindex_opt s ';' with
    | None -> loop ~first_call:false new_acc
    | Some n ->
       if n < String.length s && n >= 1 && s.[n-1] = ';' then new_acc else
       loop ~first_call:false new_acc
  in loop ~first_call:true ""
  (* todo: accept comments in the input *)

let syntax_error_handler f lexbuf =
    try f(lexbuf)
    with Parser.Error -> 
           Prelude.Errors.syntax_error (Lexer.get_loc lexbuf)

let smap_operators_of_list rev_l =
  List.fold_right (fun (x,((_,(_,_,_,loc)) as v)) m -> 
     (match SMap.find_opt x m with
     | Some (_,(_,_,_,last_loc)) -> 
         let open Prelude.Errors in 
         warning ~loc:last_loc (fun fmt -> 
            Format.fprintf fmt "Redefinition of operator %s\nPrevious declaration at location: %a\n" 
              x
              (emph_pp blue pp_loc) loc)
     | None -> ());
     SMap.add x v m) rev_l SMap.empty


let check_externals_nodup rev_l =
  ( List.fold_right (fun (x,((_,_,loc) as v)) m -> 
     (match SMap.find_opt x m with
     | Some (_,_,last_loc) -> 
         let open Prelude.Errors in 
         warning ~loc:last_loc (fun fmt -> 
            Format.fprintf fmt "Redefinition of external function %s\nPrevious declaration at location: %a\n" 
              (String.lowercase_ascii x)
              (emph_pp blue pp_loc) loc)
     | None -> ());
     SMap.add x v m) rev_l SMap.empty ) |> ignore


let frontend ~(inputs : string list) repl ?(when_repl=(fun _ ~genv:_ _ _ -> ())) 
             ?(relax=false) main_name str_arg : pi * e list =
  let (ess_from_files, 
       gss_from_files, 
       tss_from_files, 
       dss_from_files) =
    Prelude.map_split4 (fun path ->
                let ic = open_in path in
                begin try
                      Current_filename.current_file_name := path;
                      let lexbuf = Lexing.from_channel ic in
                      syntax_error_handler (fun lexbuf ->
                      let exts,gs,ts,ds = Parser.pi Lexer.token lexbuf in
                      close_in ic;
                      exts,gs,ts,ds) lexbuf
                    with excp ->
                      close_in_noerr ic;
                      (match excp with
                      | Parser.Error ->
                          error (fun fmt ->
                            Format.fprintf fmt "Syntax error (file %s)" path)
                      | _ -> ());
                      raise excp 
                end
               ) inputs in
  let (exts_from_files,
       gs_from_files,
       ts_from_files,
       ds_from_files) = let e1, e2 = List.split ess_from_files in
                        (List.concat e1,List.concat e2), 
                         List.concat gss_from_files, 
                         List.concat tss_from_files, 
                         List.concat dss_from_files 
  in
  let (exts, gs, ts, ds) = 
         (if repl || List.length inputs < (if !no_stdlib_flag then 1 else 2) then
            (
             Printf.printf "=== eclat toploop ===.\nEnter phrases (separated by ';;') then compile (or run) with ``#q.''\n";
             flush stdout;
             let rec loop i (exts1,exts2) gs ts ds =
               Current_filename.current_file_name := ("%stdin>"^string_of_int i);
               Printf.printf "\n> ";
               let l = read_phrase () in
               if String.contains l '#' then 
                 (if List.exists (fun ((p,_),_) -> SMap.mem main_name @@ vars_of_p p) ds then (exts1,exts2),gs,ts,ds else
                    (main_function_undefined Format.std_formatter main_name;
                     emph_pp purple (fun fmt () -> 
                                          Format.fprintf fmt " Please continue, or quit with ^C.") 
                                    Format.std_formatter ();
                     Format.print_flush ();
                     loop (i+1) (exts1,exts2) gs ts ds))
               else
               try
                 (let lexbuf = (Lexing.from_string l) in
                  caml_error_handler ~on_error:(fun _ ->
                      (** given the declarations [ds],
                          when parsing [ds'] new declarations,
                          if one of the [ds'] is ill-typed, 
                          the toplevel execution continue with [ds]. 

                          note: because the Eclat typer relies on destructive unification,
                          type error may be an effect on the remaining toplevel execution.
                        **)
                      Format.print_flush ();
                      loop (i+1) (exts1,exts2) gs ts ds)
                  (fun () -> 
                     syntax_error_handler (fun lexbuf ->               
                       let (exts1',exts2'),gs',ts',ds' = Parser.pi Lexer.token lexbuf in
                       let exts1'' = exts1@exts1' in
                       let exts2'' = exts2@exts2' in
                       let gs'' = gs@gs' in
                       let ts'' = ts@ts' in
                       let ds'' = ds@ds' in
                       check_externals_nodup exts1'';
                       let genv = {abstract_types=create_abstract_type_smap ();
                                   statics=gs'';
                                   operators=smap_operators_of_list exts2'';
                                   externals=exts1'';
                                   sums=ts'';
                                   record_fields=SMap.empty} in
                       List.iter (when_repl gs'' ~genv true) ds';
                       loop (i+1) (exts1'', exts2'') gs'' ts'' ds'') lexbuf)
                  ())
                with End_of_file -> (exts1,exts2),gs,ts,ds
             in
             let genv = {abstract_types=create_abstract_type_smap ();
                         statics=List.rev @@ gs_from_files;
                         operators=smap_operators_of_list (snd exts_from_files);
                         externals=(fst exts_from_files);
                         sums=List.rev @@ ts_from_files;
                         record_fields=SMap.empty } in
             List.iter (when_repl genv.statics ~genv false) ds_from_files;
             loop 1
                  exts_from_files
                  genv.statics
                  genv.sums
                  ds_from_files)
        else exts_from_files,gs_from_files,ts_from_files,ds_from_files) in

  let values_list =
    Current_filename.current_file_name := "%command-line-argument";
    let lexbuf = Lexing.from_string str_arg in
    syntax_error_handler (fun lexbuf ->
    Parser.arguments_eof Lexer.token lexbuf)
    lexbuf
  in
  
  (* check if the entry point is defined *)
  if List.exists (fun ((p,_),loc) -> 
                   SMap.mem main_name (vars_of_p p)) ds then () 
  else error (fun fmt -> main_function_undefined fmt main_name);
  
  let entry_point = E_var main_name in
  let main = List.fold_right (fun ((p,e1),_) e ->
                              E_letIn(p,Types.new_ty_unknown(),e1,e)
                        ) ds (let y = gensym () in 
                              E_fun (P_var y,(Types.new_ty_unknown(),
                                              Types.new_tyB_unknown()), 
                                       E_app(entry_point,E_var y))) 
  in
  check_externals_nodup (fst exts);

  (* return both parsed program and its inputs *)
  ({genv={abstract_types=create_abstract_type_smap ();
          statics=gs;
          operators=smap_operators_of_list (snd exts);
          externals=(fst exts);sums=ts;
          record_fields=SMap.empty};
         main}, values_list)

