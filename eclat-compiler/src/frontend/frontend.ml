open Ast
open Prelude.Errors

let read_phrase () =
  (* phrases terminate with ";;" *)
  let rec loop acc =
    let s = read_line () in
    let new_acc = acc^"\n"^s in (* todo (improvment): use a buffer to avoid multiple concatenation *)
    if String.contains s '.' 
    then new_acc 
    else
    (* since the "end of phrase" marker is ";;", we detect the last occurring ';'
       of each line and we test if the previous caracter is also a ';' *) 
    match String.rindex_opt s ';' with
    | None -> loop new_acc
    | Some n ->
       if n < String.length s && s.[n-1] = ';' then new_acc else
       loop new_acc
  in loop ""
  (* todo: accept comments in the input *)

let syntax_error_handler f lexbuf =
    try f(lexbuf)
    with Parser.Error -> 
           Prelude.Errors.syntax_error (Lexer.get_loc lexbuf)

let frontend ~(inputs : string list) repl ?(when_repl=(fun _ _ _ _ _ -> ())) 
             ?(relax=false) main str_arg : pi * e list =
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
         (if repl || List.length inputs < 2 then
            (* let () = List.iter (when_repl ([],[]) [] []) ds_from_files in *)
            (Current_filename.current_file_name := "%stdin";
             Printf.printf "=== eclat toploop ===.\nEnter phrases (separated by ';;') then compile (or run) with ``#q.''\n";
             flush stdout;
             let rec loop (exts1,exts2) gs ts ds =
               Printf.printf "\n> ";
               let l = read_phrase () in
               if String.contains l '#' then 
                 (if List.exists (fun ((p,_),_) -> SMap.mem main @@ vars_of_p p) ds then (exts1,exts2),gs,ts,ds else
                    (Format.fprintf Format.std_formatter "entry point *%s* not set\n" main;
                     Format.print_flush ();
                     loop (exts1,exts2) gs ts ds))
               else
               try
                 (let lexbuf = (Lexing.from_string l) in
                  let (exts1',exts2'),gs',ts',ds' = Parser.pi Lexer.token lexbuf in
                  caml_error_handler ~on_error:(fun _ ->
                      Format.print_flush ();
                      let () = List.iter (when_repl (exts1,exts2) gs ts false) ds in
                      loop (exts1,exts2) gs ts ds)
                  (fun () -> let exts1'' = exts1@exts1' in
                             let exts2'' = exts2@exts2' in
                             let gs'' = gs@gs' in
                             let ts'' = ts@ts' in
                             let ds'' = ds@ds' in
                       let w = (when_repl (exts1'', exts2'') gs'' ts'') in
                       List.iter (w false) ds;
                       List.iter (w true) ds'; 
                       loop (exts1'', exts2'') gs'' ts'' ds'')
                  ())
                with End_of_file -> (exts1,exts2),gs,ts,ds
             in
             loop ((fun (ext1,ext2) -> List.rev ext1, List.rev ext2) exts_from_files)
                  (List.rev @@ gs_from_files) 
                  (List.rev @@ ts_from_files) 
                  (ds_from_files))
        else exts_from_files,gs_from_files,ts_from_files,ds_from_files) in

  let values_list =

    String.split_on_char ';' str_arg |>
    (function | [""] -> [] | l -> l) |>
    List.mapi (fun i s ->
        Current_filename.current_file_name := "%command-line-argument-"^string_of_int (i+1)^" (input: "^s^")";
        let lexbuf = Lexing.from_string s in
        syntax_error_handler (fun lexbuf ->
        Parser.exp_eof Lexer.token lexbuf)
        lexbuf)
  in
  let entry_point = (* if relax then (E_var main) else (Ast.ty_annot ~ty:(Ast_mk.fresh_node ())*) (E_var main) in
  let ds = List.concat @@
           List.map (function ((p,e),loc) ->
                       try Pattern.bindings p e |> SMap.bindings with
                       | Pattern.CannotMatch _ ->
                           error ~loc (fun fmt ->
                                  Format.fprintf fmt
                                    "@[<v>This global pattern does not match statically the right-hand side.@]")) ds in
  let main = List.fold_right (fun (x,v) e -> E_letIn(P_var x,Types.new_ty_unknown(),v,e)) ds (let y = gensym () in 
      E_fun (P_var y,(Types.new_ty_unknown(),Types.new_tyB_unknown()), E_app(entry_point,E_var y))) in


  (*

  (** check that the given inputs are combinational *)
  List.iteri (fun i a ->
    if Combinatorial.combinatorial a then ()
    else error (fun fmt ->
                  Format.fprintf fmt
                    "@[<v>Input %d should be combinatorial, but I found:@,%a@,@]" i
                       (emph_pp purple Ast_pprint.pp_exp) a)) values_list;

  *)

  (* return both parsed program and its inputs *)
  ({statics=gs_from_files;externals=exts;sums=ts_from_files;main}, values_list)

