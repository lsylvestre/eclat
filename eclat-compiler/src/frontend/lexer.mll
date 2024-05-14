{
  open Parser
  exception Eof

  let hashtbl_of_list_assoc l =
    let h = Hashtbl.create (List.length l) in
    List.iter (fun (k,v) -> Hashtbl.add h k v) l;
    h

  let keywords =
    hashtbl_of_list_assoc @@
     [ "let",   LET;
       "node",  NODE;
       "rec",   REC;
       "fun",   FUN;
       "register", REGISTER;
       "reg",   REGISTER;
       "exec",  EXEC;
       "and",   AND;
       "in",    IN;
       "if",    IF;
       "of",    OF;
       "then",  THEN;
       "else",  ELSE;
       "fix",   FIX;
       "true",  BOOL_LIT true;
       "false", BOOL_LIT false;
       "not",   NOT;
       "or",    OR;
       "mod",   MOD;
       "last",  LAST;
       "default",  DEFAULT;
       "static", STATIC;
       "match", MATCH;
       "with", WITH;
       "end", END;
       "xor", XOR;
       "land", LAND;
       "lor", LOR;
       "lxor", LXOR;
       "lsl", LSL;
       "lsr", LSR;
       "asr", ASR;
       "resize_int", RESIZE_INT;
       "tuple_of_int",TUPLE_OF_INT;
       "int_of_tuple",INT_OF_TUPLE;
       "type", TYPE;
       
       "to", TO;
       "do", DO;
       "done", DONE;
       "for", FOR;
       "ref", REF;
       "macro_for", MACRO_FOR;
       "parfor", PARFOR;
       "macro_generate", MACRO_GENERATE;
       "immediate", IMMEDIATE;
       "array_create",ARRAY_CREATE;
       "init_tuple",INIT_TUPLE;
       "init_int",INIT_INT;
       "array_length", ARRAY_LENGTH;
       "unroll", UNROLL;
       "set", SET;
       "create", CREATE;
       "length", LENGTH;
       "size_create", SIZE_CREATE;
       "vect_create", VECTOR_CREATE;
       "vect_mapi", VECTOR_MAPI;
       "int_mapi", INT_MAPI
     ]


  let nested_comment_depth = ref 0

let paren_lvl = ref 0 
let brack_lvl = ref 0 

let get_loc lexbuf =
  let startp = Lexing.lexeme_start_p lexbuf in
  let endp = Lexing.lexeme_end_p lexbuf in
  (!Current_filename.current_file_name,(startp,endp))

}

(* let tvar_ident = [''']['a'-'z'] ['a'-'z''A'-'Z''0'-'9''_''A'-'Z'''']* *)
let ident = ['a'-'z''_'] ['a'-'z''A'-'Z''0'-'9''_''A'-'Z']*
let up_ident = ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_''A'-'Z']*
let tvar_ident = ['''] ident

rule token = parse
| ident as id         { try Hashtbl.find keywords id with
                        | Not_found -> IDENT id }
| up_ident as id      { UP_IDENT id }
| tvar_ident as lxm   { TVAR_IDENT lxm }
| '('                 { incr paren_lvl; LPAREN }
| ')'                 { if !paren_lvl <= 0 then
                           Prelude.Errors.raise_error ~loc:(get_loc lexbuf)
                              ~msg:"unbalanced parenthesis" ()
                        else ();
                        decr paren_lvl; RPAREN }
| '['                 { incr brack_lvl; LBRACKET }
| ']'                 { if !brack_lvl <= 0 then
                           Prelude.Errors.raise_error ~loc:(get_loc lexbuf)
                              ~msg:"unbalanced bracket" ()
                        else ();
                        decr brack_lvl; RBRACKET }
| "#exit"             { EXIT_REPL }
| "#[|"               { SHARP_PIPE_LBRACKET }
| "|]"                { PIPE_RBRACKET }
| "[|"                { LBRACKET_PIPE }
| '@'                 { AT }
| "@@"                { AT_AT }
| ','                 { COMMA }
| ':'                 { COL }
| '''                 { QUOTE }
| "<-"                { LEFT_ARROW }
| "->"                { RIGHT_ARROW }
| "=>"                { IMPLY }
| "|"                 { PIPE }
| "||"                { PIPE_PIPE }
| "|,|"               { PIPE_COMMA_PIPE }
| ['0'-'9']+ as s
| ('0'('b'|'x')['0'-'9']['0'-'9''_']*) as s { INT_LIT (int_of_string s) }
| "+"                 { PLUS }
| "-"                 { MINUS }
| "*"                 { TIMES }
| "/"                 { DIV }
| "<"                 { LT }
| "<="                { LE }
| ">"                 { GT }
| ">="                { GE }
| "=="                { EQ_EQ }
| "="                 { EQ }
| ":="                { COL_EQ }
| "!"                 { BANG }
| "!=" | "<>"         { NEQ }
| "&&"                { AMP_AMP }
| "&"                 { AMP }
| "^"                 { HAT }
| ".length"           { DOT_LENGTH }
| ['"']([^'"']* as s)['"'] { STRING_LIT s }
| ['\n' '\r']         { (Lexing.new_line lexbuf) ; (token lexbuf) }
| [' ' '\t']          { token lexbuf }
| ';'                 { SEMI }
| '.'                 { DOT }
| ";;"                { SEMI_SEMI }
| eof | "%eof"        { EOF }
| "(*"                { incr nested_comment_depth; comment lexbuf }
| _  as lxm           { Prelude.Errors.raise_error ~loc:(get_loc lexbuf)
                              ~msg:(Printf.sprintf "Unexpected character: %c"  lxm) () }

and comment = parse
| "(*"                { incr nested_comment_depth; comment lexbuf }
| ['\n' '\r' ]        { (Lexing.new_line lexbuf) ; (comment lexbuf) }
| "*)"                { decr nested_comment_depth;
                        if !nested_comment_depth <= 0 (* comments can be nested *)
                        then token lexbuf
                        else comment lexbuf }
| eof                 { Prelude.Errors.raise_error ~loc:(get_loc lexbuf)
                              ~msg:("unclosed comment at the end of file"^ !Current_filename.current_file_name) () }
| _                   { comment lexbuf }
