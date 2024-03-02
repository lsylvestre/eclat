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
       "var", VAR;
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
       "type", TYPE;
       "par",  PAR;
       "spawn", PAR;
       "generate", GENERATE;
       "to", TO;
       "do", DO;
       "done", DONE;
       "for", FOR;
       "ref", REF
     ]


  let nested_comment_depth = ref 0

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
| '('                 { LPAREN }
| ')'                 { RPAREN }
| '['                 { LBRACKET }
| ']'                 { RBRACKET }
| "#exit"             { EXIT_REPL }
| "#[|"               { SHARP_PIPE_LBRACKET }
| "|]"                { PIPE_RBRACKET }
| ','                 { COMMA }
| ':'                 { COL }
| '''                 { QUOTE }
| "<-"                { LEFT_ARROW }
| "->"                { RIGHT_ARROW }
| "=>"                { IMPLY }
| "|"                 { PIPE }
| "||"                { PIPE_PIPE }
| "|,|"               { PIPE_COMMA_PIPE }
| "~"                 { TILDE }
| "/\\"               { BIG_LAMBDA }
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
| ['\n' ]             { (Lexing.new_line lexbuf) ; (token lexbuf) }
| [' ' '\t']          { token lexbuf }
| ';'                 { SEMI }
| '.'                 { DOT }
| ";;"                { SEMI_SEMI }
| eof | "%eof"        { EOF }
| "(*"                { incr nested_comment_depth; comment lexbuf }
| _  as lxm           { failwith (Printf.sprintf "Unexpected character: %c"  lxm) }

and comment = parse
| "(*"                { incr nested_comment_depth; comment lexbuf }
| ['\n' '\r' ]        { (Lexing.new_line lexbuf) ; (comment lexbuf) }
| "*)"                { decr nested_comment_depth;
                        if !nested_comment_depth <= 0 (* comments can be nested *)
                        then token lexbuf
                        else comment lexbuf }
| _                   { comment lexbuf }
