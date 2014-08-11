(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/Lexer.mll *)

{    
 open Parser   
 open Lexing
 open Common
 open Error  
   
 exception SyntaxError of string 
}

(* regular expressions (regexps) *)
let white   = [' ' '\t']+
let newline = '\n'
let cite    = '\"'
let str     = [^ '"']* 
let id      = ['A'-'Z' 'a'-'z' '_']['0'-'9' 'A'-'Z' 'a'-'z' '_']*  
let digits  = ['0'-'9']+
let enter_c = "#>"
let exit_c  = "<#"
let params  = ( [^ '*' ')'] [^ ')']* )?   
  
(* lexing rules *)  
rule lex = parse
  | "module"             { MODULE }                             (* module system related *)
  | "include"            { INCLUDE }
   
  | "pend"               { PEND }                               (* statements *)
  | "sync"               { SYNC }
  | "async"              { ASYNC }
  | "claim"              { CLAIM }
  
  | "ISR"                { ISR }                                (* top level *)
  | "Task"               { TASK }
  | "Func"               { FUNC }
  | "Reset"              { RESET }
  
  | '{'                  { LCP }                                (* delimeters *)
  | '}'                  { RCP }
  | ';'                  { SC } 
  
  | digits as i          { INTVAL (int_of_string i) }           (* literals/values *)
  | cite (str as s) cite { STRINGVAL (s) }
  
  | enter_c              { set_info lexbuf; c (Buffer.create 100) lexbuf }
  | id as s              { ID (s) }
  | white                { lex lexbuf }                         (* white space *)
  | newline              { next_line lexbuf; lex lexbuf }       (* new line *)
  | "//"                 { set_info lexbuf; comment lexbuf }    (* single line comment *) 
  | "(*"                 { set_info lexbuf; comments 0 lexbuf } (* nested comment *) 
  | '('(params as p) ')' { PARAMS (p) }                         (* must come after comment *)
  | eof                  { EOF }
  | _                    { raise Parser.Error }
    
and comment = parse
  | newline              { next_line lexbuf; lex lexbuf }
  | eof                  { EOF }                                (* // at last line *)
  | _                    { comment lexbuf; }    
    
and comments level = parse
  | "*)"                 { if level = 0 then lex lexbuf else comments (level-1) lexbuf }
  | "(*"                 { comments (level+1) lexbuf }
  | newline              { next_line lexbuf; comments level lexbuf }
  | _                    { comments level lexbuf }
  | eof                  { bol lexbuf; raise (SyntaxError("Comment not closed.")) }
    
and c buf = parse
  | exit_c               { CCODE (Buffer.contents buf) }
  | newline              { next_line lexbuf; Buffer.add_string buf (Lexing.lexeme lexbuf); c buf lexbuf }
  | enter_c              { raise (SyntaxError("C statement not closed.")) }       
  | _                    { Buffer.add_string buf (Lexing.lexeme lexbuf); c buf lexbuf }
  | eof                  { bol lexbuf; raise (SyntaxError("C statement not closed.")) }    
                         
    
    
