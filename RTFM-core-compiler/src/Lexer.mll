(* RTFM-core/Lexer.mll *)
{
 (* tokens and dependenceies *)   
 type token =
   | MODULE
   | INCLUDE
   | PEND
   | AFTER
   | SYNC
   | ENABLE
   | CLAIM
   | HALT
   | ISR
   | TASK
   | FUNC
   | RESET
   | ID of string
   | INTVAL of int
   | BOOLVAL of bool
   | CCODE of string
   | PARAMS of string
   | STRINGVAL of string
   | SC
   | LCP
   | RCP
   | EOF   
     
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
  | "module"             { MODULE }
  | "include"            { INCLUDE }
  | "pend"               { PEND }
  | "after"              { AFTER }
  | "sync"               { SYNC }
  | "enable"             { ENABLE }
  | "claim"              { CLAIM }
  | "halt"               { HALT }
  | "ISR"                { ISR }
  | "Task"               { TASK }
  | "Func"               { FUNC }
  | "Reset"              { RESET }
  | '{'                  { LCP }
  | '}'                  { RCP }
  | ';'                  { SC } 
  | "true"               { BOOLVAL (true) }
  | "false"              { BOOLVAL (false) }
  | digits as i          { INTVAL (int_of_string i) }
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
                         
    
    