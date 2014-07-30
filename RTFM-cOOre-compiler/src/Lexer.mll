{
 (* tokens and dependenceies *)   
 type token =
  | CLASS
	| TASK
	| PEND
  | RETURN
     
  | ID of string
  | INTVAL of int
  | BOOLVAL of bool
  | CHARVAL of char
   
  | COMMA
  | INT 
  | CHAR 
  | BOOL
  | VOID
  | ASSIGN  
  | LT 
  | GT
  | SC
  | LCP
  | RCP
  | DOT
  
  | EOF   
     
 open Parser   
 open Lexing
 open Common
 open Error  
   
 exception SyntaxError of string 
}

(* reg exps *)
let white 	= [' ' '\t']+
let newline = '\n'
let cite    = '\''
let char    = [^ '\'']   
let id 		  = ['A'-'Z' 'a'-'z' '_']['0'-'9' 'A'-'Z' 'a'-'z' '_']*  
let digits  = ['0'-'9']+

(* lexing rules *)  
rule lex = parse
  | "class"               { CLASS }
	| "task"								{ TASK }
	| "pend"                { PEND }
  | "return"              { RETURN }
                          
  | "int"                 { INT }
  | "char"                { CHAR }
  | "bool"                { BOOL }
  | "void"                { VOID }
                          
  | ','                   { COMMA }
  | ":="                  { ASSIGN }
  | '<'                   { LT }
  | '>'                   { GT }
  | '{'			              { LCP }
  | '}'			              { RCP }
  | '('		                { LP }
  | ')'		                { RP }
  | '.'                   { DOT }
  | ';'			              { SC } 
    
  | "true"		            { BOOLVAL (true) }
  | "false"		            { BOOLVAL (false) }
  | digits as i	          { INTVAL (int_of_string i) }
  | cite (char as c) cite { CHARVAL (c) }                        (* does not handle escpaded characters *)
  
  | id as s               { ID (s) }
  | white		              { lex lexbuf } 									       (* white space *)
  | newline               { next_line lexbuf; lex lexbuf }			 (* new line *)
  | "//" 		              { set_info lexbuf; comment lexbuf } 	 (* single line comment *) 
  | "(*"		              { set_info lexbuf; comments 0 lexbuf } (* nested comment *) 
                          
  | eof                   { EOF }
  | _			                { raise Parser.Error }
    
and comment = parse
  | newline 	            { next_line lexbuf; lex lexbuf }
  | eof			              { EOF } 										           (* // at last line *)
  | _   		              { comment lexbuf; }    
    
and comments level = parse
  | "*)"		              { if level = 0 then lex lexbuf else comments (level-1) lexbuf }
  | "(*"		              { comments (level+1) lexbuf }
  | newline               { next_line lexbuf; comments level lexbuf }
  | _											{ comments level lexbuf }
  | eof			              { bol lexbuf; raise (SyntaxError("Comment not closed.")) }
    
