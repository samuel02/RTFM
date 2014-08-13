(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/Parser.mly *)

%token <string> ID
%token <int>    INTVAL
%token <string> STRINGVAL
%token <string> CCODE
%token <string> PARAMS
%token MODULE INCLUDE ISR TASK FUNC RESET SYNC ASYNC PEND AFTER CLAIM SC LCP RCP EOF

%{
  open AST 
%}

%start prog

%type <AST.prog option> prog 

%%

prog:
  | MODULE ID use* top* EOF                 { Some (Prog ($2,$3, $4)) }
  | use* top* EOF                           { Some (Prog ("",$1, $2)) }
  
use:
  | INCLUDE STRINGVAL                       { $2 }
    
top:
  | CCODE                                   { TopC ($1) }
  | ISR INTVAL ID LCP stmt* RCP             { Isr ($2, $3, $5) }
  | FUNC ID ID PARAMS LCP stmt* RCP         { Func ($2, $3, $4, $6) } 
  | TASK ID PARAMS LCP stmt* RCP            { TaskDef ($2, $3, $5) }
  | RESET LCP stmt* RCP                     { Reset ($3) }
              
stmt:
  | CCODE                                   { ClaimC ($1) }
  | CLAIM ID LCP stmt* RCP                  { Claim ($2, $4) }
  | PEND ID SC                              { Pend (0, $2) }
  | PEND AFTER INTVAL ID SC                 { Pend ($3, $4) }              
  | ASYNC INTVAL ID PARAMS SC               { Async (0, $2, $3, $4) }             
  | ASYNC AFTER INTVAL INTVAL ID PARAMS SC  { Async ($3, $4, $5, $6) }             
  | SYNC ID PARAMS SC                       { Sync ($2, $3) }
  
