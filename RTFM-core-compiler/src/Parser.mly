(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/Parser.mly *)

%token <string> ID
%token <int>    INTVAL
%token <string> STRINGVAL
%token <string> CCODE
%token <string> PARAMS
%token MODULE INCLUDE ISR TASK FUNC RESET SYNC ASYNC PEND AFTER PRIO CLAIM SC LCP RCP EOF

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
  | ASYNC after prio ID PARAMS SC           { Async ($2, $3, $4, $5) }             
  | SYNC ID PARAMS SC                       { Sync ($2, $3) }
  
after:
  | AFTER INTVAL                            { $2 }
  |                                         { 0 }
     
prio:
  | PRIO INTVAL                             { $2 }
  |                                         { 0 }
  
