(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/Parser.mly *)

%token <string> ID
%token <int>    INTVAL
%token <string> STRINGVAL
%token <string> CCODE
%token <string> PARAMS
%token MODULE INCLUDE ISR TASK FUNC RESET IDLE SYNC ASYNC HALT PEND AFTER BEFORE ABORT (* PRIO *) CLAIM USEC MSEC SEC SC ASSIGN LCP RCP EOF

%{
  open AST 
  open Common
%}

%start prog

%type <AST.prog option> prog 

%%

prog:
  | mname use* top* EOF                     { Some (Prog ($1, $2, $3)) }

mname:
  | MODULE ID                               { $2 }
  |                                         { "" }
      
use:
  | INCLUDE STRINGVAL                       { $2 }
    
top:
  | CCODE                                   { TopC ($1) }
  | ISR INTVAL ID LCP stmt* RCP             { Isr ($2, $3, $5) }
  | FUNC ID ID PARAMS LCP stmt* RCP         { FuncDef ($2, $3, $4, $6) } 
  | TASK ID PARAMS LCP stmt* RCP            { TaskDef ($2, $3, $5) }
  | RESET LCP stmt* RCP                     { Reset ($3) }
  | IDLE LCP stmt* RCP                      { Idle ($3) }
              
stmt:
  | CCODE                                   { ClaimC ($1) }
  | CLAIM ID LCP stmt* RCP                  { Claim ($2, $4) }
  | PEND before ID PARAMS SC                { Pend ($2, $3, $4) }   
  | assign ASYNC after before ID PARAMS SC  { Async ($1, $3, $4, $5, $6) }             
  | SYNC ID PARAMS SC                       { Sync ($2, $3) }
  | HALT PARAMS SC                          { Halt ($2) }
  | ABORT PARAMS SC                         { Abort ($2) }

assign:
  | ID ASSIGN                               { Some $1 }
  |                                         { None }

after:
  | AFTER time                              { $2 }
  |                                         { Usec (0) }

before:
  | BEFORE time                             { $2 }
  |                                         { Usec (0) }

time:
  | INTVAL USEC                             { Usec($1) }                                                                       
  | INTVAL MSEC                             { Msec($1) }
  | INTVAL SEC                              { Sec($1) } 
  | INTVAL                                  { Usec($1) }

(*
prio:
  | PRIO INTVAL                             { $2 }
  |                                         { 0 }
*)  
