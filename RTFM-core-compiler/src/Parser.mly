(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/Parser.mly *)

%token <string> ID
%token <int>    INTVAL
%token <int64>  TIME
%token <string> STRINGVAL
%token <string> CCODE
%token <string> PARAMS
%token MODULE INCLUDE AS ISR TASK FUNC RESET IDLE SYNC ASYNC HALT PEND AFTER BEFORE ABORT (* PRIO *) 
%token CLAIM RETURN BREAK CONTINUE GOTO LABEL COLON SC ASSIGN STATE LCP RCP EOF

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
  | INCLUDE STRINGVAL AS ID                 { Include($2, $4) }
  | INCLUDE STRINGVAL                       { Include($2, "") }

top:
  | CCODE                                   { TopC ($1) }
  | ISR before ID LCP stmt* RCP             { Isr ($2, $3, $5) }
  | FUNC ID ID PARAMS LCP stmt* RCP         { FuncDef ($2, $3, $4, $6) }
  | TASK ID PARAMS LCP stmt* RCP            { TaskDef ($2, $3, $5) }
  | RESET LCP stmt* RCP                     { Reset ($3) }
  | IDLE LCP stmt* RCP                      { Idle ($3) }

stmt:
  | CCODE                                   { StmtC ($1) }
  | STATE                                   { State ("") }
  | CLAIM ID LCP stmt* RCP                  { Claim ($2, $4) }
  | CLAIM LCP stmt* RCP                     { Claim ("", $3) }
  | PEND before ID PARAMS SC                { Pend ($2, $3, $4) }
  | assign ASYNC after before ID PARAMS SC  { Async ($1, $3, $4, $5, Some $6) }
  | assign ASYNC after before ID SC         { Async ($1, $3, $4, $5, None) }
  | SYNC ID PARAMS SC                       { Sync ($2, $3) }
  | HALT PARAMS SC                          { Halt ($2) }
  | ABORT PARAMS SC                         { Abort ($2) }
  | RETURN CCODE SC                         { Return ($2, []) }
  | RETURN SC                               { Return ("", []) }
  | BREAK SC                                { Break (1, []) }
  | BREAK INTVAL SC                         { Break ($2, []) }
  | CONTINUE SC                             { Continue (1, []) }
  | CONTINUE INTVAL SC                      { Continue ($2, []) } 
  | GOTO ID SC                              { Goto ($2, 1, []) }
  | GOTO INTVAL ID SC                       { Goto ($3, $2, []) }
  | LABEL ID COLON                          { Label ($2, []) } 
  

assign:
  | ID ASSIGN                               { Some $1 }
  |                                         { None }

after:
  | AFTER time                              { $2 }
  |                                         { Time.USec (0L) }

before:
  | BEFORE time                             { $2 }
  |                                         { Time.Undef }

time:
  | TIME                                    { Time.USec($1) }
  | INTVAL                                  { Time.USec(Int64.of_int $1) }

(*
prio:
  | PRIO INTVAL                             { $2 }
  |                                         { 0 }
*)
