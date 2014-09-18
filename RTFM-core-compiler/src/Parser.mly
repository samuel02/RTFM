(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/Parser.mly *)

%token <string> ID
%token <int>    INTVAL
%token <string> STRINGVAL
%token <string> CCODE
%token <string> PARAMS
%token MODULE INCLUDE ISR TASK FUNC RESET IDLE SYNC ASYNC PEND AFTER BEFORE (* PRIO *) CLAIM USEC MSEC SEC SC LCP RCP EOF HALT

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
  | PEND ID SC                              { Pend (0, $2) }
  | PEND AFTER INTVAL ID SC                 { Pend ($3, $4) }
  | ASYNC after before ID PARAMS SC         { Async ($2, $3, $4, $5) }
  | SYNC ID PARAMS SC                       { Sync ($2, $3) }
  | HALT SC                                 { Halt }

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
