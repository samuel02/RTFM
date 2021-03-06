(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-cOOre/Parser.mly *)

%token <string> ID
%token <int>    INTVAL
%token <bool>   BOOLVAL
%token <char>   CHARVAL 
%token <string> STRVAL
%token TASK ISR RESET IDLE PEND ASYNC AFTER BEFORE (* PRIO *) CLASS RETURN EXTERN
%token USEC MSEC SEC
%token RT_SLEEP RT_PRINTF RT_RAND RT_GETC RT_PUTC
%token ASSIGN COMMA LT GT LP RP LCP RCP DOT SC LSQ RSQ
%token INT CHAR BOOL BYTE VOID STRING
%token ADD SUB MUL DIV MOD
%token EQ NEQ GTEQ LTEQ
%token IF ELSE WHILE
%token EOF

%{
  open AST
  open Common
%}

%start prog

%type <AST.prog option> prog

%%

prog:
  | classDef* EOF                                                   { Some (Prog ($1)) }

classDef:
  | CLASS ID LT classArgs GT extern LCP classDecl* RCP              { ClassDef ($2, $4, $6, $8) }

extern:
  | EXTERN STRVAL                                                   { $2 }
  |                                                                 { "" }

classArgs:
  calist = separated_list(COMMA, classArg)                          { calist }

classArg:
  | pType ID                                                        { CPArg ($1, $2) }
  | pType LP mSig RP ID                                             { CMArg ($1, $3, $5) }

mSig:
  tlist = separated_list(COMMA, pType)                              { tlist }

classDecl:
  | pType ID ASSIGN expr SC                                         { CPVar ($1, $2, $4) }
  | ID LT params GT ID  SC                                          { COVar ($1, $3, $5) }
  | EXTERN pType ID LP mSig RP SC                                   { ExtMDecl ($2, $3, $5) }
  | pType ID LP mArgs RP LCP stmt* RCP                              { CMDecl ($1, $2, $4, $7) }
  | TASK ID LP mArgs RP LCP stmt* RCP                               { CTaskDecl ($2, $4, $7) }
  | ISR INTVAL ID LCP stmt* RCP                                     { CIsrDecl ($2, $3, $5) }
  | RESET LCP stmt* RCP                                             { CResetDecl ($3) }
  | IDLE LCP stmt* RCP                                              { CIdleDecl ($3) }

mArgs:
  alist = separated_list(COMMA, mArg)                               { alist }

mArg:
  | pType ID                                                        { MPArg ($1, $2) }

pType:
  | INT                                                             { Int }
  | CHAR                                                     { Char }
  | BOOL                                                     { Bool }
  | BYTE                                                     { Byte }
  | VOID                                                     { Void }
  | STRING                                                   { String }

math:
  | ADD                                                      { OpPlus }
  | SUB                                                      { OpSub }
  | MUL                                                      { OpMult }
  | DIV                                                      { OpDiv }
  | MOD                                                      { OpMod }

compare:
  | EQ                                                       { OpEq }
  | NEQ                                                      { OpNeq }
  | GTEQ                                                     { OpGeq }
  | LTEQ                                                     { OpLeq }

params:
  plist = separated_list(COMMA, expr)                               { plist }

expr:
  | ASYNC after before ids LP params RP                      { AsyncExp ($2, $3, $4, $6) }
  | PEND ids                                                 { PendExp ($2) }
  | ids                                                      { IdExp ($1) }
  | ids LSQ expr RSQ                                         { IndexExp ($1, $3) }
  | ids LP params RP                                         { CallExp ($1, $3) }
  | INTVAL                                                   { IntExp ($1) }
  | expr math expr                                           { MathExp ($2, $1, $3) }
  | expr compare expr                                        { CompExp ($2, $1, $3) }
  | LP expr RP                                               { ParExp ($2) }
  | CHARVAL                                                  { CharExp ($1) }
  | BOOLVAL                                                  { BoolExp ($1) }
  | STRVAL                                                   { StrExp ($1) }
  | RT_RAND LP expr RP                                       { RT_Rand ($3) }
  | RT_GETC LP RP                                            { RT_Getc }

ids:
  | ID DOT ID                                                       { $1::[$3] }
  | ID                                                              { $1::[] }

after:
  | AFTER time                                                      { $2 }
  |                                                                 { Usec (0) }

before:
  | BEFORE time                                                     { $2 }
  |                                                                 { Usec (0) }

time:
  | INTVAL USEC                                                     { Usec($1) }
  | INTVAL MSEC                                                     { Msec($1) }
  | INTVAL SEC                                                      { Sec($1) }
  | INTVAL                                                          { Usec($1) }

(*
prio:
  | PRIO expr                                                       { $2 }
  |                                                                 { IntExp(0) }
*)

stmt:
  | LCP stmt* RCP                                                   { Stmt ($2) }
  | expr SC                                                         { ExpStmt ($1) }
  | pType ID ASSIGN expr SC                                         { MPVar ($1, $2, $4) }
  | ID ASSIGN expr SC                                               { Assign ($1, $3) }
  | RETURN expr SC                                                  { Return ($2) }
  | IF LP expr RP stmt                                              { If($3, $5) }
  | ELSE stmt                                                       { Else( $2) }
  | WHILE LP expr RP stmt                                           { While($3, $5) }
  | RT_SLEEP LP expr RP SC                                          { RT_Sleep ($3) }
  | RT_PRINTF LP STRVAL COMMA params RP SC                          { RT_Printf ($3, $5) }
  | RT_PRINTF LP STRVAL RP SC                                       { RT_Printf ($3, []) }
  | RT_PUTC LP expr RP SC                                           { RT_Putc ($3) }

