%token <string> ID
%token <int> INTVAL
%token <bool> BOOLVAL
%token <string> CCODE
%token <string> PARAMS
%token ISR TASK FUNC RESET PEND AFTER SYNC ENABLE CLAIM HALT SC LCP RCP (* LP RP *) EOF

%{
  open AST 
%}

%start prog

%type <AST.prog option> prog 

%%

prog:
  | top* EOF                        { Some (Prog ($1)) }
  
top:
  | CCODE                           { TopC ($1) }
    (* | PEND ID SC                      { TopPend ($2) } *)
  | ISR ID INTVAL LCP stmt* RCP 	{ Isr (HARD, $2, $3, $5) }
  | TASK ID INTVAL LCP stmt* RCP 	{ Isr (SOFT, $2, $3, $5) }
  | FUNC ID ID PARAMS LCP stmt* RCP	{ Func ($2, $3, $4, $6) } 
  | RESET LCP stmt* RCP             { Reset ($3) }
              
stmt:
  | CCODE 							{ ClaimC ($1) }
  | CLAIM ID LCP stmt* RCP			{ Claim ($2, $4) }
  | PEND ID	SC						{ Pend ($2) }             
  | PEND ID AFTER INTVAL SC         { PendAfter ($2, $4) }
  | SYNC ID PARAMS SC 				{ Sync ($2, $3) }
  | ENABLE BOOLVAL SC 				{ Enable ($2) }
  | HALT SC                         { Halt }
  
