%token <string> ID
%token <int> INTVAL
%token <bool> BOOLVAL
%token <char> CHARVAL
%token CLASS TASK PEND RETURN VOID ASSIGN COMMA INT CHAR BOOL LT GT LP RP LCP RCP DOT SC EOF
  
%{
  open AST 
%}

%start prog

%type <AST.prog option> prog 

%%
        
prog: 
  | classDef* EOF                                            { Some (Prog ($1)) }
        
classDef:
  | CLASS ID LT classArgs GT LCP classDecl* RCP              { ClassDef ($2, $4, $7) }
   
classArgs:
  calist = separated_list(COMMA, classArg)                   { calist }
    
classArg:
  | pType ID                                                 { CPArg ($1, $2) }
  | pType LP mSig RP ID                                      { CMArg ($1, $3, $5) }
    
mSig:
  tlist = separated_list(COMMA, pType)                       { tlist }
  
classDecl:
  | pType ID ASSIGN expr SC                                  { CPVar ($1, $2, $4) }  
  | ID LT params GT ID  SC                                   { COVar ($1, $3, $5) }
  | pType ID LP mArgs RP LCP stmt* RCP                       { CMDecl ($1, $2, $4, $7) }
	| TASK ID INTVAL LCP stmt* RCP														 { CTDecl ($2, $3, $5) }
    
mArgs:
  alist = separated_list(COMMA, mArg)                        { alist }
  
mArg:
  | pType ID                                                 { MPArg ($1, $2) }
    
pType:
  | INT                                                      { Int }
  | CHAR                                                     { Char }
  | BOOL                                                     { Bool }
  | VOID                                                     { Void }  
     
params:
  plist = separated_list(COMMA, expr)                        { plist }
    
expr:                                                        
  | ID                                                       { IdExp ($1) }
  | ID LP params RP                                          { CallExp ([$1], $3) }
  | ID DOT ID LP params RP                                   { CallExp ($1::[$3], $5) }
	| PEND ID                                                  { PendExp ([$2]) }
	| PEND ID DOT ID                                           { PendExp ($2::[$4]) }
  | INTVAL                                                   { IntExp ($1) }
  | CHARVAL                                                  { CharExp ($1) }
  | BOOLVAL                                                  { BoolExp ($1) }
    
        
stmt:
  | pType ID ASSIGN expr SC                                  { MPVar ($1, $2, $4) }  
  | ID ASSIGN expr SC                                        { Assign ($1, $3) }
  | RETURN expr SC                                           { Return ($2) }
    