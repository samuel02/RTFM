exception Error

type token = 
  | TASK
  | SYNC
  | SC
  | RCP
  | PEND
  | PARAMS of (string)
  | LCP
  | ISR
  | INTVAL of (int)
  | ID of (string)
  | HALT
  | FUNC
  | EOF
  | ENABLE
  | CLAIM
  | CCODE of (string)
  | BOOLVAL of (bool)
  | AFTER


val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (AST.prog option)