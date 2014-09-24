(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-cOOre/AST *)

open Common 

type id = string

type expr =
    | IdExp     of id list
    | CallExp   of id list * expr list
    | AsyncExp  of time * time * id list * expr list      (* after before id[.id]? (exprs) *)
    | PendExp   of id list
    | IntExp    of int
    | MathExp   of char * expr * expr
    | ParExp    of expr
    | CharExp   of char
    | BoolExp   of bool
    | RT_Rand   of expr
    | RT_Getc

type pType =
    | Int
    | Char
    | Bool
    | Byte
    | Void

type mPArg =
    | MPArg     of pType * id

type stmt =
    | ExpStmt   of expr
    | MPVar     of pType * id * expr
    | Assign    of id * expr
    | Return    of expr
    | If        of expr * stmt list
    | RT_Sleep  of expr
    | RT_Printf of string * expr list
    | RT_Putc   of expr    

 
type classArg =
    | CPArg     of pType * id
    | CMArg     of pType * pType list * id

type classDecl =
    | CPVar      of pType * id * expr
    | COVar      of id * expr list * id
    | CMDecl     of pType * id * mPArg list * stmt list
    | CTaskDecl  of id * mPArg list * stmt list
    | CIsrDecl   of int * id * stmt list
    | CResetDecl of stmt list
    | CIdleDecl  of stmt list

type classDef =
    | ClassDef  of id * classArg list * classDecl list

type prog =
    | Prog      of classDef list

(* pretty printing *)
let string_par m l = " (" ^ String.concat ", " (mymap m l) ^ ") "
let string_pp m l  = " <" ^ String.concat ", " (mymap m l) ^ "> "
let string_cur m l = " {" ^ String.concat ", " (mymap m l) ^ "} "

let rec string_of_expr = function
    | IdExp (idl)               -> String.concat "." idl
    | CallExp (m, el)           -> String.concat "." m ^ string_par string_of_expr el
    | AsyncExp (af, be, il, el) -> "async after " ^ string_of_time af ^ " before " ^ string_of_time be ^ " " ^ String.concat "." il ^ string_par string_of_expr el
    | PendExp (il)              -> "pend " ^ String.concat "." il
    | IntExp (i)                -> string_of_int i
    | MathExp (e, a, b)         -> string_of_expr a ^ " " ^ String.make 1 e ^ " " ^ string_of_expr b
    | ParExp (e)                -> "(" ^ string_of_expr e ^ ")"
    | CharExp (c)               -> ecit ^ String.make 1 c ^ ecit
    | BoolExp (b)               -> string_of_bool b
    | RT_Rand (e)               -> "RT_rand(" ^ string_of_expr e ^ ")"
    | RT_Getc                   -> "RT_getc()"
  
let string_of_pType = function
    | Int  -> "int"
    | Char -> "char"
    | Bool -> "bool"
    | Byte -> "byte"
    | Void -> "void"

let string_of_mPArg = function
    | MPArg (t, i) -> string_of_pType t ^ " " ^ i

let rec string_of_stmt = function
    | ExpStmt (e)       -> tab ^ tab ^ string_of_expr e ^ sc ^ nl
    | MPVar (t, i, e)   -> tab ^ tab ^ string_of_pType t ^ " " ^ i ^ " := " ^ string_of_expr e ^ sc ^ nl
    | Assign (i, e)     -> tab ^ tab ^ i ^ " := " ^ string_of_expr e ^ sc ^ nl
    | Return (e)        -> tab ^ tab ^ "return " ^ string_of_expr e ^ sc ^ nl
    | If (e, sl)        -> tab ^ tab ^ "if ( " ^ string_of_expr e ^ " )" ^ op ^ tab ^ myconcat (tab) (List.map string_of_stmt sl) ^ tab ^ cl ^ nl
    | RT_Sleep (e)      -> tab ^ tab ^ "RT_sleep(" ^ string_of_expr e ^ ")" ^ sc ^ nl
    | RT_Printf (s, el) -> tab ^ tab ^ "RT_printf(" ^ String.concat ", " (s :: List.map string_of_expr el) ^ ")" ^ sc ^ nl
    | RT_Putc (e)       -> tab ^ tab ^ "RT_putc(" ^ string_of_expr e ^ ")" ^ sc ^ nl

let string_of_classArg = function
    | CPArg (t, i)      -> string_of_pType t ^ " " ^ i
    | CMArg (t, tl, i ) -> string_of_pType t ^ " " ^ i ^ string_par string_of_pType tl

let string_of_classDecl = function
    | CPVar (t, i, e)        -> tab ^ string_of_pType t ^ " " ^ i ^ " := " ^ string_of_expr e ^ ";"
    | COVar (o, el, i)       -> tab ^ o ^ string_pp string_of_expr el ^ i ^ ";"
    | CMDecl (t, i, al, sl)  ->
            tab ^ string_of_pType t ^ " " ^ i ^ string_par string_of_mPArg al ^ "{" ^ nl
            ^ myconcat "" (List.map string_of_stmt sl)
            ^ tab ^ "}"
    | CTaskDecl (i, al, sl ) -> 
      tab ^ "TaskDef " ^ i ^ " " ^ string_par string_of_mPArg al ^ "{" ^ nl ^ 
      myconcat "" (List.map string_of_stmt sl) ^
      tab ^ "}"
    | CIsrDecl (pr, i, sl)   -> 
      tab ^ "ISR @prio " ^ string_of_int pr  ^ i ^ " {" ^ nl ^ 
      myconcat "" (List.map string_of_stmt sl) ^
      tab ^ "}"
    | CResetDecl (sl)        -> tab ^ "Reset {" ^ nl 
         ^ myconcat "" (List.map string_of_stmt sl)
        ^ tab ^ "}"
    | CIdleDecl (sl)         -> tab ^ "Idle {" ^ nl 
         ^ myconcat "" (List.map string_of_stmt sl)
        ^ tab ^ "}"


let string_of_classDef = function
    | ClassDef (i, cal, cdl) ->
            "class " ^ i ^ string_pp string_of_classArg cal ^ " {" ^ nl
            ^ myconcat nl (List.map string_of_classDecl cdl) 
            ^ "} " ^ nl

let string_of_prog = function
    | Prog (cl) -> "AST dump: " ^ nl ^ String.concat nl (List.map string_of_classDef cl)