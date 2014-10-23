(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-cOOre/AST *)

open Common

type id = string
type extern = string

type op =
    | OpPlus 
    | OpSub  
    | OpMult 
    | OpDiv 
    | OpMod
    | OpEq 
    | OpNeq
    | OpGrt  
    | OpGeq  
    | OpLet  
    | OpLeq  

type pType =
    | Int
    | Char
    | String
    | Bool
    | Byte
    | Void
    | Class
    | ClassInstance of string
    
type expr =
    | IdExp     of id list
    | CallExp   of id list * expr list
    | AsyncExp  of time * time * id list * expr list      (* after before id[.id]? (exprs) *)
    | PendExp   of id list
    | IntExp    of int
    | MathExp   of op * expr * expr
    | ParExp    of expr
    | CharExp   of char
    | BoolExp   of bool
    | StrExp    of string
    | RT_Rand   of expr
    | CompExp   of op * expr * expr
    | IndexExp  of id list * expr
    | RT_Getc

type mPArg =
    | MPArg     of pType * id

type stmt =
    | Stmt      of stmt list
    | ExpStmt   of expr
    | MPVar     of pType * id * expr
    | Assign    of id * expr
    | Return    of expr
    | If        of expr * stmt
    | Else      of stmt
    | While     of expr * stmt
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
    | ClassDef  of id * classArg list * extern * classDecl list

type prog =
    | Prog      of classDef list

(* pretty printing *)
let string_par m l = " (" ^ String.concat ", " (List.map m l) ^ ") "
let string_pp m l  = " <" ^ String.concat ", " (List.map m l) ^ "> "
let string_cur m l = " {" ^ String.concat ", " (List.map m l) ^ "} "

let string_of_pType = function
    | Int  -> "int"
    | Char -> "char"
    | Bool -> "bool"
    | Byte -> "byte"
    | Void -> "void"
    | String -> "string"
    | Class  -> "class"
    | ClassInstance(o) -> o

let string_of_op = function
    | OpPlus    ->  "+"
    | OpSub     ->  "-"
    | OpMult    ->  "*"
    | OpDiv     ->  "/"
    | OpMod     ->  "%"
    | OpEq      ->  "=="
    | OpNeq     ->  "!="
    | OpGrt     ->  ">"
    | OpGeq     ->  ">="
    | OpLet     ->  "<"
    | OpLeq     ->  "<="

let rec string_of_expr = function
    | IdExp (idl)               -> String.concat "." idl
    | IndexExp (idl, e)         -> String.concat "." idl ^ "[" ^ string_of_expr e ^ "]"
    | CallExp (m, el)           -> String.concat "." m ^ string_par string_of_expr el
    | AsyncExp (af, be, il, el) -> "async after " ^ string_of_time af ^ " before " ^ string_of_time be ^ " " ^ String.concat "." il ^ string_par string_of_expr el
    | PendExp (il)              -> "pend " ^ String.concat "." il
    | IntExp (i)                -> string_of_int i
    | MathExp (op, a, b)        -> string_of_expr a ^ " " ^  string_of_op op ^ " " ^ string_of_expr b
    | ParExp (e)                -> "(" ^ string_of_expr e ^ ")"
    | CharExp (c)               -> ecit ^ String.make 1 c ^ ecit
    | BoolExp (b)               -> string_of_bool b
    | StrExp (s)                -> "\"" ^ s ^ "\""
    | RT_Rand (e)               -> "RT_rand(" ^ string_of_expr e ^ ")"
    | RT_Getc                   -> "RT_getc()"
    | CompExp (op, e1, e2)       -> string_of_expr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr e2


let string_of_mPArg = function
    | MPArg (t, i) -> string_of_pType t ^ " " ^ i

let rec string_of_stmt ti = function
    | Stmt (sl)         -> ti ^ op  ^ tab ^  String.concat (tab) (List.map (string_of_stmt (ti)) sl) ^ ti ^ "}" ^nl
    | ExpStmt (e)       -> ti ^ string_of_expr e ^ sc ^ nl
    | MPVar (t, i, e)   -> ti ^ string_of_pType t ^ " " ^ i ^ " := " ^ string_of_expr e ^ sc ^ nl
    | Assign (i, e)     -> ti ^ i ^ " := " ^ string_of_expr e ^ sc ^ nl
    | Return (e)        -> ti ^ "return " ^ string_of_expr e ^ sc ^ nl
    | If (e, s)         -> ti ^ "if ( " ^ string_of_expr e ^ " )" ^ string_of_stmt (ti^tab) s
    | Else (s)          -> ti ^ "else" ^ string_of_stmt (ti^tab) s
    | While (e, s)      -> ti ^ "while ( " ^ string_of_expr e ^ " )" ^ string_of_stmt (ti^tab) s
    | RT_Sleep (e)      -> ti ^ "RT_sleep(" ^ string_of_expr e ^ ")" ^ sc ^ nl
    | RT_Printf (s, el) -> ti ^ "RT_printf(" ^ String.concat ", " (s :: List.map string_of_expr el) ^ ")" ^ sc ^ nl
    | RT_Putc (e)       -> ti ^ "RT_putc(" ^ string_of_expr e ^ ")" ^ sc ^ nl

let string_of_classArg = function
    | CPArg (t, i)      -> string_of_pType t ^ " " ^ i
    | CMArg (t, tl, i ) -> string_of_pType t ^ " " ^ i ^ string_par string_of_pType tl

let string_of_classDecl = function
    | CPVar (t, i, e)        -> tab ^ string_of_pType t ^ " " ^ i ^ " := " ^ string_of_expr e ^ ";"
    | COVar (o, el, i)       -> tab ^ o ^ string_pp string_of_expr el ^ i ^ ";"
    | CMDecl (t, i, al, sl)  ->
            tab ^ string_of_pType t ^ " " ^ i ^ string_par string_of_mPArg al ^ "{" ^ nl
            ^ String.concat "" (List.map (string_of_stmt (tab^tab)) sl)
            ^ tab ^ "}"
    | CTaskDecl (i, al, sl ) ->
      tab ^ "TaskDef " ^ i ^ " " ^ string_par string_of_mPArg al ^ "{" ^ nl ^
      String.concat "" (List.map (string_of_stmt (tab^tab)) sl) ^
      tab ^ "}"
    | CIsrDecl (pr, i, sl)   ->
      tab ^ "ISR @prio " ^ string_of_int pr  ^ i ^ " {" ^ nl ^
      String.concat "" (List.map (string_of_stmt (tab^tab)) sl) ^
      tab ^ "}"
    | CResetDecl (sl)        -> tab ^ "Reset {" ^ nl
         ^ String.concat "" (List.map (string_of_stmt (tab^tab)) sl)
        ^ tab ^ "}" ^ nl
    | CIdleDecl (sl)         -> tab ^ "Idle {" ^ nl
         ^ String.concat "" (List.map (string_of_stmt (tab^tab)) sl)
        ^ tab ^ "}" ^ nl

let string_of_extern e =
    if String.length e = 0 then "" else "extern " ^ e


let string_of_classDef = function
    | ClassDef (i, cal, extern, cdl) ->
            "class " ^ i ^ string_pp string_of_classArg cal ^ string_of_extern extern ^ "{" ^ nl
            ^ String.concat nl (List.map string_of_classDecl cdl)
            ^ "} " ^ nl

let string_of_prog = function
    | Prog (cl) -> "AST dump: " ^ nl ^ String.concat nl (List.map string_of_classDef cl)
