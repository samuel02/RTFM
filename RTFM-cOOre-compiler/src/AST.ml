(* RTFM-cOOre/AST *)
open Common

type id = string

type expr =
	| IdExp   of id list
	| CallExp of id list * expr list
	| PendExp of id list
	| IntExp  of int
	| CharExp of char
	| BoolExp of bool
  | RT_Rand of expr


type pType =
	| Int
	| Char
	| Bool
	| Byte
	| Void

type mPArg =
	| MPArg    of pType * id

type stmt =
	| ExpStmt   of expr
	| MPVar     of pType * id * expr
	| Assign    of id * expr
	| Return    of expr
  | RT_Sleep  of expr
  | RT_Printf of string * expr list
 
type classArg =
	| CPArg    of pType * id
	| CMArg    of pType * pType list * id

type classDecl =
	| CPVar    of pType * id * expr
	| COVar    of id * expr list * id
	| CMDecl   of pType * id * mPArg list * stmt list
	| CTDecl   of id * int * stmt list
	| CRDecl   of stmt list

type classDef =
	| ClassDef of id * classArg list * classDecl list

type prog =
	| Prog     of classDef list

(* pretty printing *)
let string_par m l = " (" ^ String.concat ", " (mymap m l) ^ ") "
let string_pp m l  = " <" ^ String.concat ", " (mymap m l) ^ "> "
let string_cur m l = " {" ^ String.concat ", " (mymap m l) ^ "} "

let rec string_of_expr = function
	| IdExp (idl) 		 -> String.concat "." idl
	| CallExp (m, el) -> String.concat "." m ^ string_par string_of_expr el
	| PendExp (il) 		-> "pend " ^ String.concat "." il
	| IntExp (i) 			-> string_of_int i
	| CharExp (c) 		-> ecit ^ String.make 1 c ^ ecit
	| BoolExp (b) 		-> string_of_bool b
  | RT_Rand (e)     -> "RT_rand(" ^ string_of_expr e ^ ")"
  
let string_of_pType = function
	| Int  -> "int"
	| Char -> "char"
	| Bool -> "bool"
	| Byte -> "byte"
	| Void -> "void"

let string_of_mPArg = function
	| MPArg (t, i) -> string_of_pType t ^ " " ^ i

let string_of_stmt = function
	| ExpStmt (e)			  -> tab ^ tab ^ string_of_expr e
	| MPVar (t, i, e)   -> tab ^ tab ^ string_of_pType t ^ " " ^ i ^ " := " ^ string_of_expr e
	| Assign (i, e) 	  -> tab ^ tab ^ i ^ " := " ^ string_of_expr e
	| Return (e) 			  -> tab ^ tab ^ "return " ^ string_of_expr e
  | RT_Sleep (e)      -> tab ^ tab ^ "RT_sleep(" ^ string_of_expr e ^ ")"
  | RT_Printf (s, el) -> tab ^ tab ^ "RT_printf(" ^ String.concat ", " (s :: List.map string_of_expr el) ^ ")"
  
let string_of_classArg = function
	| CPArg (t, i) 			-> string_of_pType t ^ " " ^ i
	| CMArg (t, tl, i ) -> string_of_pType t ^ " " ^ i ^ string_par string_of_pType tl

let string_of_classDecl = function
	| CPVar (t, i, e)        -> tab ^ string_of_pType t ^ " " ^ i ^ " := " ^ string_of_expr e ^ ";"
	| COVar (o, el, i)       -> tab ^ o ^ string_pp string_of_expr el ^ i ^ ";"
	| CMDecl (t, i, al, sl)  ->
			tab ^ string_of_pType t ^ " " ^ i ^ string_par string_of_mPArg al ^ "{" ^ nl
			^ myconcat (";" ^ nl) (List.map string_of_stmt sl)
			^ tab ^ "}"
	| CTDecl (i, p, sl)      -> tab ^ "task " ^ i ^ " " ^ string_of_int p ^ " () {" ^ nl 
		 ^ myconcat (";" ^ nl) (List.map string_of_stmt sl)
		^ tab ^ "}"
	| CRDecl (sl)            -> tab ^ "reset {" ^ nl 
         ^ myconcat (";" ^ nl) (List.map string_of_stmt sl)
        ^ tab ^ "}"

let string_of_classDef = function
	| ClassDef (i, cal, cdl) ->
			"class " ^ i ^ string_pp string_of_classArg cal ^ " {" ^ nl
			^ myconcat nl (List.map string_of_classDecl cdl) 
			^ "} " ^ nl

let string_of_prog = function
	| Prog (cl) -> "AST dump: " ^ nl ^ String.concat nl (List.map string_of_classDef cl)