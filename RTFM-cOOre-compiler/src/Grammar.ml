(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-cOOre/AST *)

open Common
open AST

exception TypeError of string
exception NotImplemented of string



(* pretty printing 
let string_par m l = " (" ^ String.concat ", " (List.map m l) ^ ") "
let string_pp m l  = " <" ^ String.concat ", " (List.map m l) ^ "> "
let string_cur m l = " {" ^ String.concat ", " (List.map m l) ^ "} "*)

type op =
    | OpPlus 
    | OpSub  
    | OpMult 
    | OpDiv 
    | OpComp 
    | OpGrt  
    | OpGeq  
    | OpLet  
    | OpLeq    

let op_of_string s = function
    | "+"     -> OpPlus
    | _       -> raise (NotImplemented("Math operator"))


type id = string;;

let string_of_type t = match t with
    | Int  -> "Int"
    | Char  -> "Char"
    | _  -> raise Not_found
;;


type env = 
    {   local: binding list;
        scope: string;
        parent: env option ;
    }
    and binding = id * pType
;;

let rec type_of id env =
    try
        List.assoc id env.local
    with
        Not_found -> match env.parent with
            | Some parent  -> type_of id parent
            | None         -> raise (Not_found)
;;

let unify t1 t2 = 
    if t1 == t2 then t1
    else
        raise (TypeError(" Types not matching: arg1 = " ^ to_string t1 ^ ", arg2 = " ^ to_string t2 ^ ". "))(* ^ string_of_pType t1 ^ " " ^ string_of_pType t2)))*)

        
let well_op op o1 o2 t_env =
    let equal rt t = rt = t in
    let type_list rt l = List.exists (equal rt) l in
    let t1 = type_of o1 t_env in
    let t2 = type_of o2 t_env in
    let rt = unify t1 t2 in
    match op with
    | OpPlus -> if type_list rt [Int]           then rt else raise (TypeError("Addition"))
    | OpSub  -> if type_list rt [Int]           then rt else raise (TypeError("Subtraction"))
    | OpMult -> if type_list rt [Int]           then rt else raise (TypeError("Multiplication"))
    | OpDiv  -> if type_list rt [Int]           then rt else raise (TypeError("Division"))
    | OpComp -> if type_list rt [Int;Bool;Byte] then rt else raise (TypeError("Greater than"))
    | OpGrt  -> if type_list rt [Int;Bool;Byte] then rt else raise (TypeError("Greater than"))
    | OpGeq  -> if type_list rt [Int;Bool;Byte] then rt else raise (TypeError("Greater or equal"))
    | OpLet  -> if type_list rt [Int;Bool;Byte] then rt else raise (TypeError("Less than"))
    | OpLeq  -> if type_list rt [Int;Bool;Byte] then rt else raise (TypeError("Less or equal"))

let rec typecheck_expr env = function
    | IdExp (idl)               -> type_of (String.concat "." idl) env
    | CallExp (m, el)           -> 
        (*Input parameter check here*)
        type_of m env
    | AsyncExp (af, be, il, el) -> Void
    | PendExp (il)              -> Void
    | IntExp (i)                -> Int
    | MathExp (e, a, b)         -> well_op (op_of_string e) (typecheck_expr a) (typecheck_expr b) env
    | ParExp (e)                -> typecheck_expr e
    | CharExp (c)               -> Char
    | BoolExp (b)               -> Bool
    | RT_Rand (e)               -> Int
    | RT_Getc                   -> Char
    | CompExp (s, e1, e2)       -> well_op (op_of_string s) (typecheck_expr e1) (typecheck_expr e2) env

let string_of_pType = function
    | Int  -> "int"
    | Char -> "char"
    | Bool -> "bool"
    | Byte -> "byte"
    | Void -> "void"

let string_of_mPArg = function
    | MPArg (t, i) -> string_of_pType t ^ " " ^ i

let rec typecheck_stmt env = function
    | ExpStmt (e)       -> typecheck_expr env e; Void
    | MPVar (t, i, e)   -> 
        unify t (typecheck_expr env e);
        env.local = (i, t) :: env.local;
        Void
    | Assign (i, e)     -> unify (type_of i env)  (typecheck_expr env e); Void
    | Return (e)        -> typecheck_expr env e; Void
    | If (e, sl)        ->
        unify Bool (typecheck_expr env e);
        List.map (typecheck_stmt env) sl;
        Void
    | Else (sl)         -> List.map (typecheck_stmt env) sl; Void
    | While (e, sl)     -> 
        unify Bool (typecheck_expr env e);
        List.map (typecheck_stmt env) sl;
        Void
    | RT_Sleep (e)      -> typecheck_expr env e; Void
    | RT_Printf (s, el) -> typecheck_expr env e; Void
    | RT_Putc (e)       -> typecheck_expr env e; Void

let typecheck_classArg = function
    | CPArg (t, i)      -> env.local = (i, t) :: env.local
    | CMArg (t, tl, i ) -> env.local = (i, t) :: env.local


let typecheck_classDecl env = function
    | CPVar (t, i, e)        ->
        unify t (typecheck_expr env e);
        env.local = (i, t) :: env.local;
    | COVar (o, el, i)       -> typecheck_expr env el

    | CMDecl (t, i, al, sl)  ->
        let new_scope = {local=[]; scope=i; parent= Some env} in
        List.map((typecheck_stmt new_scope) sl);
    | CTaskDecl (i, al, sl ) ->
        let new_scope = {local=[]; scope=i; parent= Some env} in
        List.map((typecheck_stmt new_scope) sl);
    | CIsrDecl (pr, i, sl)   ->
        let new_scope = {local=[]; scope=i; parent= Some env} in
        List.map((typecheck_stmt new_scope) sl);
    | CResetDecl (sl)        -> 
        let new_scope = {local=[]; scope="Reset"; parent= Some env} in
        List.map((typecheck_stmt new_scope) sl);
    | CIdleDecl (sl)         -> 
        let new_scope = {local=[]; scope="Idle"; parent= Some env} in
        List.map((typecheck_stmt new_scope) sl);
      

let typecheck_classDef = function
    | ClassDef (i, cal, cdl) ->
        let env = {local= []; scope= i; parent= None} in
        List.map typecheck_classArg env cal;
        List.map typecheck_classDecl env cdl;
in
let typecheck_prog = function
    | Prog (cl) -> "Typechecking: " ^ nl ^ String.concat nl (List.map typecheck_classDef cl);


