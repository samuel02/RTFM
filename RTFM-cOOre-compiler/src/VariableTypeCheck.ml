(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-cOOre/AST *)

open Common
open AST

exception TypeError of string
exception NameError of string
exception NotImplemented of string

let raise_type_error msg = raise (TypeError("TypeError: " ^ msg));;

type binding = id * pType

let rec type_of id env =
    try List.assoc id env
    with Not_found  ->
        raise (NameError("NameError: " ^ id ^ " is not defined."))

let unify t1 t2 =
    if t1 == t2 then t1
    else
        raise_type_error ("Types not matching: arg1 = " ^ string_of_pType t1 ^ ", arg2 = " ^ string_of_pType t2 ^ ". ")

let in_list rt l =
    let equal rt t = rt = t in
    List.exists (equal rt) l

let typecheck_op env op t1 t2 =
    let rt = unify t1 t2 in
    let type_error_msg t = string_of_op op ^ " operator is not defined for type " ^ string_of_pType t ^ "." in
    match op with
    | OpPlus -> if in_list rt [Int]           then rt     else raise_type_error (type_error_msg rt)
    | OpSub  -> if in_list rt [Int]           then rt     else raise_type_error (type_error_msg rt)
    | OpMult -> if in_list rt [Int]           then rt     else raise_type_error (type_error_msg rt)
    | OpDiv  -> if in_list rt [Int]           then rt     else raise_type_error (type_error_msg rt)
    | OpMod  -> if in_list rt [Int]           then rt     else raise_type_error (type_error_msg rt)
    | OpEq   -> if in_list rt [Int;Bool;Byte] then Bool   else raise_type_error (type_error_msg rt)
    | OpNeq  -> if in_list rt [Int;Bool;Byte] then Bool   else raise_type_error (type_error_msg rt)
    | OpGrt  -> if in_list rt [Int;Bool;Byte] then Bool   else raise_type_error (type_error_msg rt)
    | OpGeq  -> if in_list rt [Int;Bool;Byte] then Bool   else raise_type_error (type_error_msg rt)
    | OpLet  -> if in_list rt [Int;Bool;Byte] then Bool   else raise_type_error (type_error_msg rt)
    | OpLeq  -> if in_list rt [Int;Bool;Byte] then Bool   else raise_type_error (type_error_msg rt)

let rec typecheck_expr env = function
    | IndexExp (idl, e)         -> if ((typecheck_expr env e) = Int) && ((type_of (List.nth idl ((List.length idl)-1)) env) = String) then Char else raise_type_error ("Incorrect string indexing")
    | MathExp (op, a, b)        -> typecheck_op env op (typecheck_expr env a) (typecheck_expr env b)
    | CompExp (op, e1, e2)      -> typecheck_op env op (typecheck_expr env e1) (typecheck_expr env e2)
    | ParExp (e)                -> typecheck_expr env e
    | IdExp (idl)               -> type_of (List.nth idl ((List.length idl)-1)) env
    | CallExp (m, el)           -> List.map (typecheck_expr env) el; Void
    | AsyncExp (af, be, il, el) -> Void
    | PendExp (il)              -> Void
    | IntExp (i)                -> Int
    | CharExp (c)               -> Char
    | BoolExp (b)               -> Bool
    | StrExp (s)                -> String
    | RT_Rand (e)               -> Int
    | RT_Getc                   -> Char

let rec typecheck_stmt env =  function
    | Stmt (sl)         -> List.fold_left typecheck_stmt env sl
    | RT_Printf (s, el) -> List.map (typecheck_expr env) el; env
    | Else (s)          -> typecheck_stmt env s
    | ExpStmt (e) | Return (e) | RT_Sleep (e) | RT_Putc (e) -> typecheck_expr env e; env
    | MPVar (t, i, e)   -> if typecheck_expr env e = t then (i, t)::env else raise_type_error (string_of_expr e ^ " is not of type " ^ string_of_pType t ^ ".")
    | Assign (i, e)     -> if type_of i env = typecheck_expr env e then env else raise_type_error ("Cannot assign " ^ string_of_pType (typecheck_expr env e) ^ " " ^ string_of_expr e ^ " to " ^ string_of_pType (type_of i env) ^ " " ^ i ^ ".")
    | If (e, s)         -> if in_list (typecheck_expr env e) [Bool; Int] then typecheck_stmt env s else raise_type_error ("If: "^string_of_expr e)
    | While (e, s)      -> if in_list (typecheck_expr env e) [Bool; Int] then typecheck_stmt env s else raise_type_error ("While: "^string_of_expr e)

let typecheck_classDecl env =
  let rec binding_argList env arg = match arg with
    | MPArg (t, i)::[]          -> (i, t)::env
    | MPArg (t, i)::tail        -> (i, t)::binding_argList env tail
    | []                        -> env
    in
    function
    | CPVar (t, i, e)        -> if typecheck_expr env e = t then (i, t)::env else raise_type_error (string_of_expr e ^ " is not of type " ^ string_of_pType t ^ ".")
    | COVar (o, el, i)       -> if (List.length (List.map (typecheck_expr env) el) >= 0) then (*(i, Void)::*)env else raise_type_error ("")
    | CMDecl (t, i, al, sl)  -> if (List.length (List.fold_left typecheck_stmt (binding_argList env al) sl) >= 0) then (*(i, t)::*)env else raise_type_error ("")
    | CTaskDecl (i, al, sl ) -> if (List.length (List.fold_left typecheck_stmt (binding_argList env al) sl) >= 0) then (*(i, Void)::*)env else raise_type_error ("")
    | CIsrDecl (pr, i, sl)   -> if (List.length (List.fold_left typecheck_stmt env sl)) >= 0 then env else raise_type_error ("")
    | CResetDecl (sl)        -> if (List.length (List.fold_left typecheck_stmt env sl)) >= 0 then env else raise_type_error ("")
    | CIdleDecl (sl)         -> if (List.length (List.fold_left typecheck_stmt env sl)) >= 0 then env else raise_type_error ("")

let typecheck_classDef =
    let rec binding_classEnv = function
    | CPArg (t, i)::[]          -> (i, t)::[]
    | CPArg (t, i)::tail        -> (i, t)::binding_classEnv tail
    | []                        -> []
    in
    function
    | ClassDef (i, cal, cdl) -> List.length (List.fold_left typecheck_classDecl (binding_classEnv cal) cdl) >= 0

let typecheck_prog = function
    | Prog (cl) -> if (List.for_all typecheck_classDef cl) then "Passed type checking." else "TypeError."
