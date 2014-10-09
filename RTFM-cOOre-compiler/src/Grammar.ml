(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-cOOre/AST *)

open Common
open AST

exception TypeError of string
exception NameError of string
exception NotImplemented of string


type binding = id * pType
;;

(*
let rec string_of_env ti {local=l; scope=s; parent=p} = 
    let rec string_of_bind_list ti = function
        | (i, t)::tail  -> ti ^ i ^ " : " ^ string_of_pType t ^ "\n" ^ string_of_bind_list ti tail
        | []            -> ""
    in
    let local = s ^ "\n" ^ti^"-------------\n" ^ string_of_bind_list ti l in 
        match p with
            | None      -> local
            | Some p    -> local ^ "\n" ^ ti ^ "  Parent = " ^ string_of_env (ti^"  ") p
;;*)
let rec type_of id env =
    try
        List.assoc id env

    with Not_found  ->
        raise (NameError("Identifier " ^ id ^ " is not defined"))
;;

let unify t1 t2 = 
    if t1 == t2 then t1
    else
        raise (TypeError(" Types not matching: arg1 = " ^ string_of_pType t1 ^ ", arg2 = " ^ string_of_pType t2 ^ ". "))(* ^ string_of_pType t1 ^ " " ^ string_of_pType t2)))*)

let well_op op t1 t2 t_env =
    let equal rt t = rt = t in
    let type_list rt l = List.exists (equal rt) l in
    let rt = unify t1 t2 in
    match op with
    | OpPlus -> if type_list rt [Int]           then rt else raise (TypeError("Addition"))
    | OpSub  -> if type_list rt [Int]           then rt else raise (TypeError("Subtraction"))
    | OpMult -> if type_list rt [Int]           then rt else raise (TypeError("Multiplication"))
    | OpDiv  -> if type_list rt [Int]           then rt else raise (TypeError("Division"))
    | OpMod  -> if type_list rt [Int]           then rt else raise (TypeError("Modulo"))
    | OpEq   -> if type_list rt [Int;Bool;Byte] then rt else raise (TypeError("Equality"))
    | OpNeq  -> if type_list rt [Int;Bool;Byte] then rt else raise (TypeError("Non-equality"))
    | OpGrt  -> if type_list rt [Int;Bool;Byte] then rt else raise (TypeError("Greater than"))
    | OpGeq  -> if type_list rt [Int;Bool;Byte] then rt else raise (TypeError("Greater or equal"))
    | OpLet  -> if type_list rt [Int;Bool;Byte] then rt else raise (TypeError("Less than"))
    | OpLeq  -> if type_list rt [Int;Bool;Byte] then rt else raise (TypeError("Less or equal"))


let rec typecheck_expr env = function
    | IdExp (idl)               -> type_of (List.nth  idl 0) env
    | IndexExp (idl, e)         -> if ((typecheck_expr env e) = Int) && ((type_of (List.nth idl 0) env) = String)
                                        then Char
                                        else raise (TypeError("Incorrect string indexing"))
    | CallExp (m, el)           -> type_of (String.concat "." m) env
    | AsyncExp (af, be, il, el) -> Void
    | PendExp (il)              -> Void
    | IntExp (i)                -> Int
    | MathExp (op, a, b)        -> well_op op (typecheck_expr env a) (typecheck_expr env b) env
    | ParExp (e)                -> typecheck_expr env e
    | CharExp (c)               -> Char
    | BoolExp (b)               -> Bool
    | StrExp (s)                -> String
    | RT_Rand (e)               -> Int
    | RT_Getc                   -> Char
    | CompExp (op, e1, e2)      -> well_op op (typecheck_expr env e1) (typecheck_expr env e2) env



let rec typecheck_stmt env =  function
    | MPVar (t, i, e)   -> if typecheck_expr env e = t then (i, t)::env else raise (TypeError(""))
    | Stmt (sl)         -> List.fold_left typecheck_stmt env sl
    | ExpStmt (e)       -> typecheck_expr env e; env
    | Assign (i, e)     -> if type_of i env = typecheck_expr env e then env else raise (TypeError(""))
    | Return (e)        -> typecheck_expr env e; env
    | If (e, s)         -> if (typecheck_expr env e) = Bool then typecheck_stmt env s else raise (TypeError(""))
    | Else (s)          -> typecheck_stmt env s
    | While (e, s)      -> if (typecheck_expr env e) = Bool then typecheck_stmt env s else raise (TypeError(""))
    | RT_Sleep (e)      -> typecheck_expr env e; env
    | RT_Printf (s, el) -> List.map (typecheck_expr env) el; env
    | RT_Putc (e)       -> typecheck_expr env e; env


let typecheck_classDecl env = 
  let rec binding_argList env arg = match arg with
    | MPArg (t, i)::[]          -> (i, t)::env
    | MPArg (t, i)::tail        -> (i, t)::binding_argList env tail
    | []                        -> env
    in
    function
    | CPVar (t, i, e)        -> if typecheck_expr env e = t then (i, t)::env else raise (TypeError(""))
    | COVar (o, el, i)       -> if (List.length (List.map (typecheck_expr env) el) >= 0) then (i, Void)::env else raise (TypeError(""))
    | CMDecl (t, i, al, sl)  -> if (List.length (List.fold_left typecheck_stmt (binding_argList env al) sl) >= 0) then (i, t)::env else raise (TypeError(""))
    | CTaskDecl (i, al, sl ) -> if (List.length (List.fold_left typecheck_stmt (binding_argList env al) sl) >= 0) then (i, Void)::env else raise (TypeError(""))
    | CIsrDecl (pr, i, sl)   -> if (List.length (List.fold_left typecheck_stmt env sl)) >= 0 then env else raise (TypeError(""))
    | CResetDecl (sl)        -> if (List.length (List.fold_left typecheck_stmt env sl)) >= 0 then env else raise (TypeError(""))
    | CIdleDecl (sl)         -> if (List.length (List.fold_left typecheck_stmt env sl)) >= 0 then env else raise (TypeError(""))
;;

let typecheck_classDef = 
    let rec binding_classEnv = function
    | CPArg (t, i)::[]          -> (i, t)::[]
    | CPArg (t, i)::tail        -> (i, t)::binding_classEnv tail
    | []                        -> []
    in
    function
    | ClassDef (i, cal, cdl) -> List.length (List.fold_left typecheck_classDecl (binding_classEnv cal) cdl) >= 0
(*        List.for_all (typecheck_classDecl [(binding_classEnv cal)]) cdl)*)
;;
    

let typecheck_prog = function
    | Prog (cl) -> if (List.for_all typecheck_classDef cl) then "Typechecking successful" else "Typechecking failed"
