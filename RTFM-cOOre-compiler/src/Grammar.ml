(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-cOOre/AST *)

open Common
open AST

exception TypeError of string
exception NameError of string
exception NotImplemented of string



(* pretty printing 
let string_par m l = " (" ^ String.concat ", " (List.map m l) ^ ") "
let string_pp m l  = " <" ^ String.concat ", " (List.map m l) ^ "> "
let string_cur m l = " {" ^ String.concat ", " (List.map m l) ^ "} "*)

  

type id = string;;

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
        Not_found -> 
        try 
            match env.parent with
                | Some parent  -> type_of id parent
                | None         -> raise (Not_found)
        with Not_found  ->
            raise (NameError("Identifier " ^ id ^ " is not defined in scope " ^ env.scope))
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
    | IdExp (idl)               -> type_of (String.concat "." idl) env
    | IndexExp (idl, e)         -> if ((typecheck_expr env e) = Int) && ((type_of (String.concat "." idl) env) = String)
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


let add_to_env i t env = 
    if List.mem_assoc i env.local then
    begin
        env.local = (i, t) :: env.local;
        true;
    end
    else 
        false;;


let rec typecheck_stmt env = function
    | Stmt (sl)         -> List.for_all (typecheck_stmt env) sl
    | ExpStmt (e)       -> typecheck_expr env e; true
    | MPVar (t, i, e)   -> typecheck_expr env e = t  && add_to_env i t env
    | Assign (i, e)     -> type_of i env = typecheck_expr env e
    | Return (e)        -> typecheck_expr env e; true
    | If (e, s)         -> (typecheck_expr env e) = Bool && typecheck_stmt env s
    | Else (s)          -> typecheck_stmt env s
    | While (e, s)      -> (typecheck_expr env e) = Bool && typecheck_stmt env s
    | RT_Sleep (e)      -> typecheck_expr env e; true
    | RT_Printf (s, el) -> List.map (typecheck_expr env) el; true
    | RT_Putc (e)       -> typecheck_expr env e; true

let typecheck_classArg env = function
    | CPArg (t, i)      -> begin env.local = (i, t) :: env.local; print_string ("added " ^ i ^ " to environment " ^ env.scope ^"\n"); true; end
    | CMArg (t, tl, i ) -> begin env.local = (i, t) :: env.local; true; end


let typecheck_classDecl env = 
    let new_scope i par = {local=[]; scope=i; parent= Some par} in
    function
    | CPVar (t, i, e)        -> begin unify t (typecheck_expr env e); env.local = (i, t) :: env.local; true; end
    | COVar (o, el, i)       -> begin List.map (typecheck_expr env) el; true; end
    | CMDecl (t, i, al, sl)  -> List.for_all (typecheck_stmt (new_scope i env)) sl
    | CTaskDecl (i, al, sl ) -> List.for_all (typecheck_stmt (new_scope i env)) sl
    | CIsrDecl (pr, i, sl)   -> List.for_all (typecheck_stmt (new_scope i env)) sl
    | CResetDecl (sl)        -> List.for_all (typecheck_stmt (new_scope "Reset" env)) sl
    | CIdleDecl (sl)         -> List.for_all (typecheck_stmt (new_scope "Idle" env)) sl
      


let typecheck_classDef = 
    let rec binding_classEnv = function
    | CPArg (t, i)::[]          -> (i, t)::[]
    | CPArg (t, i)::tail        -> (i, t)::binding_classEnv tail
    | []                        -> []
    in
    let env li sc = {local= li ; scope= sc; parent= None} in 
function
    | ClassDef (i, cal, cdl) ->
        List.for_all (typecheck_classDecl (env (binding_classEnv cal) i)) cdl
    

let typecheck_prog = function
    | Prog (cl) -> if (List.for_all typecheck_classDef cl) then "Typechecking successful" else "Typechecking failed"
