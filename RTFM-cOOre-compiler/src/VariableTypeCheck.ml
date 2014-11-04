(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-cOOre/AST *)

open Common
open AST
open TypeTree

exception TypeError of string
exception NameError of string
exception ShouldNotHappen


let raise_type_error msg = raise (TypeError("TypeError: " ^ msg));;

let rec return_type_of_meth env cls mtd =
    let rec find_meth_in_class = function
        | {mi=mi;t=t;a=a;l=l}::tl   -> if (mtd = mi) then t else find_meth_in_class tl
        | []      -> raise_type_error("Method "^mtd^" not found.")
    in
    match env with
        | {ci=ci;t=t;a=a;l=l;m=m}::tl -> if (cls = ci) then find_meth_in_class m else return_type_of_meth tl cls mtd
        | []      -> raise_type_error("Method "^mtd^" not found.")

let rec type_of_var env cls mtd i =
    let in_this_scope a l=
        if List.mem_assoc i a then List.assoc i a else
        if List.mem_assoc i l then List.assoc i l else
        raise( NameError("NameError: "^i^" is not defined."))
    in
    let rec find_in_meth = function
        | {mi=mi;t=t;a=a;l=l}::tl       -> if mtd = mi then in_this_scope a l else find_in_meth tl
        | []                            -> raise( NameError("Reference to variable "^i^". Variable not found."))
    in
    match env with
        | {ci=ci;t=t;a=a;l=l;m=m}::tl   ->
            if cls = ci then
                match mtd with
                    | ""            -> in_this_scope a l
                    | _             -> try find_in_meth m with NameError (msg) -> in_this_scope a l
            else type_of_var tl cls mtd i
        | []                            -> raise( NameError("Reference to variable "^i^". Variable not found."))

let rec typecheck_meth_arg env cls mtd args =
    let rec compare_args inargs methargs = match methargs with
        | (i, t)::arg_tl  -> (match inargs with
            | in_t::in_tl   -> if t = in_t then compare_args in_tl arg_tl else raise_type_error("Incorrect input arguments to function "^mtd^".")
            | []            -> raise_type_error("Not enough input arguments to function "^mtd^"."))
        | []              -> match inargs with
            | []            -> ()
            | _             -> raise_type_error("Too many input arguments to function "^mtd^".")
    in
    let rec find_meth_in_class = function
        | {mi=mi;t=t;a=a;l=l}::tl   -> if mtd = mi then compare_args args a else find_meth_in_class tl
        | []      -> raise_type_error("Method "^mtd^" not found.")
    in
    match env with
        | {ci=ci;t=t;a=a;l=l;m=m}::tl -> if cls = ci then find_meth_in_class m else typecheck_meth_arg tl cls mtd args
        | []      -> raise_type_error("Method "^mtd^" not found.")

let rec class_of_instance env cls cls_instance =
    let rec instance_in_scope = function
        | (i, t)::tl -> if i = cls_instance then
            match t with
                | ClassInstance(o)  -> o
                | _                 -> raise(NameError(cls_instance^" is not a class instance"))
        else instance_in_scope tl
        | []    -> raise(NameError("Could not find class instance "^cls_instance^" in scope"^cls))
    in
    match env with
        | {ci=ci;t=t;a=a;l=l;m=m}::tl -> if cls = ci then instance_in_scope l else class_of_instance tl cls cls_instance
        | []                          -> raise(NameError("Could not find class instance "^cls_instance))


let rec typecheck_class_arg env cls args =
    let rec compare_args inargs methargs = match methargs with
        | (i, t)::arg_tl  -> (match inargs with
            | in_t::in_tl   -> if t = in_t then compare_args in_tl arg_tl else raise_type_error("Incorrect instance arguments to class "^cls^".")
            | []            -> raise_type_error("Not enough instance arguments to class "^cls^"."))
        | []              -> match inargs with
            | []            -> ()
            | _             -> raise_type_error("Too many instance arguments to class "^cls^".")
    in
    match env with
        | {ci=ci;t=t;a=a;l=l;m=m}::tl -> if cls = ci then compare_args args a else typecheck_class_arg tl cls args
        | []      -> raise_type_error("Class "^cls^" not found.")


let rec type_of id env =
    try List.assoc id env
    with Not_found  ->
        raise (NameError("NameError: " ^ id ^ " is not defined."))

let unify t1 t2 =
    if t1 == t2 then t1
    else
        raise_type_error ("Cannot compare " ^ string_of_pType t1 ^ " with " ^ string_of_pType t2 ^ ".")

let in_list rt l =
    let equal rt t = rt = t in
    List.exists (equal rt) l

let typecheck_op scope_tree class_name meth_name op t1 t2 =
    let rt = unify t1 t2 in
    let type_error_msg t = string_of_op op ^ " operator is not defined for type " ^ string_of_pType t ^ "." in
    match op with
    | OpPlus | OpSub                 -> if in_list rt [Int] then rt   else raise_type_error(type_error_msg rt)
    | OpMult | OpDiv | OpMod         -> if in_list rt [Int]      then rt   else raise_type_error(type_error_msg rt)
    | OpGrt  | OpGeq | OpLet | OpLeq -> if in_list rt [Int;Byte] then Bool else raise_type_error(type_error_msg rt)
    | OpEq   | OpNeq                 -> if in_list rt [Int;Char;Bool;Byte] then Bool else raise_type_error(type_error_msg rt)

let class_of_call env idl cls = match idl with
    | c::m::[]       -> class_of_instance env cls c
    | m::[]          -> cls
    | _              -> raise(ShouldNotHappen)

let meth_of_call idl = match idl with
    | c::m::[]   -> m
    | m::[]      -> m
    | _              -> raise(ShouldNotHappen)

let rec typecheck_expr scope_tree class_name meth_name = function
    | IndexExp (idl, e)         -> if ((typecheck_expr scope_tree class_name meth_name e) = Int) && (type_of_var scope_tree (class_of_call scope_tree idl class_name) meth_name (meth_of_call idl) = String) then Char else raise_type_error ("Incorrect string indexing")
    | MathExp (op, a, b)        -> typecheck_op scope_tree class_name meth_name op (typecheck_expr scope_tree class_name meth_name a)  (typecheck_expr scope_tree class_name meth_name b)
    | CompExp (op, e1, e2)      -> typecheck_op scope_tree class_name meth_name op (typecheck_expr scope_tree class_name meth_name e1) (typecheck_expr scope_tree class_name meth_name e2)
    | ParExp (e)                -> typecheck_expr scope_tree class_name meth_name e
    | IdExp (idl)               -> type_of_var scope_tree (class_of_call scope_tree idl class_name) meth_name (meth_of_call idl)
    | CallExp (m, el)           -> 
        typecheck_meth_arg scope_tree (class_of_call scope_tree m class_name) (meth_of_call m) (List.map (typecheck_expr scope_tree class_name meth_name) el);
        return_type_of_meth scope_tree (class_of_call scope_tree m class_name) (meth_of_call m)
    | AsyncExp (af, be, il, el) -> Void
    | PendExp (il)              -> Void
    | IntExp (i)                -> Int
    | CharExp (c)               -> Char
    | BoolExp (b)               -> Bool
    | StrExp (s)                -> String
    | RT_Rand (e)               -> Int
    | RT_Getc                   -> Char

let rec typecheck_stmt scope_tree class_name meth_name = function
    | Stmt (sl)         -> List.map (typecheck_stmt scope_tree class_name meth_name) sl; ()
    | RT_Printf (s, el) -> List.map (typecheck_expr scope_tree class_name meth_name) el; ()
    | ExpStmt (e) | RT_Sleep (e) | RT_Putc (e) -> typecheck_expr scope_tree class_name meth_name e; ()
    | Return (e)        -> if (typecheck_expr scope_tree class_name meth_name e) = (return_type_of_meth scope_tree class_name meth_name) then () else raise_type_error ("Method "^meth_name^" has return_type " ^string_of_pType (return_type_of_meth scope_tree class_name meth_name)^".")
    | MPVar (t, i, e)   -> if typecheck_expr scope_tree class_name meth_name e = t then () else raise_type_error ("Cannot assign " ^ string_of_pType (typecheck_expr scope_tree class_name meth_name e) ^ " " ^ string_of_expr e ^ " to " ^ string_of_pType (type_of_var scope_tree class_name meth_name i) ^ " " ^ i ^ ".")
    | Assign (i, e)     -> if type_of_var scope_tree class_name meth_name i = typecheck_expr scope_tree class_name meth_name e then () else raise_type_error ("Cannot assign " ^ string_of_pType (typecheck_expr scope_tree class_name meth_name e) ^ " " ^ string_of_expr e ^ " to " ^ string_of_pType (type_of_var scope_tree class_name meth_name i) ^ " " ^ i ^ ".")
    | If (e, s)         -> if in_list (typecheck_expr scope_tree class_name meth_name e) [Bool; Int] then typecheck_stmt scope_tree class_name meth_name s else raise_type_error ("Condition in if-statement must be evaluated to type int or bool.")
    | Else (s)          -> typecheck_stmt scope_tree class_name meth_name s
    | While (e, s)      -> if in_list (typecheck_expr scope_tree class_name meth_name e) [Bool; Int] then typecheck_stmt scope_tree class_name meth_name s else raise_type_error ("Condition in while-statement must be evaluated to type int or bool.")

let typecheck_classDecl scope_tree class_name = function
    | CPVar (t, i, e)        -> if typecheck_expr scope_tree class_name "" e = t then () else  raise_type_error ("Cannot assign " ^ string_of_pType (typecheck_expr scope_tree class_name "" e) ^ " " ^ string_of_expr e ^ " to " ^ string_of_pType (type_of_var scope_tree class_name "" i) ^ " " ^ i ^ ".")
    | COVar (o, el, i)       -> typecheck_class_arg scope_tree o (List.map (typecheck_expr scope_tree class_name "") el)
    | ExtMDecl (t, i, tl)    -> ()
    | CMDecl (t, i, al, sl)  -> List.map (typecheck_stmt scope_tree class_name i) sl; ()
    | CTaskDecl (i, al, sl ) -> List.map (typecheck_stmt scope_tree class_name i) sl; ()
    | CIsrDecl (pr, i, sl)   -> List.map (typecheck_stmt scope_tree class_name i) sl; ()
    | CResetDecl (sl)        -> List.map (typecheck_stmt scope_tree class_name "Reset") sl; ()
    | CIdleDecl (sl)         -> List.map (typecheck_stmt scope_tree class_name "Idle") sl; ()

let typecheck_classDef scope_tree = function
    | ClassDef (i, cal, e, cdl)    -> List.map (typecheck_classDecl scope_tree i) cdl

let typecheck_prog scope_tree = function
    | Prog (cl) -> List.map (typecheck_classDef scope_tree) cl; "Passed type checking.\n"
