(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-cOOre/AST *)

open Common
open AST

type binding = id * pType
type scope = {i: id ; t: pType; a: binding list; l: binding list; s: scope list}

let rec list_of_scope_stmt = function
    | MPVar (t, i, e)::tl       -> (i, t)::list_of_scope_stmt tl
    | Stmt (sl)::tl             -> list_of_scope_stmt sl   @ list_of_scope_stmt tl
    | If (e, s)::tl             -> list_of_scope_stmt [s]  @ list_of_scope_stmt tl
    | Else (s)::tl              -> list_of_scope_stmt [s]  @ list_of_scope_stmt tl
    | While (e, s)::tl          -> list_of_scope_stmt [s]  @ list_of_scope_stmt tl
    | _::tl                     -> list_of_scope_stmt tl
    | []                        -> []
    
let rec list_of_scope_classDecl =
  let rec binding_argList = function
    | MPArg (t, i)::tl           -> (i, t)::binding_argList tl
    | []                         -> []
    in
    function
    | CMDecl (t, i, al, sl)::tl  -> {i=i; t=t; a=(binding_argList al); l=(list_of_scope_stmt sl); s=[]}::list_of_scope_classDecl tl
    | CTaskDecl (i, al, sl )::tl -> {i=i; t=Void; a=(binding_argList al); l=(list_of_scope_stmt sl); s=[]}::list_of_scope_classDecl tl
    | CIsrDecl (pr, i, sl)::tl   -> {i=i; t=Void; a=[]; l=(list_of_scope_stmt sl); s=[]}::list_of_scope_classDecl tl
    | CResetDecl (sl)::tl        -> {i="Reset"; t=Void; a=[]; l=(list_of_scope_stmt sl); s=[]}::list_of_scope_classDecl tl
    | CIdleDecl (sl) ::tl        -> {i="Idle"; t=Void; a=[]; l=(list_of_scope_stmt sl); s=[]}::list_of_scope_classDecl tl
    | _::tl                      -> list_of_scope_classDecl tl
    | []                         -> []

let rec list_of_local_scope_classDecl = function
    | CPVar (t, i, e) ::tl       -> (i, t):: list_of_local_scope_classDecl tl
    | COVar (o, el, i) ::tl      -> (i, ClassInstance):: list_of_local_scope_classDecl tl
    | _::tl                      -> list_of_local_scope_classDecl tl
    | []                         -> []

let rec list_of_scope_classDef =
    let rec binding_classEnv = function
    | CPArg (t, i)::[]          -> (i, t)::[]
    | CPArg (t, i)::tl          -> (i, t)::binding_classEnv tl
    | []                        -> []
    in
    function
    | ClassDef (i, cal, cdl)::tl ->
        {i=i; t=Class; a=(binding_classEnv cal); l=(list_of_local_scope_classDecl cdl); s=(list_of_scope_classDecl cdl)}::list_of_scope_classDef tl
    | []                         -> []

let build_scope_tree = function
    | Prog (cl) -> list_of_scope_classDef cl


let rec string_of_scope_tree ti =
    let rec string_of_bind_list = function
        | (i, t)::[]        -> string_of_pType t ^ " " ^ i
        | (i, t)::tl        -> string_of_pType t ^ " " ^ i ^ ", " ^ string_of_bind_list tl
        | []                -> ""
    in
    let string_of_arguments = function
        | []     -> ""
        | a      -> ti ^ "Arguments: " ^ string_of_bind_list a ^ "\n"
    in
    let string_of_locals = function
        | []     -> ""
        | l      -> ti ^ "Locals: " ^ string_of_bind_list l ^ "\n"
    in
    let string_of_children = function
        | []     -> ""
        | s      -> ti ^ "Children:\n" ^ string_of_scope_tree (ti^"   ") s ^ "\n"
    in
    function
    | {i=i;t=t;a=a;l=l;s=s}::tl -> ti ^ string_of_pType t ^ " " ^ i ^ "\n"^ti^"--------------\n" ^ string_of_arguments a ^ string_of_locals l ^ string_of_children s ^ string_of_scope_tree ti tl
    | []                        -> ""
