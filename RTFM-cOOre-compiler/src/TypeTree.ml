(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-cOOre/AST *)

open Common
open AST

type binding = id * pType

type meth = {mi: id; t: pType; a: binding list; l: binding list}
type cls  = {ci: id; t: pType; a: binding list; l: binding list; m: meth list}

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
    | CMDecl (t, i, al, sl)::tl  -> {mi=i; t=t; a=(binding_argList al); l=(list_of_scope_stmt sl)}::list_of_scope_classDecl tl
    | CTaskDecl (i, al, sl)::tl  -> {mi=i; t=Void; a=(binding_argList al); l=(list_of_scope_stmt sl)}::list_of_scope_classDecl tl
    | CIsrDecl (pr, i, sl)::tl   -> {mi=i; t=Void; a=[]; l=(list_of_scope_stmt sl)}::list_of_scope_classDecl tl
    | CResetDecl (sl)::tl        -> {mi="Reset"; t=Void; a=[]; l=(list_of_scope_stmt sl)}::list_of_scope_classDecl tl
    | CIdleDecl (sl)::tl         -> {mi="Idle";  t=Void; a=[]; l=(list_of_scope_stmt sl)}::list_of_scope_classDecl tl
    | _::tl                      -> list_of_scope_classDecl tl
    | []                         -> []

let rec list_of_local_scope_classDecl = function
    | CPVar (t, i, e) ::tl       -> (i, t)::list_of_local_scope_classDecl tl
    | COVar (o, el, i) ::tl      -> (i, ClassInstance(o)):: list_of_local_scope_classDecl tl
    | _::tl                      -> list_of_local_scope_classDecl tl
    | []                         -> []

let rec list_of_scope_classDef =
    let rec binding_classEnv = function
    | CPArg (t, i)::tl          -> (i, t)::binding_classEnv tl
    | CMArg (t, al, i)::tl      -> binding_classEnv tl
    | []                        -> []
    in
    let rec method_arguments_classEnv =
        let enumerate_bindings i t = ("arg"^string_of_int i, t) in
    function
    | CPArg (t, i)::tl          -> method_arguments_classEnv tl
    | CMArg (t, al, i)::tl      -> {mi=i;t=t;a=(List.mapi enumerate_bindings al);l=[]}::method_arguments_classEnv tl
    | []                        -> []
    in
    function
    | ClassDef (i, cal, e, cdl)::tl ->
        {ci=i; t=Class; a=(binding_classEnv cal); l=(list_of_local_scope_classDecl cdl); m=(method_arguments_classEnv cal @ list_of_scope_classDecl cdl)}::list_of_scope_classDef tl
    | []                         -> []

let build_scope_tree = function
    | Prog (cl) -> list_of_scope_classDef cl

let rec string_of_class =
    let rec string_of_bind_list = function
        | []                -> ""
        | (i, t)::[]        -> string_of_pType t ^ " " ^ i
        | (i, t)::tl        -> string_of_pType t ^ " " ^ i ^ ", " ^ string_of_bind_list tl
    in
    let rec string_of_methods = function
        | {mi=mi;t=t;a=a;l=l}::tl     -> "    "^string_of_pType t ^ " " ^ mi ^ " ( " ^  string_of_bind_list a ^ " ) \n      { " ^  string_of_bind_list l ^ " }\n" ^ string_of_methods tl
        | []                          -> ""
    in
function
    | {ci=ci;t=t;a=a;l=l;m=m}::tl -> string_of_pType t ^ " " ^ ci ^ " ( " ^ string_of_bind_list a ^ " ) \n { " ^ string_of_bind_list l ^ " }\n" ^ string_of_methods m ^ nl ^ string_of_class tl
    | []                          -> ""
