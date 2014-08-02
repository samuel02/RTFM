(* CoreGen *)
open Common
open AST
open Env

(* --- *)
let rec c_defs_of_classDef ce path argl cd =
  let p = path ^ "_" in
  let r = "RES_" ^ p in
  
  let rec c_of_expr = function
    | IdExp (id)       -> p ^ id
    | CallExp (m, el)  -> c_e ^ " sync " ^p ^ String.concat "_" m ^ string_par c_of_expr el ^ "; " ^ e_c
    | PendExp (il)     -> c_e ^ " pend " ^ p ^ String.concat "_" il ^ "; " ^ e_c 
    | IntExp (i)       -> string_of_int i
    | CharExp (c)      -> ecit ^ String.make 1 c ^ ecit
    | BoolExp (b)      -> string_of_bool b
  in
  
  let c_t_of_mPArg = function
    | MPArg (t, _)     -> string_of_pType t
  in
  
  let c_of_mPArg = function
    | MPArg (t, i) -> string_of_pType t ^ " " ^ p ^ i
  in
  
  let c_of_stmt ti = function
    | ExpStmt (e)     -> ti ^ tab ^ c_of_expr e
    | MPVar (t, i, e) -> ti ^ tab ^ string_of_pType t ^ " " ^ p ^ i ^ " = " ^ c_of_expr e
    | Assign (i, e)   -> ti ^ tab ^ p ^ i ^ " = " ^ c_of_expr e
    | Return (e)      -> ti ^ tab ^ "return " ^ c_of_expr e
  in
  
  (* method prototypes *)
  let c_mp_of_classDecl =
    function
    | CMDecl (t, i, al, _) -> string_of_pType t ^ " " ^ p ^ i ^ string_par c_t_of_mPArg al
    | CTDecl (i, pr, sl) -> "// task " ^ p ^ i ^ "()"
    | _ -> raise (UnMatched)
  in
  
  let c_of_classArg cal arg = match cal with
    | CPArg (t, i)      -> "const " ^ string_of_pType t ^ " " ^ p ^ i ^ " = " ^ arg
    | CMArg (t, tl, i ) -> string_of_pType t ^ " (* const " ^ p ^ i ^ ")" ^ string_par string_of_pType tl ^ " = " ^ arg
  in
  
  (* state initialization *)
  let c_state_of_classDecl = function
    | CPVar (t, i, e)       -> string_of_pType t ^ " " ^ p ^ i ^ " = " ^ c_of_expr e
    | _ -> raise (UnMatched)
  in
  
  (* method declarations *)
  let c_md_of_classDecl = 
    let claim_stmts sl =
        tab ^ "claim " ^ r ^ " { " ^ e_c ^ nl
        ^ myconcat (";" ^ nl) (List.map (c_of_stmt tab) sl)
        ^ tab ^ c_e ^ " } " ^ e_c ^ nl
    in
    function
    | CMDecl (t, i, al, sl) ->
        c_e ^ " Func " ^ string_of_pType t ^ " " ^ p ^ i ^ string_par c_of_mPArg al ^ "{" ^ nl
        ^ claim_stmts sl
        ^ c_e ^ " } " ^ e_c
    | CTDecl (i, pr, sl)    -> c_e ^ " " ^ "Task " ^ p ^ i ^ " " ^ string_of_int pr ^ " { " ^ nl
        ^ claim_stmts sl
        ^ c_e ^ " } " ^ e_c
    | CRDecl (sl)           -> c_e ^ " " ^ "Reset { " ^ nl
        ^ claim_stmts sl
        ^ c_e ^ " } " ^ e_c
    | _ -> raise (UnMatched)
  in
  
  (* span tree recusively *)
  let c_ioi_of_classDecl = function
    | COVar (o, al, i) ->
        let cd = myass o ce in
        c_defs_of_classDef ce (p ^ i) (List.map c_of_expr al) cd
    | _ -> raise (UnMatched)
  in
  
  match cd with
  | ClassDef (i, cal, cdl) ->
      "// generating RTFM-core code for " ^ i ^ ":" ^ path ^ nl
      ^ "// method prototypes for " ^ i ^ ":" ^ path ^ nl
      ^ myconcat (";" ^ nl) (mymap c_mp_of_classDecl cdl) ^ nl 
      ^ "// class instance parameters for " ^ i ^ ":" ^ path ^ nl
      ^ myconcat (";" ^ nl) (List.map2 c_of_classArg cal argl) ^ nl
      ^ "// class instance variables for " ^ i ^ ":" ^ path ^ nl
      ^ myconcat (";" ^ nl) (mymap c_state_of_classDecl cdl) ^ nl
      (* span each object instance recursively *)
      ^ myconcat nl (mymap c_ioi_of_classDecl cdl)
      ^ "// methods declarations for " ^ i ^ ":" ^ path ^ nl
      ^ myconcat nl (mymap c_md_of_classDecl cdl) ^ nl

let c_of_Prog p =
  let ce = cEnv_of_classDef p in
  let cd =
    try
      List.assoc "Root" ce
    with
    | _ -> raise (RtfmError ("Root not defined"));
  in
  match p with
  | Prog cl ->
      "// RTFM-cOOre, Per Lindgren (C) 2014" ^ nl
      ^ e_c ^ nl                                  (* escape to C *)
      ^ c_defs_of_classDef ce "Root" [] cd ^ nl   (* no args ( [] ) at top level *)
      ^ c_e ^ nl                                  (* back to RTFM-core *)