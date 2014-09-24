(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-cOOre/CoreGen *)

open Common
open Options
open AST
open Env

let deb s = if opt.debug then "// RTFM-cOOre : " ^ s ^ nl else ""

let rec c_defs_of_classDef ce path argl cd =
  let p = path ^ "_" in
  let r = "RES_" ^ p in

  let rec c_of_expr = function
    | IdExp (idl)               -> p ^ String.concat "_" idl
    | CallExp (m, el)           -> c_e ^ " sync " ^ p ^ String.concat "_" m ^ string_par c_of_expr el ^ "; " ^ e_c
    | AsyncExp (af, be, il, el) -> c_e ^ " async" ^
      (if (usec_of_time af > 0) then " after " ^ string_of_time af else "") ^
      (if (usec_of_time be > 0) then " before " ^ string_of_time be else "") ^
      " " ^ p ^ String.concat "_" il ^ string_par c_of_expr el ^ "; " ^ e_c
    | PendExp _                 -> raise (RtfmError ("PendExp not implemented"))
    | IntExp (i)                -> string_of_int i
    | MathExp (e, a, b)         -> c_of_expr a ^ " " ^ String.make 1 e ^ " " ^ c_of_expr b
    | ParExp (e)                -> "(" ^ c_of_expr e ^ ")"
    | CharExp (c)               -> ecit ^ String.make 1 c ^ ecit
    | BoolExp (b)               -> string_of_bool b
    | RT_Rand (e)               -> "RT_rand(" ^ c_of_expr e ^ ")"
    | RT_Getc                   -> "RT_getc()"

  in

  let c_of_mPArg = function
    | MPArg (t, i) -> string_of_pType t ^ " " ^ p ^ i
  in

  let rec c_of_stmt ti = function
    | ExpStmt (e)       -> ti ^ tab ^ c_of_expr e ^ sc ^ nl
    | MPVar (t, i, e)   -> ti ^ tab ^ string_of_pType t ^ " " ^ p ^ i ^ " = " ^ c_of_expr e ^ sc ^ nl
    | Assign (i, e)     -> ti ^ tab ^ p ^ i ^ " = " ^ c_of_expr e ^ sc ^ nl
    | Return (e)        -> ti ^ tab ^ "return " ^ c_of_expr e ^ sc ^ nl
    | If (e, sl)        -> ti ^ tab ^ "if ( " ^ c_of_expr e ^ " )" ^ op ^ String.concat ("") (List.map (c_of_stmt (ti^tab)) sl) ^ ti ^ tab ^ cl ^ nl
    | Else (sl)         -> ti ^ tab ^ "else" ^ op ^ String.concat ("") (List.map (c_of_stmt (ti^tab)) sl) ^ ti ^ tab ^ cl ^ nl
    | RT_Sleep (e)      -> ti ^ tab ^ "RT_sleep(" ^ c_of_expr e ^ ")"  ^ sc ^ nl
    | RT_Printf (s, el) -> ti ^ tab ^ "RT_printf(" ^ String.concat ", " ((ec ^ s ^ ec) :: List.map c_of_expr el) ^ ")" ^ sc ^ nl
    | RT_Putc (e)       -> ti ^ tab ^ "RT_putc(" ^ c_of_expr e ^ ")"  ^ sc ^ nl
  in

  let c_of_classArg cal arg = match cal with
    | CPArg (t, i)      -> "const " ^ string_of_pType t ^ " " ^ p ^ i ^ " = " ^ arg
    | CMArg (t, tl, i ) ->
      let nri = ref 0 in (* ugly but works to blend in some imperative stuff *)
      let argin t = nri := !nri + 1; string_of_pType t ^ " i" ^ string_of_int !nri in
      let nro = ref 0 in
      let argout t = nro := !nro + 1; "i" ^ string_of_int !nro in
      c_e ^ " Func " ^ string_of_pType t ^ " " ^ p ^ i ^ string_par argin tl ^ " { sync " ^ arg ^ string_par argout tl ^ "; } " ^ e_c
  in

  (* state initialization *)
  let c_state_of_classDecl = function
    | CPVar (t, i, e)       -> string_of_pType t ^ " " ^ p ^ i ^ " = " ^ c_of_expr e ^ sc
    | _ -> raise (UnMatched)
  in

  (* method declarations *)
  let c_md_of_classDecl =
    let claim_stmts sl =
        tab ^ "claim " ^ r ^ " { " ^ e_c ^ nl
        ^ String.concat "" (List.map (c_of_stmt tab) sl)
        ^ tab ^ c_e ^ " } " ^ e_c ^ nl
    in
    function
    | CMDecl (t, i, al, sl)  ->
        c_e ^ " Func " ^ string_of_pType t ^ " " ^ p ^ i ^ string_par c_of_mPArg al ^ "{" ^ nl
        ^ claim_stmts sl
        ^ c_e ^ " } " ^ e_c

    | CTaskDecl (i, al, sl)     ->
      let c_data_of_mPArg path = function
        | MPArg (t, i) -> string_of_pType t ^ " " ^ path ^ "_" ^ i

        in
        c_e ^ " " ^ "Task " ^ p ^ i ^ string_par (c_data_of_mPArg path) al ^ " { "  ^ nl ^
        claim_stmts sl ^
        c_e ^ " } " ^ e_c

    | CResetDecl (sl)           ->
        c_e ^ " " ^ "Reset {" ^ nl ^
        claim_stmts sl ^
        c_e ^ " } " ^ e_c
    | CIdleDecl (sl)            ->
        c_e ^ " " ^ "Idle {" ^ nl ^
        claim_stmts sl ^
        c_e ^ " } " ^ e_c
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
      deb ("generating RTFM-core code for " ^ i ^ ":" ^ path) ^
      deb ("method prototypes for " ^ i ^ ":" ^ path) ^
      (* String.concat (";" ^ nl) (mymap c_mp_of_classDecl cdl) ^ nl ^   *)
      deb ("class instance parameters for " ^ i ^ ":" ^ path) ^
      String.concat (";" ^ nl) (List.map2 c_of_classArg cal argl) ^ nl ^
      deb ("class instance variables for " ^ i ^ ":" ^ path) ^
      String.concat (";" ^ nl) (mymap c_state_of_classDecl cdl) ^ nl ^
      String.concat nl (mymap c_ioi_of_classDecl cdl) ^  (* span each object instance recursively *)
      deb ("methods declarations for " ^ i ^ ":" ^ path) ^
      String.concat nl (mymap c_md_of_classDecl cdl) ^ nl

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
      "// RTFM-cOOre, Per Lindgren (C) 2014" ^ nl ^
      e_c ^ nl ^                                  (* escape from RTFM-core to C *)
      c_defs_of_classDef ce "Root" [] cd ^ nl ^   (* no args ( [] ) at top level *)
      c_e ^ nl                                    (* escpae back to RTFM-core *)