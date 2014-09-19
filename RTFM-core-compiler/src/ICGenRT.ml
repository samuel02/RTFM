(* Copyright Per Lindgren 2014, see the file "LICENSE" for the full        *)
(* license governing this code.                                            *)

(* RTFM-core/CGenRT *)

open Common
open Options
open AST
open SpecAST
open SRP

let deb s = if opt.debug then "// RTFM-core: " ^ s ^ nl else ""

let def_par par =
  let part = String.trim par in
  if mycompare part "" then "(int RTFM_id)" else "(int RTFM_id, " ^ part ^ ")"

let pass_par par =
  let part = String.trim par in
  if mycompare part "" then "(RTFM_id)" else "(RTFM_id, " ^ part ^ ")"

let c_rt_of_i dlp spec v r =

  (* resources and ceilings *)
  let quote x = "\"" ^ x ^ "\"" in
  let c_of_r rl =
    "enum resources {" ^ mycon "," ((List.map fst rl) @ ["RES_NR"]) ^ "};" ^ nl ^
    "int ceilings[] = {" ^ mycon ", " (List.map string_of_int (List.map snd rl)) ^ "};" ^ nl ^
    "const char* res_names[] = {" ^ mycon "," (List.map quote (List.map fst rl)) ^ "};" ^ nl

  in
  (* generate code for entry definitions *)
  let c_entry_of_top topl =
    let proto_top = function
      | ITask (_, dl, id, _, par, _) ->
          "// Task instance definition: @prio " ^ string_of_dl dlp dl ^ " " ^ id ^ nl ^
          "void " ^ id ^ def_par par ^ "; // function prototype for the instance task" ^ nl ^
          "void entry_" ^ id ^ "(int RTFM_id); // function prototype for the instance task" ^ nl
      | _ -> raise (UnMatched)
    in
    let entries_top = function
      | IIsr (_, id, _)               -> id
      | ITask (_, _, id, pa, _, _)    -> id
      | _                             -> raise (UnMatched)
    in
    let priorities_top = function
      | IIsr (p, _, _)                -> string_of_int p
      | ITask (_, dl, _, pa, _, _)    -> string_of_dl dlp dl
      | _                             -> raise (UnMatched)
    in
    let entries = "user_reset" :: "user_idle" :: mymap entries_top topl in
    let pre_str s1 s2 = s1 ^ s2 in
    let post_str s1 s2 = s2 ^ s1 in
    "enum entry_nr {" ^ mycon ", " ((List.map (post_str "_nr") entries) @ ["ENTRY_NR"]) ^ "};" ^ nl ^
    "int entry_prio[] = {" ^ mycon ", " ("0" :: "0" :: mymap priorities_top topl) ^ "};" ^ nl ^
    "char* entry_names[] = {" ^ mycon ", " (mymap quote entries) ^ "};" ^ nl ^ nl ^
    mycon nl (mymap proto_top topl) ^ nl ^
    "ENTRY_FUNC entry_func[] = {"^ mycon ", " ("user_reset" :: "user_idle" :: (List.map (pre_str "entry_") (mymap entries_top topl))) ^ "};" ^ nl ^ nl

  (* generate code for task arguments and entry points *)
  and c_entry_inst = function
    | ITask (_, _, id, pa, al, sl) ->
        let sl = Str.split (Str.regexp ",") al in
        let arg a = Str.split (Str.regexp "[ \t]+") a in
        let argl = List.map arg sl in
        let arga ar = match ar with
          | a :: b :: [] -> " arg_" ^ id ^ "." ^ b
          | _ -> failwith("Error parsing argument (" ^ al ^ ") of task " ^ id)
        in
        let args = mycon "," (List.map arga argl) in
        let tpar = function
          | ',' -> ';'
          | c   -> c
        in
        (* let par = lookup_itasktype_par oid topl in *)
        (* "typedef struct {" ^ String.map tpar par ^ ";} ARG_" ^ id ^ "; // type definition for arguments" ^ nl ^ *)
        "typedef struct {" ^ String.map tpar al ^ ";} ARG_" ^ id ^ "; // type definition for arguments" ^ nl ^

        "ARG_" ^ id ^ " arg_" ^ id ^ "; // instance for argument" ^ nl ^
        "void entry_" ^ id ^ "(int RTFM_id) {" ^ nl ^
        tab ^ id ^ pass_par args ^ "; // (inlined) call to the async function" ^ nl ^
        "}"
    | IFunc (_, rt, id, par, s) ->
      rt ^ " " ^ id ^ def_par par ^ "; // function prototype"
    | _ -> raise (UnMatched)

  in
  (* generate code for instances *)
  (* let rec stmts path sl =
    let nr_ref = ref 0 in
    myconcat nl (mymap (stmt path nr_ref) sl)
     and stmt path nr_ref = function
 *)
  let rec stmts path sl = myconcat nl (mymap (stmt path) sl)
  and stmt path = function
    | Claim (r, csl)          -> "RTFM_lock(RTFM_id, " ^ r ^ ");" ^ nl ^ (stmts path) csl ^ "RTFM_unlock(RTFM_id, " ^ r ^ ");"
    | Pend (be, id, par)      ->
        "arg_" ^ id ^ " = (ARG_" ^ id ^ "){" ^ par ^ "};" ^ nl ^
        "RTFM_pend(" ^ string_of_int (usec_of_time be) ^ ",  RTFM_id, " ^ id ^ "_nr);"
    | Async (af, be, id, par) ->
        "arg_" ^ id ^ " = (ARG_" ^ id ^ "){" ^ par ^ "}; " ^ nl ^
        "RTFM_async(" ^ string_of_int (usec_of_time af) ^ ", " ^ string_of_int (usec_of_time be) ^ ", RTFM_id, " ^ id ^ "_nr);"
    | Sync ( id, par )        -> id ^ pass_par par ^ ";"

    | ClaimC (c)              -> String.trim c
    | Halt                    -> "RT_halt();"

  and top = function
    | IIsr (p, id, sl)                -> "void " ^ id ^ "(int RTFM_id) {" ^ nl ^ (stmts id) sl ^ "}"
    | ITask (_, p, id, pa, par, sl)   ->
        "void " ^ id ^ def_par par ^ "{ // function implementation for the task:" ^ id ^ "[" ^ pa ^ "]" ^ nl ^
        (stmts pa) sl ^
        "}"
    | IFunc (_, r, id, par, sl)       -> r ^ " " ^ id ^ def_par par ^ "{" ^ nl ^ (stmts id) sl ^ "}"
    | IReset (sl)                     -> "void user_reset(int RTFM_id) {" ^ nl ^ (stmts "reset") sl ^ "}"
    | IIdle (sl)                      -> "void user_idle(int RTFM_id) {" ^ nl ^ (stmts "reset") sl ^ "}"
    | _                               -> raise (UnMatched)

  in
  let c_top = function
    | IC (c) -> deb ("top level code ") ^ c
    | _      -> raise (UnMatched)
  in
  let info = "const char* CORE_FILE_INFO = \"Compiled with : " ^ String.escaped (string_of_opt opt) ^ "\";" ^ nl

  in
  "// RTFM-core for RTFM-RT" ^ nl ^
  info ^ nl ^
	deb ("Includes etc.") ^ myconcat nl (mymap c_top spec) ^
  deb ("Resources and ceilings") ^ c_of_r r ^
  deb ("Entry points") ^ c_entry_of_top spec ^
  deb ("Argument instances") ^ myconcat nl (mymap c_entry_inst spec) ^ nl ^
	deb ("Application") ^ myconcat nl (mymap top spec)