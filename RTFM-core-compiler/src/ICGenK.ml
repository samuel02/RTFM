(* Copyright Per Lindgren 2014, see the file "LICENSE" for the full        *)
(* license governing this code.                                            *)

(* RTFM-core/CGenK *)

open Common
(* open Time *)
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

let c_rt_of_i dlp spec r id_vid t_div =
  
  (* resources and ceilings *)
  let quote x = "\"" ^ x ^ "\"" in
  let c_of_r rl =
    "enum resources {" ^ mycon "," ((List.map fst rl) @ ["RES_NR"]) ^ "};" ^ nl ^
    "int ceilings[] = {" ^ mycon ", " (List.map string_of_int (List.map snd rl)) ^ "};" ^ nl ^
    "const char* res_names[] = {" ^ mycon "," (List.map quote (List.map fst rl)) ^ "};" ^ nl
  
  in
  (* generate code for entry definitions *)
  let c_entry_of_top topl =
    let entry_info id dl = 
      "void entry_" ^ id ^ "(int RTFM_id); // function prototype for the instance task" ^ nl ^
      "volatile RT_Time bl_" ^ id ^ " = 0;" ^ nl ^ 
      "volatile RT_Time new_bl_" ^ id ^ " = 0;" ^ nl ^ 
      "const RT_Time dl_" ^ id ^ " = " ^ Time.c_of_time dl t_div ^ ";" ^ nl 
    in  
    let proto_top = function
      | ITask (_, dl, id, _, par, _) ->
          "// Task instance definition: prio " ^ string_of_dl dlp dl ^ " " ^ id ^ nl ^
          "void " ^ id ^ def_par par ^ "; // function prototype for the instance task" ^ nl ^
          entry_info id dl
          
      | IIsr (dl, id, _) ->
          "// Isr prio " ^ string_of_dl dlp dl ^ " " ^ id ^ nl ^
          entry_info id dl
      | IReset (_) ->
          "// Reset " ^ 
          entry_info "user_reset" Time.Infinite
      
      | IIdle (_) ->
          "// Idle " ^ 
          entry_info "user_idle" Time.Infinite
            
      | _ -> raise (UnMatched)
    in
    let entries_top = function
      | IIsr (_, id, _)               -> id
      | ITask (_, _, id, pa, _, _)    -> id
      | _                             -> raise (UnMatched)
    in
    let priorities_top = function
      | IIsr (dl, _, _)               -> string_of_dl dlp dl
      | ITask (_, dl, _, pa, _, _)    -> string_of_dl dlp dl
      | _                             -> raise (UnMatched)
    in
    (* let entries = "user_reset" :: "user_idle" :: mymap entries_top topl in *)
    let entries = mymap entries_top topl in
    (* let pre_str s1 s2 = s1 ^ s2 in *)
    let post_str s1 s2 = s2 ^ s1 in 
    (*"enum entry_nr {" ^ mycon ", " ((List.map (post_str "_nr") entries) @ ["ENTRY_NR"]) ^ "};" ^ nl ^ *)
    "enum entry_nr {" ^ mycon ", " (mymap (IsrCGen.entries_enum id_vid) entries) ^ "};" ^ nl ^ 
    "const int entry_vi[] = {" ^ mycon ", " (List.map (post_str "_nr") entries) ^ "};" ^ nl ^
    "int entry_prio[] = {" ^ mycon ", " (mymap priorities_top topl) ^ "};" ^ nl ^ 
    "const char* entry_names[] = {" ^ mycon ", " (mymap quote entries) ^ "};" ^ nl ^ nl ^ 
    mycon nl (mymap proto_top topl) ^ nl 
    (* ^ 
    "ENTRY_FUNC entry_func[] = {"^ mycon ", " ("user_reset" :: "user_idle" :: (List.map (pre_str "entry_") (mymap entries_top topl))) ^ "};" ^ nl ^ nl 
   *)
  
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
        (* alternative solution suing the ITaskDefType for generating typedef, not used, and may be omitted in th future *)
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
 
  let rec stmts path sl = myconcat "" (mymap (stmt path) sl) 
  and stmt path = function
    | Claim (r, csl)          -> "RTFM_lock(RTFM_id, " ^ r ^ ");" ^ nl ^ (stmts path) csl ^ "RTFM_unlock(RTFM_id, " ^ r ^ ");" ^ nl
    | Pend (be, id, par)      -> 
      "arg_" ^ id ^ " = (ARG_" ^ id ^ "){" ^ par ^ "};" ^ nl ^
      "RTFM_pend(" ^ Int64.to_string (Time.usec_of_time be) ^ ",  RTFM_id, " ^ id ^ "_nr);" ^ nl
    | Async (mi, af, be, id, spar) ->
      (
      match spar with 
        | Some par -> 
          nl ^ "arg_" ^ id ^ " = (ARG_" ^ id ^ "){" ^ par ^ "};" ^ nl 
        | None     -> nl
      ) ^ 
      nl ^ "new_bl_" ^ id ^ " = " ^ "bl_" ^ path ^ " + " ^ Time.c_of_time af t_div ^ ";" ^ nl ^ 
      (
        match mi with
        | Some i -> "RT_msg " ^ i ^ " = "
        | None   -> ""
      ) ^ 
      "RTFM_async(" ^ Time.c_of_time af t_div ^ ", " ^ Time.c_of_time be t_div ^ ", bl_" ^ path ^ ", " ^ id ^ "_nr);" ^ nl  
    
    | Sync ( id, par )        -> id ^ pass_par par ^ ";" ^ nl
    
    | StmtC (c)               -> String.trim c
    | State (s)               -> s
    | Halt (s)                -> "RT_halt(" ^ s ^ ");" ^ nl
    | Abort (par)             -> "RT_abort(" ^ par ^ ");" ^ nl
    | Return (c, cs)          -> 
      (
      match cs with
        | [] -> "return " ^ String.trim c ^ "; // return outside claim (native C return)" ^ nl
        | _  ->
        "// code generated to return from within claims : " ^ myconcat "," cs ^ nl ^
        "{ ret_val = " ^ String.trim c ^ ";" ^ nl ^
        myconcat nl (mymap (fun r -> "RTFM_unlock(RTFM_id, " ^ r ^ ");") cs) ^ 
        "return ret_val; }" ^ nl
       ) 
    | Break(n, cs)               ->
      (
      match cs with
      | []      -> "break; // break outside claim (native C break)" ^ nl 
      | cl :: _ ->
        let rec n_cs i = function
         | []     -> raise (RtfmError("The given break depth " ^ string_of_int n ^ ", is larger than the actual claim depth " ^ string_of_int (List.length cs)))
         | r :: l -> if (i > 1) then r :: n_cs (i-1) l else [r]
        in
        "// code generated to break within claims : " ^ myconcat "," cs ^ nl ^
        "{ " ^ myconcat nl (mymap (fun r -> "RTFM_unlock(RTFM_id, " ^ r ^ ");") (n_cs n cs)) ^ 
        "break; }" ^ nl
      )
    | Continue (n, cs)               ->
      (
      match cs with
      | []      -> "continue; // continue outside claim (native C break)" ^ nl 
      | cl :: _ ->
        let rec n_cs i = function
         | []     -> raise (RtfmError("The given continue depth " ^ string_of_int n ^ ", is larger than the actual continue depth " ^ string_of_int (List.length cs)))
         | r :: l -> if (i > 1) then r :: n_cs (i-1) l else [r]
        in
        "// code generated to continue within claims : " ^ myconcat "," cs ^ nl ^
        "{ " ^ myconcat nl (mymap (fun r -> "RTFM_unlock(RTFM_id, " ^ r ^ ");") (n_cs n cs)) ^ 
        "continue; }" ^ nl
      )  
    | Goto (l, n, cs)               ->
      match cs with
      | []      -> "goto " ^ l ^ " // goto outside claim (native C goto) " ^ nl 
      | cl :: _ ->
        let rec n_cs i = function
         | []     -> raise (RtfmError("The given goto depth " ^ string_of_int n ^ ", is larger than the actual goto depth " ^ string_of_int (List.length cs)))
         | r :: l -> if (i > 1) then r :: n_cs (i-1) l else [r]
        in
        "// code generated to goto within claims : " ^ myconcat "," cs ^ nl ^
        "{ " ^ myconcat nl (mymap (fun r -> "RTFM_unlock(RTFM_id, " ^ r ^ ");") (n_cs n cs)) ^ 
        "goto " ^ l ^ "; }" ^ nl
       
  and top = function
    | IIsr (dl, id, sl)                -> 
        "// Isr prio " ^ string_of_dl dlp dl ^ " " ^ id ^ nl ^
        "void " ^ id ^ "() {" ^ nl ^ 
        "bl_" ^ id ^ " = " ^ "new_bl_" ^ id ^ ";" ^ nl ^
        (stmts id) sl ^ nl ^ 
        "}"
    | ITask (_, dl, id, pa, par, sl)   ->
        "// Task instance implementation: prio " ^ string_of_dl dlp dl ^ " " ^ id ^ nl ^
        "void " ^ List.assoc id id_vid ^ def_par par ^ "{" ^ nl ^ 
        (stmts pa) sl ^ nl ^
        "}"
    | IFunc (_, r, id, par, sl)      -> 
      r ^ " " ^ id ^ def_par par ^ "{" ^ nl ^ 
      r ^ " ret_val;" ^ nl ^ 
     (stmts id) sl ^ "}" ^ nl
      
    | IReset (sl)                     -> "void user_reset() {" ^ nl ^ (stmts "user_reset") sl ^ nl ^ "}"
    | IIdle (sl)                      -> "void user_idle() {" ^ nl ^ (stmts "user_idle") sl ^ nl ^ "}"
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
