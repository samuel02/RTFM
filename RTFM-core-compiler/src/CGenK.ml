(* Copyright Per Lindgren 2014, see the file "LICENSE" for the full        *)
(* license governing this code.                                            *)

(* RTFM-core/CGenK *)

open Common
open Options
open AST


let deb s = if opt.debug then "// RTFM-core: " ^ s ^ nl else ""

let def_par par = 
  let part = String.trim par in
  if mycompare part "" then "()" else "(" ^ part ^ ")"

let pass_par par = 
  let part = String.trim par in
  if mycompare part "" then "()" else "(" ^ part ^ ")"

let ck_of_p topl v r tidl =
  let quote x = "\"" ^ x ^ "\"" in
  let c_of_r rl =
    "enum resources {" ^ mycon "," ((List.map fst rl) @ ["RES_NR"]) ^ "};" ^ nl ^
    "const int ceilings[] = {" ^ mycon ", " (List.map string_of_int (List.map snd rl)) ^ "};" ^ nl ^ 
    "char* res_names[] = {" ^ mycon "," (List.map quote (List.map fst rl)) ^ "};" ^ nl 
  
  in
  let c_entry_of_top topl =
    let proto_top =
      let tpar = function
        | ',' -> ';' 
        | c -> c
      in
      function
      | TaskDef (id, par, _) ->
          "// Task defintion:" ^ id ^ nl ^
          "void " ^ id  ^ def_par par ^ "; // function prototype for the task" ^ nl ^ 
          "typedef struct {" ^ String.map tpar par ^ ";} ARG_" ^ id ^ "; // type definition for arguments"^ nl
      | Task (p, id, par, _) ->
          let hw = myass id tidl in
          "// Task instance defintion: @prio " ^ string_of_int p ^ " " ^ id ^  nl ^
          "void " ^ hw ^ "(); // function prototype for the instance task" ^ nl 
      | _ -> raise (UnMatched)
    in    
    let entries_top s = function
      | Isr (_, id, _)     -> id ^ s
      | Task (_, id, _, _) -> id ^ s
      | _                  -> raise (UnMatched)  
    
    in    
    let entries_enum_top = 
      let rec vindex n i = function
        | [] -> failwith ("Entry not found" ^ i )
        | (id, vid) :: l when compare id i == 0 -> n
        | _ :: l -> vindex (n+1) i l
      in
      let index id = id ^ "_nr = " ^ string_of_int (vindex (-16) id tidl) in 
      function
      | Isr (_, id, _)     -> index id
      | Task (_, id, _, _) -> index id
      | _                  -> raise (UnMatched)
      
    in
    let priorities_top = function
      | Isr (p, _, _)      -> string_of_int p
      | Task (p, _, _, _)  -> string_of_int p  
      | _                  -> raise (UnMatched) 
    in
    let entries = mymap (entries_top "") topl in
    
    "enum entry_nr {" ^ mycon ", " (mymap entries_enum_top topl) ^ "};" ^ nl ^
    "const int entry_vi[] = {" ^ mycon ", " (mymap (entries_top "_nr") topl ) ^ "};" ^ nl ^  
    "const int entry_prio[] = {" ^ mycon ", " (mymap priorities_top topl) ^ "};" ^ nl ^ 
    "// const char* entry_names[] = {" ^ mycon ", " (mymap quote entries) ^ "};" ^ nl ^ nl ^
    mycon nl (mymap proto_top topl) ^ nl ^
    "// ENTRY_FUNC entry_func[] = {" ^ mycon ", " entries ^ "};" ^ nl ^ nl
      
  in
  let rec pargs path sl = 
    let nr_ref = ref 0 in
    mycon nl (mymap (parg path nr_ref) sl) 
  and parg path nr_ref = 
    function
    | Claim (r, csl)         -> pargs path csl 
    | Async (_, p, id, par)  ->
      (
       let nr = !nr_ref in
        nr_ref := nr + 1;
        match Env.lookup_task id topl with 
        | TaskDef (id, al, sl) -> 
          let idp = path ^ "_" ^ id ^ if nr > 0 then string_of_int nr else "" in
          let hw = myass idp tidl in
          let sl = Str.split (Str.regexp ",") al in
          let arg a = Str.split (Str.regexp "[ /t]+") a in
          let argl = List.map arg sl in
          let arga ar = match ar with
          | a :: b :: [] -> " arg_" ^ idp ^ "." ^ b
          | _ -> failwith("Error parsing argument of task" ^ id)
          in 
          let args = mycon "," (List.map arga argl) in
          "ARG_" ^ id ^ " arg_" ^ idp ^ "; // instance for argument" ^ nl ^
          "void " ^ hw ^ "() {" ^ nl ^
          tab ^ id ^ pass_par args ^ "; // (inlined) call to the async function" ^ nl ^
          "}"
        | _                  -> raise (RtfmError("Lookup failed in Env.lookup_task id topl"))
      )
     | Sync (id, par)        -> pargs (path ^ "_" ^ id) (Env.lookup_func_sl id topl)  
     | _                     -> ""
  
  and ptop = function
     | Isr (p, id, sl)       -> pargs id sl
     | Task (p, id, _, sl)   -> pargs id sl
     | Reset (sl)            -> pargs "reset" sl 
     | _                     -> raise (UnMatched)
  
  in
  let rec stmts path sl = 
    let nr_ref = ref 0 in
    myconcat nl (mymap (stmt path nr_ref) sl) 
  and stmt path nr_ref = function
     | Claim (r, csl)           -> "RTFM_lock(" ^ r ^ ");" ^ nl ^ (stmts path) csl ^ "RTFM_unlock(" ^ r ^ ");"
     | Pend (_, id)             -> "RTFM_pend(" ^ id ^ "_nr);"
     | Async (_, pr, id, par)   ->
      let nr = !nr_ref in
        nr_ref := nr + 1;
         let idp = path ^ "_" ^ id ^ if nr > 0 then string_of_int nr else "" in
       "arg_" ^ idp ^ " = (ARG_" ^ id ^ "){" ^ par ^ "}; " ^ nl ^
       "RTFM_pend(" ^ idp ^ "_nr);"       
     | Sync ( id, par )      -> stmts (path ^ "_" ^ id) (SRP.lookup id topl)
     | ClaimC (c)            -> String.trim c
  
  and top = function
    | TopC (c)               -> deb ("top level code ") ^ c
    | Isr (p, id, sl)        -> "void " ^ id ^ "() {" ^ nl ^ (stmts id) sl ^ "}"
    | TaskDef (id, par, sl)  -> 
      "void " ^ id  ^ def_par par ^ "{ // function implementation for the task" ^ nl ^ 
      (stmts "") sl ^ 
      "}"
    | Reset (sl)            -> "void user_reset() {" ^ nl ^ (stmts "reset") sl ^ "}"
    | _                     -> raise (UnMatched)
      
  in
  let info = "const char* CORE_FILE_INFO = \"Compiled with : " ^ String.escaped (string_of_opt opt) ^ "\";" ^ nl
  
  in
  "// RTFM-core for RTFM-PT" ^ nl ^
  info ^ nl ^ 
  deb ("Resources and ceilings") ^ c_of_r r ^
  deb ("Entry points") ^ c_entry_of_top topl  ^ 
  deb ("Argument instances") ^ 
  myconcat nl (mymap ptop topl) ^ nl ^
  deb ("Application") ^ 
  myconcat nl (mymap top topl)  