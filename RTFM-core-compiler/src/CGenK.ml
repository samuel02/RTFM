(* Copyright Per Lindgren 2014, see the file "LICENSE" for the full        *)
(* license governing this code.                                            *)

(* RTFM-core/CGenK *)

open Common
open Options
open AST
open IsrCGen
open SRP

let deb s = if opt.debug then "// RTFM-core: " ^ s ^ nl else ""

let def_par par = "(" ^ String.trim par ^ ")"
let pass_par par = "(" ^ String.trim par ^ ")"

let ck_of_p topl v r =
    let quote x = "\"" ^ x ^ "\"" in
  let c_of_r rl =
    "enum resources {" ^ mycon "," ((List.map fst rl) @ ["RES_NR"]) ^ "};" ^ nl ^
    "int ceilings[] = {" ^ mycon ", " (List.map string_of_int (List.map snd rl)) ^ "};" ^ nl ^ 
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
          "// Task instance defintion: @prio " ^ string_of_int p ^ " " ^ id ^  nl ^
          "void " ^ id ^ "(int RTFM_id); // function prototype for the instance task" ^ nl 
      | _ -> raise (UnMatched)
    in    
    let entries_top = function
      | Isr (_, id, _)     -> id
      | Task (_, id, _, _) -> id  
      | _                  -> raise (UnMatched)  
    
    in
    let priorities_top = function
      | Isr (p, _, _)      -> string_of_int p
      | Task (p, _, _, _)  -> string_of_int p  
      | _                  -> raise (UnMatched) 
    in
    let entries = "user_reset" :: mymap entries_top topl in
    let addstr s2 s1 = s1 ^ s2 in
    "enum entry_nr {" ^ mycon ", " ((List.map (addstr "_nr") entries) @ ["ENTRY_NR"]) ^ "};" ^ nl ^
    "int entry_prio[] = {" ^ mycon ", " ("0" :: mymap priorities_top topl) ^ "};" ^ nl ^ 
    "char* entry_names[] = {" ^ mycon ", " (mymap quote entries) ^ "};" ^ nl ^ nl ^
    mycon nl (mymap proto_top topl) ^ nl ^
    "ENTRY_FUNC entry_func[] = {" ^ mycon ", " entries ^ "};" ^ nl ^ nl
      
  in
  let c_init_prio_of_prog topl =
    let c t = match t with
      | Isr (pri, id, _) ->
          "    RTFM_set_priority(IRQ_NR_" ^ id ^ ", " ^ "H(IRQ_PRI_" ^ id ^ "));" ^ nl ^
          "    RTFM_enable_irq(IRQ_NR_" ^ id ^ ");" ^ nl
      | _ -> ""
    in
    "void RTFM_init_priorities() {" ^ nl ^ String.concat nl (List.map c topl) ^ "}" ^ nl
  
  in
    let rec stmts path sl = 
    let nr_ref = ref 0 in
    myconcat nl (mymap (stmt path nr_ref) sl) 
  and stmt path nr_ref = function
     | Claim (r, csl)        -> "RTFM_lock(RTFM_id, " ^ r ^ ");" ^ nl ^ (stmts path) csl ^ "RTFM_unlock(" ^ r ^ ");"
     | Pend (id)             -> "RTFM_pend(RTFM_id, " ^ id ^ "_nr);"
     | Async (pr, id, par)   ->
      let nr = !nr_ref in
        nr_ref := nr + 1;
         let idp = path ^ "_" ^ id ^ if nr > 0 then string_of_int nr else "" in
       "arg_" ^ idp ^ " = (ARG_" ^ id ^ "){" ^ par ^ "}; " ^ nl ^
       "RTFM_pend(IRQ_NR_" ^ id ^ ");"       
     | Sync ( id, par )      -> stmts (path ^ "_" ^ id) (SRP.lookup id topl)
     | ClaimC (c)            -> String.trim c
  
  and top = function
    | TopC (c)               -> deb ("top level code ") ^ c
    | Isr (p, id, sl)        -> "void " ^ id ^ "(int RTFM_id) {" ^ nl ^ (stmts id) sl ^ "}"
    | TaskDef (id, par, sl)  -> 
      "void " ^ id  ^ def_par par ^ "{ // function implementation for the task" ^ nl ^ 
      (stmts "") sl ^ 
      "}"
    | Reset (sl)            -> "void user_reset(int RTFM_id) {" ^ nl ^ (stmts "reset") sl ^ "}"
    | _                     -> raise (UnMatched)
    
  in
  let info = "const char* CORE_FILE_INFO = \"Compiled with : " ^ String.escaped (string_of_opt opt) ^ "\";" ^ nl
  in
      "// RTFM-core for RTFM-KERNEL" ^ nl ^
      info ^ nl ^
      deb ("Resource Ceiling Bindings") ^ c_of_r r ^
      deb ("Defintions for IRQ_NR") ^ isrv_to_c_isr_nr v ^
      (*
      deb ("Defintions for IRQ_PRI") ^ c_prio_of_top topl ^
      
      deb ("Initiate interrupt priorities") ^ c_init_prio_of_prog topl ^
      deb ("// RTFM-Application ") ^
      top topl ^ nl
      *)
  
  