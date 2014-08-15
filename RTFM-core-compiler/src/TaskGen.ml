(* Copyright Per Lindgren 2014, see the file "LICENSE" for the full        *)
(* license governing this code.                                            *)

(* RTFM-core/TaskGen *)

open Common
open Options
open AST

(* Traverse each top level ISR/Task and transform async's to tasks *)

let tasks_of_p topl =
  let rec newTaskDef path nr pr id par = 
    let idp = path ^ "_" ^ id ^ if nr > 0 then string_of_int nr else "" in 
    
    match Env.lookup_task id topl with 
        | TaskDef (_, al, sl)  -> Task (pr, idp, al, sl)::tasks idp sl
        | _                   -> [] 
  and tasks path sl = 
    let nr_ref = ref 0 in
    List.concat (mymap (task path nr_ref ) sl) 
  and task path nr_ref  = 
    function
    | Claim (r, csl)          -> tasks path csl  
    | Sync (id, par)          -> tasks (path ^ "_" ^ id) (Env.lookup_func_sl id topl) 
    | Async (_, pr, id, par)  -> 
      begin
        let nr = !nr_ref in
        nr_ref := nr + 1;
        newTaskDef path nr pr id par
      end
    | _                       -> []
  
  and tasktop = function
    | Isr (p, id, sl)       -> tasks id sl 
    | Reset (sl)            -> tasks "reset" sl
    | _                     -> raise (UnMatched)
    
  in
  List.concat (mymap tasktop topl)

































 (*   
        
            
                    
    let proto = function
      | TaskDef (id, par, _) -> 
        "void " ^ id ^ "(int);" ^ nl ^ 
        "typedef struct {" ^ nl ^
        "} ARG_" ^ id ^ ";" ^ nl 
      | _                        -> raise (UnMatched)
    in
    ""
    (*
    let user_reset = "user_reset" in
    "enum entry_nr {" ^ String.concat ", " ((user_reset ^ "_nr") :: (mymap tasktop topl ) @ ["ENTRY_NR"]) ^ "};" ^ nl ^ nl ^
    "int entry_prio[] = {" ^ String.concat ", " ("0" :: (mymap prio topl)) ^ "};" ^ nl ^ nl ^
    String.concat "" (mymap proto topl) ^ nl ^
    "ENTRY_FUNC entry_func[] = {" ^ String.concat ", " (user_reset:: mymap tasktop topl) ^ "};" ^ nl ^ nl ^
    "char* entry_names[] = {" ^ String.concat ", " (mymap quote (user_reset :: mymap tasktop topl)) ^ "};" ^ nl
   *)
  in
  let func_prot = function
    | Func (t, id, par, sl) -> t ^ " " ^ id ^ (if String.compare par "" == 0 then "(int RTFM_id" else "(int RTFM_id, ") ^ par ^ ");"
    | _ -> ""
       
  in
  let rec pargs path sl = myconcat nl (mymap (parg path) sl) 
  and parg path = function
     | Claim (r, csl)        -> pargs path csl 
     | Pend (pr, id, p)      -> "ARG_" ^ id ^ " arg_" ^ path ^ "_" ^ id ^ ";"
     | Sync (id, par)        -> pargs (path ^ "_" ^ id) (SRP.lookup id topl) 
     | _                     -> raise (UnMatched)
  
  and ptop = function
     | Isr (b, p, id, sl)    -> pargs id sl
     | Reset (sl)            -> pargs "reset" sl 
     | _                     -> raise (UnMatched)
  
  in
  let rec stmts path sl = myconcat nl (mymap (stmt path) sl) 
  and stmt path = function
     | Claim (r, csl)        -> "RTFM_lock(RTFM_id, " ^ r ^ ");" ^ nl ^ (stmts path) csl ^ "RTFM_unlock(RTFM_id, " ^ r ^ ");"
     | Pend (pr, id, p)      -> 
       "arg_" ^ path ^ "_" ^ id ^ " = (ARG_" ^ id ^ "){" ^ p ^ "}; " ^ nl ^
       "RTFM_pend(RTFM_id, " ^ id ^ "_nr;"     
     | Sync ( id, par )      -> stmts (path ^ "_" ^ id) (SRP.lookup id topl)
     | ClaimC (c)            -> c
  
  and top = function
     | TopC (c)              -> deb ("top level code ") ^ c
     | Isr (b, p, id, sl)    -> "void " ^ id ^ "(int RTFM_id) {" ^ nl ^ (stmts id) sl ^ "}"
     | Func (t, id, par, sl) -> raise (UnMatched)
      (* t ^ " " ^ id ^ (if String.compare par "" == 0 then "(int RTFM_id" else "(int RTFM_id, ") ^ par ^ ") " ^ "{" ^ nl ^ stmts sl ^ "}" *)
     | Reset (sl)            -> "void user_reset(int RTFM_id) {" ^ nl ^ (stmts "reset") sl ^ "}"
      
  in
  let info = "const char* CORE_FILE_INFO = \"Compiled with : " ^ String.escaped (string_of_opt opt) ^ "\";" ^ nl
  
  in
  "// RTFM-core for RTFM-PT" ^ nl ^
  info ^ nl ^ 
  deb ("Resources and ceilings") ^ c_of_r r  ^
  deb ("Entry points") ^ c_entry_of_top topl  ^
  deb ("Prototypes") ^
  myconcat nl (mymap func_prot topl) ^
  deb ("Arguments") ^
  myconcat nl (mymap ptop topl) ^
  deb ("Application") ^ 
  myconcat nl (mymap top topl)  
  
  *)