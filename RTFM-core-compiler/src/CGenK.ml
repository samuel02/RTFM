(* Copyright Per Lindgren 2014, see the file "LICENSE" for the full        *)
(* license governing this code.                                            *)

(* RTFM-core/CGenK *)

open Common
open Options
open AST
open IsrCGen
open SRP

let deb s = if opt.debug then "// RTFM-core : " ^ s ^ nl else ""

let c_of_p topl v r =
  let quote x = "\"" ^ x ^ "\"" in
  let c_of_r rl = String.concat nl (List.map (fun (id, prio) -> "#define " ^ id ^ " " ^ string_of_int prio) rl)
    
  in
  let c_prio_of_top topl = 
     match t with
          | Isr (_, pri, id, _, _) -> "#define IRQ_PRI_" ^ id ^ " " ^ string_of_int pri
          | _ -> ""
  in
  let proto t = match t with
          | Isr (_, pri, id, _, _) -> "void " ^ id ^ "();" ^ nl
          | _ -> ""
  in
  let user_reset = "user_reset" in
        "enum entry_nr {" ^ String.concat ", " ((user_reset ^ "_nr") :: ((c topl "_nr" ) @ ["ENTRY_NR"])) ^ "};" ^ nl ^ nl ^
        "int entry_prio[] = {" ^ String.concat ", " ("0" :: (prio topl)) ^ "};" ^ nl ^ nl ^
        String.concat "" (List.map proto topl) ^ nl ^
        "ENTRY_FUNC entry_func[] = {" ^ String.concat ", " (user_reset:: (c topl "")) ^ "};" ^ nl ^ nl ^
        "char* entry_names[] = {" ^ (String.concat ", " (List.map quote (user_reset:: (c topl "")))) ^ "};" ^ nl
  
  in
  let c_init_prio_of_prog topl =
    let c t = match t with
      | Isr (_, pri, id, _, _) ->
          "    RTFM_set_priority(IRQ_NR_" ^ id ^ ", " ^ "H(IRQ_PRI_" ^ id ^ "));" ^ nl ^
          "    RTFM_enable_irq(IRQ_NR_" ^ id ^ ");" ^ nl
      | _ -> ""
    in
    "void RTFM_init_priorities() {" ^ nl ^ String.concat nl (List.map c topl) ^ "}" ^ nl
  
  in
  let rec stmt sl = match sl with
    | [] -> nl
    | s :: l -> let st = match s with
          | Claim ( r, csl ) -> "RTFM_lock(" ^ r ^ ");" ^ nl ^ stmt csl ^ "RTFM_unlock(" ^ r ^ ");"
              
          | Pend ( id, p ) -> "RTFM_pend(IRQ_NR_" ^ id ^ ");"
          | PendAfter ( id, p, t) -> "RTFM_pend_after(IRQ_NR_^" ^ id ^ ", " ^ string_of_int t ^ ");" 
          | Sync ( id, par ) -> id ^ "(" ^ par ^ ");"
          | ClaimC (c) -> c
        in
        st ^ nl ^ stmt l
  in
  let rec top tl = match tl with
    | [] -> nl
    | t :: l ->
        let st = match t with
          | TopC (c) -> deb ("top level code ") ^ c
          (* | TopPend (id) -> pend id *)
          | Isr (b, p, id, _, sl) ->"void " ^ id ^ "() {" ^ nl ^ stmt sl ^ "}"
          | Func (t, id, par, sl) -> t ^ " " ^ id ^ "(" ^ par ^ ") " ^ "{" ^ nl ^ stmt sl ^ "}"
             
          | Reset (sl) -> "void user_reset() {" ^ nl ^ stmt sl ^ "}"
             
        in
        st ^ nl ^ top l ^ nl
  
  in
  let func_prot = function
    | Func (t, id, par, sl) -> t ^ " " ^ id ^ "(" ^ par ^ ");" 
    | _ -> raise (UnMatched)
  in
  let info = "const char* CORE_FILE_INFO = \"Compiled with : " ^ String.escaped (string_of_opt opt) ^ "\";" ^ nl
  in
  
      "// RTFM-core for RTFM-KERNEL" ^ nl ^
      info ^ nl ^
      deb ("Resource Ceiling Bindings") ^ c_of_r r ^
      deb ("Defintions for IRQ_NR") ^ isrv_to_c_isr_nr v ^
      deb ("Defintions for IRQ_PRI") ^ c_prio_of_top topl ^
      
      deb ("Initiate interrupt priorities") ^ c_init_prio_of_prog topl ^
      deb ("// RTFM-Application ") ^
      top topl ^ nl
  
  