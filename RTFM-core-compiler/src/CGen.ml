(* RTFM-core/CGen *)
open Common
open Options
open AST
open IsrCGen
open SRP

let deb s = if opt.debug then "// RTFM-core : " ^ s ^ nl else ""

let pend id = match opt.target with
  | RTFM_KERNEL -> "RTFM_pend(IRQ_NR_" ^ id ^ ");"
  | RTFM_PT -> "RTFM_pend(RTFM_id, " ^ id ^ "_nr);"

let c_of_p topl v r =
  let quote x = "\"" ^ x ^ "\"" in
  let c_of_r rl = match opt.target with
    | RTFM_KERNEL -> String.concat nl (List.map (fun (id, prio) -> "#define " ^ id ^ " " ^ string_of_int prio) rl)
    | RTFM_PT ->
        "enum resources {" ^ String.concat "," ((List.map fst rl) @ ["RES_NR"]) ^ "};" ^ nl ^
        "char* res_names[] = {" ^ String.concat "," (List.map quote (List.map fst rl)) ^ "};" ^ nl ^
        "int ceilings[RES_NR] = {" ^ String.concat ", " (List.map string_of_int (List.map snd rl)) ^ "};" ^ nl ^ nl
  
  in
  let c_prio_of_top topl = match opt.target with
    | RTFM_KERNEL ->
        let c t = match t with
          | Isr (_, id, pri, _)	-> "#define IRQ_PRI_" ^ id ^ " " ^ string_of_int pri
          | _ -> ""
        in
        String.concat nl (List.map c topl)
    | RTFM_PT -> let rec c t pa = match t with
          | [] -> []
          | Isr (_, id, pri, _) :: l	-> (id ^ pa) :: c l pa
          | _	:: l -> c l pa
        and prio t = match t with
          | [] -> []
          | Isr (_, id, pri, _) :: l	-> (string_of_int pri) :: prio l
          | _	:: l -> prio l
        in
        let proto t = match t with
          | Isr (_, id, pri, _) ->
              ( match opt.target with
                | RTFM_KERNEL -> "void " ^ id ^ "();" ^ nl
                | RTFM_PT -> "void " ^ id ^ "(int);" ^ nl
              )
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
      | Isr (_, id, pri, _) ->
          "    RTFM_set_priority(IRQ_NR_" ^ id ^ ", " ^ "H(IRQ_PRI_" ^ id ^ "));" ^ nl ^
          "    RTFM_enable_irq(IRQ_NR_" ^ id ^ ");" ^ nl
      | _ -> ""
    in
    "void RTFM_init_priorities() {" ^ nl ^ String.concat nl (List.map c topl) ^ "}" ^ nl
  
  in
  let rec stmt sl = match sl with
    | [] -> nl
    | s :: l -> let st = match s with
          | Claim ( r, csl ) ->
              ( match opt.target with
                | RTFM_KERNEL -> "RTFM_lock(" ^ r ^ ");" ^ nl ^ stmt csl ^ "RTFM_unlock(" ^ r ^ ");"
                | RTFM_PT -> "RTFM_lock(RTFM_id, " ^ r ^ ");" ^ nl ^ stmt csl ^ "RTFM_unlock(RTFM_id, " ^ r ^ ");"
              )
          | Pend ( id ) -> if (isrv_lookup id v >= 0) then pend id else raise (RtfmError("Failed to lookup " ^ id))
          | PendAfter ( id, time ) -> if (isrv_lookup id v > 0) then "RTFM_pend_after(IRQ_NR_^" ^ id ^ ", " ^ string_of_int time ^ ");" else ""
          | Sync ( id, par ) ->
              (
                match opt.target with
                | RTFM_KERNEL -> id ^ "(" ^ par ^ ");"
                | RTFM_PT -> id ^ (if String.compare par "" == 0 then "(RTFM_id" else "(RTFM_id, ") ^ par ^ ");"
              )
          
          | Enable ( b ) -> if b then "RTFM_enable();" else "RTFM_disable():"
          | Halt -> "while (1) ;"
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
          | Isr (b, id, p, sl) ->
              ( match opt.target with
                | RTFM_KERNEL -> "void " ^ id ^ "() {" ^ nl ^ stmt sl ^ "}"
                | RTFM_PT -> "void " ^ id ^ "(int RTFM_id) {" ^ nl ^ stmt sl ^ "}"
              )
          | Func (t, id, par, sl) ->
              (
                match opt.target with
                | RTFM_KERNEL -> t ^ " " ^ id ^ "(" ^ par ^ ") " ^ "{" ^ nl ^ stmt sl ^ "}"
                | RTFM_PT -> t ^ " " ^ id ^ (if String.compare par "" == 0 then "(int RTFM_id" else "(int RTFM_id, ") ^ par ^ ") " ^ "{" ^ nl ^ stmt sl ^ "}"
              )
          | Reset (sl) ->
              ( match opt.target with
                | RTFM_KERNEL -> "void user_reset() {" ^ nl ^ stmt sl ^ "}"
                | RTFM_PT -> "void user_reset(int RTFM_id) {" ^ nl ^ stmt sl ^ "}"
              )
        in
        st ^ nl ^ top l ^ nl
  
  in
  let func_prot = function
    | Func (t, id, par, sl) ->
        (
          match opt.target with
          | RTFM_KERNEL -> t ^ " " ^ id ^ "(" ^ par ^ ");" 
          | RTFM_PT -> t ^ " " ^ id ^ (if String.compare par "" == 0 then "(int RTFM_id" else "(int RTFM_id, ") ^ par ^ ");" 
        )
    | _ -> raise (UnMatched)
  in
  let info = "const char* CORE_FILE_INFO = \"Compiled with : " ^ String.escaped (string_of_opt opt) ^ "\";" ^ nl
  in
  match opt.target with
  | RTFM_KERNEL ->
      "// RTFM-core for RTFM-KERNEL" ^ nl ^
      info ^ nl ^
      deb ("Resource Ceiling Bindings") ^ c_of_r r ^
      deb ("Defintions for IRQ_NR") ^ isrv_to_c_isr_nr v ^
      deb ("Defintions for IRQ_PRI") ^ c_prio_of_top topl ^
      
      deb ("Initiate interrupt priorities") ^ c_init_prio_of_prog topl ^
      deb ("// RTFM-Application ") ^
      top topl ^ nl
  
  | RTFM_PT	->
      "// RTFM-core for RTFM-PT" ^ nl ^
      info ^ nl ^
      deb ("Resources and ceilings") ^ c_of_r r ^
      deb ("Entry points") ^ c_prio_of_top topl ^
      deb ("Prototypes") ^
      myconcat nl (mymap func_prot topl) ^ 
      deb ("Application") ^
      top topl ^ nl