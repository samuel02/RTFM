(* CGen *)
open Common
open AST
open IsrCGen
open SRP  
  
let pend id =  match opt.target with
  | RTFM_KERNEL -> "RTFM_pend(IRQ_NR_" ^ id ^ ");"
  | RTFM_PT		-> "RTFM_pend(" ^ id ^ "_nr);"
    
let c_of_p topl v r = 
  let c_of_r rl = match opt.target with 
    | RTFM_KERNEL 	-> String.concat nl (List.map (fun (id, prio) -> "#define " ^ id ^ " " ^ string_of_int prio) rl)
    | RTFM_PT		-> 
      "enum resources {" ^ String.concat "," ((List.map fst rl) @ ["RES_NR"]) ^ "};" ^ nl ^
        "int ceilings[RES_NR] = {" ^ String.concat ", " (List.map string_of_int (List.map snd rl)) ^ "};" 
        
  in
  let c_prio_of_top topl = match opt.target with
    | RTFM_KERNEL 	-> 
      let c t = match t with
        | Isr (_, id, pri, _)	-> "#define IRQ_PRI_" ^ id ^ " " ^ string_of_int pri 
        | _						-> ""
      in
      String.concat nl (List.map c topl)
    | RTFM_PT		-> 
      let rec c  t pa = match t with
        | []						-> []
        | Isr (_, id, pri, _) :: l	-> (id ^ pa) :: c l pa 
        | _	:: l					-> c l pa
      and prio t = match t with
        | []						-> []
        | Isr (_, id, pri, _) :: l	-> (string_of_int pri) :: prio l  
        | _	:: l					-> prio l 
      in
      let proto t = match t with
        | Isr (_, id, pri, _) 	->  "void " ^ id ^ "();" ^ nl
        | _	-> ""
      in   
      "enum entry_nr {" ^ String.concat ", "  ((c topl "_nr" ) @ ["ENTRY_NR"]) ^ "};" ^ nl ^
        "int entry_prio[] = {" ^ String.concat ", "  (prio topl) ^ "};" ^ nl ^ 
        String.concat "" (List.map proto topl) ^
        "ENTRY_FUNC entry_func[] = {" ^ String.concat ", "  (c topl "") ^ "};" ^ nl
        
        
        
  in
  let c_init_prio_of_prog topl = 
    let c t = match t with
      | Isr (_, id, pri, _) 	->
        "    RTFM_set_priority(IRQ_NR_" ^ id ^ ", " ^ "H(IRQ_PRI_" ^ id ^ "));" ^ nl ^ 
          "    RTFM_enable_irq(IRQ_NR_" ^ id ^ ");" ^ nl     
      | _ 					-> ""
    in
    "void RTFM_init_priorities() {" ^ nl ^ String.concat nl (List.map c topl) ^ "}" ^ nl 
      
  in    
  let rec stmt sl =  match sl with
    | [] -> nl
    | s :: l -> let st = match s with
      | Claim ( r, csl ) 			-> "RTFM_lock(" ^ r ^ ");" ^ nl ^ stmt csl ^ nl ^ "RTFM_unlock(" ^ r ^ ");" ^ nl 
      | Pend ( id ) 				-> if (isrv_lookup id v > 0) then pend id else ""	
      | PendAfter ( id, time ) 	-> if (isrv_lookup id v > 0) then "RTFM_pend_after(IRQ_NR_^" ^ id ^ ", " ^ string_of_int time ^ ");" else  "" 
      | Sync ( id, pars ) 		-> id ^ pars ^ ";"
      | Enable ( b ) 				-> if b then "RTFM_enable();" else "RTFM_disable():"
      | Halt 						-> "while (1) ;"
      | ClaimC (c) 				-> c 
                in
      st ^ nl ^ stmt l
  in            	  
  let rec top tl = match tl with 
    | [] -> nl 
    |  t :: l -> 
      let st = match t with 
        | TopC (c) 					-> "// top level code " ^ nl ^ c 
        | TopPend (id) 				-> pend id  
        | Isr (b, id, p, sl) 		-> "void " ^ id ^ "() {" ^ nl ^ stmt sl ^ "}" 
        | Func (t, id, par, sl) 	-> t ^ " " ^ id ^ par ^ "{" ^ stmt sl ^ "}"
      in
      st ^ nl ^ top l ^ nl
  in
  match opt.target with
    | RTFM_KERNEL ->
      "// RTFM-core for RTFM-KERNEL" ^ nl ^
      (* "#include \"../RTFM-kernel/RTFM-kernel.h\"" ^ nl ^ nl ^ *)
        "// Resource Ceiling Bindings" ^ nl ^ c_of_r r ^ nl ^
        "// Defintions for IRQ_NR" ^  nl ^ isrv_to_c_isr_nr v ^ nl ^
        "// Defintions for IRQ_PRI" ^  nl ^ c_prio_of_top topl ^ nl ^
        
        "// Initiate interrupt priorities" ^ nl ^ c_init_prio_of_prog topl ^ nl ^
        "// RTFM-Application " ^ nl ^
        top topl ^ nl   
         
    | RTFM_PT	->
      "// RTFM-core for RTFM-PT" ^ nl ^
        "// RTFM-Resources" ^ nl ^ c_of_r r ^ nl ^
        "// RTFM-Entry points" ^ nl ^ c_prio_of_top topl ^ nl ^
        
        "// RTFM-Application " ^ nl ^
        top topl ^ nl    
        
        