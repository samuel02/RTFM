open IsrVector
open AST
open Common
  
let isrv_to_c vl =
  let rec isr_to_c vl = match vl with 
    | [] -> ""
    | (vty, vid) :: l -> tab ^ (
                                  match vty with
                                    | F -> "Default_Handler; " ^ tab ^ "(* " ^ vid ^ " *)" 
                                    | _ -> vid ^ ";"
                               ) ^ nl ^ isr_to_c l
  in
  "void (* const g_pfnVectors[])(void) {" ^ nl ^ isr_to_c vl ^ "}" ^ nl
    
let assign_vectors v p =
  (* isr *)
  let rec isrs v ils b = match ils with
    | [] 					-> v
    (* first assign the named vectors b == true *)
    | Isr (SOFT, _, _, _) :: l	when b -> isrs v l b
    | Isr (HARD, id, p, s) :: l	when b ->
      (* apply for all Isr's *) 
      (* assign id *)
      let rec assignv il =
        match il with
          | [] -> failwith("No matching Isr entry for " ^ id ^ nl ^ isrv_to_c il)
          | (vty, vid) :: l when 
            compare id vid = 0 && ((vty == F) || (vty == O)) -> (U, vid) :: l
          | e :: l -> e :: assignv l
      in
      assignv (isrs v l b)
    (* then assign the unnamed vectors b == false *)
    | Isr (HARD, _, _, _) :: l	when (not b) -> isrs v l b
    | Isr (SOFT, id, p, s) :: l when (not b) ->
      (* apply for all Isr's *) 
      (* assign id *)
      let rec assignv il =
        match il with
          | [] -> failwith("No free Isr entry for " ^ id ^ nl ^ isrv_to_c il)
          | (F, vid) :: l -> (U, id) (* ^ tab ^ "(* optionally used for " ^ vid ^ " *)") *) :: l
          | e :: l -> e :: assignv l
      in
      assignv (isrs v l b)
    | _ :: l  -> isrs v l b        
      
  in 
  (* prog *)
  isrs (isrs v p true) p false 
    
let second (_, b) = b
 
let wf_of_v v =
  let rec wf vl = match vl with
    | [] -> true
    | (vty, vid) :: l -> 
      let idl = List.map second l in
      if List.mem vid idl then begin
        if not ((String.compare vid "0") == 0) then begin  
          p_stderr ("Warning, duplicate entries in vector table " ^ vid ^ nl);
        end;
      end; 
      wf l
  in 
  wf v
 
let isrv_lookup id vl = 
  let rec isri i id vl =
    match vl with
      | [] -> raise (RtfmError ("Lookup of Isr or Task failed " ^ id))
      | (_, vid) :: l -> if (compare id vid) = 0 then i else isri (i+1) id l
  in
  isri 0 id vl 
    
let isrv_to_c_isr_nr vl =
  let rec isrv i vl = match vl with
    | [] -> ""
    | (vty, vid) :: l -> match vty with
      | U -> "#define " ^ "IRQ_NR_" ^ vid ^ " " ^ string_of_int i ^ nl ^ isrv (i + 1) l
      | _ -> isrv (i + 1) l
  in
  isrv (-16) vl
  (* interrupts are labeled -14..-1 for core interrupts, >= 0 for vendor defined interrupts *) 
    
    
    