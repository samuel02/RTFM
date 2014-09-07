(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/IsrCGen *)

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

(*        
let assign_vector v p =
  (* isr *)
  let rec isrs v ils b = match ils with
    | []          -> v
    (* first assign the named vectors b == true *)
    | Task (_, _, _, _) :: l  when b -> isrs v l b
    | Isr (p, id, s) :: l when b ->
      (* apply for all Isr's *) 
      (* assign id *)
      let rec assignv = function
          | []     -> failwith("No matching Isr entry for " ^ id )
          | (vty, vid) :: l when 
            compare id vid = 0 && ((vty == F) || (vty == O)) -> (U, vid) :: l
          | e :: l -> e :: assignv l
      in
      assignv (isrs v l b)
    (* then assign the unnamed vectors b == false *)
    | Isr ( _, _, _) :: l  when (not b) -> isrs v l b
    | Task (p, id, _, s) :: l when (not b) ->
      (* apply for all Isr's *) 
      (* assign id *)
      let rec assignv = function
          | []            -> failwith("No free Isr entry for " ^ id )
          | (F, vid) :: l -> (U, id) (* ^ tab ^ "(* optionally used for " ^ vid ^ " *)") *) :: l
          | e :: l        -> e :: assignv l
      in
      assignv (isrs v l b)
    | _ :: l  -> isrs v l b        
      
  in 
  (* prog *)
  isrs (isrs v p true) p false 
*)

let assign_isr v p =
  (* isr *)
  let rec isrs v ils = match ils with
    | []                   -> v
    | Isr (_, id, _) :: l  ->
      let rec assignv = function
          | []     -> failwith("No matching Isr entry for " ^ id )
          | (vty, vid) :: l when compare id vid = 0 && ((vty == F) || (vty == O)) -> (U, vid) :: l
          | e :: l -> e :: assignv l
      in
      assignv (isrs v l)
    | _ :: l  -> isrs v l
  in isrs v p
  
(*
let assign_task v p =
  (* task *)
  let rec tasks v ils = match ils with
    | [] -> v 
    | Task (_, id, pa, _, s) :: l ->
      let rec assignv = function
          | []            -> failwith("No free Isr entry for " ^ id )
          | ((F, vid),_) :: l -> ((U, id), (id, vid)) (* ^ tab ^ "(* optionally used for " ^ vid ^ " *)") *) :: l
          | e :: l        -> e :: assignv l
      in
      assignv (tasks v l)
    | _ :: l  -> tasks v l         
      
  in 
  (* prog *)
  let pair (t,s) = ((t,s), (s,s)) in
  tasks (List.map pair (assign_isr v p )) p  

let assign_vectors v p = 
  List.map fst (assign_task v p)
  
let assign_tasks v p = 
  List.map snd (assign_task v p)
*)
 
(*
let assign_vector v p =
  (* task *)
  let rec tasks v ils = match ils with
    | [] -> v 
    | Task (_, id, _, s) :: l ->
      let rec assignv = function
          | []            -> failwith("No free Isr entry for " ^ id )
          | (F, vid) :: l -> (U, id) (* ^ tab ^ "(* optionally used for " ^ vid ^ " *)") *) :: l
          | e :: l        -> e :: assignv l
      in
      assignv (tasks v l)
    | _ :: l  -> tasks v l         
      
  in 
  (* prog *)
  tasks (assign_isr v p ) p          
*)
                         
let wf_of_v v =
  let rec wf vl = match vl with
    | [] -> true
    | (vty, vid) :: l -> 
      let idl = List.map snd l in
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
      | []            -> raise (RtfmError ("Lookup of Isr or Task failed " ^ id))
      | (_, vid) :: l -> if (compare id vid) = 0 then i else isri (i+1) id l
  in
  isri 0 id vl 
    
let isrv_to_c_isr_nr vl =
  let rec isrv i vl = match vl with
    | []  -> ""
    | (vty, vid) :: l -> match vty with
      | U -> "#define " ^ "IRQ_NR_" ^ vid ^ " " ^ string_of_int i ^ nl ^ isrv (i + 1) l
      | _ -> isrv (i + 1) l
  in
  isrv (-16) vl
(* interrupts are labeled -14..-1 for core interrupts, >= 0 for vendor defined interrupts *) 
    
let rec task_vector p = match p with
  | []                         -> []
  | Isr (_, id, s) :: l        -> (U, id) :: task_vector l 
(*  | Task (_, id, pa, _, _) :: l-> (U, id) :: task_vector l *)
  | _ :: l                     -> task_vector l


    