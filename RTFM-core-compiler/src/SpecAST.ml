(* Copyright Per Lindgren 2014, see the file "LICENSE" for the full        *)
(* license governing this code.                                            *)

(* RTFM-core/SpecAST *)

open Common
open AST

type spec =
  | IC        of string                                                  (* code_c                                     *)
  | IIsr      of int * string * stmt list                                (* prio, isr_id, stmts                        *)
  | IReset    of stmt list                                               (* stmts                                      *)
  | IIdle     of stmt list                                               (* stmts                                      *)
  | ITask     of time * time * string * string * string * stmt list      (* after, before, inst_id, path, par_c, stmts *)
  | ITaskType of string * string                                         (* id, par                                    *)
  | IFunc     of time * string * string * string * stmt list             (* before, rtype_c, path_f_id, arg_c, stmts   *)
  
let s_op = function
  | Some s -> s ^ " := "
  | None   -> ""

let string_of_spec tl = 
  (* top *)
  let rec stmts t sl = myconcat nl  (mymap (stmt (t ^ tab)) sl)  
  and top = function
    | IC (c)                        -> "#> " ^ c ^ "<#"
    | IIsr (p, id, s)               -> "Isr prio " ^ string_of_int p ^ " " ^ id ^ op ^ stmts "" s ^ cl ^ nl 
    | ITask (pe, dl, id, pa, par, s)-> "Task period " ^ string_of_time pe ^ " before " ^ string_of_time dl ^ " " ^ id ^ "[" ^ pa ^ "] (" ^ par ^ ")" ^ op ^ stmts "" s ^ cl ^ nl 
    | ITaskType (id, par)           -> "TaskType " ^  id ^ " (" ^ par ^ ")" 
    | IFunc (dl, t, id, par, s)     -> "Func before " ^ string_of_time dl ^ " " ^ t ^ " " ^ id ^ " " ^ par ^ op ^ stmts "" s ^ cl ^ nl
    | IReset (s)                    -> "Reset " ^ op ^ stmts "" s ^ cl ^ nl 
    | IIdle (s)                     -> "Idle " ^ op ^ stmts "" s ^ cl ^ nl 
  
  and stmt t = function
    | Claim (id, s)               -> t ^ "claim " ^ id ^ op ^ stmts t s ^ t ^ cl
    | Pend (be, id, par)          -> t ^ "pend before" ^ string_of_time be ^ " " ^ id ^ " (" ^ par ^ ")" 
    | Sync (id, par )             -> t ^ "sync " ^ id ^ "(" ^ par ^ ")"  
    | Async (mi, af, pr, id, par) -> t ^ s_op mi ^ "async after " ^ string_of_time af ^ " before " ^ string_of_time pr ^ " " ^ id ^ " (" ^ par ^ ")" 
    | ClaimC (c)                  -> t ^ "#> " ^ c ^ "<#"
    | Halt  (s)                   -> t ^ "My Halt" ^ s
    | Abort (par)                 -> t ^ "abort (" ^ par ^ ")"
  in
  myconcat nl (mymap top tl)   

 