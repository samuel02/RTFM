(* Copyright Per Lindgren 2014, see the file "LICENSE" for the full        *)
(* license governing this code.                                            *)

(* RTFM-core/SpecAST *)

open Common
open AST

type spec =
  | IC        of string                                                              (* code_c                                     *)
  | IIsr      of Time.time * string * stmt list                                      (* time, isr_id, stmts                        *)
  | IReset    of stmt list                                                           (* stmts                                      *)
  | IIdle     of stmt list                                                           (* stmts                                      *)
  | ITask     of (* bool * *) Time.time * Time.time * string * string * string * stmt list (* isafter, pebefore, inst_id, path, par_c, stmts *)
  | ITaskType of string * string                                                     (* id, par                                    *)
  | IFunc     of Time.time * string * string * string * stmt list                    (* before, rtype_c, path_f_id, arg_c, stmts   *)
  | IPeriodic of Time.time * string                                                  (* period, id                                 *)
let s_op = function
  | Some s -> s ^ " := "
  | None   -> ""

let string_of_spec tl = 
  (* top *)
  let rec stmts t sl = myconcat nl  (List.map (stmt (t ^ tab)) sl)  
  and top = function
    | IC (c)                        -> "#> " ^ c ^ "<#"
    | IIsr (be, id, s)              -> "Isr before " ^ Time.string_of_time be ^ " " ^ id ^ op ^ stmts "" s ^ cl ^ nl 
    | ITask (pe, dl, id, pa, par, s)-> "Task period " ^ Time.string_of_time pe ^ " before " ^ Time.string_of_time dl ^ " " ^ id ^ "[" ^ pa ^ "] (" ^ par ^ ")" ^ op ^ stmts "" s ^ cl ^ nl 
    | ITaskType (id, par)           -> "TaskType " ^  id ^ " (" ^ par ^ ")" 
    | IFunc (dl, t, id, par, s)     -> "Func before " ^ Time.string_of_time dl ^ " " ^ t ^ " " ^ id ^ " " ^ par ^ op ^ stmts "" s ^ cl ^ nl
    | IReset (s)                    -> "Reset " ^ op ^ stmts "" s ^ cl ^ nl 
    | IIdle (s)                     -> "Idle " ^ op ^ stmts "" s ^ cl ^ nl 
    | IPeriodic (pe, id)            -> "Periodic, period = " ^ Time.string_of_time pe ^ " " ^ id ^ nl 
  
  and stmt t = function
    | Claim (id, s)                     -> t ^ "claim " ^ id ^ op ^ stmts t s ^ t ^ cl
    | Pend (be, id, par)                -> t ^ "pend before" ^ Time.string_of_time be ^ " " ^ id ^ " (" ^ par ^ ")" 
    | Sync (id, par )                   -> t ^ "sync " ^ id ^ "(" ^ par ^ ")"  
    | Async (mi, af, be, id, Some par)  -> t ^ s_op mi ^ "async after " ^ Time.string_of_time af ^ " before " ^ Time.string_of_time be ^ " " ^ id ^ " (" ^ par ^ ")" 
    | Async (mi, af, be, id, None)      -> t ^ s_op mi ^ "async ISR after " ^ Time.string_of_time af ^ " before " ^ Time.string_of_time be ^ " " ^ id 
    | StmtC (c)                         -> t ^ "#> " ^ c ^ "<#"
    | State (s)                         -> t ^ s
    | Halt  (s)                         -> t ^ "My Halt" ^ s
    | Abort (par)                       -> t ^ "abort (" ^ par ^ ")"
    | Return (c, cs)                    -> t ^ "return " ^ c ^ ";" ^ "// inside claims : " ^ myconcat "," cs
    | Break (n, cs)                     -> t ^ "break; // " ^ string_of_int n ^ " nested claims, inside claims : " ^ myconcat "," cs
    | Continue (n, cs)                  -> t ^ "continue; // " ^ string_of_int n ^ " nested claims, inside claims : " ^ myconcat "," cs
    | Goto (l, n, cs)                   -> t ^ "goto " ^ l ^ "; // " ^ string_of_int n ^ " nested claims, inside claims : " ^ myconcat "," cs
  in
  myconcat nl (List.map top tl)  

 