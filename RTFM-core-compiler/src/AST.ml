(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/AST *)

open Common

type stmt =
  | Claim     of string * stmt list                                             (* res_id, stmts                      *)
  | Pend      of Time.time * string * string                                    (* before, par_c, isr_id              *)
  | Sync      of string * string                                                (* func_id, par_c                     *)
  | Async     of string option * Time.time * Time.time * string * string option (* msg_id, expr, prio, task_id, par_c *)
  | StmtC     of string                                                         (* code_c                             *)
  | State     of string
  | Halt      of string
  | Abort     of string
  | Return    of string * string list                                     (* code_c, claim stack                *)
  | Break     of int * string list                                        (* n_level, claim stack               *)
  | Continue  of int * string list                                        (* n_level, claim stack               *)
  | Goto      of string * int * string list                               (* n_level, claim stack               *)
       
type top =
  | TopC      of string                                         (* code_c                             *)
  | Isr       of Time.time * string * stmt list                 (* time, isr_id, stmts                *)
  | FuncDef   of string * string * string * stmt list           (* rtype_c, func_id, arg_c, stmts     *)
  | TaskDef   of string * string * stmt list                    (* task_id, arg_c, stms               *)
  | Reset     of stmt list                                      (* stmts                              *)
  | Idle      of stmt list                                      (* idle                               *)

type incl =
  | Include   of string * string 
          
type prog =
  | Prog      of string * incl list * top list

let s_op = function
  | Some s -> s
  | None   -> ""
    
let string_of_tops tl = 
  let rec stmts t sl = myconcat nl (mymap (stmt (t ^ tab)) sl)  
  and top = function
    | TopC (c)                 -> c (* "#> CCODE <#" *)
    | Isr (p, id, s)           -> "Isr before " ^ Time.string_of_time p ^ " " ^ id ^ op ^ stmts "" s ^ cl ^ nl 
    | FuncDef (t, id, par, s)  -> "Func " ^ t ^ " " ^ id ^ " " ^ par ^ op ^ stmts "" s ^ cl ^ nl
    | TaskDef (id, par, s)     -> "TaskDef " ^ id ^ " " ^ par ^ op ^ stmts "" s ^ cl ^ nl 
    | Reset (s)                -> "Reset " ^ op ^ stmts "" s ^ cl ^ nl
    | Idle (s)                 -> "Idle " ^ op ^ stmts "" s ^ cl ^ nl
      
  and stmt t = function 
    | Claim (id, s)                    -> t ^ "Claim " ^ id ^ op ^ stmts t s ^ t ^ cl
    | Pend (be, id, par)               -> t ^ "Pend before" ^ Time.string_of_time be ^ " " ^ id ^ "(" ^ par ^ ")" 
    | Sync (id, par )                  -> t ^ "Sync " ^ id ^ "(" ^ par ^ ")"  
    | Async (mi, af, pr, id, Some par) -> t ^ s_op mi ^ "Async after " ^ Time.string_of_time af ^ " before " ^ Time.string_of_time pr ^ " " ^ id ^ "(#>" ^ par ^ "<#)" 
    | Async (mi, af, pr, id, None)     -> t ^ s_op mi ^ "Async ISR after " ^ Time.string_of_time af ^ " before " ^ Time.string_of_time pr ^ " " ^ id 
    | StmtC (c)                        -> t ^ c
    | State (s)                        -> t ^ s
    | Halt (s)                         -> t ^ "My HALT" ^ s
    | Abort (par)                      -> t ^ "abort (" ^ par ^ ")"
    | Return (c, cs)                   -> t ^ "claim_return " ^ c ^ ";" ^ "// in claims" ^ myconcat "," cs
    | Break (n, cs)                    -> t ^ "claim_break; " ^ string_of_int n ^ ", in claims " ^ myconcat "," cs
    | Continue (n, cs)                 -> t ^ "claim_continue; " ^ string_of_int n ^ ",in claims " ^ myconcat "," cs
    | Goto (l, n, cs)                  -> t ^ "claim_goto " ^ string_of_int n ^ " to " ^ l ^ "; in claims " ^ myconcat "," cs
  in
  myconcat nl (mymap top tl) 

let string_of_prog = 
  let inc = function
  | Include (fname, aspath) -> fname ^ " as " ^ aspath 
  in
  function
  | Prog (mName, mIncl, mTop) ->
      "Module:" ^ mName ^ nl ^
      "Use :" ^ String.concat "," (mymap inc mIncl) ^ nl ^
      "Prog:" ^ nl ^ string_of_tops mTop
                  
let rec prio_to_string r = match r with
  | []            -> ""
  | (id, p) :: l  -> "Isr " ^ id ^ ", priority " ^ string_of_int p ^ nl ^ prio_to_string l
    
