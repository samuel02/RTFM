(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/AST *)

open Common
(*    
  
type time =
  | Usec of int
  | Msec of int
  | Sec  of int
  | Infinite
*)

type stmt =
  | Claim     of string * stmt list                             (* res_id, stmts                      *)
  | Pend      of time * string * string                         (* before, par_c, isr_id              *)
  | Sync      of string * string                                (* func_id, par_c                     *)
  | Async     of string option * time * time * string * string  (* msg_id, expr, prio, task_id, par_c *)
  | ClaimC    of string                                         (* code_c                             *)
  | Halt      of string
  | Abort     of string
       
type top =
  | TopC      of string                                         (* code_c                             *)
  | Isr       of int * string * stmt list                       (* prio, isr_id, stmts                *)
  | FuncDef   of string * string * string * stmt list           (* rtype_c, func_id, arg_c, stmts     *)
  | TaskDef   of string * string * stmt list                    (* task_id, arg_c, stms               *)
  | Reset     of stmt list                                      (* stmts                              *)
  | Idle      of stmt list                                      (* idle                               *)
    
type prog =
  | Prog      of string * string list * top list

(*  
let usec_of_time = function
  | Usec i   -> i
  | Msec i   -> 1000 * i
  | Sec i    -> 1000 * 1000 * i  
  | Infinite -> max_int (* compatible over 32/64 bit systems *)
    
(* pretty printing *)

let string_of_time = function 
    | Usec i   -> string_of_int i ^ " us"
    | Msec i   -> string_of_int i ^ " ms"
    | Sec  i   -> string_of_int i ^ " s"
    | Infinite -> "Infinite"
*)
let s_op = function
  | Some s -> s
  | None   -> ""
    
let string_of_tops tl = 
  let rec stmts t sl = myconcat nl (mymap (stmt (t ^ tab)) sl)          
  and top = function
    | TopC (c)                 -> c (* "#> CCODE <#" *)
    | Isr (p, id, s)           -> "Isr prio " ^ string_of_int p ^ " " ^ id ^ op ^ stmts "" s ^ cl ^ nl 
    | FuncDef (t, id, par, s)  -> "Func " ^ t ^ " " ^ id ^ " " ^ par ^ op ^ stmts "" s ^ cl ^ nl
    | TaskDef (id, par, s)     -> "TaskDef " ^ id ^ " " ^ par ^ op ^ stmts "" s ^ cl ^ nl 
    | Reset (s)                -> "Reset " ^ op ^ stmts "" s ^ cl ^ nl
    | Idle (s)                 -> "Idle " ^ op ^ stmts "" s ^ cl ^ nl
      
  and stmt t = function 
    | Claim (id, s)               -> t ^ "Claim " ^ id ^ op ^ stmts t s ^ t ^ cl
    | Pend (be, id, par)          -> t ^ "Pend before" ^ string_of_time be ^ " " ^ id ^ "(" ^ par ^ ")" 
    | Sync (id, par )             -> t ^ "Sync " ^ id ^ "(" ^ par ^ ")"  
    | Async (mi, af, pr, id, par) -> t ^ s_op mi ^ "Async after " ^ string_of_time af ^ " before " ^ string_of_time pr ^ " " ^ id ^ "(#>" ^ par ^ "<#)" 
    | ClaimC (c)                  -> t ^ "#> CCODE <#"
    | Halt (s)                    -> t ^ "My HALT" ^ s
    | Abort (par)                 -> t ^ "abort (" ^ par ^ ")"
  in
  myconcat nl (mymap top tl)
      
let string_of_prog = function
  | Prog (mName, mIncl, mTop) ->
      "Module:" ^ mName ^ nl ^
      "Use :" ^ String.concat "," mIncl ^ nl ^
      "Prog:" ^ nl ^ string_of_tops mTop
                  
let rec prio_to_string r = match r with
  | []            -> ""
  | (id, p) :: l  -> "Isr " ^ id ^ ", priority " ^ string_of_int p ^ nl ^ prio_to_string l
    
