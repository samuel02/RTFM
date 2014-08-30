(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/AST *)

open Common
     
type time =
  | Usec of int
  | Msec of int
  | Sec  of int
  | Infinite

type stmt =
  | Claim     of string * stmt list                             (* res_id, stmts                    *)
  | Pend      of int * string                                   (* prio, isr_id                     *)
  | Sync      of string * string                                (* func_id, par_c                   *)
  | Async     of time * time * string * string                  (* expr, prio, task_id, par_c       *)
  | ClaimC    of string                                         (* code_c                           *)
       
type top =
  | TopC      of string                                         (* code_c                           *)
  | Isr       of int * string * stmt list                       (* prio, isr_id, stmts              *)
  | FuncDef   of string * string * string * stmt list           (* rtype_c, func_id, arg_c, stmts   *)
  | TaskDef   of string * string * stmt list                    (* task_id, arg_c, stms             *)
  | Reset     of stmt list                                      (* stmts                            *)
  | Idle      of stmt list                                      (* idle                             *)
                                                                (* Task is internally generated by  *)
                                                                (* RTFM-core compiler               *)
  | Task      of int * string * string * string * stmt list     (* prio, isr_id, path, par_c, stmts *)
  | Func      of string * string * string * stmt list           (* rtype_c, path_f_id, arg_c, stmts *)
     
type prog =
  | Prog      of string * string list * top list

let usec_of_time = function
  | Usec i -> i
  | Msec i -> 1000 * i
  | Sec i  -> 1000 * 1000 * i  
  | Infinite -> 60*60*24*7 * 1000 * 1000 (* a week should be enough *)
    
(* pretty printing *)

let string_of_time = function 
    | Usec i   -> string_of_int i ^ " us"
    | Msec i   -> string_of_int i ^ " ms"
    | Sec  i   -> string_of_int i ^ " s"
    | Infinite -> "Infinite"

  
let string_of_tops tl = 
  let rec stmts t sl = myconcat nl (mymap (stmt (t ^ tab)) sl)          
  and top = function
    | TopC (c)                 -> c (* "#> CCODE <#" *)
    | Isr (p, id, s)           -> "Isr prio " ^ string_of_int p ^ " " ^ id ^ op ^ stmts "" s ^ cl ^ nl 
    | Task (p, id, pa, par, s) -> "Task before " ^ string_of_int p ^ " " ^ id ^ "[" ^ pa ^ "] (" ^ par ^ ")" ^ op ^ stmts "" s ^ cl ^ nl 
    | FuncDef (t, id, par, s)  -> "Func " ^ t ^ " " ^ id ^ " " ^ par ^ op ^ stmts "" s ^ cl ^ nl
    | Func (t, id, par, s)     -> "Func " ^ t ^ " " ^ id ^ " " ^ par ^ op ^ stmts "" s ^ cl ^ nl
    | TaskDef (id, par, s)     -> "TaskDef " ^ id ^ " " ^ par ^ op ^ stmts "" s ^ cl ^ nl 
    | Reset (s)                -> "Reset " ^ op ^ stmts "" s ^ cl ^ nl
    | Idle (s)                 -> "Idle " ^ op ^ stmts "" s ^ cl ^ nl
      
  and stmt t = function 
    | Claim (id, s)           -> t ^ "Claim " ^ id ^ op ^ stmts t s ^ t ^ cl
    | Pend (af, id)           -> t ^ "Pend after" ^ string_of_int af ^ " " ^ id  
    | Sync (id, par )         -> t ^ "Sync " ^ id ^ "(#>" ^ par ^ "<#)"  
    | Async (af, pr, id, par) -> t ^ "Async after " ^ string_of_time af ^ " before " ^ string_of_time pr ^ " " ^ id ^ "(#>" ^ par ^ "<#)" 
    | ClaimC (c)              -> t ^ "#> CCODE <#"
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
    
