(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/KleeAST *)

open Common

type entry =
  | Object    of string * string * int * obj
  
       
type obj =
  | Obj       of string * int * string
    
let string_of_entry el = 
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
    
