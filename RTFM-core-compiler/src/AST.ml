(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/AST *)

open Common
     
type stmt =
  | Claim     of string * stmt list
  | Pend      of int * string 
  | Sync      of string * string
  | Async     of int * int * string * string
  | ClaimC    of string
     
type top =
  | TopC      of string
  | Isr       of int * string * stmt list
  | Task      of int * string * string * stmt list
  | Func      of string * string * string * stmt list
  | TaskDef   of string * string * stmt list
  | Reset     of stmt list
   
type prog =
  | Prog      of string * string list * top list
    
(* pretty printing *)
      
(* prog *)
let string_of_tops tl = 
  (* stmts *)
  let rec stmts t sl = match sl with
    | []      -> t
    | s :: l  -> t ^ tab ^ stmt t s ^ nl ^ stmts t l 
      
  (* top *)
  and tops is = match is with
    | []      -> ""
    | i :: is -> top i ^ "\n" ^ tops is 
      
  (* top *)
  and top i = match i with
    | TopC (c)              -> "#> CCODE <#"
    | Isr (p, id, s)        -> "Isr prio " ^ string_of_int p ^ " " ^ id ^ op ^ stmts "" s ^ cl ^ nl 
    | Task (p, id, par, s)  -> "Task prio " ^ string_of_int p ^ " " ^ id ^ "(" ^ par ^ ")" ^ op ^ stmts "" s ^ cl ^ nl 
    | Func (t, id, par, s)  -> "Func " ^ t ^ " " ^ id ^ " " ^ par ^ op ^ stmts "" s ^ cl ^ nl
    | TaskDef (id, par, s)  -> "TaskDef " ^ id ^ " " ^ par ^ op ^ stmts "" s ^ cl ^ nl 
    | Reset (s)             -> "Reset " ^ op ^ stmts "" s ^ cl ^ nl 
      
  (* stmt *)
  and stmt t s = match s with
    | Claim (id, s)           -> "Claim " ^ id ^ op ^ stmts (tab ^ t) s ^ cl
    | Pend (af, id)           -> "Pend after" ^ string_of_int af ^ " " ^ id  
    | Sync (id, par )         -> "Sync " ^ id ^ "#>" ^ par ^ "<#"  
    | Async (af, pr, id, par) -> "Async after " ^ string_of_int af ^ " @prio " ^ string_of_int pr ^ " " ^ id ^ " (" ^ par ^ ")" 
    | ClaimC (c)              -> "#> CCODE <#"
  in
  tops tl
      
let string_of_prog = function
  | Prog (mName, mIncl, mTop) ->
      "Module:" ^ mName ^ nl ^
      "Use :" ^ String.concat "," mIncl ^ nl ^
      "Prog:" ^ nl ^ string_of_tops mTop
                  
                        
let rec prio_to_string r = match r with
  | []            -> ""
  | (id, p) :: l  -> "Isr " ^ id ^ ", priority " ^ string_of_int p ^ nl ^ prio_to_string l
    
    
    
    
