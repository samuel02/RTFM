(* RTFM-core/AST *)

open Common
   
type stmt =
  | Claim     of string * stmt list
  | Pend      of string	
  | PendAfter of string * int
  | Sync      of string * string
  | Enable    of bool	
  | Halt
  | ClaimC    of string
 
type isr_type = 
  | HARD
  | SOFT
    
type top =
  | TopC      of string
  | Isr       of isr_type * string * int * stmt list
  | Func      of string * string * string * stmt list
  | Reset     of stmt list
    
type prog =
  | Prog of top list
    
(* pretty printing *)
    
let string_of_isr_type it = match it with
  | HARD -> "Isr"
  | SOFT -> "Task"
  
(* prog *)
let string_of_p p = 
  (* stmts *)
  let rec stmts t sl = match sl with
    | [] 		  -> t
    | s :: l	-> t ^ tab ^ stmt t s ^ nl ^ stmts t l 
      
  (* top *)
  and tops is = match is with
    | [] 		-> ""
    | i :: is	-> top i ^ "\n" ^ tops is 
      
  (* top *)
  and top i = match i with
    | TopC (c)				      -> "#> CCODE <#"
    | Isr (_, id, p, s) 	  -> "Isr " ^ id ^ ", priority " ^ string_of_int p ^ op ^ stmts "" s ^ cl ^ nl 
    | Func (t, id, par, s)	-> "Func " ^ t ^ " " ^ id ^ " " ^ par ^ op ^ stmts "" s ^ cl ^ nl
    | Reset (s)             -> "Reset " ^ op ^ stmts "" s ^ cl ^ nl 
      
  (* stmt *)
  and stmt t s = match s with
    | Claim (id, s) 		    -> "Claim " ^ id ^ op ^ stmts (tab ^ t) s ^ cl
    | Pend (id) 			      -> "Pend " ^ id 
    | PendAfter (id, time)	-> "Pend " ^ id ^ " after " ^ string_of_int time 
    | Sync (id, par )		    -> "Sync " ^ id ^ "#>" ^ par ^ "<#"  
    | Enable (b)			      -> "Enable " ^ (if b then "true" else "false")
    | Halt					        -> "Halt "
    | ClaimC (c)        	  -> "#> CCODE <#"
  in
  (* prog *)
  "Prog:" ^ nl ^ tops p    
    
let rec prio_to_string r = match r with
  | []				-> ""
  | (id, p) :: l	-> "Isr " ^ id ^ ", priority " ^ string_of_int p ^ nl ^ prio_to_string l
    
    
    
    