(* AST *)
open Common
  
type stmt =
  | Claim of string * stmt list
  | Pend of string	
  | PendAfter of string * int
  | Sync of string * string
  | Enable of bool	
  | Halt
  | ClaimC of string				
    
type isr_type = 
  | HARD
  | SOFT
    
type top =
  | TopC of string
  | TopPend of string
  | Isr of isr_type * string * int * stmt list
  | Func of string * string * string * stmt list
    
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
    | [] 		-> t
    | s :: l	-> t ^ tab ^ stmt t s ^ nl ^ stmts t l 
      
  (* top *)
  and tops is = match is with
    | [] 		-> ""
    | i :: is	-> top i ^ "\n" ^ tops is 
      
  (* top *)
  and top i = match i with
    | TopC (c)				-> "#> CCODE <#"
    | TopPend (id)			-> "Pend " ^ id 
    | Isr (_, id, p, s) 	-> "Isr " ^ id ^ ", priority " ^ string_of_int p ^ op ^ stmts "" s ^ cl ^ nl 
    | Func (t, id, par, s)	-> "Func " ^ t ^ " " ^ id ^ " " ^ par ^ op ^ stmts "" s ^ cl ^ nl
      
  (* stmt *)
  and stmt t s = match s with
    | Claim (id, s) 		-> "Claim " ^ id ^ op ^ stmts (tab ^ t) s ^ cl
    | Pend (id) 			-> "Pend " ^ id 
    | PendAfter (id, time)	-> "Pend " ^ id ^ " after " ^ string_of_int time 
    | Sync (id, par )		-> "Sync " ^ id ^ "#>" ^ par ^ "<#"  
    | Enable (b)			-> "Enable " ^ (if b then "true" else "false")
    | Halt					-> "Halt "
    | ClaimC (c)        	-> "#> CCODE <#"
  in
  (* prog *)
  "Prog \n" ^ tops p
    
(* resources *)
(*    
   let prio p =
   (* top *)
   let rec res_i r il = match il with
   | [] 						-> r
   | Isr (_, id, p, s) :: l	-> (id, p) :: res_i r l
   | _ :: l					->  res_i r l
   in 
   (* prog *)
   match p with
   | Prog isrs -> res_i [] isrs 
 *)
    
    
let rec prio_to_string r = match r with
  | []				-> ""
  | (id, p) :: l	-> "Isr " ^ id ^ ", priority " ^ string_of_int p ^ nl ^ prio_to_string l
    
    
    
    