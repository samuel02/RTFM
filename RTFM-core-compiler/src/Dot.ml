(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/Dot *)

open Common
open Options
open AST
open SRP
  
type dstmt =       
  | DotClaim of int * string * string * string * dstmts
  | DotSync of int * string * string 
  | DotPend of int * string * string 
  | DotPendAfter of int * string * string * int
  | DotHalt of int
  | DotC of int * string 
and
  dstmts =
  | Ds of string * dstmt list 
    
type dtop =
  | DIsr of isr_type * string * int * dstmts
  | DFunc of string * string * dstmts 
  | DReset of dstmts 
    
(* strip from leading ws and end at internal nl *)
let strip str = 
  let printable c = match c with
    | 'a'..'z' | 'A'..'Z' | '0' .. '9' | '/' | '*' | '+' | '-' | '%' | '$' | '&' | '=' | '<' | '>' | '(' | ')' | '[' | ']'    
      -> c
    | _   -> ' ' 
  in String.trim (String.map printable str)
    
(* create a record entry *)    
let d_of_ds d =
  let record_line i s = 
    let dbg = match opt.debug with
      | true    -> string_of_int i ^ "_" 
      | false   -> ""
    in      
    "  <TR><TD port=\"L"^ string_of_int i ^ "\" align=\"LEFT\">" ^ dbg ^ s ^ "</TD></TR>"  
  in
  let st = match d with
    | DotClaim (i, _, _, s, _)   -> record_line i ("claim " ^ s) 
    | DotSync (i, _, s)          -> record_line i ("sync " ^ s) 
    | DotPend (i, _, s)          -> record_line i ("pend " ^ s)
    | DotPendAfter (i, _, s, ti) -> record_line i ("pendAfter " ^ string_of_int ti ^ " " ^ s)
    | DotHalt   (i)              -> record_line i ("halt ")
    | DotC (i, s)                -> record_line i ("#&gt; " ^ strip (String.sub s 0 (min 8 (String.length s))) ^ "...&lt;#")  
  in
  st 
    
(* create records for the program *)    
let d_of_dt rl d =
  let l_of_s dl = String.concat (" <HR/> " ^ nl) (List.map d_of_ds dl) in
  let record id c dl = 
      id ^ " [label= " ^ nl
      ^ "<<TABLE CELLBORDER=\"0\" CELLSPACING=\"0\">" ^ nl 
      ^ "  <TR><TD>" ^ id ^ "</TD></TR> <HR/>" ^ nl
      ^ l_of_s dl ^ nl 
      ^ "</TABLE>>] [shape = none, style=filled, fillcolor =  " ^ c ^ ", margin = 0] " ^ nl 
      in
  let rec d_of_ds_rec id d = match d with
    | DotClaim (i, cr, ft, s, (Ds (t, l)))  -> (
                                                 (record t "tan1") l ^ nl 
                                                 ^ ft ^ ":L" ^ string_of_int i ^ ":e -> " ^ t ^ ":n [dir = both, arrowtail = dot, arrowhead = none]" ^ nl
                                                 ^ "{ rank=same; " ^ ec ^ "P" ^ string_of_int (List.assoc cr rl) ^ ec ^ "; " ^ ec ^ t ^ ec ^ "; }" ^ nl 
                                                 ^ String.concat nl (List.map (d_of_ds_rec id) l) ^ nl
                                              )
    | DotSync (i, ft, id)                   -> ft ^ ":L" ^ string_of_int i ^ ":e -> " ^ id ^ ":n [arrowhead = none, arrowtail = none]" ^ nl 
    | DotPend (i, ft, id)                   -> ft ^ ":L" ^ string_of_int i ^ ":e -> " ^ (* "ISR_" ^ *) id ^ ":n [dir = both, arrowtail = invempty, arrowhead = none, style=dotted]" ^ nl 
    | _ -> ""
      
  in      
  let cs id l = String.concat nl (List.map (d_of_ds_rec id) l) in
  
  let name_of_isr ty id prio = ec ^ string_of_isr_type ty ^ " " ^ id ^ enl ^ "priority = " ^ string_of_int prio ^ ec in
  
  let node_of_isr ty = match ty with
    | HARD -> ", shape = box "
    | SOFT -> "" (* default ellipse *)
  in
  match d with
    | DIsr (ty, id, prio, (Ds (t, l)))  -> (
                                             (record (id) "lightblue") l ^ cs id l
                                             ^ "{ rank=same; " ^ ec ^ "P" ^ string_of_int prio ^ ec ^ "; " ^ ec ^ id ^ ec ^ "; }" ^ nl 
                                             ^ "ISR_" ^ id ^ " [style=filled, fillcolor = tan3, label=" ^ name_of_isr ty id prio ^ node_of_isr ty ^ "]" ^ nl 
                                             ^ "{ rank=same; ISR; ISR_" ^ id  ^ " ; }" ^ nl
                                             ^ "ISR_" ^ id ^ " -> " ^ id ^ ":nw [arrowhead = none]"^ nl
                                          )
    | DFunc (t, id, (Ds (i, l)))        -> (record  (id) "lightgrey") l ^ nl ^ cs id l
    | DReset (Ds (t, l))                -> (
                                            let id = "User_Reset" in (record (id) "yellow") l ^ nl ^ cs id l ^ nl
                                              ^ "{ rank=source; " ^ id ^ "  ; }" ^ nl 
                                           )
        
(* parse the program stmts*)
let label = ref (0);;

let d_of_p p rml = 
  let rec stmts t s = 
    label := !label + 1;
    if opt.debug then p_stderr ("--- generating unique label " ^ string_of_int !label ^ " ----"^ nl ); 
    let i = !label in
    match s with
      | Claim (cr, cs)      -> let de = cr ^ "_" ^ t in DotClaim (i, cr, t, de, Ds (de , List.map (stmts de) cs))
      | Sync (sid, _)       -> DotSync (i, t, sid) 
      | Pend (pid)          -> DotPend (i, t, pid)
      | PendAfter (pid, ti) -> DotPendAfter (i, t, pid, ti)
      | ClaimC (s)          -> DotC (i, s)
      | Halt                -> DotHalt (i)
      | _                   -> raise (RtfmError("Enable not implemented"))      
  in
  
  (* parse the program entry points *)
  let mytop = function 
    | Isr (t, id, prio, sl) -> DIsr (t, id, prio, Ds ("", (List.map (stmts id) sl) ) )
    | Func (t, id, _, sl)   -> DFunc (t, id, Ds ("", (List.map (stmts id) sl) ) )
    | Reset (sl)            -> DReset (Ds ("", (List.map (stmts "User_Reset") sl ) ) )
    | _                     -> raise UnMatched 
  in
  
  (* leftmost column is the prio/priority ceiling legend *)
  let dot_of pl = 
    let def = function
      | (p, ill) -> "P" ^ string_of_int p  ^ " [shape=plaintext, label = " ^ ec ^ "Priority/Ceiling " ^ string_of_int p ^ enl ^ String.concat ", " ill ^ ec ^ "]" ^ nl    
    in
    let chain = function
      | (p, ill) -> "P" ^ string_of_int p      
    in
    String.concat nl (List.map def pl) ^ String.concat " -> " ("ISR" :: (List.rev (List.map chain pl))) ^ "[dir=none]" ^ nl
      
  in
  let pd = mymap mytop p in
  "digraph RTFM {" ^ nl 
    ^ "ISR [shape=plaintext, label = ISR_VECTOR]" ^ nl ^ dot_of (pl p rml) (* priorities/resources to the left *)
    ^ String.concat nl (List.map (d_of_dt rml) pd) ^ nl 
    ^ "}"
    