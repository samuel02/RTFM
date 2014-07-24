(* Dot *)
open Common
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
    | _ 	-> ' ' 
  in String.trim (String.map printable str)
    
(* create a record entry *)    
let d_of_ds d =
  let record_line i s = 
    let dbg = match opt.debug with
      | true 	-> string_of_int i ^ "_" 
      | false 	-> ""
    in		
    "<L" ^ string_of_int i ^">" ^ dbg ^ s   
  in
  let st = match d with
    | DotClaim (i, _, _, s, _)	-> record_line i ("claim " ^ s) 
    | DotSync (i, _, s) 		-> record_line i ("sync " ^ s) 
    | DotPend (i, _, s)			-> record_line i ("pend " ^ s)
    | DotPendAfter (i, _, s, ti)-> record_line i ("pendAfter " ^ string_of_int ti ^ " " ^ s)
    | DotHalt	(i)				-> record_line i ("halt ")
    | DotC (i, s)				-> record_line i ("\\#\\> " ^ strip (String.sub s 0 (min 8 (String.length s))) ^ "...\\<\\#")  
  in
  st ^ "\\l"    
    
(* create records for the program *)    
let d_of_dt rl d =
  let l_of_s dl = String.concat " | " (List.map d_of_ds dl) in
  let record id dl = id ^ " [shape=record, label=\"{" ^ id ^ " | " ^ l_of_s dl ^ "}\"]" ^ nl in
  let rec d_of_ds_rec id d = match d with
    | DotClaim (i, cr, ft, s, (Ds (t, l)))	-> (record t) l ^ nl ^ String.concat nl (List.map (d_of_ds_rec id) l) 
      ^ ft ^ ":<L" ^ string_of_int i ^ ">:e -> " ^ t ^ ":n" ^ nl
      (* ^ ft ^ ":<L" ^ string_of_int i ^ ">:e -> " ^ t ^ ":n" ^ " [constraint=false]" ^ nl *)
        ^ "{ rank=same; " ^ ec ^ "P" ^ string_of_int (List.assoc cr rl) ^ ec ^ "; " ^ ec ^ t ^ ec ^ "; }" ^ nl 
        
    | DotSync (i, ft, id)				 	-> ft ^ ":<L" ^ string_of_int i ^ ">:e -> " ^ id ^ nl
    | DotPend (i, ft, id)				  	-> ft ^ ":<L" ^ string_of_int i ^ ">:e -> " ^ "ISR_" ^ id ^ " [style=dotted, constraint=false]" ^ nl
      
    | _ -> ""
      
  in      
  let cs id l = String.concat nl (List.map (d_of_ds_rec id) l) in
  
  let name_of_isr ty id prio = ec ^ string_of_isr_type ty ^ " " ^ id ^ enl ^ "priority = " ^ string_of_int prio ^ ec in
  
  let node_of_isr ty = match ty with
    | HARD -> ", shape = box "
    | SOFT -> "" (* default ellipse *)
  in
  match d with
    | DIsr (ty, id, prio, (Ds (t, l)))	-> 
      "ISR_" ^ id ^ " [label=" ^ name_of_isr ty id prio ^ node_of_isr ty ^ "]" ^ nl 
        ^ "{ rank=source; ISR_" ^ id  ^ " ; }" ^ nl
        ^ "ISR_" ^ id ^ " -> " ^ id   (* ^ node_of_isr ty ^ *) (* " [constraint=false]"   *) ^ nl
        ^ (record (id)) l ^ cs id l
        ^ "{ rank=same; " ^ ec ^ "P" ^ string_of_int prio ^ ec ^ "; " ^ ec ^ id ^ ec ^ "; }" ^ nl 
    | DFunc (t, id, (Ds (i, l))) 		-> (record  (id)) l ^ nl ^ cs id l
    (* | DCcode  							-> "" *)
    (* | DPend id 							-> "CCODE -> " ^ id ^ nl ^ "{ rank=source; CCODE}" ^ nl ^ "CCODE [shape=diamond]" ^ nl   *)
    | DReset (Ds (t, l))                -> let id = "User Reset" in (record (id)) l ^ nl ^ cs id l
      
(* parse the program stmts*)
let label = ref (0);;

let d_of_p p rml = 
  let rec stmts t s = 
    label := !label + 1;
    if opt.debug then p_stderr ("--- generating unique label " ^ string_of_int !label ^ " ----"^ nl ); 
    let i = !label in
    match s with
      | Claim (cr, cs) 		-> let de = cr ^ "_" ^ t in DotClaim (i, cr, t, de, Ds (de , List.map (stmts de) cs))
      | Sync (sid, _) 		-> DotSync (i, t, sid) 
      | Pend (pid) 			-> DotPend (i, t, pid)
      | PendAfter (pid, ti) -> DotPendAfter (i, t, pid, ti)
      | ClaimC (s) 			-> DotC (i, s)
      | Halt  				-> DotHalt (i)
      | _					-> raise (RtfmError("Enable not implemented"))      
  in
  (* parse the program entry points *)
  let mytop = function 
    | Isr (t, id, prio, sl) -> DIsr (t, id, prio, Ds ("", (List.map (stmts id) sl) ) )
    | Func (t, id, _, sl) 	-> DFunc (t, id, Ds ("", (List.map (stmts id) sl) ) )
    | Reset (sl)            -> DReset (Ds ("", (List.map (stmts "User Reset") sl ) ) )
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
    String.concat nl (List.map def pl) ^ String.concat " -> " (List.map chain pl) ^ "[dir=none]" ^ nl
      
  in
  let pd = mymap mytop p in
  "digraph RTFM {" ^ nl ^
  (*
     dot_of_rml (sort rml) ^ (* list resrouces to the left *)	*)
    dot_of (pl p rml) ^
    String.concat nl (List.map (d_of_dt rml) pd) ^
    
    nl ^ "}"
    