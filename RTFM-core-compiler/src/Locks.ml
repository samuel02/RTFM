(* Locks.ml *)
open Common
open AST
open SRP
  
(* prog -> [(cr, id)] *)   
let dep_of_p p = 
  (* stmts *)
  let rec stmts cr sl = match sl with
    | [] 					-> []
    | Claim (id, s) :: l 	-> (cr, id) :: stmts id s @ stmts id l
    | Sync (sid, _) :: l 	-> (stmts cr (lookup sid p)) @ stmts cr l 
    | _ :: l 				-> stmts cr l 
      
  (* tops *)
  and tops il = match il with
    | [] 						-> []
    | Isr (_, id, p, s) :: l	-> (stmts id s) @ tops l  
    | _ :: l 					-> tops l 
  in
  (* prog *)
  tops p 
    
let string_of_dep dep =
  let str (a, b) = a ^ " -> " ^ b  in
  String.concat nl (List.map str dep) ^ nl
    
    
let rec entry ll = match ll with
  | []						-> []
  | Isr (_, id, _, _) :: l	-> id :: entry l 
  | _ :: l					-> entry l
    
    
let string_of_entry e = "Entry points :" ^ String.concat ", " e ^ nl  
  
(*
   
   let dot_of_dep dep =
   "digraph RTFM {" ^ nl ^ string_of_dep dep ^ "}"        
   
   let rec hull dep =
   let rec iter ll  = 
   match ll with
   | [] 			-> ([],[])
   | (a,b) :: l 	-> 
   try
   let c = List.assoc b ll in
   if (a = c) then 
   ([], [(a,b)]) (* cycle detected *)
   else if (List.mem (a,c) dep) then 
   iter l (* already added *) 
   else 
   ([(a,c)], []) (* add, but not cycle *)
   
   with
   Not_found -> iter l;
   in            
   let (ndep, cycle) = iter dep in
   
   (*
   p_stderr ("dep :" ^ (string_of_dep dep) ^ nl);
   p_stderr ("ndep :" ^ string_of_dep ndep ^ nl);
   p_stderr ("in cycle :" ^ string_of_dep cycle ^ nl);
 *)
   
   if ndep = [] then dep else hull (ndep @ dep)
   
   let rec add i ll = 
   match ll with
   | [] 		-> [(i, 1)]
   | (a, b)::l -> if i = a then (a, b + 1) :: l else (a, b) :: add i l
   
   let rec sub i ll = 
   match ll with
   | [] 		-> raise (RtfmError("Internal error, sub " ^ i))
   | (a, b)::l -> 
   if i = a then 
   if b = 0 then 
   raise (RtfmError("Internal error, sub " ^ i)) 
   else ((a, b - 1) :: l, b < 2) 
   else 
   let (ab, iszero) = sub i l in
   ((a, b) :: ab, iszero)
   
   let count dep =
   let rec coun ll nr =
   match ll with
   | [] 			-> nr
   | (a, b) :: l	-> coun l (add b nr)
   in        
   coun dep []      
   
   let string_of_count c =
   let str (a, b) = a ^ " :: " ^ string_of_int b in
   "count : " ^ String.concat ", " (List.map str c) ^ nl
   
   
   let topsort e dep count =
   (* traverses graphs starting from entry point, resulting in (reamin, count) *)
   let rec travelgraph f (remain, count) noncon = match remain with
   | [] 			-> (noncon, count)
   | (a, b) :: l 	-> p_stderr ("f: (a, b) " ^ f ^ " : " ^ a ^ ", " ^ b ^ nl);
   if (f = a) then begin
   p_stderr ("f=a" ^ nl ) ;
   let (ncount, iszero) = sub b count in 
   if iszero then begin
   p_stderr ("zero" ^ nl ) ;
   let (r,c) = travelgraph b (l @ noncon, ncount) [] in
   travelgraph f (r, c) []
   end else begin
   p_stderr ("false" ^ nl ) ;
   travelgraph f (l, ncount) (noncon)
   end
   end else begin
   p_stderr ("f<>a" ^ nl ) ;
   
   travelgraph f (l, count) ((a,b) :: noncon)      
   end    
   
   and travelentries el (remain, count) = 
   p_stderr ("te : remain " ^ nl ^ string_of_dep remain ^ nl ^ " te: count " ^ string_of_count count);
   match el with
   | []		-> (remain, count)
   | f :: l 	-> p_stderr (" f " ^ f ^ nl); travelentries l (travelgraph f (remain, count) [])  
   
   in
   travelentries e (dep, count)
 *)
  
let rec successors n = function
  [] 				-> []
  | (s, t) :: edges ->
    if s = n then
      t :: successors n edges
    else
      successors n edges
        
exception Cyclic of string
  
let tsort edges entries =
  let rec sort path visited = function
    []			-> visited
    | n::nodes 	->
      if List.mem n path then 
        raise (Cyclic ("Potential deadlock involving " ^ n )) 
      else
        let v'= 
          if List.mem n visited 
          then 
            visited 
          else
            n :: sort (n::path) visited (successors n edges)
        in 
        sort path v' nodes
  in
  sort [] [] entries
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    