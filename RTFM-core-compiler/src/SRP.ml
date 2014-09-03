(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/SRP.ml *)

open AST
open SpecAST
open Common

(* resource is ass assoicative list (a->b) *)
let id_of r = fst r
let prio_of r = snd r

(* lookup resource id -> priority option *)
(*
let lookup e ml =
  try
    Some (List.assoc e ml)
  with
    _ -> None
*)
(* add/update map resrouce id -> priority *)
let rec add (id, prio) ml = match ml with
  | []                                -> [(id, prio)] (* add *)
  | ((mid, mprio) :: l) when id = mid ->
      (
        if prio > mprio then
          (id, prio) :: l       (* keys match, and we have a higher value, so replace and return *)
        else (mid, mprio) :: l  (* keys match, but keep and return *)
      )
  | m :: l                            -> m :: (add (id, prio) l) (* continue looking *)

let rec string_of_r r = match r with
  | []              -> ""
  | (id, prio) :: l -> "Resource " ^ id ^ ", priority ceiling " ^ string_of_int prio ^ nl ^ string_of_r l

(* lookup id -> prog -> stmts *)
let rec lookup id p = match p with
  | []                                                  -> failwith("Failed to lookup Func " ^ id)
  | FuncDef (_, fid, _, sl) :: l when (compare id fid = 0) -> sl
  | _ :: l                                              -> lookup id l

let rec lookup_ifunc id p = match p with
  | []                                                   -> failwith("Failed to lookup IFunc " ^ id)
  | IFunc (_, _, fid, _, sl) :: l when (compare id fid = 0) -> sl
  | _ :: l                                               -> lookup_ifunc id l

(* priorities from deadlines *)
let dl_to_pr spec =
  (* extract the prtiorities of task instances *)
  let prl = function
    | ITask (_, dl, _, _, _, _) -> usec_of_time dl 
    | _ -> raise (UnMatched)
  in
  
  (* get them sorted *)
  let sort_prl = List.sort compare (mymap prl spec) in
  
  (* remove dupicate deadlines *)
  let rec compress = function
    | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
    | s -> s
  in
  let u_prl = compress sort_prl in
  
  (* associate priorities by decreasing deadline *)
  let prnrl = range 1 (List.length u_prl) in
 
  List.combine (List.rev u_prl) prnrl 

let pr_of_dl dlp dl =
  try
    List.assoc dl dlp 
  with _ -> raise (RtfmError("Internal Error looking up priority for deadline " ^ string_of_int dl))  

let string_of_dl dlp dl =
  string_of_int (pr_of_dl dlp (usec_of_time dl))
  
(* ceiling_spec *)
let ceiling_spec spec dlp =
  (* stmts *)
  let rec stmts cp rm sl = match sl with
    | [] -> rm
    | Claim (id, s) :: l -> 
      let rm' = add (id, cp) (stmts cp rm s) in
      stmts cp rm' l
    | Sync (sid, _) :: l ->
        let rm' = stmts cp rm (lookup_ifunc sid spec)  in
        stmts cp rm' l 
    | _ :: l             -> stmts cp rm l 
  
  (* tops *)
  and tops rm il = match il with
    | [] -> rm
    | IIsr (p, id, s) :: l  ->
      let rm' = stmts p rm s in
      tops rm' l
    | ITask (_, dl, pa, id, _, s) :: l -> 
      let rm' = stmts (pr_of_dl dlp (usec_of_time dl)) rm s  in
      tops rm' l
    | IReset (s) :: l -> 
      let rm' = stmts 0 rm s  in
      tops rm' l         
    | _ :: l -> tops rm l
  in
  (* prog *)
  tops [] spec

  
(* associative list (id, [names]) *)
(*
let pl dlp topl rl =
  let rec add (p, id) pl = match pl with
    | []                            -> [(p, [id])]          (* add new *)
    | (lp, idl) :: l when (p = lp)  -> (p, id:: idl) :: l   (* add to existing *)
    | e :: l                        -> e :: (add (p, id) l)
  
  and top pl t = match t with
    | []                           -> pl
    | Isr (p, id,  _) :: l         -> let pl' = add (p, id ) pl in top pl' l
    | Task (dl, id, pa,  _, _) :: l-> let pl' = add (pr_of_dl dlp dl, id ) pl in top pl' l
    | _ :: l                       -> top pl l
  
  and res pl rl = match rl with
    | []           -> pl
    | (id, p) :: l -> let pl' = add (p, "[" ^ id ^ "]") pl in res pl' l
  
  in
  let isrs = top [] topl in
  List.sort (fun (a, _) (b, _) -> a - b) (res isrs rl)
*)

(* associative list (id, [names]) *)
let pl_spec dlp spec rl =
  let rec add (p, id) pl = match pl with
    | []                            -> [(p, [id])]          (* add new *)
    | (lp, idl) :: l when (p = lp)  -> (p, id:: idl) :: l   (* add to existing *)
    | e :: l                        -> e :: (add (p, id) l)
  
  and top pl t = match t with
    | []                            -> pl
    | IIsr (p, id,  _) :: l         -> let pl' = add (p, id ) pl in top pl' l
    | ITask (_, dl, id, pa,  _, _) :: l -> let pl' = add (pr_of_dl dlp (usec_of_time dl), id ) pl in top pl' l
    | _ :: l                        -> top pl l
  
  and res pl rl = match rl with
    | []           -> pl
    | (id, p) :: l -> let pl' = add (p, "[" ^ id ^ "]") pl in res pl' l
  
  in
  let isrs = top [] spec in
  List.sort (fun (a, _) (b, _) -> a - b) (res isrs rl)

let rec string_of pl = match pl with
  | []           -> ""
  | (p, il) :: l -> "Priority " ^ string_of_int p ^ ":" ^ String.concat ", " il ^ nl ^ string_of l

