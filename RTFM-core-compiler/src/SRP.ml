(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/SRP.ml *)

open AST
open Common

(* resource is ass assoicative list (a->b) *)
let id_of r = fst r
let prio_of r = snd r

(* lookup resource id -> priority option *)
let lookup e ml =
  try
    Some (List.assoc e ml)
  with
    _ -> None

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
  | Func (_, fid, _, sl) :: l when (compare id fid = 0) -> sl
  | _ :: l                                              -> lookup id l

(* ceiling *)
let ceiling p =
  (* stmts *)
  let rec ceil_of_stmts cp rm sl k = match sl with
    | [] -> rm
    | Claim (id, s) :: l -> let rm' = add (id, cp) (ceil_of_stmts cp rm s k) in
        ceil_of_stmts cp rm' l k
    | Sync (sid, _) :: l ->
        (
          match (List.mem sid k) with
          | true -> raise (RtfmError ("Failed due to cyclic synch chain in Ist/Task " ^ (String.concat " -> " (k @ [sid]))))
          | _    -> let rm' = ceil_of_stmts cp rm (lookup sid p) (k @ [sid]) in
              ceil_of_stmts cp rm' l k
        )
    | _ :: l             -> ceil_of_stmts cp rm l k
  
  (* tops *)
  and tops rm il = match il with
    | [] -> rm
    | Isr (p, id, s) :: l     -> let rm' = ceil_of_stmts p rm s [id] in
        tops rm' l
    | Task (p, id, _, s) :: l -> let rm' = ceil_of_stmts p rm s [id] in
        tops rm' l
    | Reset (s) :: l          -> let rm' = ceil_of_stmts 0 rm s ["Reset"] in
        tops rm' l
            
    | _ :: l -> tops rm l
  in
  (* prog *)
  tops [] p

(* associative list (id, [names]) *)
let pl topl rl =
  let rec add (p, id) pl = match pl with
    | []                            -> [(p, [id])]          (* add new *)
    | (lp, idl) :: l when (p = lp)  -> (p, id:: idl) :: l   (* add to existing *)
    | e :: l                        -> e :: (add (p, id) l)
  
  and top pl t = match t with
    | []                           -> pl
    | Isr (p, id,  _) :: l         -> let pl' = add (p, id ) pl in top pl' l
    | Task (p, id,  _, _) :: l     -> let pl' = add (p, id ) pl in top pl' l
    | _ :: l                       -> top pl l
  
  and res pl rl = match rl with
    | []           -> pl
    | (id, p) :: l -> let pl' = add (p, "[" ^ id ^ "]") pl in res pl' l
  
  in
  let isrs = top [] topl in
  List.sort (fun (a, _) (b, _) -> a - b) (res isrs rl)

let rec string_of pl = match pl with
  | []           -> ""
  | (p, il) :: l -> "Priority " ^ string_of_int p ^ ":" ^ String.concat ", " il ^ nl ^ string_of l
