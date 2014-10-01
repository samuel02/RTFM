(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-cOOre/AST *)

open Common
open AST

exception TypeError of string


type pType =
  | Int
  | Char
  | Bool
  | Byte
  
let to_string = function    
  | Int  -> "Int"
  | Char -> "Char"
  | Bool -> "Bool"
  | Byte -> "Byte"

type op =
  | OpPlus 
  | OpSub  
  | OpMult 
  | OpDiv 
  | OpComp 
  | OpGrt  
  | OpGeq  
  | OpLet  
  | OpLeq    


type id = string;;

let string_of_type t = match t with
 | Int  -> "Int"
 | Char  -> "Char"
 | _  -> raise Not_found
;;


type env = 
 { local: binding list;
   scope: string;
   parent: env option ;
 }
 and binding = id * pType
;;

let rec type_of id e =
 try
  List.assoc id e.local
 with
  Not_found -> match e.parent with
   | Some parent  -> type_of id parent
   | None    -> raise (Not_found)
;;

let unify t1 t2 = 
  if t1 == t2 then t1
  else
    raise (TypeError(" Types not matching: arg1 = " ^ to_string t1 ^ ", arg2 = " ^ to_string t2 ^ ". "))(* ^ string_of_pType t1 ^ " " ^ string_of_pType t2)))*)

        
let well_op op o1 o2 t_env =
    let equal rt t = rt = t in
    let type_list rt l = List.exists (equal rt) l in
    let t1 = type_of o1 t_env in
    let t2 = type_of o2 t_env in
    let rt = unify t1 t2 in
    match op with
    | OpPlus -> if type_list rt [Int] then true else raise (TypeError("Addition"))
    | OpSub  -> if type_list rt [Int] then true else raise (TypeError("Subtraction"))
    | OpMult -> if type_list rt [Int] then true else raise (TypeError("Multiplication"))
    | OpDiv  -> if type_list rt [Int] then true else raise (TypeError("Division"))
    | OpComp -> if type_list rt [Int;Bool;Byte] then true else raise (TypeError("Greater than"))
    | OpGrt  -> if type_list rt [Int;Bool;Byte] then true else raise (TypeError("Greater than"))
    | OpGeq  -> if type_list rt [Int;Bool;Byte] then true else raise (TypeError("Greater or equal"))
    | OpLet  -> if type_list rt [Int;Bool;Byte] then true else raise (TypeError("Less than"))
    | OpLeq  -> if type_list rt [Int;Bool;Byte] then true else raise (TypeError("Less or equal"))



let main() =
  let t_env = {local= [("kalle", Int); ("anka", Int); ("kajsa", Bool); ("joakim", Bool); ("lina", Char); ("bertil", Char) ]; scope = "Root"; parent = None} in
  let t_env2 = {local= [("anna", Int); ("nils", Int); ]; scope = "Root"; parent = Some t_env} in
  try
    (* Addition of two Int's, OK! *)
    if (well_op OpGrt "anna" "kalle" t_env2) then print_string "OK!\n";
    (* Addition of two Char's, WARNING! *)
    (* And operation of two Bool's, OK!*)
    (*
    if (well_op OpPlus "bertil" "lina" t_env) then print_string "OK!\n";
    if (well_op OpAnd "joakim" "kajsa" t_env) then print_string "OK!\n";
    if (well_op OpPlus "kalle" "kajsa" t_env) then print_string "OK!\n";
    if (well_op OpPlus "joakim" "kajsa" t_env) then print_string "OK!\n";
    if (well_op OpPlus "kalle" "anka" t_env) then print_string "OK!\n";
    if (well_op OpAnd "joakim" "anka" t_env) then print_string "OK!\n";
    *)


    
  with
  | Not_found -> print_string "Not_found exception caught\n";
  | TypeError(s) -> print_string ("TypeError exception caught!" ^ s ^ "\n");
  | _ -> failwith "Warning"
;;

main ();;