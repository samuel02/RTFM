(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/Env.ml *)

open AST
open Common

(* lookup id -> topl -> TaskDef *)
let rec lookup_task id p = match p with
  | []                                                      -> failwith("Failed to lookup Task " ^ id)
  | TaskDef (tid, par, sl):: l when (compare id tid == 0)   -> TaskDef (tid, par, sl)
  | _ :: l                                                  -> lookup_task id l

(* lookup id -> topl -> stmt list *)
let rec lookup_func_sl id p = match p with
  | []                                                      -> failwith("Failed to lookup Func " ^ id)
  | FuncDef (_, fid, _, sl) :: l when (compare id fid == 0) -> sl
  | _ :: l                                                  -> lookup_func_sl id l

(* lookup id -> topl -> Func *)
let rec lookup_func id p = match p with
  | []                                                      -> failwith("Failed to lookup Func " ^ id)
  | FuncDef (r, fid, p, sl) :: l when (compare id fid == 0) -> FuncDef (r, fid, p, sl)
  | _ :: l                                                  -> lookup_func id l