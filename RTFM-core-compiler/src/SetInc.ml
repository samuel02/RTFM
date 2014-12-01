(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/SetInc *)

open Common
open AST

let setinc inc tl = 
  let rec stmts sl = mymap stmt sl  
  and top = function
    | FuncDef (t, id, par, s)  -> FuncDef (t, inc ^ id, par, stmts s)
    | TaskDef (id, par, s)     -> TaskDef (inc ^ id, par, stmts s) 
    | x                        -> x
      
  and stmt = function 
    | Claim (id, s)               -> 
      if compare inc "" != 0 then
        Claim ("RES_" ^ inc ^ id, stmts s)
      else
        Claim (id, stmts s)
    | Pend (be, id, par)          -> Pend (be, inc ^ id, par) 
    | Sync (id, par )             -> Sync (inc ^ id, par) 
    | Async (mi, af, pr, id, par) -> Async (mi, af, pr, inc ^ id, par) 
    | State (_)                   -> State (inc)
    | x                           -> x
  in
  mymap top tl
      