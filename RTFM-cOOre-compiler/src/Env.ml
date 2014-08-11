(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-cOOre/Env *)

open Common
open AST
  
(* create a mapping from id -> ClassDef *)
    
let cEnv_of_classDef =  
  let cEnv_of_classDef = function       
    | ClassDef (i, cal, cdl) -> (i, ClassDef(i, cal, cdl)) 
  in 
  function 
    | Prog cdl -> List.map cEnv_of_classDef cdl   
