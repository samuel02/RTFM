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
