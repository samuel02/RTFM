(* Graphviz output generation *)
(* Supports Object, Task and Resource level of detail *)


open Common
open AST
open Env

let gv_of_obj ce p =
	let rec gv_of_cdl path = function
		| COVar (o, el, i) ->
        let po = path ^ "_" ^ i in
        let cd = myass o ce in
        po ^ " [label = " ^ ec ^ o ^enl ^ i ^ ec ^ nl ^ "shape = "^ ec ^"record" ^ ec ^ "]" ^ nl ^
        path ^ " -> " ^ po ^ nl ^
        gv_of_cd (po) cd
    
    | CTaskDecl (i, al, sl ) -> 
        let po = path ^ "_" ^ i in
        po ^ " [label = "^ ec ^"Task" ^enl ^ i ^ nl ^ ec ^ "]" ^ nl ^
        path ^ " -> " ^ po ^ nl 

    | CMDecl (t, i, al, sl) -> 
        let po = path ^ "_" ^ i in
        po ^ " [label = "^ ec ^"Function" ^enl ^ i ^ nl ^ ec ^ "]" ^ nl ^
        path ^ " -> " ^ po ^ nl 
    | _ -> "" (*raise UnMatched*)

  and gv_of_cd path = function
  	| ClassDef (i, cal, cdl) -> String.concat nl (List.map (gv_of_cdl path) cdl)

  in

  let se = cEnv_of_classDef p in
  let cd = 
  	try
  		List.assoc "Root" ce
  	with
  	| _ -> raise (RtfmError ("Root not defined"))

  in
  "digraph RTFM {" ^ nl
  ^ "Root [shape=diamond]" ^ nl
  ^ gv_of_cd "Root" cd 
  ^ "}"

let def_of_obj p =
  let ce = cEnv_of_classDef p in
  gv_of_obj ce p

(* check cyclic instances, if cyclic raise an RtfmError *)
let cycle_of_p ce p =
  let rec cycle_of_cdl path = function
    | COVar (o, el, i) ->
        let po = o ^ ":" ^ i in
        if List.mem po path then raise (RtfmError ("Cyclic instances in " ^ String.concat "-> " (List.rev path) ^ po));

        let cd = myass o ce in
        cycle_of_cd (po :: path) cd
    | _                -> () (* unit value *)

  and cycle_of_cd path = function
    | ClassDef (i, cal, cdl) ->
        (List.iter (cycle_of_cdl path) cdl)

  in
  let ce = cEnv_of_classDef p in
  let cd =
    try
      List.assoc "Root" ce
    with
    | _ -> raise (RtfmError ("Root not defined"))

  in
  cycle_of_cd ["Root"] cd

let cyclic p =
  let ce = cEnv_of_classDef p in
  cycle_of_p ce p

let gv_of_res =
	"digraph RTFM {" ^ nl ^ "gv_of_res if working [shape=diamond] "
  ^ "}" 

let gv_of_task =
	"digraph RTFM {" ^ nl ^ "gv_of_task_is_working"^ " [shape=diamond] "
  ^ "}" 