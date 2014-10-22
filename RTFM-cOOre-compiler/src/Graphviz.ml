(* Graphviz output generation *)
(* Supports Object, Task and Resource level of detail *)
(* gv_of_obj defines the behaviour, def_o_obj is called from main.ml to
		generate the correct Graphviz output *)

open Common
open AST
open Env


(*Deffinition of Graphviz output for resource option*)
let gv_of_obj ce p =
	(*Define the recursive loop to 'catch' all objects and their respective tasks/functions *)
	let rec gv_of_cdl path = function
		(*Object level, prints a rectangle with the class- and classinstance-names*)
		| COVar (o, el, i) ->
        let po = path ^ "_" ^ i in
        let cd = myass o ce in
        po ^ " [label = " ^ ec ^ o ^enl ^ i ^ ec ^ nl ^ "shape = "^ ec ^"record" ^ ec ^ "]" ^ nl ^
        nl ^ gv_of_cd (po) cd
    (* Reset *)
    | CResetDecl (sl) -> 
        let po = path ^ "_" ^ "Reset" in
        po ^ " [label = "^ ec ^"Reset" ^ nl ^ ec ^ "shape = "^ ec ^"record" ^ ec ^ "]" ^ nl ^
        path ^ " -> " ^ po ^ nl
		(* Idle *)
    | CIdleDecl (sl)  -> 
        let p = path ^ "_" ^ "Idle" in
        p ^ " [label = "^ ec ^"Idle" ^ nl ^ ec ^ "shape = "^ ec ^"record" ^ ec ^ "]" ^ nl ^
        path ^ " -> " ^ p ^ nl 

    (*Task level, prints an oval with Task' as title and its name*)
    | CTaskDecl (i, al, sl ) -> 
        let po = path ^ "_" ^ i in
        po ^ " [label = "^ ec ^"Task" ^enl ^ i ^ nl ^ ec ^ "]" ^ nl ^
        path ^ " -> " ^ po ^ nl 

    (*Task level, prints an oval with 'Function' as title and its name*)
    | CMDecl (t, i, al, sl) -> 
        let po = path ^ "_" ^ i in
        po ^ " [label = "^ ec ^"Function" ^enl ^ i ^ nl ^ ec ^ "]" ^ nl ^
        path ^ " -> " ^ po ^ nl 
    | _ -> "" (*raise UnMatched*)

  (*This appends the list of objects, functions and tasks...*)
  and gv_of_cd path = function
  	| ClassDef (i, cal, cdl) -> String.concat nl (List.map (gv_of_cdl path) cdl)(*String.concat nl (List.map (gv_of_cdl path) cdl)*)
  in
  
  (*let se = cEnv_of_classDef p in*)
  let cd = 
  	try
  		List.assoc "Root" ce
  	with
  	| _ -> raise (RtfmError ("Root not defined"))

  in
  "digraph RTFM {" ^ nl
  ^ "Root_class[shape=diamond]" ^ nl
  ^ gv_of_cd "Root_class" cd
  ^ "}"

let def_of_obj p =
  let ce = cEnv_of_classDef p in
  gv_of_obj ce p

(*--------------------------------------------------------------*)
(*Deffinition of Graphviz output for resource option*)
let gv_of_res =
	"digraph RTFM {" ^ nl ^ "gv_of_res if working [shape=diamond] "
  ^ "}" 



(*--------------------------------------------------------------*)
(*Deffinition of Graphviz output for task option*)
let gv_of_task ce p =
	let rec gv_of_tdl path = function
		(*Object level, prints a rectangle with the class- and classinstance-names*)
		| COVar (o, el, i) ->
        let po = path ^ "_" ^ i in
        let cd = myass o ce in
        po ^ " [label = " ^ ec ^ o ^ " ( " ^  String.concat nl (List.map string_of_expr el) ^ " ) " ^enl ^ i ^ enl  ^ ec ^ nl ^ "shape = "^ ec ^"record" ^ ec ^ "]" ^ nl ^
				nl ^ gv_of_td (po) cd

		(*Task level, prints an oval with Task' as title and its name*)
		| CTaskDecl (i, al, sl ) -> 
  	  let po = path ^ "_" ^ i in
  	  po ^ " [label = "^ ec ^"Task" ^enl ^ i ^ nl ^ String.concat "" (List.map (string_of_stmt (tab^tab)) sl) ^ ec ^ "]" ^ nl ^ 
  	  path ^ " -> " ^ po ^ nl 

  	(*Task level, prints an oval with 'Function' as title and its name*)
    | CMDecl (t, i, al, sl) -> 
        let po = path ^ "_" ^ i in
        po ^ " [label = "^ ec ^"Function" ^enl ^ i ^ nl ^ String.concat "" (List.map (string_of_stmt (tab^tab)) sl) ^ ec ^ "]" ^ nl ^
        path ^ " -> " ^ po ^ nl 
     
    (* Reset *)    
    | CResetDecl (sl) -> 
        let po = path ^ "_" ^ "Reset" in
        po ^ " [label = "^ ec ^"Reset" ^ nl ^ String.concat "" (List.map (string_of_stmt (tab^tab)) sl) ^ ec ^ "shape = "^ ec ^"record" ^ ec ^ "]" ^ nl ^
        path ^ " -> " ^ po ^ nl

    (* Idle *)
    | CIdleDecl (sl)  -> 
        let p = path ^ "_" ^ "Idle" in
        p ^ " [label = "^ ec ^"Idle" ^ nl ^ String.concat "" (List.map (string_of_stmt (tab^tab)) sl) ^ ec ^ "shape = "^ ec ^"record" ^ ec ^ "]" ^ nl ^
        path ^ " -> " ^ p ^ nl 

    (* ?!?? Not wokring atm *)
    | CIsrDecl (pr, i, sl)   ->
    		let po = path ^ "_" ^ string_of_int pr in
        po ^ " [label = "^ ec ^"ISR" ^enl ^ i ^ nl ^ string_of_int pr ^ ec   ^ "]" ^ nl ^ String.concat "" (List.map (string_of_stmt (tab^tab)) sl)
        ^ path ^ " -> " ^ po ^ nl 

    | _ -> "" (*raise UnMatched*)
  
  and gv_of_td path = function
  	| ClassDef (i, cal, cdl) -> String.concat nl (List.map (gv_of_tdl path) cdl)(*String.concat nl (List.map (gv_of_cdl path) cdl)*)
  in

  let cd = 
  	try
  		List.assoc "Root" ce
  	with
  	| _ -> raise (RtfmError ("Root not defined"))

  in
  "digraph RTFM {" ^ nl
  ^ "Root [shape=diamond]" ^ nl
  ^ gv_of_td "Root" cd
  ^ "}"

let def_of_task p =
  let ce = cEnv_of_classDef p in
  gv_of_task ce p

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
   (*let se = cEnv_of_classDef p in*)
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