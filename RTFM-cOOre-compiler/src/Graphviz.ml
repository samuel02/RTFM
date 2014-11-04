(* Graphviz output generation *)
(* Supports Object, Task and Resource level of detail *)
(* gv_of_obj defines the behaviour, def_o_obj is called from main.ml to
		generate the correct Graphviz output *)

open Common
open AST
open Env


(*Deffinition of Graphviz output for resource option*)
let gv_of_inst ce p =
	(*Define the recursive loop to 'catch' all objects and their respective tasks/functions *)
	let rec gv_of_cdl path = function
		(*Object level, prints a rectangle with the class- and classinstance-names*)
		| COVar (o, el, i) ->
        let po = path ^ "_" ^ i in
        let cd = myass o ce in
        po ^ " [label = " ^ ec ^ o ^enl ^ i ^ ec ^ nl ^ "shape = tripleoctagon, style = filled, fillcolor = deepskyblue2]" ^ nl ^
        path ^ " -> " ^ po ^ nl ^
        gv_of_cd (po) cd
    (* Reset *)
    | CResetDecl (sl) -> 
        let po = path ^ "_" ^ "Reset" in
        po ^ " [label = "^ ec ^"Reset" ^ nl ^ ec ^ "shape = tab, style = filled, fillcolor = yellow]" ^ nl ^
        path ^ " -> " ^ po ^ nl
		(* Idle *)
    | CIdleDecl (sl)  -> 
        let p = path ^ "_" ^ "Idle" in
        p ^ " [label = "^ ec ^"Idle" ^ nl ^ ec ^ "shape = tab, style = filled, fillcolor = yellow]" ^ nl ^
        path ^ " -> " ^ p ^ nl 

    (*Task level, prints an oval with Task' as title and its name*)
    | CTaskDecl (i, al, sl ) -> 
        let po = path ^ "_" ^ i in
        po ^ " [label = "^ ec ^"Task" ^enl ^ i ^ nl ^ ec ^ "shape = folder, style = filled, fillcolor = orange]" ^ nl ^
        path ^ " -> " ^ po ^ nl 

    (*Task level, prints an oval with 'Function' as title and its name*)
    | CMDecl (t, i, al, sl) -> 
        let po = path ^ "_" ^ i in
        po ^ " [label = "^ ec ^"Function" ^enl ^ i ^ nl ^ ec ^ "shape = folder, style = filled, fillcolor = firebrick2]" ^ nl ^
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
  ^ "Root[shape=diamond,style = filled, fillcolor = darkorchid1]" ^ nl
  ^ gv_of_cd "Root" cd
  ^ "}"

let def_of_inst p =
  let ce = cEnv_of_classDef p in
  gv_of_inst ce p

(*--------------------------------------------------------------*)
(*Deffinition of Graphviz output for Class (object) option*)
let gv_of_obj ce p=
  let rec gv_of_odl path = function
    (*Object level, prints a rectanglle with the class- and classinstance-names*)
    | COVar (o, el, i) ->
        let po = path ^ "_"  in
        let cd = myass o ce in
        po ^ " [label = " ^ ec ^ o ^ ec ^ nl ^ "shape = tripleoctagon, style = filled, fillcolor = deepskyblue2]" ^ nl ^
        path ^ " -> " ^ po ^ nl (*^
        gv_of_od (po) cd*)
    (* Reset *)
    | CResetDecl (sl) -> 
        let po = path ^ "_" ^ "Reset" in
        po ^ " [label = "^ ec ^"Reset" ^ ec ^ nl ^  "shape = tab, style = filled, fillcolor = yellow]" ^ nl ^
        path ^ " -> " ^ po ^ nl
    (* Idle *)
    | CIdleDecl (sl)  -> 
        let p = path ^ "_" ^ "Idle" in
        p ^ " [label = "^ ec ^"Idle" ^ ec ^ nl  ^ "shape = tab, style = filled, fillcolor = yellow]" ^ nl ^
        path ^ " -> " ^ p ^ nl 

    (*Task level, prints an oval with Task' as title and its name*)
    | CTaskDecl (i, al, sl ) -> 
        let po = path ^ "_" ^ i in
        po ^ " [label = "^ ec ^"Task" ^enl ^ i ^ ec ^ nl ^ "shape = folder, style = filled, fillcolor = orange]" ^ nl
        ^ path ^ " -> " ^ po ^ nl 

    (*Task level, prints an oval with 'Function' as title and its name*)
    | CMDecl (t, i, al, sl) -> 
        let po = path ^ "_" ^ i in
        po ^ " [label = "^ ec ^"Function" ^enl ^ i ^ ec ^ nl ^ "shape = folder, style = filled, fillcolor = firebrick2]" ^ nl ^
        path ^ " -> " ^ po ^ nl 

    | _ -> "" (*raise UnMatched*)

  (*This appends the list of objects, functions and tasks...*)
  and gv_of_od path = function
    | ClassDef (i, cal, cdl) -> String.concat nl (List.map (gv_of_odl path) cdl)(*String.concat nl (List.map (gv_of_cdl path) cdl)*)
  in
  
  (*let se = cEnv_of_classDef p in*)
  let cd = 
    try
      List.assoc "Root" ce
    with
    | _ -> raise (RtfmError ("Root not defined"))

  in
  "digraph RTFM {" ^ nl
  ^ "Root[shape=diamond,style = filled, fillcolor = darkorchid1]" ^ nl
  ^ gv_of_od "Root" cd
  ^ "}"


let def_of_obj p =
  let ce = cEnv_of_classDef p in
  gv_of_obj ce p
(*--------------------------------------------------------------*)

(*
let gv_of_spec dlp rml spec = 
  let rec stmts t tp sl = mymap (stmt t tp) sl
  and stmt t tp s = 
    lable := !lable + 1;
    if opt.debug then p_stderr ("--- generating unique label " ^ string_of_int !label ^ " ----"^ nl );
    let id = !lable in
    match s with
      | CMDecl (t, i, al, sl)  -> "Test method"
      | CTaskDecl (i, al, sl ) -> "Test Task"
*)

(*--------------------------------------------------------------*)
(*Deffinition of Graphviz output for task option*)
let gv_of_task ce p =
  let gv_of_expr path p = function
    | AsyncExp (af, be, il, el) -> p ^ "->" ^ path ^ "_" ^ (String.concat "_" il) ^ "[dir=none, style=dotted]"
    | _ -> ""
  in
  let gv_of_s path p = function
    | ExpStmt(e) -> (gv_of_expr path p) e
    | _          -> ""
  in
	let rec gv_of_tdl path = function
		(*Object level, prints a rectangle with the class- and classinstance-names*)
		| COVar (o, el, i) ->
        let po = path ^ "_" ^ i in
        let cd = myass o ce in
        po ^ " [label = " ^ ec ^ o ^enl ^ i ^ enl ^  ec ^ nl ^ "shape = tripleoctagon, style = filled, fillcolor = deepskyblue2]" ^ nl ^
				path ^ " -> " ^ po ^ nl ^
        gv_of_td (po) cd

		(*Task level, prints an oval with Task' as title and its name*)
		| CTaskDecl (i, al, sl ) -> 
  	  let po = path ^ "_" ^ i in
  	  po ^ " [label = "^ ec ^"Task" ^enl ^ i ^ nl ^  ec ^ "shape = folder, style = filled, fillcolor = orange]" ^ nl ^ 
  	  path ^ " -> " ^ po ^ nl ^ (String.concat "" (List.map (gv_of_s path po) sl))

  	(*Task level, prints an oval with 'Function' as title and its name*)
    | CMDecl (t, i, al, sl) -> 
        let po = path ^ "_" ^ i in
        po ^ " [label = "^ ec ^"Function" ^ enl ^ i ^ nl ^ ec ^ "shape = folder, style = filled, fillcolor = firebrick2]" ^ nl ^
        path ^ " -> " ^ po ^ nl ^ (String.concat "" (List.map (gv_of_s path po) sl))
     
    (* Reset *)    
    | CResetDecl (sl) -> 
        let po = path ^ "_" ^ "Reset" in
        po ^ " [label = "^ ec ^"Reset" ^ nl ^  ec ^ "shape = tab, style = filled, fillcolor = yellow]" ^ nl ^
        path ^ " -> " ^ po ^ nl ^ (String.concat "" (List.map (gv_of_s path po) sl))

    (* Idle *)
    | CIdleDecl (sl)  -> 
        let p = path ^ "_" ^ "Idle" in
        p ^ " [label = "^ ec ^"Idle" ^ nl ^  ec ^ "shape = tab, style = filled, fillcolor = yellow]" ^ nl ^
        path ^ " -> " ^ p ^ nl ^ (String.concat "" (List.map (gv_of_s path p) sl))

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
  ^ "Root[shape=diamond,style = filled, fillcolor = darkorchid1]" ^ nl
  ^ gv_of_td "Root" cd
  ^ "}"

let def_of_task p =
  let ce = cEnv_of_classDef p in
  gv_of_task ce p

(*	"digraph RTFM {" ^ nl ^ "gv_of_task_is_working"^ " [shape=diamond] "
  ^ "}" *)
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

(*
  (*Deffinition of Graphviz output for task option*)
let gv_of_task ce p =
  let rec gv_of_tdl path = function
    (*Object level, prints a rectangle with the class- and classinstance-names*)
    | COVar (o, el, i) ->
        let po = path ^ "_" ^ i in
        let cd = myass o ce in
        po ^ " [label = " ^ ec ^ o ^enl ^ i ^ enl ^ "Input argument = " ^String.concat nl (List.map string_of_expr el) ^ ec ^ nl ^ "shape = "^ ec ^"record" ^ ec ^ "]" ^ nl ^
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
  *)
