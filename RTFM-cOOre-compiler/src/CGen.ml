(* CGen *)
open Common
open AST
open Env

(* --- *)
let rec c_defs_of_classDef ce path argl cd =
	let p = path ^ "_" in
	
	let rec c_of_expr = function
		| IdExp (id)      -> p ^ id
		| CallExp (m, el) -> p ^ String.concat "_" m ^ string_par c_of_expr el
		| PendExp (il)    -> "pend " ^ String.concat "_" il
		| IntExp (i)      -> string_of_int i
		| CharExp (c)     -> ecit ^ String.make 1 c ^ ecit
		| BoolExp (b)     -> string_of_bool b
	in
	
	let c_t_of_mPArg = function
		| MPArg (t, _) -> string_of_pType t
	in
	
	let c_of_mPArg = function
		| MPArg (t, i) -> string_of_pType t ^ " " ^ p ^ i
	in
	
	let c_of_stmt = function
		| MPVar (t, i, e) -> tab ^ string_of_pType t ^ " " ^ p ^ i ^ " = " ^ c_of_expr e
		| Assign (i, e)   -> tab ^ p ^ i ^ " = " ^ c_of_expr e
		| Return (e)      -> tab ^ "return " ^ c_of_expr e
	in
	 
	(* method prototypes *)
	let c_mp_of_classDecl =
		function
		| CMDecl (t, i, al, _) -> string_of_pType t ^ " " ^ p ^ i ^ string_par c_t_of_mPArg al
		| CTDecl (i, pr, sl) 	 -> "// task " ^ p ^ i ^ "()"
| _                        -> raise (UnMatched)
	in
	
	let c_of_classArg cal arg = match cal with
		| CPArg (t, i)      -> "const " ^ string_of_pType t ^ " " ^ p ^ i ^ " = " ^ arg
		| CMArg (t, tl, i ) -> string_of_pType t ^ " (* const " ^ p ^ i ^ ")" ^ string_par string_of_pType tl ^ " = " ^ arg
	in
	
	(* state initialization *)
	let c_state_of_classDecl = function
		| CPVar (t, i, e) -> string_of_pType t ^ " " ^ p ^ i ^ " = " ^ c_of_expr e
		| _               -> raise (UnMatched)
	in
	
	(* method declarations *)
	let c_md_of_classDecl = function
		| CMDecl (t, i, al, sl) ->
				string_of_pType t ^ " " ^ p ^ i ^ string_par c_of_mPArg al ^ "{" ^ nl
				^ myconcat (";" ^ nl) (List.map c_of_stmt sl)
				^ "}"
		| CTDecl (i, pr, sl) -> "Task " ^ p ^ i ^ " " ^ string_of_int pr ^ " () {" ^ nl
		    ^ myconcat (";" ^ nl) (List.map c_of_stmt sl) 
        ^ "}"

		| _ -> raise (UnMatched)
	in
	
	(* span tree recusively *)
	let c_ioi_of_classDecl = function
		| COVar (o, al, i) ->
				let cd = myass o ce in
				c_defs_of_classDef ce (p ^ i) (List.map c_of_expr al) cd
		| _ -> raise (UnMatched)
	in
	
	match cd with
	| ClassDef (i, cal, cdl) ->
			"// generating code for " ^ i ^ ":" ^ path ^ nl
			^ "// method prototypes for " ^ i ^ ":" ^ path ^ nl
			^ myconcat (";" ^ nl) (mymap c_mp_of_classDecl cdl) ^ nl
			^ "// class instance parameters for " ^ i ^ ":" ^ path ^ nl
			^ myconcat (";" ^ nl) (List.map2 c_of_classArg cal argl) ^ nl
			^ "// class instance variables for " ^ i ^ ":" ^ path ^ nl
			^ myconcat (";" ^ nl) (mymap c_state_of_classDecl cdl) ^ nl
			(* span each object instance recursively *)
			^ myconcat nl (mymap c_ioi_of_classDecl cdl)
			^ "// methods declarations for " ^ i ^ ":" ^ path ^ nl
			^ myconcat nl (mymap c_md_of_classDecl cdl) ^ nl

let c_of_Prog p =
	let ce = cEnv_of_classDef p in
	let cd =
		try
			List.assoc "Main" ce
		with
		| _ -> raise (RtfmError ("main not defined"));
	in
	match p with
	| Prog cl ->
			"// RTFM-cOOre, Per Lindgren (C) 2014" ^ nl
			^ "#include <stdbool.h>" ^ nl
			^ c_defs_of_classDef ce "Main" [] cd ^ nl  (* no args ( [] ) at top level *)
