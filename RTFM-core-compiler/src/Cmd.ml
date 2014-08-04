(* RTFM-core/Cmd *)
open Common
open Options
  
let usage = 
  "The RTFM-core compiler v1.0, Per Lindgren (c) 2014" ^ nl ^ nl
    ^ "Usage core [-gcc|-ccomp|-clang] [-rt|-km3] [-v] [-D] [-gv_task file.gv] [-gv_res file.gv] [-d_ast] [-o outfile.c] [-i] infile.core" ^ nl
    ^ "Recognized source file:" ^ nl
    ^ ".core            RTFM-core source infile (root)" ^ nl
    ^ nl
    ^ "Output C file:" ^ nl
    ^ "-o outfile.c     Compiler output (defaults to input.c)" ^ nl
    ^ nl 
    ^ "Backend options:" ^ nl
    ^ "-gcc             Generate code for GCC backend (default)" ^ nl
    ^ "-ccomp           Generate code for CCOMP backend" ^ nl
    ^ "-clang           Generate code for CLANG backend" ^ nl
    ^ nl 
    ^ "Target options:" ^ nl
    ^ "-rt              Target RTFM Run-Time (OSX/Linux/Windows) (default)" ^ nl
    ^ "-km3             Target RTFM Kernel ARM Cortex M3" ^ nl
    ^ nl 
    ^ "General options:" ^ nl
    ^ "-v               Enable : Verbose compiltion (default disbaled)" ^ nl
    ^ "-D               Enable : Generate output for debugging (default disbaled)" ^ nl
    ^ nl 
    ^ "Additional options:" ^ nl
    ^ "-gv_task file.gv Enable : Task and resource structure in gv format (default disabled)" ^ nl
    ^ "-gv_res  file.gv Enable : Resource dependency graph (lock structure) in gv format (default disabled)" ^ nl
    ^ "-d_ast           Enable : Dump internal AST (default disabled)" ^ nl
    ^ nl
    ^ "All file paths are relative to the current directory" ^ nl 
    ^ "For further documentation see www.rtfm-lang.org" ^ nl
    ^ nl
    ^ "Options summary:"
    
let o_gcc		    = ref false
let o_ccomp		  = ref false
let o_clang		  = ref false
  
let o_km3		    = ref false
let o_ptl		    = ref false  
  
let o_verbose   = ref false
let o_debug		  = ref false
  
let f_infile	  = ref ""
let f_outfile 	= ref ""
let f_dotfile 	= ref ""
let f_ldotfile 	= ref ""
    
let d_ast		= ref false  
  
let speclist = 
  [
    ("-i", Arg.Set_string f_infile,   "\t\t: infile");
    ("-o", Arg.Set_string f_outfile,  "\t\t: outfile (default infile.c)");
    ("-gv_task", Arg.Set_string f_dotfile,  "\t\t: graphviz file (default none)");
    ("-gv_res", Arg.Set_string f_ldotfile,"\t: graphviz file (default none)");
    
    ("-gcc", Arg.Set o_gcc, 		  "\t\t: for gcc (default)");
    ("-ccomp", Arg.Set o_ccomp, 		"\t: for ccomp");
    ("-clang", Arg.Set o_clang, 		"\t: for clang");
    
    ("-rt", Arg.Set o_ptl,  		  "\t\t: for Pthread Linux (default)");
    ("-km3", Arg.Set o_km3, 		  "\t\t: for ARM Cortex M3");
    
    ("-v", Arg.Set o_verbose, 		  "\t\t: verbose mode");
    ("-D", Arg.Set o_debug, 		  "\t\t: debug mode");
  
    ("-d_ast", Arg.Set d_ast, 		    "\t: dump AST");   
  ]
    
(* check if e is a file extension of s *)    
let ext s e =
  let le = String.length e in
  let ls = String.length s in    
  try
    compare (String.sub s (ls - le) le) e == 0  
  with
    _ -> false  
      
(* replace file extension e with r*)
let rep_ext s e r =
  let le = String.length e in
  let ls = String.length s in 
  String.sub s 0 (ls - le) ^ r
    
let cmd =
  let n o = if !o then 1 else 0 in (* count option *)
  (* Read the arguments *) 
  Arg.parse speclist (fun x -> f_infile := x) usage;
  try
  let check_ext name ex err = 
    if (not (ext name ex)) then raise (Arg.Bad(err ^ " " ^ name));
  in                                       
    (* infile *)
    if (String.compare !f_infile "" == 0) then raise (Arg.Bad("No infile selected"));
    check_ext !f_infile ".core" "Bad infile extention (.core exptected)";
    opt.infile <- !f_infile;
    
    (* outfile *)
    opt.outfile <- if (String.compare (!f_outfile) "" == 0) then rep_ext (!f_infile) ".core" ".c" else !f_outfile;
    if (not (ext opt.outfile ".c")) then raise (Arg.Bad("Bad outfile extention (.c exptected) " ^ opt.outfile));
    p_stderr ("outfile :" ^ opt.outfile);
    
    (* backend *)
    if (n o_gcc) + (n o_ccomp) + (n o_clang) > 1 then raise (Arg.Bad("Conflicting backend compiler options"));
    opt.backend <- if (!o_ccomp) then CCOMP else if (!o_clang) then CLANG else GCC; 
    
    (* target *)
    if (n o_ptl) + (n o_km3)  > 1 then raise (Arg.Bad("Conflicting target options"));
    opt.target <- if (!o_km3) then RTFM_KERNEL else RTFM_PT;
    
    (* general options *)
    opt.debug 	<- ! o_debug;
    opt.verbose <- ! o_verbose;
    
    (* additional options *)
    opt.dotout <- (not (String.compare (!f_dotfile) "" == 0));
    
    let gv_ext = ".gv" in
    let gv_ext_err = "Bad Graphviz extension (.gv expected) " in
    if opt.dotout then begin
      opt.dotfile <- !f_dotfile;
      check_ext opt.dotfile gv_ext gv_ext_err
    end;
    opt.ldotout <- (not (String.compare (!f_ldotfile) "" == 0));
    if opt.ldotout then begin
      opt.ldotfile <- !f_ldotfile;
      check_ext opt.ldotfile gv_ext gv_ext_err
    end;
    opt.d_ast <- !d_ast;  
  with
    | Arg.Bad msg -> p_stderr ("Command line error: " ^ msg); exit (-1); 