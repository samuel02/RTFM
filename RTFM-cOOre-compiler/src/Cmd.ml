(* RTFM-cOOre/Cmd *)

open Common
open Options

let usage =
  "The RTFM-coore compiler v1.0, Per Lindgren (c) 2014" ^ nl ^ nl ^
  "Usage rtfm-coore [-v] [-D] [-d_ast] [-o outfile.core] [-i] infile.coore" ^ nl ^
  "Recognized source file:" ^ nl ^
  ".coore           RTFM-core source infile (root)" ^ nl ^
  nl ^
  "Output C file:" ^ nl ^
  "-o outfile.core  Compiler output (defaults to <infile>.core)" ^ nl ^
  nl ^
  "General options:" ^ nl ^
  "-v               Enable : Verbose compiltion (default disbaled)" ^ nl ^
  "-D               Enable : Generate output for debugging (default disbaled)" ^ nl ^
  nl ^
  "Additional options:" ^ nl ^
  "-gv_obj file.gv  Enable : Object structure in gv format (default disabled)" ^ nl ^
  "-d_ast           Enable : Dump internal AST (default disabled)" ^ nl ^
  nl ^
  "All file paths are relative to the current directory" ^ nl ^
  "For further documentation see www.rtfm-lang.org" ^ nl ^
  nl ^
  "Options summary:"

let o_gcc = ref false
let o_ccomp = ref false
let o_clang = ref false

let o_km3 = ref false
let o_ptl = ref false

let o_verbose = ref false
let o_debug = ref false

let f_infile = ref ""
let f_outfile = ref ""
let f_dotfile = ref ""
let f_ldotfile = ref ""

let d_ast = ref false

let speclist =
  [
  ("-i", Arg.Set_string f_infile, "\t\t: infile.coore");
  ("-o", Arg.Set_string f_outfile, "\t\t: outfile (default <infile>.core)");
  ("-gv_obj", Arg.Set_string f_dotfile, "\t: graphviz file (default none)");
  
  ("-v", Arg.Set o_verbose, "\t\t: verbose mode");
  ("-D", Arg.Set o_debug, "\t\t: debug mode");
  
  ("-d_ast", Arg.Set d_ast, "\t: dump AST");
  ]

(* check if e is a file extension of s *)
let ext s e =
  let le = String.length e in
  let ls = String.length s in
  try
    compare (String.sub s (ls - le) le) e == 0
  with
    _ -> false

(* replace file extension e with r *)
let rep_ext s e r =
  let le = String.length e in
  let ls = String.length s in
  String.sub s 0 (ls - le) ^ r

let cmd =
  (* Read the arguments *)
  Arg.parse speclist (fun x -> f_infile := x) usage;
  try
    let check_ext name ex err =
      if (not (ext name ex)) then raise (Arg.Bad(err ^ " " ^ name));
    in
    (* infile *)
    if (String.compare !f_infile "" == 0) then raise (Arg.Bad("No infile selected"));
    check_ext !f_infile ".coore" "Bad infile extention (.coore exptected)";
    opt.infile <- !f_infile;
    
    (* outfile *)
    opt.outfile <- if (String.compare (!f_outfile) "" == 0) then rep_ext (!f_infile) ".coore" ".core" else !f_outfile;
    if (not (ext opt.outfile ".core")) then raise (Arg.Bad("Bad outfile extention (.core exptected) " ^ opt.outfile));
    p_stderr ("outfile :" ^ opt.outfile);
    
    (* general options *)
    opt.debug <- ! o_debug;
    opt.verbose <- ! o_verbose;
    
    (* additional options *)
    opt.dotout <- (not (String.compare (!f_dotfile) "" == 0));
    
    let gv_ext = ".gv" in
    let gv_ext_err = "Bad Graphviz extension (.gv expected) " in
    if opt.dotout then begin
      opt.dotfile <- !f_dotfile;
      check_ext opt.dotfile gv_ext gv_ext_err
    end;
    opt.d_ast <- !d_ast;
  with
  | Arg.Bad msg -> p_stderr ("Command line error: " ^ msg); exit (-1);