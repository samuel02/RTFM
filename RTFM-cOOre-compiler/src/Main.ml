(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-cOOre/Main *)


open Common
open Options
open AST
open Env
open CoreGen
open Graphviz
open VariableTypeCheck
open TypeTree

open Error
open Cmd
open Lexing

let main () =
  cmd; (* parse command line options and put into opt *)
  p_stderr (string_of_opt opt);
  
  let inBuffer = open_in opt.infile in
  let lexbuf = Lexing.from_channel inBuffer in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = opt.infile };
  
  try
    let res = Parser.prog Lexer.lex lexbuf in
    let scope_tree p = build_scope_tree p in
    match res with
    | None   -> p_stderr ("Not accepted!" ^ nl); exit (-1);
    | Some p ->
        if opt.verbose then p_stderr ("Parsing succeeded:" ^ nl);
        if opt.d_ast then p_stderr (string_of_prog p);
        p_stderr (string_of_class (scope_tree p));
        if opt.typecheck then p_stderr (typecheck_prog opt.typecheck_object_arguments (scope_tree p) p);
        
        (* check cyclic *)
        cyclic p;
        

        (* gv_obj optout *)
        if (opt.gv_obj) then begin
          let oc = open_out opt.gv_objf in
          p_oc oc (def_of_obj p);
          close_out oc;
        end;
        (* gv_task optout *)
        if (opt.gv_task) then begin
          let oc = open_out opt.gv_taskf in
          p_oc oc (def_of_task p);
          close_out oc;
        end;
        (* gv_res optout *)
        if (opt.gv_inst) then begin
          let oc = open_out opt.gv_instf in
          p_oc oc (def_of_inst p);
          close_out oc;
        end;

        let oc = open_out opt.outfile in
        p_oc oc (c_of_Prog p);
  
  (* exception handling *)
  with
  | Lexer.SyntaxError msg -> p_stderr (msg ^ parse_err_msg lexbuf); exit(-1)
  | RtfmError msg         -> p_stderr msg; exit(-1)
  | Failure msg           -> p_stderr msg; exit(-1)
  | Parser.Error          -> p_stderr ("Parser error." ^ parse_err_msg lexbuf); exit(-1)
  | TypeError msg         -> p_stderr msg; exit(-1)
  | NameError msg         -> p_stderr msg; exit(-1)
;;

(* exit 0;; *)

main ();;
