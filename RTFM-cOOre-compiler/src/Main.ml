(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-cOOre/Main *)

open Common
open Options
open AST
open Env
open CoreGen
open Dot
open Grammar

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
    
    match res with
    | None   -> p_stderr ("Not accepted!" ^ nl); exit (-1);
    | Some p ->
        if opt.verbose then p_stderr ("Parsing succeeded:" ^ nl);
        if opt.d_ast then p_stderr (string_of_prog p);
        if opt.typecheck then p_stderr (typecheck_prog p);
        
        (* check cyclic *)
        cyclic p;
        
        (* dot for task/resource structure *)
        if opt.dotout then begin
          let dots = (d_of_p p) in
          if opt.verbose then p_stderr dots;
          let ocd = open_out opt.dotfile in 
          begin
            p_oc ocd dots;
            close_out ocd;
          end;  
        end;
        let oc = open_out opt.outfile in
        p_oc oc (c_of_Prog p);
  
  (* exception handling *)
  with
  | Lexer.SyntaxError msg -> p_stderr (msg ^ parse_err_msg lexbuf);
  | RtfmError msg         -> p_stderr msg;
  | Failure msg           -> p_stderr msg;
  | Parser.Error          -> p_stderr ("Parser error." ^ parse_err_msg lexbuf);
  | TypeError msg         -> p_stderr msg;  
  | NameError msg         -> p_stderr msg;
      
      exit (-1);;
(* exit 0;; *)

main ();;