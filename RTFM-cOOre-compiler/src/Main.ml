open Common
open AST
open Env
open CGen

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
				let oc = open_out opt.outfile in
        p_oc oc (c_of_Prog p);
        
  (* exception handling *)            
  with 
    | Lexer.SyntaxError msg -> p_stderr (msg ^ parse_err_msg lexbuf);
    | RtfmError msg 		-> p_stderr msg;
    | Failure msg 			-> p_stderr msg; 
    | Parser.Error 			-> p_stderr ("Parser error." ^ parse_err_msg lexbuf);
    
      exit (-1);;    
(* exit 0;; *)

main ();;