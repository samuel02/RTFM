open Common
open AST
open SRP
open IsrVector
open IsrCGen
open CGen
open Dot
open Locks
open Error  
open Cmd
open Lexing
  
let main () =
  cmd; (* parse command line options and put into opt *)      
  p_stderr (string_of_opt opt);
  
  let inBuffer = open_in opt.infile in
  let oc = open_out opt.outfile in
  let ocd1 = open_out "out1.dot" in
  let lexbuf = Lexing.from_channel inBuffer in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = opt.infile };
  
  try
    let res = Parser.prog Lexer.lex lexbuf in
    
    match res with
      | None -> p_stderr ("Not accepted!" ^ nl); exit (-1);
      | Some (Prog p)-> 
        if opt.verbose then p_stderr ("Parsing succeeded:" ^ nl ^ nl); 
        if opt.d_ast then p_stderr (string_of_p p); 
        
        let rm = ceiling p in
        if opt.verbose then begin
          p_stderr ("Resource ceilings:" ^ nl ^ string_of_r rm );
          p_stderr ("Tasks/Isrs per priority: " ^ nl^ string_of (pl p rm) );
        end;
        
        (* dot for task/resource structure *)
        if opt.dotout then begin
          let dots = (d_of_p p rm) in
          if opt.verbose then p_stderr dots;
          let ocd = open_out opt.dotfile in 
          begin
            p_oc ocd dots;
            close_out ocd;
          end;  
        end;
        
        match opt.target with
          | RTFM_KERNEL -> begin
            (* vectors *)
            let nv = assign_vectors isr_vector p in
            if opt.verbose then begin
              p_stderr (nl ^ "Original Vector table " ^ nl ^ isrv_to_c isr_vector); 
              p_stderr (nl ^ "After assignments Vector table " ^ nl ^ isrv_to_c nv);
            end;
            match wf_of_v nv with
              | false 	-> p_stderr (nl ^ "Error in Vector table!" ^ nl);
              | true 	-> 
                (* generate c code *)
                p_oc oc (c_of_p p nv rm); 
                (* generate vector table in case of CompCert *)
                if opt.backend = CCOMP then p_oc oc (isrv_to_c_isr_nr nv); 
                
                if opt.verbose then begin
                  p_stderr ("Resource ceilings:" ^ nl ^ string_of_r rm ^ nl);
                  p_stderr (isrv_to_c_isr_nr nv); 
                  p_stderr (nl ^ "Code generation succeeded:" ^ nl); 
                end;
          end;   
            
          | RTFM_PT ->
            (* generate c code *)
            let tasks = task_vector p in
            p_oc oc (c_of_p p tasks rm); 
            
  (*
     p_oc stderr (d_of_p p);
     p_oc ocd1 (d_of_p_r p rm); 
   *)
  (*
     p_stderr (d_of_p p ); 
     p_oc ocd1 (d_of_p p); 
   *) 
              
  (*
     let dep = dep_of_p p in
     p_stderr (string_of_dep dep);
     p_oc ocd (dot_of_dep dep);
     
     let hdep = hull dep in 
     p_stderr ("hull " ^ nl ^ string_of_dep hdep);
     p_oc ocd1 (dot_of_dep hdep);
     
     let c = count dep in
     p_stderr (string_of_count c);
     
     let e = entry p in
     p_stderr (string_of_entry e);
     
     (*
     let (rem,con) = topsort e dep c in 
     p_stderr ("---- dep " ^ string_of_dep rem);
     p_stderr ("---- count " ^ string_of_count con);
     p_stderr ("---- tsort " ^ String.concat ", " (tsort dep e) ^ nl)
   *)
     p_stderr ("---- tsort " ^ String.concat ", " (tsort dep e) ^ nl); 
   *)
              
              
              
  (* exception handling *)            
  with 
    | Lexer.SyntaxError msg -> p_stderr (msg ^ parse_err_msg lexbuf);
    | RtfmError msg 		-> p_stderr msg;
    | Failure msg 			-> p_stderr msg; 
    | Parser.Error 			-> p_stderr ("Parser error." ^ parse_err_msg lexbuf);
    | Cyclic msg			-> p_stderr ("Cyclic " ^ msg);
      exit (-1);;    
(* exit 0;; *)

main ();;