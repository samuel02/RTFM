(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/Main *)

open Common
open Options
open AST
open SRP
open IsrVector
open IsrCGen
open CGenPT 
open CGenK
open Dot
open Locks
open Error
open Cmd
open Lexing

let rec parse_prog path inf =
  let addpath name =
    try
    let lastSlash = String.rindex name '/'  in
    String.sub name 0 (lastSlash + 1)
    with _ -> ""
  in  
  let infile = path ^ inf in
  let npath = path ^ addpath inf in
  p_stderr ("Processing : " ^ infile ^ nl);
  let inBuff =
    try Some (open_in infile)
    with _ -> None
  in
  match inBuff with
  | None -> p_stderr ("File open error :" ^ infile); raise (RtfmError("Exit"))
  | Some inBuffer ->
      let lexbuf = Lexing.from_channel inBuffer in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = infile };
      
      try
        let res = Parser.prog Lexer.lex lexbuf in
        match res with
        | None -> p_stderr ("Not accepted!" ^ nl); raise (RtfmError("Exit"))
        | Some (p) -> match p with
            | Prog (mName, mIncl, mTops) ->
                if opt.verbose then p_stderr ("Parsing of " ^ infile ^ " succeeded:" ^ nl);
                if opt.d_ast then p_stderr (string_of_prog p);
                mTops @ List.concat (List.map (parse_prog npath) mIncl)
      (* exception handling *)
      with
      | Lexer.SyntaxError msg -> p_stderr ("Parser error." ^ msg ^ parse_err_msg lexbuf); raise (RtfmError("Exit"))
      | Parser.Error -> p_stderr ("Parser error." ^ parse_err_msg lexbuf); raise (RtfmError("Exit"))

let main () =
  cmd; (* parse command line options and put into opt *)
  p_stderr (string_of_opt opt);
  
  let oc = open_out opt.outfile in
  try
    let mTops = parse_prog "" opt.infile in
    
    if opt.d_ast then p_stderr ("Input AST:" ^ nl ^ string_of_tops mTops);
    
    let pnt = TaskGen.tasks_of_p mTops in
    
    p_stderr ("Tasks created:" ^ nl ^ string_of_tops pnt);
    let meTops = pnt @ mTops in
           
    let rm = ceiling meTops in
    if opt.verbose then begin
      p_stderr ("Resource ceilings:" ^ nl ^ string_of_r rm );
      p_stderr ("Tasks/ISRs per priority: " ^ nl^ string_of (pl mTops rm) );
    end;
    
    let tasks = task_vector meTops in
    p_stderr ("Tasks : " ^ String.concat ", " (List.map snd tasks) ^ nl );
    
    (* dot for task/resource structure *)
    if opt.dotout then begin
      let dots = (d_of_p meTops rm) in
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
          let nv = assign_vectors isr_vector meTops in
          let tidm = assign_tasks isr_vector meTops in
          if opt.verbose then begin
            p_stderr (nl ^ "Original Vector table " ^ nl ^ isrv_to_c isr_vector);
            p_stderr (nl ^ "After assignments Vector table " ^ nl ^ isrv_to_c nv);
            let pair (a, b) = a ^ "->" ^ b in
            p_stderr (nl ^ "Task to isr_entries" ^ nl ^ String.concat ", " (List.map pair tidm));
          end;
          match wf_of_v nv with
          | false -> p_stderr (nl ^ "Error in Vector table!" ^ nl);
          | true ->
          (* generate c code *)
              p_oc oc (ck_of_p meTops tasks rm tidm);
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
        p_oc oc (crt_of_p meTops tasks rm);
        (* comupte cyclic dependencies *)
        let dep = dep_of_p meTops in
        let e = entry meTops in
        if (opt.verbose) then begin
          p_stderr (string_of_dep dep);
          p_stderr (string_of_entry e);
        end;
        
        (* ldot optout *)
        if (opt.ldotout) then begin
          let ocl = open_out opt.ldotfile in
          begin
            p_oc ocl (dot_of_dep dep meTops);
            close_out ocl;
          end;
        end;
        let ts = (tsort dep e) in
        match ts with
        | Some top -> p_stderr (
                "Deadlock free execution can be guaranteed " ^ nl ^
                "Topological order obtained: " ^ (String.concat ", " top) ^ nl
              )
        | None -> p_stderr "Exiting";  
          
  (* exception handling *)
  with
  | RtfmError msg -> p_stderr msg;
  | Failure msg -> p_stderr msg;
      exit (-1);;
(* exit 0;; *)

main ();;
