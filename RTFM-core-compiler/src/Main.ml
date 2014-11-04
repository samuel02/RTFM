(* Copyright Per Lindgren 2014, see the file "LICENSE" for the full        *)
(* license governing this code.                                            *)

(* RTFM-core/Main *)

open Common
open Options
open AST
open SetInc
(* open SRP *)
open IsrVector
open IsrCGen
open ICGenRT
(* open CGenRT open CGenK open Dot *)
open Gv
open Locks
open Error
open Cmd
open Lexing

let rec parse_prog path inc =
  let addpath name =
    try
      let lastSlash = String.rindex name '/' in
      String.sub name 0 (lastSlash + 1)
    with _ -> ""
  in
  function
    | Include (fname, inc_as) ->

  let infile = path ^ fname in
  let npath = path ^ addpath fname in
  let ninc = if compare inc "" != 0 then inc ^ "_" ^ inc_as else if compare inc_as "" != 0 then inc_as ^ "_" else "" in
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
                let nmTops = setinc ninc mTops in
                (* mTops @ List.concat (List.map (parse_prog npath) mIncl) (* include after *) *)
                List.concat (List.map (parse_prog npath ninc) mIncl) @ nmTops

      (* exception handling *)
      with
      | Lexer.SyntaxError msg -> p_stderr ("Parser error." ^ msg ^ parse_err_msg lexbuf); raise (RtfmError("Exit"))
      | Parser.Error -> p_stderr ("Parser error." ^ parse_err_msg lexbuf); raise (RtfmError("Exit"))

let main () =
  let t_start = Sys.time() in
  cmd; (* parse command line options and put into opt *)
  p_stderr (string_of_opt opt);

  try
    let mTops = parse_prog "" "" (Include (opt.infile, "")) in
    let p = Prog("top", [], mTops) in
    if opt.d_ast then p_stderr (string_of_prog p);

    (* if opt.d_ast then p_stderr ("Input AST:" ^ nl ^ string_of_tops mTops); *)

    let task = timed ("generate task/func instances") TaskGenSpec.task_of_p mTops in
    if opt.d_ast then p_stderr ("Pass1 : Generation of task and function instances:" ^ nl ^ SpecAST.string_of_spec task);

    let spec = timed ("specialice task/func instances") TaskGenSpec.spec_of_p task in
    if opt.d_ast then p_stderr ("Pass2 : Specialization of task and function instances:" ^ nl ^ SpecAST.string_of_spec spec);



    let dlp  = timed ("derive priorities") SRP.dl_to_pr spec in
    if opt.verbose then
      p_stderr ("Mapping from deadlines to priorities: " ^ myconcat "," (mymap (fun (a, b) -> "(" ^ Int64.to_string a ^ ", " ^ string_of_int b ^ ")") dlp) );

    let rm = timed ("deriving ceilings") (SRP.ceiling_spec spec) dlp in

    if opt.verbose then begin
      p_stderr ("Resource ceilings:" ^ nl ^ SRP.string_of_r rm);
      p_stderr ("Tasks/ISRs per priority: " ^ nl ^ SRP.string_of (SRP.pl_spec dlp spec rm));
    end;

    (* gv_task optout *)
    if opt.gv_task then begin
      let oc = open_out opt.gv_taskf in
      p_oc oc (gv_of_spec dlp rm spec);
      close_out oc;
    end;

    let dep = timed ("compute resource dependencies") dep_of_p spec in
    (* gv_res optout *)
    if (opt.gv_res) then begin
      let oc = open_out opt.gv_resf in
      p_oc oc (gv_of_res dep spec);
      close_out oc;
    end;

    match opt.target with
    | RTFM_KERNEL ->
        (*
        let tasks = task_vector mTops in
        p_stderr ("Tasks : " ^ String.concat ", " (List.map snd tasks) ^ nl );
        *)

        (* vectors *)
        (* nv for use with CompCert *)
        (* id_vid is a list of pairs (task/isr_id -> vector_id) *)
        let nv = IsrCGen.assign_vector isr_vector spec in
        let id_vid = IsrCGen.assign_tasks isr_vector spec in
        if opt.verbose then begin
            p_stderr (nl ^ "Original Vector table " ^ nl ^ isrv_to_c isr_vector);
            p_stderr (nl ^ "After assignments Vector table " ^ nl ^ isrv_to_c nv);
            let pair (a, b) = a ^ "->" ^ b in
            p_stderr (nl ^ "Task to isr_entries" ^ nl ^ String.concat nl (List.map pair id_vid));
        end;

        (* generate c code *)
        let oc = open_out opt.outfile in
        let c_rt = timed ("generate C-output") (ICGenK.c_rt_of_i dlp spec rm id_vid) (Some (Int64.of_int 100)) in
        timed ("write C-output") p_oc oc c_rt ;
        close_out oc;

        ();

    | RTFM_RT ->
        (* generate c code *)
        let oc = open_out opt.outfile in
        let c_rt = timed ("generate C-output") (c_rt_of_i dlp spec 0 rm) None in
        timed ("write C-output") p_oc oc c_rt;
        close_out oc;

        let e = entry spec in
        if (opt.verbose) then begin
          p_stderr (string_of_dep dep);
          p_stderr (string_of_entry e);
        end;

        (* compute cyclic dependencies *)
        let ts = timed ("compute cyclic dependencies") (tsort dep) e in
        (
        match ts with
        | Some top -> p_stderr (
                "Deadlock free execution can be guaranteed " ^ nl ^
                "Topological order obtained: " ^ (String.concat ", " top) ^ nl;
              )
        | None -> p_stderr "Deadlock might occur";
        );
        let s_time = Printf.sprintf "%.5f" (Sys.time() -. t_start) in
        p_stderr ("Compilation finished in " ^ s_time ^ "s.");

  (* exception handling *)
  with
  | RtfmError msg -> p_stderr msg;
  | Failure msg -> p_stderr msg;
      exit (-1);;
(* exit 0;; *)

main ();;

     (*
          let tasks = task_vector meTops in
          p_stderr ("Tasks : " ^ String.concat ", " (List.map snd tasks) ^ nl );
          (* vectors *)
          let nv = assign_vectors isr_vector meTops in
          let tidm = assign_tasks isr_vector meTops in
          if opt.verbose then begin
            p_stderr (nl ^ "Original Vector table " ^ nl ^ isrv_to_c isr_vector);
            p_stderr (nl ^ "After assignments Vector table " ^ nl ^ isrv_to_c nv);
            let pair (a, b) = a ^ "->" ^ b in
            p_stderr (nl ^ "Task to isr_entries" ^ nl ^ String.concat nl (List.map pair tidm));
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
              *)
