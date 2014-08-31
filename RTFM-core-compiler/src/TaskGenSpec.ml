(* Copyright Per Lindgren 2014, see the file "LICENSE" for the full        *)
(* license governing this code.                                            *)

(* RTFM-core/TaskGen *)

open Common
open Options
open AST
open SpecAST
open SRP

(* Traverse each top level ISR and Reset and transform async's to tasks *)
(* Perform complete specialization of functions (FuncDef->Func *)

let async_err_handling s =
  if opt.async_err then 
    raise (RtfmError("Error: " ^ s))
  else 
    p_stderr("Async warning : " ^ s)

let streq s1 s2 = String.compare s1 s2 == 0 
let mcount id sfl = List.length (List.filter (streq id) sfl) 
            
let task_of_p topl =
  let rec tasks nr i_bl i_dl path aal afl sal sfl sl =
    let streq s1 s2 = String.compare s1 s2 == 0 in
    let mcount id sfl = List.length (List.filter (streq id) sfl) in
    match sl with
    | [] -> []                                           (* no more stmts, hence no more task or function instances to generate *)
    | Claim (r, csl) :: l ->
        tasks nr i_bl i_dl (path ^ "_" ^ r) aal afl [] [] csl @    (* analyse inside claim for asyncs                                     *)
        tasks nr i_bl i_dl path aal afl sal sfl l                  (* analyse remaining stmts                                             *)
    | Sync (id, par) :: l ->
        let nrsyncs = mcount id sfl in 
        let new_path = (path ^ "_" ^ id ^ "_" ^ string_of_int nrsyncs) in
        if List.memq id afl then
          raise (RtfmError("Cyclic function calls involving " ^ id))
        else begin
          match Env.lookup_func id topl with
          | FuncDef (frt, fid, fal, fsl) ->
              IFunc(i_dl, frt, new_path, fal, fsl) ::                   (* create a corresponding function instance (non specialized fsl) *)
              tasks nr i_bl i_dl new_path aal (id::afl) [] [] fsl @     (* analyse the called function for asyncs                         *)
              tasks nr i_bl i_dl path aal afl sal (id::sfl) l           (* add function id to sfl and anlyse remaining statements         *)
          | _ -> raise (RtfmError("failed lookup: [" ^ id ^ "]"))
        end
    | Async (af, be, id, _) :: l ->
        begin
          let nrsyncs = mcount id ((List.map fst) sal) in
          let new_path = path ^ "_" ^ id ^ "_" ^ string_of_int nrsyncs in
          
          try
            let (p, p_dl) = List.assoc id aal in                (* check if async cycle, and just add a last Task that refers to itself *)
            match Env.lookup_task id topl with
            | TaskDef (_, al, sl) -> 
              if (nr > 0) then begin
                (*
                p_stderr ("l af = " ^ string_of_time af ^ nl);
                p_stderr ("l be = " ^ string_of_time be ^ nl);
                p_stderr ("l i_bl = " ^ string_of_time i_bl ^ nl);
                p_stderr ("l i_dl = " ^ string_of_time i_dl ^ nl);
                *)
                
                if (usec_of_time be != 0) then async_err_handling ("Explicit deadline not allowed for closing cyclic task chains, in async " ^ id ^ "!");
                if (usec_of_time af  < usec_of_time i_dl) then async_err_handling ("Closing async cycle before deadline not allowed, in async " ^ id ^ "!"); 
                ITask (Infinite, p_dl, new_path, p, al, sl) :: 
                tasks (nr-1) i_bl i_dl path aal afl sal sfl l
              end else
                raise (RtfmError("Only a single cycle is allowed from the (one-shot) Reset"))
            | _ -> raise (RtfmError("failed lookup: [" ^ id ^ "]"))
          with Not_found ->                                                 (* not async cycle create a new Task and analyse it *)
              begin
                match Env.lookup_task id topl with
                | TaskDef (_, al, sl) ->
                 (* 
                    p_stderr ("af = " ^ string_of_time af ^ nl);
                    p_stderr ("be = " ^ string_of_time be ^ nl);
                    p_stderr ("i_bl = " ^ string_of_time i_bl ^ nl);
                    p_stderr ("i_dl = " ^ string_of_time i_dl ^ nl);
                  *)  
                    let new_bl = if (usec_of_time af == 0) then (Usec(0)) else af in
                    let new_dl = if (usec_of_time be == 0) then Usec ((usec_of_time i_dl) - (usec_of_time new_bl)) else be in 
                    
                    ITask (Infinite, new_dl, new_path, new_path, al, sl) ::           (* the new task                                     *)
                    tasks nr new_bl new_dl new_path ((id, (new_path, new_dl))::aal) afl [] [] sl @  (* tasks created by the new task                    *)
                    tasks nr i_bl i_dl path aal afl ((id, (new_path, new_dl))::sal) sfl l       (* the reaming statements                           *) 
                | _ -> raise (RtfmError("failed lookup: [" ^ id ^ "]")) 
              end
        end    
    | _ :: l -> tasks nr i_bl i_dl path aal afl sal sfl l                             (* analyse next statement                           *)
  
  and tasktop = function
    | TopC (c) -> [IC (c)]
   (* | Isr (p, id, sl) -> IIsr (p, id, sl) :: tasks 0 Usec(p) id [] [] [] [] sl *)
    | TaskDef(id, par, sl) -> [ITaskType (id, par)]
    | Reset (sl) -> IReset (sl) :: tasks 1 (Sec(0)) Infinite "reset" [] [] [] [] sl
    | Idle (sl) -> IIdle (sl) :: tasks 1 (Sec(0)) Infinite "idle" [] [] [] [] sl
    | _ -> raise (UnMatched)
  
  in
  List.concat (mymap tasktop topl) 
  
let spec_of_p topl =
  let rec stmts i_dl path sal sfl sl =  
    match sl with
    | [] -> []
    | Claim (r, csl) :: l -> Claim (r, stmts i_dl (path ^ "_" ^ r) [] [] csl ) :: stmts i_dl path sal sfl l
    | Sync (id, par) :: l ->
      let nrsyncs = mcount id sfl in
      let new_path = (path ^ "_" ^ id ^ "_" ^ string_of_int nrsyncs) in 
      Sync (new_path, par) :: stmts i_dl path sal (id::sfl) l
    | Async (af, be, id, par) :: l -> 
      let nrsyncs = mcount id sal in
      let new_path = (path ^ "_" ^ id ^ "_" ^ string_of_int nrsyncs) in
      let new_dl = if (usec_of_time be == 0) then Usec ((usec_of_time i_dl) - (usec_of_time af)) else be in
      if usec_of_time new_dl < 0 then async_err_handling ("Negative deadline for async " ^ id ^ nl); 
      Async (af, new_dl, new_path, par) :: stmts i_dl path (id::sal) sfl l
    | s :: l -> s :: stmts i_dl path sal sfl l
  
  and spectop = function
    | IIsr (p, id, sl) -> IIsr (p, id, sl)
    | ITask (pe, dl, id, path, par, sl) -> ITask (pe, dl, id, path, par, stmts dl path [] [] sl)
    | IFunc (dl, rt, path, al, sl) -> IFunc (dl, rt, path, al, stmts dl path [] [] sl)
    | IReset (sl) -> IReset (stmts Infinite "reset" [] [] sl)
    | IIdle (sl) -> IIdle (stmts Infinite "idle" [] [] sl)
    | x -> x
    
  in
  let rec add_iidle = function 
  | []              -> [IIdle([])]
  | IIdle (sl) :: l -> IIdle (sl) :: l
  | t :: l          -> t :: add_iidle( l )
  in
  let rec add_ireset = function 
  | []               -> [IReset([])]
  | IReset (sl) :: l -> IReset (sl) :: l
  | t :: l           -> t :: add_ireset( l )
  in
  add_ireset (add_iidle (mymap spectop topl))


let rec lookup_itasktype_par id p = match p with
  | []                                                      -> failwith("Failed to lookup Task " ^ id)
  | ITaskType (tid, par) :: l when (compare id tid == 0)    -> par
  | _ :: l                                                  -> lookup_itasktype_par id l


  