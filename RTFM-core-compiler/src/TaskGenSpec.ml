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
  let rec tasks nr i_bl i_dl path aal afl sal sfl sl pe =
    let streq s1 s2 = String.compare s1 s2 == 0 in
    let mcount id sfl = List.length (List.filter (streq id) sfl) in
    match sl with 
    | [] -> []                                                     (* no more stmts, hence no more task or function instances to generate *)
    | Claim (r, csl) :: l ->
        debug ("claim path: " ^ path ^ ", resource: " ^ r);
        let inclaim = tasks nr i_bl i_dl (path ^ "_" ^ r) aal afl [] [] csl pe in     (* analyse inside claim for asyncs                     *)
        debug ("exit claim path: " ^ path ^ ", resource: " ^ r);
        inclaim @ tasks nr i_bl i_dl path aal afl sal sfl l pe       (* analyse remaining stmts                                             *)
    | Sync (id, par) :: l ->
        debug ("sync path: " ^ path ^ ", id: '" ^ id ^ "'");
        let nrsyncs = mcount id sfl in 
        let new_path = (path ^ "_" ^ id ^ "_" ^ string_of_int nrsyncs) in
        if List.memq id afl then
          raise (RtfmError("Cyclic function calls involving '" ^ id ^ "'"))
        else begin
          match Env.lookup_func id topl with
          | FuncDef (frt, fid, fal, fsl) ->
              debug ("sync enter: " ^ path ^ ", id: '" ^ id ^ "'");
              let insync = tasks nr i_bl i_dl new_path aal (id::afl) [] [] fsl pe in  (* analyse the called function for asyncs              *)
              debug ("sync exit :" ^ path ^ ", id: '" ^ id ^ "'");
              IFunc(i_dl, frt, new_path, fal, fsl) ::                   (* create a corresponding function instance (non specialized fsl) *)
              insync @    
              tasks nr i_bl i_dl path aal afl sal (id::sfl) l pe          (* add function id to sfl and anlyse remaining statements         *)
          | _ -> raise (RtfmError("failed lookup: '" ^ id ^ "'"))
        end
    | Async (mi, af, be, id, None) :: l ->
        debug ("async ISR path: " ^ path ^ ", id: '" ^ id ^ "'");
        (
          match Env.lookup_isr id topl with 
          | Isr (_,_,_) -> tasks nr i_bl i_dl path aal afl sal sfl l pe
          | _           -> raise (RtfmError ("Internal error in Async ISR lookup"))
        )
          
    | Async (mi, af, be, id, Some par) :: l ->
        begin
          debug ("async path: " ^ path ^ ", id: '" ^ id ^ "'");
          let nrasyncs = mcount id ((List.map fst) sal) in
          let new_path = path ^ "_" ^ id ^ "_" ^ string_of_int nrasyncs in
          
          try   
            let (p, p_dl) = List.assoc id aal in                  (* check if async cycle, and just add a last Task that refers to itself *)
            match Env.lookup_task id topl with
            | TaskDef (_, al, sl) -> 
              debug ("lookup cyclic '" ^ id ^ "' succeded, p_dl " ^ Time.string_of_time p_dl );
              debug ("async path cycle: " ^ path ^ ", id: '" ^ id ^ "', head chain deadline:" ^ Time.string_of_time p_dl);
              if (nr > 0) then begin 
                (* if (Time.compare p_dl Time.USec(0L)) != 0 then *)
                if Time.is_def p_dl then
                  async_err_handling ("Explicit deadline not allowed for closing cyclic task chains, in async " ^ id ^ "!"); 
                   
                if Time.is_def be then async_err_handling ("Explicit deadline required for heading cyclic task chains, in async " ^ id ^ "!");
                (* if (usec_of_time af  < usec_of_time i_dl) then async_err_handling ("Closing async cycle before deadline not allowed, in async " ^ id ^ "!");  *)
                IPeriodic (be, id) :: 
                ITask (Time.Infinite, p_dl, new_path, p, al, sl) :: 
                tasks (nr-1) i_bl i_dl path aal afl sal sfl l pe
              end else
                raise (RtfmError("Only a single cycle is allowed from the (one-shot) Reset"))
            | _ -> raise (RtfmError("failed lookup: [" ^ id ^ "]"))
          with Not_found ->                                                 (* not async cycle create a new Task and analyse it *)
              begin
                match Env.lookup_task id topl with
                | TaskDef (_, al, sl) ->
                    debug ("lookup sub-task '" ^ id ^ "' succeded");
                    debug ("async path: " ^ path ^ " -> " ^ id ^ 
                    " af = " ^ Time.string_of_time af ^ 
                    ", be = " ^ Time.string_of_time be ^ 
                    ", i_bl = " ^ Time.string_of_time i_bl ^ 
                    ", i_dl = " ^ Time.string_of_time i_dl );
                    if af == Time.Infinite then failwith ("Internal error, infinite after in async");
                    let new_bl = if Time.is_undef af then Time.zero else af in
                    let new_dl = if Time.is_undef be then Time.sub i_dl new_bl else be in 
                    
                    ITask (Time.Infinite, new_dl, new_path, new_path, al, sl) ::                         (* the new task                                     *)
                    tasks nr new_bl new_dl new_path ((id, (new_path, new_dl))::aal) afl [] [] sl (Time.add pe af) @  (* tasks created by the new task                    *)
                    tasks nr i_bl i_dl path aal afl ((id, (new_path, new_dl))::sal) sfl l pe          (* the remaining statements                         *) 
                | _ -> raise (RtfmError("failed lookup: '" ^ id ^ "'")) 
              end
        end    
    | Pend (be, id, par ) :: l -> 
      begin
          debug ("Pend path: " ^ path ^ ", id: '" ^ id ^ "'");
          let nrpends = mcount id ((List.map fst) sal) in
          let new_path = path ^ "_" ^ id ^ "_" ^ string_of_int nrpends in
              begin
                match Env.lookup_task id topl with
                | TaskDef (_, al, sl) ->
                    let new_bl = Time.zero in
                    let new_dl = 
                      if Time.is_undef be then 
                        raise (RtfmError("Explicit deadline for pend required")) 
                      else be in 
                    (* the new task *)
                    ITask (Time.Infinite, new_dl, new_path, new_path, al, sl) :: 
                    (* tasks created by the new task *)
                    tasks nr new_bl new_dl new_path ((id, (new_path, new_dl))::aal) afl [] [] sl pe @ 
                    (* the remaining statements *)
                    tasks nr i_bl i_dl path aal afl ((id, (new_path, new_dl))::sal) sfl l pe 
                | _ -> raise (RtfmError("failed lookup: [" ^ id ^ "]")) 
              end
      end
    | _ :: l -> tasks nr i_bl i_dl path aal afl sal sfl l pe                            (* analyse next statement                           *)
  
  and tasktop = function
    | TopC (c)             -> [IC (c)]
    | Isr (dl, id, sl)     -> IIsr (dl, id, sl) :: tasks 0 Time.zero dl id [] [] [] [] sl Time.zero
    | TaskDef(id, par, sl) -> [ITaskType (id, par)] (* do we really need them in the specialised AST, perhaps we will see *)
    | Reset (sl)           -> IReset (sl) :: tasks 1 Time.zero Time.Infinite "user_reset" [] [] [] [] sl Time.zero
    | Idle (sl)            -> IIdle (sl) :: tasks 1 Time.zero Time.Infinite "user_idle" [] [] [] [] sl Time.zero
    | FuncDef _            -> []
  in
    List.concat (List.map tasktop topl) 

let periodic_of_p = function 
    | IPeriodic (pe, id) -> id
    | _ -> raise (UnMatched)
    
let spec_of_p topl =
  let rec stmts cs i_dl path sal sfl sl =  
    match sl with
    | [] -> []
    | Claim (r, csl) :: l -> 
      Claim (r, stmts (r::cs) i_dl (path ^ "_" ^ r) [] [] csl ) :: stmts cs i_dl path sal sfl l
    | Sync (id, par) :: l ->
      let nrsyncs = mcount id sfl in
      let new_path = (path ^ "_" ^ id ^ "_" ^ string_of_int nrsyncs) in 
      Sync (new_path, par) :: stmts cs i_dl path sal (id::sfl) l
    | Async (mi, af, be, id, None) :: l ->
      let new_dl = if Time.is_undef be then Time.sub i_dl af else be in
      Async (mi, af, new_dl, id, None) :: stmts cs i_dl path (id::sal) sfl l
    | Async (mi, af, be, id, Some par) :: l -> 
      let nrasyncs = mcount id sal in
      let new_path = (path ^ "_" ^ id ^ "_" ^ string_of_int nrasyncs) in
      let new_dl = if Time.is_undef be then Time.sub i_dl af else be in
      if Time.compare new_dl Time.zero < 0 then async_err_handling ("Negative deadline for async " ^ id ^ nl); 
      Async (mi, af, new_dl, new_path, Some par) :: stmts cs i_dl path (id::sal) sfl l
    | Pend (be, id, par) :: l -> 
      let nrpends = mcount id sal in
      let new_path = (path ^ "_" ^ id ^ "_" ^ string_of_int nrpends) in
      Pend (be, new_path, par) :: stmts cs i_dl path (id::sal) sfl l
    | Return (c, _) :: l -> 
      Return (c, cs) :: stmts cs i_dl path sal sfl l
    | Break (n, _) :: l ->
      Break (n, cs) :: stmts cs i_dl path sal sfl l
    | Continue (n, _) :: l ->
      Continue (n, cs) :: stmts cs i_dl path sal sfl l
    | Goto (id, n, _) :: l ->
      Goto (id, n, cs) :: stmts cs i_dl path sal sfl l
    | s :: l -> s :: stmts cs i_dl path sal sfl l
  
  and spectop = function
    | IIsr (Time.USec dl, id, sl) -> IIsr (Time.USec dl, id, stmts [] (Time.USec dl) id [] [] sl)
    | IIsr (_, id, sl) -> IIsr (Time.Infinite, id, stmts [] Time.Infinite id [] [] sl)
    | ITask (pe, dl, id, path, par, sl) -> ITask (pe, dl, id, path, par, stmts [] dl path [] [] sl)
    | IFunc (dl, rt, path, al, sl) -> IFunc (dl, rt, path, al, stmts [] dl path [] [] sl)
    | IReset (sl) -> IReset (stmts [] Time.Infinite "user_reset" [] [] sl)
    | IIdle (sl) -> IIdle (stmts [] Time.Infinite "user_idle" [] [] sl)
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
  
  let spec = (List.map spectop topl) in
  add_ireset (add_iidle spec)  
  
(*
let rec lookup_itasktype_par id p = match p with
  | []                                                      -> failwith("Failed to lookup Task " ^ id)
  | ITaskType (tid, par) :: l when (compare id tid == 0)    -> par
  | _ :: l                                                  -> lookup_itasktype_par id l
*)