(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/Options *)

open Common

type options =
  {
    mutable target: target;
    mutable backend: backend;
    mutable verbose: bool;
    mutable debug: bool;
    mutable infile: string;
    mutable outfile: string;
    mutable dotout: bool;
    mutable gv_obj: bool;
    mutable gv_objf: string;
    mutable gv_task: bool;
    mutable gv_taskf: string;
    mutable gv_inst: bool;
    mutable gv_instf: string;
    mutable dotfile: string;
    mutable d_ast: bool;
    mutable typecheck: bool;
    mutable ldotout: bool;
    mutable ldotfile: string;
    (* mutable string: Time.t; -- requires Core *)
  }

let opt =
  {
    target   = RTFM_RT;
    backend  = GCC;
    verbose  = false;
    debug    = false;
    infile   = "";
    outfile  = "";
    dotout   = false;
    gv_obj   = false;
    gv_objf  = "";
    gv_task   = false;
    gv_taskf  = "";
    gv_inst   = false;
    gv_instf  = "";
    dotfile  = "";
    ldotout  = false;
    ldotfile = "";
    d_ast    = false;
    typecheck= true;
  }

let string_of_opt opt =
  "RTFM-core compilation options:" ^ nl ^
  "infile         : " ^ opt.infile ^ nl ^
  "outfile        : " ^ opt.outfile ^ nl ^
  "target         : " ^ string_of_target opt.target ^ nl ^
  "backend        : " ^ string_of_backend opt.backend ^ nl ^
  "verbose        : " ^ string_of_bool opt.verbose ^ nl ^
  "debug          : " ^ string_of_bool opt.debug ^ nl ^
  "dotout         : " ^ string_of_bool opt.dotout ^ nl ^
  "gv_obj         : " ^ string_of_bool opt.gv_obj ^ nl ^
  "gv_objf        : " ^ opt.gv_objf ^ nl ^
  "gv_task        : " ^ string_of_bool opt.gv_task ^ nl ^
  "gv_taskf       : " ^ opt.gv_taskf ^ nl ^
  "gv_inst        : " ^ string_of_bool opt.gv_inst ^ nl ^
  "gv_instf       : " ^ opt.gv_instf ^ nl ^
  "dotfile        : " ^ opt.dotfile ^ nl ^
  "ldotout        : " ^ string_of_bool opt.ldotout ^ nl ^
  "ldotfile       : " ^ opt.ldotfile ^ nl ^
  "d_ast          : " ^ string_of_bool opt.d_ast ^ nl ^
  "typecheck      : " ^ string_of_bool opt.typecheck ^ nl