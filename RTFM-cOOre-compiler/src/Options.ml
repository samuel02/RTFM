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
    mutable dotfile: string;
    mutable d_ast: bool;
    mutable ldotout: bool;
    mutable ldotfile: string;
    (* mutable string: Time.t; -- requires Core *)
  }

let opt =
  { 
    target   = RTFM_PT;
    backend  = GCC;
    verbose  = false;
    debug    = false;
    infile   = "";
    outfile  = "";
    dotout   = false;
    dotfile  = "";
    ldotout  = false;
    ldotfile = "";
    d_ast    = false;
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
  "dotfile        : " ^ opt.dotfile ^ nl ^
  "ldotout        : " ^ string_of_bool opt.ldotout ^ nl ^
  "ldotfile       : " ^ opt.ldotfile ^ nl ^
  "d_ast          : " ^ string_of_bool opt.d_ast ^ nl