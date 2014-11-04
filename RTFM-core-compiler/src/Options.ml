(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-core/Options *)

open Common

type options =
  {
   mutable target:      target;
   mutable backend:     backend;
   mutable async_err:   bool;
   mutable verbose:     bool;
   mutable debug:       bool;
   mutable infile:      string;
   mutable outfile:     string;
   mutable gv_task:     bool;
   mutable gv_taskf:    string;
   mutable gv_res:      bool;
   mutable gv_resf:     string;
   mutable d_ast:       bool;
   (* mutable string:   Time.t; -- not yet implemented *)
  }

let opt =
  {
   target    = RTFM_RT;
   backend   = GCC;
   async_err = false;
   verbose   = false;
   debug     = false;
   infile    = "";
   outfile   = "";
   gv_task   = false;
   gv_taskf  = "";
   gv_res    = false;
   gv_resf   = "";
   d_ast     = false;
  }


let string_of_opt opt =
  "RTFM-core options:" ^ nl ^
  "infile       : " ^ opt.infile ^ nl ^
  "outfile      : " ^ opt.outfile ^ nl ^
  "async_err    : " ^ string_of_bool opt.async_err ^ nl ^
  "target       : " ^ string_of_target opt.target ^ nl ^
  "backend      : " ^ string_of_backend opt.backend ^ nl ^
  "verbose      : " ^ string_of_bool opt.verbose ^ nl ^
  "debug        : " ^ string_of_bool opt.debug ^ nl ^
  "gv_task      : " ^ string_of_bool opt.gv_task ^ nl ^
  "gv_taskf     : " ^ opt.gv_taskf ^ nl ^
  "gv_res       : " ^ string_of_bool opt.gv_res ^ nl ^
  "gv_resf      : " ^ opt.gv_resf ^ nl ^
  "d_ast        : " ^ string_of_bool opt.d_ast ^ nl

let verb s  = if opt.verbose then p_stderr s
let debug s = if opt.debug then p_stderr s

let timed s f g =
  (* verb ("---> " ^ s ^ ":"); *)
  let t = Sys.time() in
  let res = f g in
  let sfloat = Printf.sprintf "%.3f" (Sys.time() -. t) in
  verb ("---> " ^ s ^ ": finished in " ^ sfloat ^ "s.");
  res
