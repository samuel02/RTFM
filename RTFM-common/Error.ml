(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-common/Error *)

open Lexing
open Common

type comment_info = {
  mutable ci_lnum : int;
  mutable ci_cnum : int;
  mutable ci_bol  : int;
}

let ci = {
  ci_lnum = 0;
  ci_cnum = 0;
  ci_bol  = 0;
}

let next_line lexbuf = Lexing.new_line lexbuf

let parse_err_msg lexbuf =
  let pos = lexbuf.lex_curr_p in
  let index = pos.pos_cnum - pos.pos_bol -1 in
  " File " ^ pos.pos_fname ^
  " : Line " ^ string_of_int pos.pos_lnum ^
  " : Position " ^ string_of_int index ^ nl ^
  submatch lexbuf.lex_buffer pos.pos_bol '\n' ^ nl ^
  (String.make index ' ' ) ^ "^" ^ nl

let set_info lexbuf =
  let pos = lexbuf.lex_curr_p in
  ci.ci_lnum <- pos.pos_lnum;
  ci.ci_cnum <- pos.pos_cnum;
  ci.ci_bol  <- pos.pos_bol;;

let bol lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
  { pos with
    pos_bol  = ci.ci_bol;
    pos_lnum = ci.ci_lnum;
    pos_cnum = ci.ci_cnum;
  }