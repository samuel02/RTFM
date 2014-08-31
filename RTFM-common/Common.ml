(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-Common/Common *)

(* characters and strings *)
let tab 	= "\t"
let tabc 	= '\t'
  
let nl 		= "\n"
let nlc 	= '\n'
  
let enl		= "\\n" 
  
let op 		= " {" ^ nl
  
let cl 		= "}"
  
let ec    = "\""  

let ecit  = "\'"

let e_c   = "#>"
let c_e   = "<#"
  
(* Error handling *)  
exception RtfmError of string
exception UnMatched    

let rec mymap m = function
    | []     -> []
    | e :: l -> try (m e) :: mymap m l with UnMatched -> mymap m l

let rec myconcat s = function
    | []     -> ""
    | "":: l -> myconcat s l
    | e :: l -> e ^ s ^ myconcat s l

let rec mycon s = function
    | []      -> ""
    | e :: [] -> e
    | "" :: l -> mycon s l
    | e :: l  -> e ^ s ^ mycon s l

let myass k m = try List.assoc k m with _ -> raise (RtfmError("lookup of " ^ k ^ " failed!"))
      
let mycompare s1 s2 = (String.compare s1 s2 == 0)  
                                         
(* Helper functions *)  
let p_stderr x = Printf.fprintf stderr "%s\n%!" x
let p_oc oc x = Printf.fprintf oc "%s\n%!" x
  
let rec range i j = if i > j then [] else i :: (range (i+1) j)
  
let size = 1024 (* Static sized parsing buffer in Parsing.ml (Standard lib) *)
  
let submatch s i m =  
  let rec subm s i m r = 
    try 
      match String.get s (i mod size) with
        | c when c == m 	  -> r
        | c when c == tabc 	-> subm s (i + 1) m (r ^ " ") 
        | c 				        -> subm s (i + 1) m (r ^ String.make 1 c) 
    with
      _ -> r (* outside string, should never happen *)
  in
  subm s i m ""
    
type target =
  | RTFM_KERNEL
  | RTFM_RT
    
let string_of_target = function
  | RTFM_KERNEL -> "RTFM_KERNEL"
  | RTFM_RT		  -> "RTFM_RT"
    
type backend =
  | GCC
  | CCOMP
  | CLANG
    
let string_of_backend = function
  | GCC		-> "GCC"
  | CCOMP	-> "CCOMP"
  | CLANG	-> "CLANG"
 
type time =
  | Usec of int
  | Msec of int
  | Sec  of int
  | Infinite
 
let string_of_time = function 
  | Usec i   -> string_of_int i ^ " us"
  | Msec i   -> string_of_int i ^ " ms"
  | Sec  i   -> string_of_int i ^ " s"
  | Infinite -> "Infinite"
 
let usec_of_time = function
  | Usec i   -> i
  | Msec i   -> 1000 * i
  | Sec i    -> 1000 * 1000 * i  
  | Infinite -> max_int (* compatible over 32/64 bit systems *)
     