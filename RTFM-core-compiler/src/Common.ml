(* Common *)

(* characters and strings *)
let tab 	= "\t"
let tabc 	= '\t'
  
let nl 		= "\n"
let nlc 	= '\n'
  
let enl		= "\\n" 
  
let op 		= " {" ^ nl
  
let cl 		= "}"
  
let ec      = "\""  
  
(* Error handling *)  
exception RtfmError of string
  
(* Helper functions *)  
let p_stderr x = Printf.fprintf stderr "%s\n%!" x
let p_oc oc x = Printf.fprintf oc "%s\n%!" x
  
let rec range i j = if i < j then [] else i :: (range (i+1) j)
  
let size = 1024
  
let submatch s i m =  
  let rec subm s i m r = 
    try 
      match String.get s (i mod size) with
        | c when c == m 	-> r
        | c when c == tabc 	-> subm s (i + 1) m (r ^ " ") 
        | c 				-> subm s (i + 1) m (r ^ String.make 1 c) 
    with
      _ -> r (* outside string, should never happen *)
  in
  subm s i m ""
    
type target =
  | RTFM_KERNEL
  | RTFM_PT
    
let string_of_target (t:target) = match t with
  | RTFM_KERNEL -> "RTFM_KERNEL"
  | RTFM_PT		-> "RTFM_PT"
    
type backend =
  | GCC
  | CCOMP
  | CLANG
    
let string_of_backend (b:backend) = match b with
  | GCC		-> "GCC"
  | CCOMP	-> "CCOMP"
  | CLANG	-> "CLANG"
    
type options = 
  {mutable target: 		target; 
   mutable backend: 	backend; 
   mutable verbose: 	bool;
   mutable debug:		bool;
   mutable infile: 		string;
   mutable outfile: 	string;
   mutable dotout:		bool;
   mutable dotfile:		string;
   mutable d_ast:		bool;
   mutable ldotout:		bool;
   mutable ldotfile:	string;
   (* mutable string:	Time.t; -- requires Core *)
  }
  
let opt = 
  {target 	= RTFM_PT; 
   backend 	= GCC;
   verbose 	= false;
   debug	= false;
   infile	= "";
   outfile	= "";
   dotout	= false;
   dotfile	= "";
   ldotout	= false;
   ldotfile	= "";
   d_ast	= false;
  } 
    
    
let string_of_opt opt = 
  "RTFM-core compilation options:" ^ nl ^  
    "infile     	: " ^ opt.infile ^ nl ^
    "outfile    	: " ^ opt.outfile ^ nl ^
    "target     	: " ^ string_of_target opt.target ^ nl ^
    "backend    	: " ^ string_of_backend opt.backend ^ nl ^
    "verbose    	: " ^ string_of_bool opt.verbose ^ nl ^
    "debug      	: " ^ string_of_bool opt.debug ^ nl ^
    "dotout     	: " ^ string_of_bool opt.dotout ^ nl ^
    "dotfile    	: " ^ opt.dotfile ^ nl ^
    "ldotout    	: " ^ string_of_bool opt.ldotout ^ nl ^
    "ldotfile   	: " ^ opt.ldotfile ^ nl ^
    "d_ast      	: " ^ string_of_bool opt.d_ast ^ nl
    
    
    
    
    
    