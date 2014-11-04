(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* RTFM-Common/Time *)
open Common 

type time =
  | USec     of int64
  | Infinite
  | Undef

let us_s   = 1000000L
let us_ms  = 1000L

let t_sec  = USec (us_s)
let t_ms   = USec (us_ms)
let zero   = USec (0L)

let usec_of_time = function
  | USec i   -> i
  | Infinite -> Int64.max_int
  | Undef    -> raise (RtfmError("Usec of Undefined")) 

let compare t1 t2 =
  Pervasives.compare (usec_of_time t1) (usec_of_time t2)

let is_usec = function
  | USec _ -> true
  | _      -> false
      
let is_undef = function
  | Undef -> true
  | _     -> false

let is_def l = not (is_undef l)

let string_of_time t =
  let f v d = Int64.to_float v /. Int64.to_float d in
  match t with 
  | USec v   -> 
    if (Int64.compare v us_s) >= 0 then begin
      string_of_float (f v us_s) ^ "s"  
    end else if (Int64.compare v us_ms) >= 0 then begin
      string_of_float (f v us_ms) ^ "ms"
    end else begin
      Int64.to_string v ^ "us"
    end
  | Infinite -> "Infinite Time"
  | Undef    -> "Undefinied Time"

let c_of_time t = function
  | None -> Int64.to_string (usec_of_time t)
  | Some tdiv  -> 
    match t with
    | Infinite -> Int32.to_string Int32.max_int
    | _ ->
    let td = Int64.mul (usec_of_time t) tdiv in
    if Int64.compare td (Int64.of_int32(Int32.max_int)) > 0 then 
      raise(RtfmError("Time does not fit 32 bit" ^ Int64.to_string td))
    else
      Int64.to_string td
      
let add t1 t2 =
  if t1 == Undef then failwith ("add t1: " ^ string_of_time t1);
  if t2 == Undef then failwith ("add t2: " ^ string_of_time t2);
  if t1 == Infinite || t2 == Infinite then Infinite else
  USec(Int64.add (usec_of_time t1) (usec_of_time t2)) 

let sub t1 t2 =
  if t2 == Infinite || t2 == Undef then failwith ("sub t2:" ^ string_of_time t2);
  if t2 == Infinite || t2 == Undef then failwith ("sub t2:" ^ string_of_time t2);
  if t1 == Undef then failwith ("sub t1:" ^ string_of_time t1);
  if t1 == Infinite then Infinite else 
  USec(Int64.sub (usec_of_time t1) (usec_of_time t2))  
