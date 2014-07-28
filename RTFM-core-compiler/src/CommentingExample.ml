(** 
 Example of commenting for OCaml Doc (ocamldoc)

 @author    Emil Fresk
 *)

(**
 Exception definition for length mismatch errors.
 *)
exception LengthMismatch of string;;

(**
 Merges two lists of the same length by the usage of the function f.
 Execution: merge f [a1; a2; ... an] [b1; b2; ... bn] => [f a1 b1; f a2 b2; ... f an bn]

 @param f   Function to apply to the lists.
 @param l1  First list.
 @param l2  Second list.

 @raise LengthMismatch  Exception if there was a size mismatch.
 @return    The merged list.

 @author    Emil Fresk
 *)
let rec merge f l1 l2 =
  match l1, l2 with
    | [], []             -> []
    | _::_, []           -> raise (LengthMismatch "merge: Lists are of different lengths!")
    | [], _::_           -> raise (LengthMismatch "merge: Lists are of different lengths!")
    | h1 :: t1, h2 :: t2 -> f h1 h2 :: merge f t1 t2
;;

(**
 Converts a list of strings to a list of the lengths of the corresponding
 strings.

 @param lst List of strings to convert.

 @return    The converted list.

 @author    Emil Fresk
 *)
let rec string_list_to_size lst =
  match lst with
    | []       -> []
    | hd :: tl -> List.map String.length hd :: string_list_to_size tl
;;

(**
 Converts a list of lists to a single list.
 Details:   Makes a list of lists and merges all of the lists to a single
            list. The function f is applied on the two merging lists as
            defined in merge.

            Call tree:
            merge_lists f [l1; l2; ... ln] => merge f ( ... merge f ( (merge f l1 l2) l3) ... ln )

 @param f   Function to apply to the lists while merging.
 @param lst List of lists to merge.

 @return    The merged list.

 @author    Emil Fresk
 *)
let rec merge_lists f lst =
  match lst with
    | []               -> lst
    | _::[]            -> lst
    | hd1 :: hd2 :: tl -> merge_lists f (merge f hd1 hd2 :: tl)
;;