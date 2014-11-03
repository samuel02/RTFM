(* Copyright Per Lindgren 2014, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(**
Example of commenting for OCaml Doc (ocamldoc).

The file contains code that creates a pretty print of string lists.

@author Emil Fresk
*)

(**
Exception definition for length mismatch errors.
*)
exception LengthMismatch of string

(**
Merges two lists of the same length by the usage of the function f.
Execution: merge f [a1; a2; ... an] [b1; b2; ... bn] => [f a1 b1; f a2 b2; ... f an bn]

@param f Function to apply to the lists.
@param l1 First list.
@param l2 Second list.

@raise LengthMismatch Exception if there was a size mismatch.
@return The merged list.
@author Emil Fresk
*)
let rec merge f l1 l2 =
  match l1, l2 with
    | [], []                -> []
    | _:: _, [] | [], _:: _ -> raise (LengthMismatch "merge: Lists are of different lengths!")
    | h1 :: t1, h2 :: t2    -> f h1 h2 :: merge f t1 t2

(**
Converts a list of lists to a single list.
Details: Makes a list of lists and merges all of the lists to a single list.
The function f is applied on the two merging lists as defined in merge.

Execution:
merge_lists f [l1; l2; ... ln] => merge f ( ... merge f ( (merge f l1 l2) l3) ... ln )

@param f Function to apply to the lists while merging.
@param lst List of lists to merge.

@return The merged list.
@author Emil Fresk
*)
let rec merge_lists f lst =
  match lst with
    | []               -> []
    | hd :: []         -> hd
    | hd1 :: hd2 :: tl -> merge_lists f (merge f hd1 hd2 :: tl)

(**
Converts a list of string lists to a list of lists with the lengths of the corresponding strings.

@param lst List of strings to convert.

@return The converted list.
@author Emil Fresk
*)
let rec string_lists_to_size lst =
  match lst with
    | []       -> []
    | hd :: tl -> (List.map String.length hd) :: (string_lists_to_size tl)

(**
Get the maximum length of each string in a list of string lists.

@param lst List of strings to convert.

@return The list where each element corresponds to the maximum string length in that column.
@author Emil Fresk
*)
let get_max_size lst =
  merge_lists max (string_lists_to_size lst)

(**
Converts the maximum string size list to a printable separator.

@param max_size Maximum string size of each column.
@param sep Seperator character to use.

@return The converted string.
@author Emil Fresk
*)
let make_seperator max_size sep =
  let rec sep_list max_size sep =
    match max_size with
      | []       -> []
      | hd :: tl -> (String.make (hd + 2) sep) :: (sep_list tl sep)
  in
    "+" ^ String.concat "+" (sep_list max_size sep) ^ "+"

(**
Makes a formated line.

@param str_lst List of strings to convert to a single string.
@param size_lst Maximum string size of each column.

@return The converted string.
@author Emil Fresk
*)
let make_formated_line str_lst size_lst =
  let rec format str_lst size_lst =
    match str_lst, size_lst with
      | [], []                 -> []
      | _:: _, [] | [], _:: _  -> raise (LengthMismatch "make_formated_line: String and Size lists are of different lengths!")
      | hd1 :: tl1, hd2 :: tl2 -> (hd1 ^ String.make (hd2 - String.length hd1) ' ') :: (format tl1 tl2)  
  in
    "| " ^ String.concat " | " (format str_lst size_lst) ^ " |"

(**
Makes a formated list.

@param str_lst List of strings to convert to a single string.
@param size_lst Maximum string size of each column.

@return The converted string.
@author Emil Fresk
*)
let rec make_formated_list str_lst size_lst =
  match str_lst with
    | [] -> []
    | hd :: tl -> (make_formated_line hd size_lst) :: (make_formated_list tl size_lst)
  
(**
Prints a list with header.

@param head Head of the table.
@param str_list Content of the table.

@author Emil Fresk
*)
let print_list_with_header head str_list =
  let size = get_max_size (head:: str_list) in begin
    print_string ( "\n" ^ make_seperator size '-' ^ "\n" );
    print_string (String.concat "\n" (make_formated_list [head] size) ^ "\n" );
    print_string ( make_seperator size '-' ^ "\n" );
    print_string (String.concat "\n" (make_formated_list str_list size) ^ "\n" );
    print_string ( make_seperator size '-' ^ "\n" ^ "\n" );
  end

(**
"Main" function
*)
let main () =
  let test = [["asd";        "gdfgdf";            "asd443ad"];
              ["asdd";       "sdfeeeeeeeeeeeee";  "asdghjsaad"];
              ["asdasdasd";  "asdsad";            "asdgddsad"]] and
      head =  ["my";         "head";              "meow"] in
  print_list_with_header head test
  
