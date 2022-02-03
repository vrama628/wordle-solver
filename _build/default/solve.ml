[@@@alert "-deprecated"]

open Base

(* the set of all 5 letter words in consideration *)
let all_words : (string, _) Set.t =
  let ic = open_in  "full_dictionary.txt" in
  let rec build_set acc =
    try
      let word = input_line ic in
      if
        String.length word = 5 && String.for_all word ~f:Char.is_alpha
      then
        build_set (Set.add acc word)
      else
        build_set acc
    with 
      End_of_file -> acc
  in
  let all_words = build_set (Set.empty (module String)) in
  close_in ic;
  all_words

let () =
  Set.iter all_words ~f:print_endline
