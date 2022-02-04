open Base
let (=) = Poly.(=)
open Stdio

(* the set of all 5 letter words in consideration *)
let all_words : (string, _) Set.t =
  let open In_channel in
  let ic = create  "full_dictionary.txt" in
  let rec build_set acc =
    match input_line ic with
    | Some word ->
      if
        String.length word = 5 && String.for_all word ~f:Char.is_alpha
      then
        build_set (Set.add acc word)
      else
        build_set acc
    | None -> acc
  in
  let all_words = build_set (Set.empty (module String)) in
  close ic;
  all_words

type guess_result_cell =
  | Green
  | Yellow
  | Gray

let all_possible_guess_result_cells : guess_result_cell list =
  [Green; Yellow; Gray]

(* invariant: length 5 *)
type guess_result = guess_result_cell list

let rec remove_one (l : 'a list) (elt : 'a) : 'a list option =
  match l with
  | [] -> None
  | x::xs ->
    if x = elt then
      Some xs
    else
      Option.map (remove_one xs elt) ~f:(List.cons x)

let check_guess ~target ~guess =
  let res = Array.create ~len:5 Gray in
  let (_, leftovers) =
    Fn.apply_n_times
      ~n:5
      (fun (i, acc) ->
        let acc =
          if guess.[i] = target.[i] then (
            res.(i) <- Green;
            acc
          ) else (
            target.[i] :: acc
          )
        in
        (i + 1, acc)
      )
      (0, [])
  in
  let _ =
    Fn.apply_n_times
      ~n:5
      (fun (i, leftovers) ->
        let leftovers =
          match res.(i), remove_one leftovers guess.[i] with
          | Gray, Some leftovers ->
            res.(i) <- Yellow;
            leftovers
          | _ -> leftovers
        in
        (i + 1, leftovers)
      )
      (0, leftovers)
  in
  Array.to_list res

let all_possible_guess_results : guess_result list =
  let open List.Let_syntax in
  let%bind a = all_possible_guess_result_cells in
  let%bind b = all_possible_guess_result_cells in
  let%bind c = all_possible_guess_result_cells in
  let%bind d = all_possible_guess_result_cells in
  let%bind e = all_possible_guess_result_cells in
  return [a; b; c; d; e]

(* the state of the game; i.e. results of past guesses *)
type state = (string * guess_result) list

let guess (state : state) : string =
  let all_remaining_possible_words : (string, _) Set.t =
    Set.filter all_words ~f:(fun target ->
      List.for_all state ~f:(fun (guess, guess_result) ->
        check_guess ~target ~guess = guess_result
      )
    )
  in
  ignore all_remaining_possible_words;
  "TOODO"

let target_word = (Sys.get_argv ()).(1)

let print_guess (guess, result) =
  List.zip_exn (String.to_list guess) result
  |> List.iter ~f:(fun (c, cell) ->
      let color =
        match cell with
        | Green -> 42
        | Yellow -> 43
        | Gray -> 40
      in
      printf "\027[1;%dm%c" color c
    );
  printf "\027[0m\n" 

let () =
  ignore all_possible_guess_result_cells;
  ignore guess;
  let guess_word = (Sys.get_argv ()).(2) in
  print_guess (guess_word, check_guess ~target:target_word ~guess:guess_word)
