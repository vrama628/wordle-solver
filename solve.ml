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
  [@@deriving sexp_of, eq]

module GuessResult = struct
  (* invariant: length 5 *)
  type t = guess_result_cell list
  [@@deriving sexp_of]
  let hash = Hashtbl.hash
  let compare = Stdlib.compare
end

let check_guess ~target ~guess =
  let res = Array.create ~len:5 Gray in
  let leftovers = Hashtbl.create ~size:5 (module Char) in
  for i = 0 to 4 do
    if Char.equal guess.[i] target.[i] then
      res.(i) <- Green
    else
      Hashtbl.incr leftovers target.[i]
  done;
  for i = 0 to 4 do
    if equal_guess_result_cell res.(i) Gray && Hashtbl.mem leftovers guess.[i] then (
      res.(i) <- Yellow;
      Hashtbl.decr ~remove_if_zero:true leftovers guess.[i]
    )
  done;
  Array.to_list res

(* the state of the game; i.e. results of past guesses *)
type state = (string * GuessResult.t) list

(* THE MAIN AI *)
let[@landmark] decide_guess (state : state) : string =
  if List.is_empty state then "crane" else (* performance optimization *)
  let all_remaining_possible_words =
    Set.filter all_words ~f:(fun target ->
      List.for_all state ~f:(fun (guess, guess_result) ->
        check_guess ~target ~guess = guess_result
      )
    )
  in
  Set.fold
    all_remaining_possible_words
    ~init:(Int.max_value, ".....")
    ~f:(fun (acc_n, acc_word) guess ->
      let result_frequencies = Hashtbl.create ~size:243 (module GuessResult) in
      Set.iter all_remaining_possible_words ~f:(fun target ->
        Hashtbl.incr result_frequencies (check_guess ~target ~guess)
      );
      let worst_case_outcome =
        Hashtbl.fold
          result_frequencies
          ~init:Int.min_value
          ~f:(fun ~key:_ ~data acc -> Int.max data acc)
      in
      if worst_case_outcome < acc_n then
        (worst_case_outcome, guess)
      else
        (acc_n, acc_word)
    )
  |> snd

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
  printf "\027[0m\n%!" 

let rec run ?(state=[]) target =
  let guess = decide_guess state in
  let result = check_guess ~target ~guess in
  (*print_guess (guess, result);*)
  if List.for_all result ~f:((=) Green) then
    (List.length state + 1)
  else
    run ~state:((guess, result) :: state) target

let () =
  let distribution =
    Set.fold all_words ~init:(Map.empty (module Int)) ~f:(fun acc word ->
      let num_guesses = run word in
      printf ".%!";
      Map.update acc num_guesses ~f:(function None -> 1 | Some n -> n + 1)
    )
  in
  let successes =
    let (success_distribution, _, _) = Map.split distribution 7 in
    Map.fold success_distribution ~init:0 ~f:(fun ~key:_ ~data acc -> data + acc)
  in
  let num_words = Set.length all_words in
  printf
    "\nSuccesses: %d/%d = %.2f\n"
    successes
    num_words
    ((Float.of_int successes) *. 100. /. (Float.of_int num_words));
  printf "Distribution:\n";
  Map.iteri distribution ~f:(fun ~key ~data ->
    printf "\t%d: %d\n" key data
  )
