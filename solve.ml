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

(* TODO: delete once optimized *)
let () = Random.init 628
let all_words = Set.filter all_words ~f:(fun _ -> Random.int 100 < 10)

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

(* THE MAIN AI *)
let decide_guess (state : state) : string =
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
      let worst_case_outcome =
        all_possible_guess_results
        |> List.map ~f:(fun guess_result ->
            Set.count all_remaining_possible_words ~f:(fun target ->
              check_guess ~target ~guess = guess_result
            )
          )
        |> List.max_elt ~compare:Int.compare
        |> (fun x -> Option.value_exn x)
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

(* TODO let target = (Sys.get_argv ()).(1) *)
let target = Set.choose_exn all_words

let rec main state =
  let guess = decide_guess state in
  let result = check_guess ~target ~guess in
  print_guess (guess, result);
  if List.for_all result ~f:((=) Green) then
    printf "Finished after %d guesses.\n" (List.length state + 1)
  else
    main ((guess, result) :: state)

let () = main []
