open! Base

module Expect_line = struct
  type t =
    | Literal of string
    | Regexp of string
    | Glob of string
    | Escaped of string
  [@@deriving variants]
end

let is_only_whitespace string = String.for_all ~f:Char.is_whitespace string

let count_leading_spaces string =
  String.lfindi string ~f:(fun _ c -> not (Char.is_whitespace c))
;;

let extract_text_block string =
  let no_vertical_padding =
    String.split string ~on:'\n'
    |> List.drop_while ~f:is_only_whitespace
    |> List.rev
    |> List.drop_while ~f:is_only_whitespace
    |> List.rev
  in
  match List.is_empty no_vertical_padding with
  | true -> []
  | false ->
    let leading_spaces =
      List.filter_map no_vertical_padding ~f:count_leading_spaces
      |> List.min_elt ~compare:Int.compare
      |> Option.value_exn
    in
    List.map no_vertical_padding ~f:(fun line ->
      String.rstrip (String.drop_prefix line leading_spaces))
;;

let make_expect_lines expect =
  let lines = extract_text_block expect in
  List.map lines ~f:(fun line ->
    let suffixes =
      [ "(literal)", Expect_line.literal
      ; "(regexp)", Expect_line.regexp
      ; "(glob)", Expect_line.glob
      ; "(escaped)", Expect_line.escaped
      ]
    in
    List.find_map suffixes ~f:(fun (suffix, constructor) ->
      Option.map (String.chop_suffix line ~suffix) ~f:(fun remaining ->
        constructor (String.rstrip remaining)))
    |> Option.value ~default:(Expect_line.Literal line))
;;

let make_actual_lines actual = extract_text_block actual
let matches_regexp ~(pat : Re.t) s = Re.execp (Re.compile (Re.whole_string pat)) s
let glob = Re.Glob.glob ~anchored:true ~pathname:false ~expand_braces:true

let match_lines actual_lines expect_lines =
  match
    List.for_all2 actual_lines expect_lines ~f:(fun actual expect ->
      match (expect : Expect_line.t) with
      | Literal expect -> String.equal expect actual
      | Glob expect -> matches_regexp ~pat:(glob expect) actual
      | Regexp expect -> matches_regexp ~pat:(Re.Emacs.re expect) actual
      | Escaped expect -> String.equal expect (String.escaped actual))
  with
  | Ok all_match -> all_match
  | Unequal_lengths -> false
;;

let require_match ?cr ~(here : [%call_pos]) expect =
  let actual = Expect_test_helpers_base.expect_test_output ~here () in
  let actual_lines = make_actual_lines actual in
  let expect_lines = make_expect_lines expect in
  if not (match_lines actual_lines expect_lines)
  then (
    Expect_test_helpers_base.print_cr
      ?cr
      ~here
      (Atom "output does not match pattern; actual output follows:");
    Expect_test_helpers_base.print_endline "";
    List.iter actual_lines ~f:Expect_test_helpers_base.print_endline)
;;
