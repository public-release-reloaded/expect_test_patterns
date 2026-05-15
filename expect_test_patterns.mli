open! Base
open Expect_test_helpers_base

(** Prints a cr unless the given pattern matches [expect_test_output here]. The pattern is
    interpreted based on several tags that may appear at the end of a line.

    [(regexp)]-tagged lines are interpreted as regular expressions. [(glob)]-tagged lines
    are intepreted as globs. [(literal)] lines are interpreted literally. [(escaped)]
    lines are interpreted as escaped literal strings.

    For example, the following pattern:

    {[
      {|
         here is the first line of *.txt (glob)
         [0-9a-f]* (regexp)
         it matches a hex regular expression (regexp) (literal)
         this output ends with a null\000 (escaped)
      |}
    ]}

    ...would match an output string like the following:

    {[
      "here is the first line of .987654.tmp.txt\n\
       6ec772595f0b4d8384a1fa5b36852a48\n\
       it matches a hex regular expression (regexp)\n\
       this output ends with a null\000"
    ]} *)
val require_match : ?cr:CR.t -> ?here:Stdlib.Lexing.position -> string -> unit
