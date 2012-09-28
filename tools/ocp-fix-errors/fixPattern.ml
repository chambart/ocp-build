(*
  - Add some missing patterns to a non-exhaustive pattern (Warning)
*)


open OcpSystem
open OcpLang
include Debug.Tag(struct let tag = "fixPattern" end)

module OCamlPatternParser : sig

  val parse_pattern : string -> OCamlPattern.pattern

end = struct

  module Lexer = Approx_lexer.Make(OCamlPatternParser)
  module StringOfToken = Approx_tokens.StringOfToken(OCamlPatternParser)

  let token lexbuf =
    let token = Lexer.token lexbuf in
    debug "APPROX-TOKEN[%s]\n%!" (StringOfToken.string_of_token token);
    token

  let parse_pattern s =
    debug "Parsing token...\n%!";
    let lexbuf = Lexing.from_string s in
    OCamlPatternParser.pattern token lexbuf

end

let no_other_indented_line lines =
  match lines with
      line :: _ when FixUtils.find_indent line > 0 -> false
    | _ -> true

let rec find_pattern open_parens pattern_lines next_lines =
  match next_lines with
      [] -> raise Parsing.Parse_error
    | line :: next_lines ->
      let open_parens = ref open_parens in
      let close_parens = ref 0 in
      String.iter (fun c ->
        match c with
            '(' | '{' | '['  -> incr open_parens
          | ')' | '}' | ']'  -> incr close_parens
          | _ -> ()
      ) line;
      if !open_parens = !close_parens && no_other_indented_line next_lines then
        (next_lines, List.rev (line :: pattern_lines))
      else
        find_pattern (!open_parens - !close_parens) (line :: pattern_lines) next_lines

(*
let clean_pattern s =
  let len = String.length s in
  let first_paren = ref max_int in
  let last_paren = ref 0 in
  for i = 0 to len - 1 do
    match s.[i] with
        '(' -> if !first_paren > i then first_paren := i
      | ')' -> last_paren := i
      | '\r' | '\t' -> s.[i] <- ' '
      | _ -> ()
  done;
  String.sub s (!first_paren+1) (!last_paren - !first_paren - 1)
*)

(*
module Test = struct
type t = Aaaaaaaaa | Bbbbbbbbbbbb | C of t | D of int | Eeeeeee | Fffffffff | Gggggggg | Hhhhhhh | Iiiiiiiii | Jjjjjjjjjjjjj

let _ =
  let a =
    match raise Not_found with
        Aaaaaaaaa -> ()
      | C Aaaaaaaaa -> ()
      | D _
      | Jjjjjjjjjjjjj
      | Iiiiiiiii
      | Hhhhhhh
      | Gggggggg
      | Fffffffff
      | Eeeeeee
      | Bbbbbbbbbbbb -> assert false

  in
  a
end
*)
open ErrorLocation

let fix loc pattern_lines =
  let file = loc.loc_file in
  let (next_linefs, pattern_lines) = find_pattern 0 [] pattern_lines in
  let pattern = String.concat " " pattern_lines in
  debug "pattern: [%s]\n%!" pattern;
  debug "Filename: %s\n%!"
    (File.to_string loc.loc_file.file_file);
  let content_size = String.length loc.loc_file.file_content in
  let indent = FixUtils.find_indent
    (String.sub loc.loc_file.file_content
       loc.loc_bol (content_size - loc.loc_bol)) in

  let pattern = OCamlPatternParser.parse_pattern pattern in
  let patterns = OCamlPattern.explose_or_pattern pattern in
  let pattern = OCamlPattern.implose_or_pattern patterns in
  let pattern = OCamlPattern.string_of_pattern indent pattern in
  let indent = String.make indent ' ' in
  let pattern = "\n" ^ indent ^ "  |" ^ pattern ^ indent  ^  " -> assert false" in
  debug "Insert in file %s at pos %d\n[%s]\n"
   (File.to_string file.file_file) loc.loc_end_pos pattern;
  [loc.loc_file, loc.loc_end_pos, loc.loc_end_pos, pattern],
  "Inserted stubs for missing match cases"
