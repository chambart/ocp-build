(*
TODO: currently, trying to fix several errors in the same file will
fail, as the next errors have incorrect locations since we fixed the file.

We should probably check if the next lines contain an error in the same
file, in which case we should move Emacs error pointer forward.

Another solution would be to Sha1 the compilation buffer, and if we have to
fix the same file with the same compilation buffer, we should stop.
*)

open ErrorLocation

let rec fix loc dirname next_lines =

  let abs_begin_pos = loc.loc_begin_pos in

  let replace = loc.loc_file, abs_begin_pos, abs_begin_pos, "_" in
  replace ::
  match next_lines with
    |  location_line :: error_line :: next_lines when
        OcpString.starts_with error_line
          "Warning 26: unused variable"
        ||
          OcpString.starts_with error_line
          "Warning 27: unused variable"
        ->
      let loc = ErrorLocation.parse_location
        dirname location_line in
      fix loc dirname next_lines

    | _ -> []

let fix loc dirname next_lines =
  let fix = fix loc dirname next_lines in
  fix,
  Printf.sprintf "Underscored %d unused variable(s)" (List.length fix)
