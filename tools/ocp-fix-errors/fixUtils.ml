
let rec find_line content pos line_pos =
  if line_pos = 1 then pos else
    match content.[pos] with
        '\n' -> find_line content (pos+1) (line_pos - 1)
      | _ -> find_line content (pos+1) line_pos

let find_indent s =
  let rec find_indent s i len =
    if i < len && s.[i] = ' ' then
      find_indent s (i+1) len
    else i
  in
  find_indent s 0 (String.length s)

let rec find_indented found_lines next_lines =
  match next_lines with
      [] | "" :: _ -> List.rev found_lines
    | line :: tail when
        (match line.[0] with '\t' |  ' ' -> true | _ -> false) ->
      find_indented (line :: found_lines) tail
    | _ -> List.rev found_lines
