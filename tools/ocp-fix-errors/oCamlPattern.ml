
type constant =
    Const_int of int
  | Const_char of char
  | Const_string of string
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint

type longident = string list

type pattern =
    Ppat_any
  | Ppat_var of string
  | Ppat_alias of pattern * string

  | Ppat_constant of constant

  | Ppat_tuple of pattern list
  | Ppat_construct of longident * pattern option * bool
  | Ppat_variant of string * pattern option
  | Ppat_record of (longident * pattern) list * bool (* true if closed *)
  | Ppat_array of pattern list
  | Ppat_or of pattern * pattern
(*  | Ppat_constraint of pattern * core_type *)
  | Ppat_type of longident
  | Ppat_lazy of pattern
  | Ppat_unpack of string

open OcpLang

let rec flatten_pattern p =
    match p with
        Ppat_or (p1, p2) -> flatten_pattern p1 @ flatten_pattern p2
      | p -> [p]

let string_of_pattern indent pattern =

    let b = Buffer.create 100 in
    let rec buf_pattern b inside_p (* indent_p *) indent pattern =
      match pattern with
          Ppat_or (p1, p2) ->
            let patterns = flatten_pattern pattern in
            if inside_p then begin
              (*              if indent_p then Buffer.add_string b indent; *)
              Buffer.add_string b "(\n";
              let indent = indent ^ "  " in
              OcpList.iteri (fun i pat ->
                if i > 0 then
                  Printf.bprintf b "%s |" indent
                else
                  Printf.bprintf b "%s  " indent;
                buf_pattern b false  (* false *) indent pat;
                Buffer.add_char b '\n'
              ) patterns;
              Buffer.add_string b indent;
              Buffer.add_string b ")\n"
            end else begin
              OcpList.iteri (fun i pat ->
                if i > 0 then Printf.bprintf b "%s|" indent;
                buf_pattern b false (* false *) indent pat;
                Buffer.add_char b '\n'
              ) patterns;
            end
        | Ppat_any ->
          (*          if indent_p then  Buffer.add_string b indent; *)
          Buffer.add_string b " _ "
        | Ppat_construct (lident, None, _) ->
          (*          if indent_p then  Buffer.add_string b indent; *)
          Printf.bprintf b " %s " (String.concat "." lident)
        | Ppat_construct (lident, Some arg, _) ->
          (*          if indent_p then  Buffer.add_string b indent; *)
          if inside_p then Printf.bprintf b "(";
          Printf.bprintf b " %s " (String.concat "." lident);
          buf_pattern b true (* false *) indent arg;
          if inside_p then Printf.bprintf b ")";
        | Ppat_tuple patterns ->
          (*          if indent_p then Buffer.add_string b indent; *)
          Buffer.add_string b "(";
          OcpList.iteri (fun i pat ->
            if i > 0 then
              Buffer.add_string b ", ";
            buf_pattern b true (* false *) indent pat;
          ) patterns;
          Buffer.add_string b ")"
        | Ppat_record (patterns, is_closed) ->
          (*          if indent_p then Buffer.add_string b indent; *)
          Buffer.add_string b " {";
          List.iteri (fun i (name, pat) ->
            let name = String.concat "." name in
            if i > 0 then
              Printf.bprintf b "; %s = " name
            else
              Printf.bprintf b " %s = " name
            ;
            buf_pattern b false (* false *) indent pat;
          ) patterns;
          Buffer.add_string b "}"
        |  Ppat_constant  cst ->
          begin match cst with
            | Const_int n -> Printf.bprintf b "%d" n
            | Const_nativeint  n   -> Printf.bprintf b "%ndn" n
            |  Const_int64  n  -> Printf.bprintf b "%Ld" n
            |  Const_int32  n  -> Printf.bprintf b "%ld" n
            |  Const_float  n  -> Printf.bprintf b "%s" n
            |  Const_string s  -> Printf.bprintf b "\"%s\"" (String.escaped s)
            |  Const_char c  -> Printf.bprintf b "'%c'" c
          end

        |  Ppat_unpack  _
        |  Ppat_lazy  _
        |  Ppat_type  _
        |  Ppat_array  _
        |  Ppat_variant ( _ ,  _ )
        |  Ppat_alias ( _ ,  _ )
        |  Ppat_var  _
          -> assert false

    in
    buf_pattern b false (* true *) (String.make indent ' ') pattern;
    Buffer.contents b

let rec explose_or_pattern pat =

  match pat with
      Ppat_any
    | Ppat_var _
    | Ppat_alias _
    | Ppat_constant _
    | Ppat_variant (_, None)
    | Ppat_type _
    | Ppat_lazy _
    | Ppat_unpack _
    | Ppat_construct (_, None, _)
        -> [ pat ]

    | Ppat_construct (lid, Some pat, closed) ->
      List.map (fun pat -> Ppat_construct (lid, Some pat, closed))
        (explose_or_pattern pat)
    | Ppat_variant (variant, Some pat) ->
      List.map (fun pat -> Ppat_variant (variant, Some pat))
        (explose_or_pattern pat)

    | Ppat_record (lid_pats, closed) ->
      List.map (fun pats -> Ppat_record (pats, closed))
        (List.fold_left (fun  pats (lid,pat) ->
          let new_pats = explose_or_pattern pat in
          List.flatten (
            List.map (fun new_pat ->
              List.map (fun old_pats -> (lid,new_pat) :: old_pats) pats
            ) new_pats)
         ) [[]] lid_pats )
    | Ppat_tuple pats ->
      List.map (fun pats -> Ppat_tuple pats)
        (List.fold_left (fun pats pat ->
          let new_pats = explose_or_pattern pat in
          List.flatten (
            List.map (fun new_pat ->
              List.map (fun old_pats -> new_pat :: old_pats) pats) new_pats)
         ) [[]] pats)
    | Ppat_array pats ->
      List.map (fun pats -> Ppat_array pats)
        (List.fold_left (fun pats pat ->
          let new_pats = explose_or_pattern pat in
          List.flatten (
            List.map (fun new_pat ->
              List.map (fun old_pats -> new_pat :: old_pats) pats) new_pats)
         ) [[]] pats)

    | Ppat_or (_, _) -> flatten_pattern pat
  (*  | Ppat_constraint of pattern * core_type *)

let rec implose_or_pattern list =
  match list with
      [] -> assert false
    | [ pat ] -> pat
    | p1 :: tail ->
      Ppat_or (p1, implose_or_pattern tail)
