(*
- Put a log file in ~/.ocp/ocp-fix-errors/edit.log
- When reading a compilation buffer, compute the buffer identifier
   (Sha1.string file)
- When reading a new file, copy it under its sha1 in
     ~/.ocp/ocp-fix-errors/cache/SHA1(compilation-buffer)-SHA1(file)
*)


open Genlex
include Debug.Tag(struct let tag = "errorLocation" end)

type edit_op =
    Insertion of (*pos: *) int * (*len: *) int
  | Deletion of (*pos: *) int * (*len: *) int

type file = {
  file_file : File.t;
  file_content : string;
  mutable file_ops : edit_op list;
}

type loc = {
  loc_file : file;
  loc_bol : int;
  loc_line_pos : int;
  loc_begin_pos : int;
  loc_end_pos : int;
}

let files = ref StringMap.empty

let reset () = files := StringMap.empty

let lexer = Genlex.make_lexer [ "," ]

let find_file filename =
  try
    StringMap.find filename !files
  with Not_found ->
    let file = File.of_string filename in
    let f = {
      file_file = file;
      file_content = File.X.read_to_string file;
      file_ops = [];
    } in
    files := StringMap.add filename f !files;
    f

let parse_location dirname line =
  let tokens = OcpGenlex.tokens_of_string lexer line in
  List.iter (fun token -> debug "error?[%s]\n%!" (OcpGenlex.string_of_token token)) tokens;
  match tokens with
    | [Ident "File"; String filename ;
       Kwd ","; Ident "line"; Int line_pos; Kwd ",";
       Ident "characters"; Int begin_pos; Int end_pos; Ident ":"] ->
      let filename = if Filename.is_relative filename then
          Filename.concat dirname filename
        else filename
      in
      debug "FOUND Error !\n%!";
      let file = find_file filename in
      let bol = FixUtils.find_line file.file_content 0 line_pos in
      {
        loc_file = file;
        loc_bol = bol;
        loc_line_pos = line_pos;
        loc_begin_pos = bol + begin_pos;
        loc_end_pos = bol - end_pos;
      }
    | _ -> raise Parsing.Parse_error

let final_pos file pos =
  let rec iter_ops pos ops =
    match ops with
        [] -> pos
      | op :: ops ->
        let pos = iter_ops pos ops in
        match op with
            Insertion (pos', len) ->
              debug "insert (%d,%d)\n%!" pos' len;
              if pos >= pos' then pos + len else pos
          | Deletion (pos', len) ->
            debug "delete (%d,%d)\n%!" pos' len;
            if pos > pos' then pos - len else pos
  in
  let new_pos = iter_ops pos file.file_ops in
  debug "moved from %d to %d\n%!" pos new_pos;
  new_pos

let add_edition file op =
  file.file_ops <- op :: file.file_ops

