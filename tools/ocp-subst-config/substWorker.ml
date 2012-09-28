(*
  Substitute strings from a config file.
*)

open StringSubst

let config_file_arg = ref "variables.config"
let output_arg = ref "-"

let rec find_config_file dirname basename =
  let filename = Filename.concat dirname basename in
  if Sys.file_exists filename then filename else
    let new_dirname = Filename.dirname dirname in
    if new_dirname <> dirname then
      find_config_file new_dirname basename
    else begin
      Printf.fprintf stderr "Could not find config file %s\n%!" basename;
      exit 2
    end

let load_config_file config_file =
  (*  Printf.fprintf stderr "load_config_file %s\n" config_file; *)
  let c = empty_subst () in
  File.iter_lines (fun line ->
    if String.length line > 0 && line.[0] <> '#' then
      let (key, v) = OcpString.cut_at line '=' in
    (*    Printf.fprintf stderr "[%s] = [%s]\n" key v; *)
      add_to_subst c key v
  ) config_file;
  c

let subst_in_file recursive config source_file dest_file =
  let s = File.string_of_file source_file in
  let nsubst,s =
     if recursive then
       iter_subst config s
     else
       subst config s
  in
  begin match dest_file with
    None ->
      Printf.printf "%s%!" s
   | Some dest_file ->
    let oc = open_out dest_file in
    output_string oc s;
    close_out oc
  end;
  nsubst

let arg_anon filename =
  let config_file =
      if Filename.is_implicit !config_file_arg then
        find_config_file
          (Filename.dirname filename) !config_file_arg
      else !config_file_arg in
  let config = load_config_file config_file in
  let dest_file =
    if !output_arg = "-" then None
    else
      if !output_arg = "-in" then
        if Filename.check_suffix filename ".in" then
          Some (Filename.chop_suffix filename ".in")
        else begin
          Printf.fprintf stderr
            "Filename %s has no .in suffix (use -o)\n%!" filename;
          exit 2
        end
      else
        Some !output_arg
  in
  ignore (subst_in_file true config filename dest_file)

let arg_list = [
  "-config-file", Arg.String ( (:=) config_file_arg ), " <config> : name of config file";
  "-o", Arg.String ( (:=) output_arg ), " <filename> : name of file to generate (- for stdout)";
  "-subst", Arg.String arg_anon, " <file> : substitute in file";
]

let arg_usage = Printf.sprintf "%s [OPTIONS] files" Sys.argv.(0)
