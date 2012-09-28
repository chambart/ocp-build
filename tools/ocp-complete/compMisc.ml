


let rec find_obuild f dir =
  let possible_dir = Filename.concat dir "_obuild" in
  if Sys.file_exists possible_dir then
    f possible_dir
  else
    let new_dir = Filename.dirname dir in
    if dir <> new_dir then find_obuild f new_dir
