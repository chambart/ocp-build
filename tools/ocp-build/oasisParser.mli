
type oasis_line = Line of string * oasis_line list ref

val parse_oasis : oasis_line list -> unit

val read_oasis : string -> oasis_line list
val print_oasis : out_channel -> oasis_line list -> unit
