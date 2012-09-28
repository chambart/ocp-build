(* This module implements a simple algorithm for substituting any
   string in any string. If several strings can be substituted at the
   same place, the one starting at the earliest position is chosen,
   and among different strings at the same position, the longest match
   is chosen.

   The current worst complexity is O(N.M) where N is the length of the
   argument string and M is the longest string that can be
   substituted. We should probably implement KMP at some point for
   applications where performance matters.
*)


type subst

val empty_subst : unit -> subst
val add_to_subst : subst -> string -> string -> unit
val subst_of_list : (string * string) list -> subst

val subst : subst -> string -> int * string
val iter_subst : subst -> string -> int * string
