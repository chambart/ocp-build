
exception CyclicGraph

(** Abstract type for a node *)
type node

(** Node creation *)
val new_node : unit -> node

module Make :
  functor
    (M : sig
      type t
      val node : t -> node
      val iter_edges : (t -> unit) -> t -> unit
      val name : t -> string
    end) ->
      sig
        val sort : M.t list -> M.t list
      end
