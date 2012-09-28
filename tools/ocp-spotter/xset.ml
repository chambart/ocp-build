(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2012 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

(* Set with binary custom search *)

module type OrderedType = Set.OrderedType

module type S = sig
  include Set.S
  val unsafe_binary : t -> (t * elt * t) option
  val unsafe_middle : t -> elt option
  val unsafe_find : elt -> t -> elt option
end

module Make(Ord : OrderedType) : S with type elt = Ord.t = struct
  include Set.Make(Ord)

  (* Warning! Unsafe operation!
     Currently, there is no easy way to get the binary tree structure from a set,
     though it is very handy for efficient binary search.
     The following easily breaks when the internal implementation of Set.t is changed.
     If the program
  *)
  type t_internal = Empty | Node of t * elt * t * int
  let unsafe_internal t = (Obj.magic t : t_internal)

  let _dummy () =  [Empty ; Node (assert false, assert false, assert false, 0)]

  let unsafe_binary t = match unsafe_internal t with
    | Empty -> None
    | Node (left, v, right, _) -> Some (left, v, right)

  let unsafe_middle t = match unsafe_internal t with
    | Empty -> None
    | Node (_, v, _, _) -> Some v

  let rec unsafe_find elt t = match unsafe_internal t with
    | Empty -> None
    | Node (left, elt', right, _) ->
	match Ord.compare elt elt' with
	| 0  -> Some elt'
	| -1 -> unsafe_find elt left
	| 1  -> unsafe_find elt right
	| _  -> assert false
end
