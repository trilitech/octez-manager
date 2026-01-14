
type t = Stdlib.Random.State.t Stdlib.Domain.DLS.key

module Repr = struct
  open Stdlib.Bigarray

  type t = (int64, int64_elt, c_layout) Array1.t

  let of_state : Stdlib.Random.State.t -> t = Stdlib.Obj.magic
end

let assign t state =
  let dst = Repr.of_state (Stdlib.Domain.DLS.get t) in
  let src = Repr.of_state state in
  Stdlib.Bigarray.Array1.blit src dst
;;

let make state =
  let split_from_parent v = Stdlib.Random.State.split v in
  let t = Stdlib.Domain.DLS.new_key ~split_from_parent (fun () -> state) in
  Stdlib.Domain.DLS.get t |> ignore;
  t
;;

let make_lazy ~f =
  let split_from_parent v = Stdlib.Random.State.split v in
  Stdlib.Domain.DLS.new_key ~split_from_parent f
;;

let[@inline always] get_state t = Stdlib.Domain.DLS.get t
