let pending_instance_detail : string option ref = ref None

let instances_dirty = ref false

let set_pending_instance_detail inst = pending_instance_detail := Some inst

let take_pending_instance_detail () =
  let value = !pending_instance_detail in
  pending_instance_detail := None ;
  value

let mark_instances_dirty () = instances_dirty := true

let consume_instances_dirty () =
  let flag = !instances_dirty in
  instances_dirty := false ;
  flag

let pending_navigation : string option ref = ref None

let navigate page = pending_navigation := Some page

let consume_navigation () =
  let value = !pending_navigation in
  pending_navigation := None ;
  value
