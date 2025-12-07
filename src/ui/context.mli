val set_pending_instance_detail : string -> unit

val take_pending_instance_detail : unit -> string option

val mark_instances_dirty : unit -> unit

val consume_instances_dirty : unit -> bool

val navigate : string -> unit

val consume_navigation : unit -> string option
