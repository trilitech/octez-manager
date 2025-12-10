val set_pending_instance_detail : string -> unit

val take_pending_instance_detail : unit -> string option

val mark_instances_dirty : unit -> unit

val consume_instances_dirty : unit -> bool

val navigate : string -> unit

val consume_navigation : unit -> string option

(** Toast notifications *)
val toast_info : string -> unit

val toast_success : string -> unit

val toast_warn : string -> unit

val toast_error : string -> unit

val tick_toasts : unit -> unit

val render_toasts : cols:int -> string

(** Spinner for loading states *)
val tick_spinner : unit -> unit

val render_spinner : string -> string

(** Progress bar for long-running tasks *)
val progress_start : label:string -> estimate_secs:float -> width:int -> unit

val progress_finish : unit -> unit

val progress_set : ?label:string -> progress:float -> unit -> unit

val render_progress : cols:int -> string
