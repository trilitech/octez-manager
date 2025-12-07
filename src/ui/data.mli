open Octez_manager_lib

module Service_state : sig
  type status = Running | Stopped | Unknown of string

  type t = {
    service : Service.t;
    enabled : bool option;
    active : bool option;
    status : status;
    status_text : string option;
  }

  val status_label : t -> string
end

module Summary : sig
  type t = {total : int; running : int; stopped : int; unknown : int}
end

val load_service_states : ?detail:bool -> unit -> Service_state.t list

val summarize : Service_state.t list -> Summary.t

val diagnostics_lines : Service_state.t list -> string list

val activity_lines : Service_state.t list -> string list

val formatted_timestamp : float -> string

val spotlight_lines : Service_state.t list -> limit:int -> string list
