type rpc_metrics = {
  chain_id : string option;
  head_level : int option;
  bootstrapped : bool option;
  last_rpc_refresh : float option;
  node_version : string option;
  data_size : string option;
  proto : string option;
  last_error : string option;
}

val set : instance:string -> rpc_metrics -> unit

val get : instance:string -> rpc_metrics option

val clear : unit -> unit
