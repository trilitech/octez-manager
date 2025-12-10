type rpc_metrics = {
  chain_id : string option;
  head_level : int option;
  bootstrapped : bool option;
  last_rpc_refresh : float option;
  node_version : string option;
  data_size : string option;
  proto : string option;
  last_error : string option;
  last_block_time : float option;
}

let table : (string, rpc_metrics) Hashtbl.t = Hashtbl.create 17

let lock = Mutex.create ()

let set ~instance v =
  Mutex.lock lock ;
  try
    Hashtbl.replace table instance v ;
    Mutex.unlock lock
  with e ->
    Mutex.unlock lock ;
    raise e

let get ~instance =
  Mutex.lock lock ;
  try
    let v = Hashtbl.find_opt table instance in
    Mutex.unlock lock ;
    v
  with e ->
    Mutex.unlock lock ;
    raise e

let clear () =
  Mutex.lock lock ;
  try
    Hashtbl.clear table ;
    Mutex.unlock lock
  with e ->
    Mutex.unlock lock ;
    raise e
