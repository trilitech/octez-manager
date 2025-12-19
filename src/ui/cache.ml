(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Centralized cache management for form validators and UI data.

    Caches are registered globally so they can be invalidated together
    (e.g., from diagnostics page). *)

(** {1 Cache Registry} *)

type sub_entry = {key : string; age : float; expired : bool}

type cache_entry = {
  name : string;
  invalidate : unit -> unit;
  get_age : unit -> float option;
      (* seconds since last refresh, None if empty *)
  get_ttl : unit -> float;
  get_sub_entries : unit -> sub_entry list; (* for keyed caches *)
  mutable hits : int;
  mutable misses : int;
}

let registry : cache_entry list ref = ref []

let register ~name ~invalidate ~get_age ~get_ttl ~get_sub_entries =
  registry :=
    {name; invalidate; get_age; get_ttl; get_sub_entries; hits = 0; misses = 0}
    :: !registry

let invalidate_all () = List.iter (fun e -> e.invalidate ()) !registry

let get_stats () =
  !registry
  |> List.map (fun e ->
      let age = e.get_age () in
      let ttl = e.get_ttl () in
      let expired = match age with Some a -> a > ttl | None -> false in
      let sub_entries = e.get_sub_entries () in
      (e.name, e.hits, e.misses, age, ttl, expired, sub_entries))

let reset_stats () =
  List.iter
    (fun e ->
      e.hits <- 0 ;
      e.misses <- 0)
    !registry

(** {1 Generic TTL Cache} *)

type 'a t = {
  name : string; [@warning "-69"]
  mutable value : 'a option;
  mutable cache_time : float;
  ttl : float;
  fetch : unit -> 'a;
  mutable entry : cache_entry option;
}

let create ~name ~ttl fetch =
  let cache =
    {name; value = None; cache_time = 0.0; ttl; fetch; entry = None}
  in
  let invalidate () =
    cache.value <- None ;
    cache.cache_time <- 0.0
  in
  let get_age () =
    if Option.is_none cache.value then None
    else Some (Unix.gettimeofday () -. cache.cache_time)
  in
  let get_ttl () = cache.ttl in
  let get_sub_entries () = [] in
  (* simple cache has no sub-entries *)
  register ~name ~invalidate ~get_age ~get_ttl ~get_sub_entries ;
  (* Store entry reference for stats *)
  let entries = !registry in
  cache.entry <- List.find_opt (fun (e : cache_entry) -> e.name = name) entries ;
  cache

let get cache =
  let now = Unix.gettimeofday () in
  let expired = now -. cache.cache_time > cache.ttl in
  match (cache.value, expired) with
  | Some v, false ->
      (match cache.entry with Some e -> e.hits <- e.hits + 1 | None -> ()) ;
      v
  | _ ->
      (match cache.entry with Some e -> e.misses <- e.misses + 1 | None -> ()) ;
      let v = cache.fetch () in
      cache.value <- Some v ;
      cache.cache_time <- now ;
      v

let invalidate cache =
  cache.value <- None ;
  cache.cache_time <- 0.0

(** {1 Generic TTL Cache with Result} *)

type 'a result_cache = {
  name : string; [@warning "-69"]
  mutable value : 'a option;
  mutable cache_time : float;
  ttl : float;
  fetch : unit -> ('a, string) result;
  mutable entry : cache_entry option;
}

let create_result ~name ~ttl fetch =
  let cache =
    {name; value = None; cache_time = 0.0; ttl; fetch; entry = None}
  in
  let invalidate () =
    cache.value <- None ;
    cache.cache_time <- 0.0
  in
  let get_age () =
    if Option.is_none cache.value then None
    else Some (Unix.gettimeofday () -. cache.cache_time)
  in
  let get_ttl () = cache.ttl in
  let get_sub_entries () = [] in
  (* result cache has no sub-entries *)
  register ~name ~invalidate ~get_age ~get_ttl ~get_sub_entries ;
  let entries = !registry in
  cache.entry <- List.find_opt (fun (e : cache_entry) -> e.name = name) entries ;
  cache

let get_result cache =
  let now = Unix.gettimeofday () in
  let expired = now -. cache.cache_time > cache.ttl in
  match (cache.value, expired) with
  | Some v, false ->
      (match cache.entry with Some e -> e.hits <- e.hits + 1 | None -> ()) ;
      Ok v
  | _ -> (
      (match cache.entry with Some e -> e.misses <- e.misses + 1 | None -> ()) ;
      match cache.fetch () with
      | Ok v ->
          cache.value <- Some v ;
          cache.cache_time <- now ;
          Ok v
      | Error _ as e -> e)

let invalidate_result cache =
  cache.value <- None ;
  cache.cache_time <- 0.0

(** {1 Keyed Cache (for per-value caching like user validation)} *)

type ('k, 'v) keyed_cache = {
  name : string; [@warning "-69"]
  table : ('k, 'v) Hashtbl.t;
  mutable cache_time : float;
  ttl : float;
  fetch : 'k -> 'v;
  mutable entry : cache_entry option;
}

let create_keyed ~name ~ttl fetch =
  let cache =
    {
      name;
      table = Hashtbl.create 17;
      cache_time = Unix.gettimeofday ();
      ttl;
      fetch;
      entry = None;
    }
  in
  let invalidate () =
    Hashtbl.clear cache.table ;
    cache.cache_time <- Unix.gettimeofday ()
  in
  let get_age () =
    if Hashtbl.length cache.table = 0 then None
    else Some (Unix.gettimeofday () -. cache.cache_time)
  in
  let get_ttl () = cache.ttl in
  let get_sub_entries () = [] in
  (* keyed cache expires all at once, no per-key info *)
  register ~name ~invalidate ~get_age ~get_ttl ~get_sub_entries ;
  let entries = !registry in
  cache.entry <- List.find_opt (fun (e : cache_entry) -> e.name = name) entries ;
  cache

let get_keyed cache key =
  let now = Unix.gettimeofday () in
  (* Invalidate entire table if TTL expired *)
  if now -. cache.cache_time > cache.ttl then (
    Hashtbl.clear cache.table ;
    cache.cache_time <- now) ;
  match Hashtbl.find_opt cache.table key with
  | Some v ->
      (match cache.entry with Some e -> e.hits <- e.hits + 1 | None -> ()) ;
      v
  | None ->
      (match cache.entry with Some e -> e.misses <- e.misses + 1 | None -> ()) ;
      let v = cache.fetch key in
      Hashtbl.replace cache.table key v ;
      v

let invalidate_keyed cache =
  Hashtbl.clear cache.table ;
  cache.cache_time <- Unix.gettimeofday ()

(** {1 Thread-safe Keyed Cache with Per-Key TTL}

    For caches that need mutex protection and per-key expiration. *)

(** GADT for key serialization - string keys are the common case *)
type 'k key_serializer =
  | String_key : string key_serializer
  | Custom_key : ('k -> string) -> 'k key_serializer

let key_to_string : type k. k key_serializer -> k -> string = function
  | String_key -> Fun.id
  | Custom_key f -> f

type ('k, 'v) safe_keyed_cache = {
  name : string; [@warning "-69"]
  table : ('k, 'v * float) Hashtbl.t; (* value * timestamp *)
  lock : Mutex.t;
  ttl : float;
  key_ser : 'k key_serializer;
  mutable entry : cache_entry option;
}

let create_safe_keyed_with ~key_ser ~name ~ttl () =
  let cache =
    {
      name;
      table = Hashtbl.create 31;
      lock = Mutex.create ();
      ttl;
      key_ser;
      entry = None;
    }
  in
  let invalidate () =
    Mutex.lock cache.lock ;
    Hashtbl.clear cache.table ;
    Mutex.unlock cache.lock
  in
  let get_age () =
    Mutex.lock cache.lock ;
    let oldest =
      Hashtbl.fold
        (fun _ (_, t) acc ->
          match acc with None -> Some t | Some a -> Some (min a t))
        cache.table
        None
    in
    Mutex.unlock cache.lock ;
    match oldest with None -> None | Some t -> Some (Unix.gettimeofday () -. t)
  in
  let get_ttl () = cache.ttl in
  let get_sub_entries () =
    let now = Unix.gettimeofday () in
    Mutex.lock cache.lock ;
    let entries =
      Hashtbl.fold
        (fun k (_, t) acc ->
          let age = now -. t in
          let expired = age > cache.ttl in
          {key = key_to_string cache.key_ser k; age; expired} :: acc)
        cache.table
        []
    in
    Mutex.unlock cache.lock ;
    List.sort (fun a b -> String.compare a.key b.key) entries
  in
  register ~name ~invalidate ~get_age ~get_ttl ~get_sub_entries ;
  let entries = !registry in
  cache.entry <- List.find_opt (fun (e : cache_entry) -> e.name = name) entries ;
  cache

(** Create a string-keyed cache (most common case) *)
let create_safe_keyed ~name ~ttl () =
  create_safe_keyed_with ~key_ser:String_key ~name ~ttl ()

(** Create a cache with custom key type *)
let create_safe_keyed_custom ~key_to_string ~name ~ttl () =
  create_safe_keyed_with ~key_ser:(Custom_key key_to_string) ~name ~ttl ()

let get_safe_keyed cache key ~fetch =
  let now = Unix.gettimeofday () in
  Mutex.lock cache.lock ;
  let cached =
    match Hashtbl.find_opt cache.table key with
    | Some (v, t) when now -. t < cache.ttl -> Some v
    | _ -> None
  in
  Mutex.unlock cache.lock ;
  match cached with
  | Some v ->
      (match cache.entry with Some e -> e.hits <- e.hits + 1 | None -> ()) ;
      v
  | None ->
      (match cache.entry with Some e -> e.misses <- e.misses + 1 | None -> ()) ;
      let v = fetch () in
      Mutex.lock cache.lock ;
      Hashtbl.replace cache.table key (v, now) ;
      Mutex.unlock cache.lock ;
      v

let set_safe_keyed cache key value =
  let now = Unix.gettimeofday () in
  Mutex.lock cache.lock ;
  Hashtbl.replace cache.table key (value, now) ;
  Mutex.unlock cache.lock

let get_safe_keyed_cached cache key =
  Mutex.lock cache.lock ;
  let v =
    match Hashtbl.find_opt cache.table key with
    | Some (v, _) -> Some v
    | None -> None
  in
  Mutex.unlock cache.lock ;
  v

let remove_safe_keyed cache key =
  Mutex.lock cache.lock ;
  Hashtbl.remove cache.table key ;
  Mutex.unlock cache.lock

let invalidate_safe_keyed cache =
  Mutex.lock cache.lock ;
  Hashtbl.clear cache.table ;
  Mutex.unlock cache.lock
