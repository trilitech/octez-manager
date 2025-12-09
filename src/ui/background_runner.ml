(* Background runner backed by an Eio stream. Producers enqueue tasks from any
   thread; a dedicated Eio runtime hosts worker fibers that execute tasks in
   separate domains to keep the UI responsive. *)

type task = unit -> unit

let stream : task Eio.Stream.t Lazy.t = lazy (Eio.Stream.create 1024)

let started = Atomic.make false

let num_workers = 4

let rec worker domain_mgr stream =
  let task = Eio.Stream.take stream in
  (try Eio.Domain_manager.run domain_mgr task with _ -> ()) ;
  worker domain_mgr stream

let start () =
  if Atomic.compare_and_set started false true then
    ignore
      (Domain.spawn (fun () ->
           Eio_main.run (fun env ->
               let stream = Lazy.force stream in
               Eio.Switch.run (fun sw ->
                   for _ = 1 to num_workers do
                     Eio.Fiber.fork ~sw (fun () -> worker env#domain_mgr stream)
                   done ;
                   (* Keep the switch alive indefinitely. *)
                   Eio.Fiber.await_cancel ()))))

let default_enqueue task =
  start () ;
  let stream = Lazy.force stream in
  try Eio.Stream.add stream task with _ -> ()

let enqueue_ref = Atomic.make default_enqueue

let enqueue task = (Atomic.get enqueue_ref) task

let submit_blocking ?on_complete f =
  let task () =
    Fun.protect ~finally:(fun () -> Option.iter (fun g -> g ()) on_complete) f
  in
  enqueue task

module For_tests = struct
  let with_synchronous_runner f =
    let original = Atomic.get enqueue_ref in
    Atomic.set enqueue_ref (fun task -> task ()) ;
    Fun.protect ~finally:(fun () -> Atomic.set enqueue_ref original) f
end
