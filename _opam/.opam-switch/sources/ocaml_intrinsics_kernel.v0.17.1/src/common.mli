(** Are optimized C stubs available? If not, naive implementation will be used.  The value
    is statically known and depends on the current compiler's configuration (system, target,
    architecture). *)
val available : bool
