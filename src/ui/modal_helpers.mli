val open_text_modal : title:string -> lines:string list -> unit

val open_choice_modal :
  title:string ->
  items:'a list ->
  to_string:('a -> string) ->
  on_select:('a -> unit) ->
  unit

val open_choice_modal_with_hint :
  title:string ->
  items:'a list ->
  to_string:('a -> string) ->
  hint:('a -> unit) ->
  describe:('a -> string list) ->
  on_select:('a -> unit) ->
  unit

val prompt_text_modal :
  ?title:string ->
  ?width:int ->
  ?initial:string ->
  ?placeholder:string option ->
  on_submit:(string -> unit) ->
  unit ->
  unit

val prompt_validated_text_modal :
  ?title:string ->
  ?width:int ->
  ?initial:string ->
  ?placeholder:string option ->
  validator:(string -> (unit, string) result) ->
  on_submit:(string -> unit) ->
  unit ->
  unit

val show_success : title:string -> string -> unit

val show_error : title:string -> string -> unit

val confirm_modal :
  ?title:string -> message:string -> on_result:(bool -> unit) -> unit -> unit
