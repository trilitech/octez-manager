(** QR Code Widget

    Display QR codes for text/URLs in both terminal and SDL backends.
    Terminal uses block characters (█ and spaces), SDL uses actual pixels.

    Example:
    {[
      let qr = Qr_code_widget.create ~data:"https://example.com" () in
      let output = Qr_code_widget.render qr ~focus:false in
      print_endline output
    ]}
*)

(** The QR code widget state *)
type t

(** Create a QR code from the given data string.

    @param data The text or URL to encode
    @param scale Scale factor for display (default: 1). Each module becomes scale×scale characters/pixels.
    @return Result with QR code widget or error message
*)
val create : data:string -> ?scale:int -> unit -> (t, string) result

(** Update the QR code with new data.

    @param data New text or URL to encode
    @return Result with updated QR code widget or error message
*)
val update_data : t -> data:string -> (t, string) result

(** Render the QR code as terminal text using block characters (█).

    @param focus Whether the widget has focus (affects border/styling)
    @return Multi-line string representation
*)
val render : t -> focus:bool -> string

(** Get the dimensions (width, height) of the QR code in modules *)
val get_dimensions : t -> int * int

(** Get the encoded data string *)
val get_data : t -> string

(** Get the value of a specific QR module (true = dark, false = light).

    @param x X coordinate (0 to size-1)
    @param y Y coordinate (0 to size-1)
    @raise Invalid_argument if coordinates are out of bounds
*)
val get_module : t -> x:int -> y:int -> bool
