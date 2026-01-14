(** Image Widget

    Display images in both terminal and SDL backends.
    Terminal uses Unicode block characters (█ ▀ ▄ and half-blocks) with ANSI colors.
    SDL uses actual pixel rendering.

    Supports common image formats via imagelib: PNG, BMP, PPM, PGM, PBM.

    Example:
    {[
      match Image_widget.load_from_file "logo.png" ~max_width:80 ~max_height:24 () with
      | Ok img ->
          let output = Image_widget.render img ~focus:false in
          print_endline output
      | Error err ->
          Printf.eprintf "Failed to load image: %s\n" err
    ]}
*)

(** The image widget state *)
type t

(** Load an image from a file path.

    The image will be scaled down to fit within max_width × max_height while
    preserving aspect ratio.

    @param max_width Maximum width in character cells (terminal) or pixels (SDL)
    @param max_height Maximum height in character cells (terminal) or pixels (SDL)
    @return Result with image widget or error message
*)
val load_from_file :
  string -> max_width:int -> max_height:int -> unit -> (t, string) result

(** Create an image from raw RGB data.

    @param width Image width in pixels
    @param height Image height in pixels
    @param rgb_data Raw RGB bytes (width × height × 3 bytes)
*)
val create_from_rgb : width:int -> height:int -> rgb_data:bytes -> unit -> t

(** Render the image as terminal text using block characters and ANSI colors.

    Uses half-height blocks (▀ ▄) to render 2 pixels per character cell,
    doubling vertical resolution.

    @param crop_center Ratio of center to display (1.0 = full image, 0.5 = center 50%)
    @param focus Whether the widget has focus (affects border/styling)
    @return Multi-line string representation
*)
val render : ?crop_center:float -> t -> focus:bool -> string

(** Get the dimensions (width, height) of the image in pixels *)
val get_dimensions : t -> int * int

(** Get the source file path if loaded from file *)
val get_file_path : t -> string option

(** RGB pixel value *)
type pixel = {r : int; g : int; b : int}

(** Get the RGB values of a specific pixel.

    @param x X coordinate (0 to width-1)
    @param y Y coordinate (0 to height-1)
    @raise Invalid_argument if coordinates are out of bounds
*)
val get_pixel : t -> x:int -> y:int -> pixel

(** Get direct access to the pixel array for efficient rendering.
    Format: pixels.(y).(x) where y is row, x is column *)
val get_pixels : t -> pixel array array

(** Prepare image data for SDL rendering.
    
    @param x X position on screen
    @param y Y position on screen  
    @param scale Scaling factor (1 = original size, 2 = 2x, etc.)
    @return (x, y, scaled_width, scaled_height, pixel_array)
*)
val render_sdl :
  t -> x:int -> y:int -> scale:int -> int * int * int * int * pixel array array
