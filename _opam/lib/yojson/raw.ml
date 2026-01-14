
(** {3 Type of the JSON tree} *)

type t =
    [
    | `Null
    | `Bool of bool
    | `Intlit of string
    | `Floatlit of string
    | `Stringlit of string
    | `Assoc of (string * t) list
    | `List of t list
    ]
(**
All possible cases defined in Yojson:
- `Null: JSON null
- `Bool of bool: JSON boolean
- `Int of int: JSON number without decimal point or exponent.
- `Intlit of string: JSON number without decimal point or exponent,
	    preserved as a string.
- `Float of float: JSON number, Infinity, -Infinity or NaN.
- `Floatlit of string: JSON number, Infinity, -Infinity or NaN,
	    preserved as a string.
- `String of string: JSON string. Bytes in the range 128-255 are preserved
	    as-is without encoding validation for both reading
	    and writing.
- `Stringlit of string: JSON string literal including the double quotes.
- `Assoc of (string * json) list: JSON object.
- `List of json list: JSON array.
*)

(*
  Note to adventurers: ocamldoc does not support inline comments
  on each polymorphic variant, and cppo doesn't allow to concatenate
  comments, so it would be complicated to document only the
  cases that are preserved by cppo in the type definition.
*)

let hex n =
  Char.chr (
    if n < 10 then n + 48
    else n + 87
  )

let write_special src start stop ob str =
  Buffer.add_substring ob src !start (stop - !start);
  Buffer.add_string ob str;
  start := stop + 1

let write_control_char src start stop ob c =
  Buffer.add_substring ob src !start (stop - !start);
  Buffer.add_string ob "\\u00";
  Buffer.add_char ob (hex (Char.code c lsr 4));
  Buffer.add_char ob (hex (Char.code c land 0xf));
  start := stop + 1

let finish_string src start ob =
  try
    Buffer.add_substring ob src !start (String.length src - !start)
  with exc ->
    Printf.eprintf "src=%S start=%i len=%i\n%!"
      src !start (String.length src - !start);
    raise exc

let write_string_body ob s =
  let start = ref 0 in
  for i = 0 to String.length s - 1 do
    match s.[i] with
        '"' -> write_special s start i ob "\\\""
      | '\\' -> write_special s start i ob "\\\\"
      | '\b' -> write_special s start i ob "\\b"
      | '\012' -> write_special s start i ob "\\f"
      | '\n' -> write_special s start i ob "\\n"
      | '\r' -> write_special s start i ob "\\r"
      | '\t' -> write_special s start i ob "\\t"
      | '\x00'..'\x1F'
      | '\x7F' as c -> write_control_char s start i ob c
      | _ -> ()
  done;
  finish_string s start ob

let write_string ob s =
  Buffer.add_char ob '"';
  write_string_body ob s;
  Buffer.add_char ob '"'

let json_string_of_string s =
  let ob = Buffer.create 10 in
  write_string ob s;
  Buffer.contents ob

let write_null ob () =
  Buffer.add_string ob "null"

let write_bool ob x =
  Buffer.add_string ob (if x then "true" else "false")

let dec n =
  Char.chr (n + 48)

let rec write_digits s x =
  if x = 0 then ()
  else
    let d = x mod 10 in
    write_digits s (x / 10);
    Buffer.add_char s (dec (abs d))

let write_int ob x =
  if x > 0 then
    write_digits ob x
  else if x < 0 then (
    Buffer.add_char ob '-';
    write_digits ob x
  )
  else
    Buffer.add_char ob '0'


let json_string_of_int i =
  string_of_int i


(*
  Ensure that the float is not printed as an int.
  This is not required by JSON, but useful in order to guarantee
  reversibility.
*)
let float_needs_period s =
  try
    for i = 0 to String.length s - 1 do
      match s.[i] with
          '0'..'9' | '-' -> ()
        | _ -> raise Exit
    done;
    true
  with Exit ->
    false

(*
  Guarantees that a sufficient number of digits are printed in order to allow
  reversibility.
*)
let write_float ob x =
  match classify_float x with
    FP_nan ->
      Buffer.add_string ob "NaN"
  | FP_infinite ->
      Buffer.add_string ob (if x > 0. then "Infinity" else "-Infinity")
  | _ ->
      let s1 = Printf.sprintf "%.16g" x in
      let s =
        if float_of_string s1 = x then s1
        else Printf.sprintf "%.17g" x
      in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

let write_normal_float_prec significant_figures ob x =
  let sprintf = Printf.sprintf in
  let s =
    match significant_figures with
        1 -> sprintf "%.1g" x
      | 2 -> sprintf "%.2g" x
      | 3 -> sprintf "%.3g" x
      | 4 -> sprintf "%.4g" x
      | 5 -> sprintf "%.5g" x
      | 6 -> sprintf "%.6g" x
      | 7 -> sprintf "%.7g" x
      | 8 -> sprintf "%.8g" x
      | 9 -> sprintf "%.9g" x
      | 10 -> sprintf "%.10g" x
      | 11 -> sprintf "%.11g" x
      | 12 -> sprintf "%.12g" x
      | 13 -> sprintf "%.13g" x
      | 14 -> sprintf "%.14g" x
      | 15 -> sprintf "%.15g" x
      | 16 -> sprintf "%.16g" x
      | _ -> sprintf "%.17g" x
  in
  Buffer.add_string ob s;
  if float_needs_period s then
    Buffer.add_string ob ".0"

let json_string_of_float x =
  let ob = Buffer.create 20 in
  write_float ob x;
  Buffer.contents ob

let write_float ob x =
  match classify_float x with
    FP_nan ->
      Common.json_error "NaN value not allowed in standard JSON"
  | FP_infinite ->
      Common.json_error
        (if x > 0. then
           "Infinity value not allowed in standard JSON"
         else
           "-Infinity value not allowed in standard JSON")
  | _ ->
      let s1 = Printf.sprintf "%.16g" x in
      let s =
        if float_of_string s1 = x then s1
        else Printf.sprintf "%.17g" x
      in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

(* to be deprecated in a future release *)
let write_std_float = write_float

(* used by atdgen *)
let write_float_prec significant_figures ob x =
  match classify_float x with
    FP_nan ->
      Common.json_error "NaN value not allowed in standard JSON"
  | FP_infinite ->
      Common.json_error
        (if x > 0. then
           "Infinity value not allowed in standard JSON"
         else
           "-Infinity value not allowed in standard JSON")
  | _ ->
      write_normal_float_prec significant_figures ob x

let write_std_float_prec = write_float_prec

let write_intlit = Buffer.add_string
let write_floatlit = Buffer.add_string
let write_stringlit = Buffer.add_string

let rec iter2_aux f_elt f_sep x = function
    [] -> ()
  | y :: l ->
      f_sep x;
      f_elt x y;
      iter2_aux f_elt f_sep x l

let iter2 f_elt f_sep x = function
    [] -> ()
  | y :: l ->
      f_elt x y;
      iter2_aux f_elt f_sep x l

let f_sep ob =
  Buffer.add_char ob ','

let rec write_json ob (x : t) =
  match x with
      `Null -> write_null ob ()
    | `Bool b -> write_bool ob b
    | `Intlit s -> Buffer.add_string ob s
    | `Floatlit s -> Buffer.add_string ob s
    | `Stringlit s -> Buffer.add_string ob s
    | `Assoc l -> write_assoc ob l
    | `List l -> write_list ob l

and write_assoc ob l =
  let f_elt ob (s, x) =
    write_string ob s;
    Buffer.add_char ob ':';
    write_json ob x
  in
  Buffer.add_char ob '{';
  iter2 f_elt f_sep ob l;
  Buffer.add_char ob '}';

and write_list ob l =
  Buffer.add_char ob '[';
  iter2 write_json f_sep ob l;
  Buffer.add_char ob ']'

let write_t = write_json

let write_std_json = write_json

(* std argument is going to be deprecated *)
let to_buffer ?(suf = "") ?(std = true) ob x =
  write_json ob x;
  Buffer.add_string ob suf

let to_string ?buf ?(len = 256) ?(suf = "") ?std x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob ->
          Buffer.clear ob;
          ob
  in
  to_buffer ~suf ?std ob x;
  let s = Buffer.contents ob in
  Buffer.clear ob;
  s

let to_channel ?buf ?(len=4096) ?(suf = "") ?std oc x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> Buffer.clear ob; ob
  in
  to_buffer ~suf ?std ob x;
  Buffer.output_buffer oc ob;
  Buffer.clear ob

let to_output ?buf ?(len=4096) ?(suf = "") ?std out x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> Buffer.clear ob; ob
  in
  to_buffer ~suf ?std ob x;
  (* this requires an int and never uses it. This is done to preserve
     backward compatibility to not break the signatur but can safely
     be changed to require unit in a future compatibility-breaking
     release *)
  let _ : int = out#output (Buffer.contents ob) 0 (Buffer.length ob) in
  Buffer.clear ob

let to_file ?len ?std ?(suf = "\n") file x =
  let oc = open_out file in
  try
    to_channel ?len ~suf ?std oc x;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

let seq_to_buffer ?(suf = "\n") ?std ob st =
  Seq.iter (to_buffer ~suf ?std ob) st

let seq_to_string ?buf ?(len = 256) ?(suf = "\n") ?std st =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob ->
          Buffer.clear ob;
          ob
  in
  seq_to_buffer ~suf ?std ob st;
  let s = Buffer.contents ob in
  Buffer.clear ob;
  s

let seq_to_channel ?buf ?(len=2096) ?(suf = "\n") ?std oc seq =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> Buffer.clear ob; ob
  in
  Seq.iter (fun json ->
    to_buffer ~suf ?std ob json;
    Buffer.output_buffer oc ob;
    Buffer.clear ob;
  ) seq

let seq_to_file ?len ?(suf = "\n") ?std file st =
  let oc = open_out file in
  try
    seq_to_channel ?len ~suf ?std oc st;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e


let rec sort = function
  | `Assoc l ->
      let l = List.rev (List.rev_map (fun (k, v) -> (k, sort v)) l) in
      `Assoc (List.stable_sort (fun (a, _) (b, _) -> String.compare a b) l)
  | `List l ->
      `List (List.rev (List.rev_map sort l))
  | x -> x

module Pretty = struct
(*
   Pretty-print JSON data in an attempt to maximize readability.

   1. What fits on one line stays on one line.
   2. What doesn't fit on one line gets printed more vertically so as to not
      exceed a reasonable page width, if possible.

   Arrays containing only simple elements ("atoms") are pretty-printed with
   end-of-line wrapping like ordinary text:

     [
        "hello", "hello", "hello", "hello", "hello", "hello", "hello", "hello",
        "hello", "hello", "hello", "hello", "hello", "hello", "hello", "hello"
     ]

   Other arrays are printed either horizontally or vertically depending
   on whether they fit on a single line:

     [ { "hello": "world" }, { "hello": "world" }, { "hello": "world" } ]

   or

     [
       { "hello": "world" },
       { "hello": "world" },
       { "hello": "world" },
       { "hello": "world" }
     ]
*)

let pp_list sep ppx out l =
  let pp_sep out () = Format.fprintf out "%s@ " sep in
  Format.pp_print_list ~pp_sep ppx out l

let is_atom (x: [> t]) =
  match x with
  | `Null
  | `Bool _
  | `Int _
  | `Float _
  | `String _
  | `Intlit _
  | `Floatlit _
  | `Stringlit _
  | `List []
  | `Assoc [] -> true
  | `List _
  | `Assoc _ -> false

let is_atom_list l =
  List.for_all is_atom l

(*
   inside_box: indicates that we're already within a box that imposes
   a certain style and we shouldn't create a new one. This is used for
   printing field values like this:

     foo: [
       bar
     ]

   rather than something else like

     foo:
       [
         bar
       ]
*)
let rec format ~inside_box (out : Format.formatter) (x : t) : unit =
  match x with
    | `Null -> Format.pp_print_string out "null"
    | `Bool x -> Format.pp_print_bool out x
    | `Intlit s -> Format.pp_print_string out s
    | `Floatlit s -> Format.pp_print_string out s
    | `Stringlit s -> Format.pp_print_string out s
    | `List [] -> Format.pp_print_string out "[]"
    | `List l ->
      if not inside_box then Format.fprintf out "@[<hv2>";
      if is_atom_list l then
        (* use line wrapping like we would do for a paragraph of text *)
        Format.fprintf out "[@;<1 0>@[<hov>%a@]@;<1 -2>]"
          (pp_list "," (format ~inside_box:false)) l
      else
        (* print the elements horizontally if they fit on the line,
           otherwise print them in a column *)
        Format.fprintf out "[@;<1 0>@[<hv>%a@]@;<1 -2>]"
          (pp_list "," (format ~inside_box:false)) l;
      if not inside_box then Format.fprintf out "@]";
    | `Assoc [] -> Format.pp_print_string out "{}"
    | `Assoc l ->
      if not inside_box then Format.fprintf out "@[<hv2>";
      Format.fprintf out "{@;<1 0>%a@;<1 -2>}" (pp_list "," (format_field)) l;
      if not inside_box then Format.fprintf out "@]";

and format_field out (name, x) =
  Format.fprintf out "@[<hv2>%s: %a@]" (json_string_of_string name) (format ~inside_box:true) x

(* [std] argument to be deprecated *)
let pp ?(std = true) out x =
  Format.fprintf out "@[<hv2>%a@]" (format ~inside_box:true) (x :> t)

let to_string ?std x =
  Format.asprintf "%a" (pp ?std) x

let to_channel ?std oc x =
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@?" (pp ?std) x
end

let rec pp fmt =
  function
  | `Null -> Format.pp_print_string fmt "`Null"
  | `Bool x ->
    Format.fprintf fmt "`Bool (@[<hov>";
    Format.fprintf fmt "%B" x;
    Format.fprintf fmt "@])"
  | `Intlit x ->
    Format.fprintf fmt "`Intlit (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
  | `Floatlit x ->
    Format.fprintf fmt "`Floatlit (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
  | `Stringlit x ->
    Format.fprintf fmt "`Stringlit (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
  | `Assoc xs ->
    Format.fprintf fmt "`Assoc (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep (key, value) ->
        if sep then
          Format.fprintf fmt ";@ ";
          Format.fprintf fmt "(@[";
          Format.fprintf fmt "%S" key;
          Format.fprintf fmt ",@ ";
          pp fmt value;
          Format.fprintf fmt "@])";
          true) false xs);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"
  | `List xs ->
    Format.fprintf fmt "`List (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep x ->
        if sep then
          Format.fprintf fmt ";@ ";
          pp fmt x;
          true) false xs);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"

let show x =
  Format.asprintf "%a" pp x

let rec equal a b =
  match a, b with
  | `Null, `Null -> true
  | `Bool a, `Bool b -> a = b
    | `Intlit a, `Intlit b -> a = b
    | `Floatlit a, `Floatlit b -> a = b
    | `Stringlit a, `Stringlit b -> a = b
    | `Assoc xs, `Assoc ys ->
      let compare_keys = fun (key, _) (key', _) -> String.compare key key' in
      let xs = List.stable_sort compare_keys xs in
      let ys = List.stable_sort compare_keys ys in
      (match List.for_all2 (fun (key, value) (key', value') ->
        match key = key' with
        | false -> false
        | true -> equal value value') xs ys with
      | result -> result
      | exception Invalid_argument _ ->
        (* the lists were of different lengths, thus unequal *)
        false)
    | `List xs, `List ys ->
      (match List.for_all2 equal xs ys with
      | result -> result
      | exception Invalid_argument _ ->
        (* the lists were of different lengths, thus unequal *)
        false)
    | _ -> false

let pretty_print ?std out x = Pretty.pp ?std out x
let pretty_to_string ?std x = Pretty.to_string ?std x
let pretty_to_channel ?std oc x = Pretty.to_channel ?std oc x

# 1 "lib/read.mll"
 
  module Lexing =
    (*
      We override Lexing.engine in order to avoid creating a new position
      record each time a rule is matched.
      This reduces total parsing time by about 31%.
    *)
  struct
    include Lexing

    external c_engine : lex_tables -> int -> lexbuf -> int = "caml_lex_engine"

    let engine tbl state buf =
      let result = c_engine tbl state buf in
      (*
      if result >= 0 then begin
        buf.lex_start_p <- buf.lex_curr_p;
        buf.lex_curr_p <- {buf.lex_curr_p
                           with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos};
      end;
      *)
      result
  end

  (* see description in common.mli *)
  type lexer_state = Common.Lexer_state.t = {
    buf : Buffer.t;
    mutable lnum : int;
    mutable bol : int;
    mutable fname : string option;
  }

  let dec c =
    Char.code c - 48

  let hex c =
    match c with
        '0'..'9' -> int_of_char c - int_of_char '0'
      | 'a'..'f' -> int_of_char c - int_of_char 'a' + 10
      | 'A'..'F' -> int_of_char c - int_of_char 'A' + 10
      | _ -> assert false

  let custom_error descr v (lexbuf : Lexing.lexbuf) =
    let offs = lexbuf.lex_abs_pos - 1 in
    let bol = v.bol in
    let pos1 = offs + lexbuf.lex_start_pos - bol - 1 in
    let pos2 = max pos1 (offs + lexbuf.lex_curr_pos - bol) in
    let file_line =
      match v.fname with
          None -> "Line"
        | Some s ->
            Printf.sprintf "File %s, line" s
    in
    let bytes =
      if pos1 = pos2 then
        Printf.sprintf "byte %i" (pos1+1)
      else
        Printf.sprintf "bytes %i-%i" (pos1+1) (pos2+1)
    in
    let msg = Printf.sprintf "%s %i, %s:\n%s" file_line v.lnum bytes descr in
    Common.json_error msg


  let lexer_error descr v lexbuf =
    custom_error
      (Printf.sprintf "%s '%s'" descr (Lexing.lexeme lexbuf))
      v lexbuf

  let long_error descr v lexbuf =
    let junk = Lexing.lexeme lexbuf in
    let buf_size = 32 in
    let buf = Buffer.create buf_size in
    let () = Lexer_utils.read_junk_without_positions buf buf_size lexbuf in
    let extra_junk = Buffer.contents buf in
    custom_error
      (Printf.sprintf "%s '%s%s'" descr junk extra_junk)
      v lexbuf

  let min10 = min_int / 10 - (if min_int mod 10 = 0 then 0 else 1)
  let max10 = max_int / 10 + (if max_int mod 10 = 0 then 0 else 1)

  exception Int_overflow

  let extract_positive_int (lexbuf : Lexing.lexbuf) =
    let start = lexbuf.lex_start_pos in
    let stop = lexbuf.lex_curr_pos in
    let s = lexbuf.lex_buffer in
    let n = ref 0 in
    for i = start to stop - 1 do
      if !n >= max10 then
        raise Int_overflow
      else
        n := 10 * !n + dec (Bytes.get s i)
    done;
    if !n < 0 then
      raise Int_overflow
    else
      !n

  let make_positive_int v lexbuf =
        `Intlit (Lexing.lexeme lexbuf)

  let extract_negative_int (lexbuf : Lexing.lexbuf)  =
    let start = lexbuf.lex_start_pos + 1 in
    let stop = lexbuf.lex_curr_pos in
    let s = lexbuf.lex_buffer in
    let n = ref 0 in
    for i = start to stop - 1 do
      if !n <= min10 then
        raise Int_overflow
      else
        n := 10 * !n - dec (Bytes.get s i)
    done;
    if !n > 0 then
      raise Int_overflow
    else
      !n

  let make_negative_int v lexbuf =
        `Intlit (Lexing.lexeme lexbuf)

  let newline v (lexbuf : Lexing.lexbuf) =
    v.lnum <- v.lnum + 1;
    v.bol <- lexbuf.lex_abs_pos + lexbuf.lex_curr_pos

  let add_lexeme buf (lexbuf : Lexing.lexbuf) =
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    Buffer.add_subbytes buf lexbuf.lex_buffer lexbuf.lex_start_pos len

  let map_lexeme f (lexbuf : Lexing.lexbuf) =
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    f (Bytes.sub_string lexbuf.lex_buffer lexbuf.lex_start_pos len) 0 len

# 152 "lib/read.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\238\255\239\255\003\000\241\255\016\000\244\255\245\255\
    \000\000\031\000\249\255\085\000\001\000\000\000\000\000\001\000\
    \000\000\001\000\002\000\255\255\000\000\000\000\003\000\254\255\
    \001\000\004\000\253\255\011\000\252\255\003\000\001\000\003\000\
    \002\000\003\000\000\000\251\255\021\000\097\000\010\000\022\000\
    \020\000\016\000\022\000\012\000\008\000\250\255\119\000\129\000\
    \139\000\161\000\171\000\181\000\193\000\209\000\242\255\011\000\
    \038\000\252\255\065\000\254\255\255\255\110\000\252\255\163\000\
    \254\255\255\255\234\000\247\255\248\255\048\001\250\255\251\255\
    \252\255\253\255\254\255\255\255\071\001\126\001\149\001\249\255\
    \039\000\253\255\254\255\038\000\187\001\210\001\248\001\015\002\
    \255\255\220\000\253\255\255\255\245\000\039\002\109\002\014\001\
    \088\002\164\002\187\002\225\002\014\000\253\255\254\255\255\255\
    \013\000\253\255\254\255\255\255\015\000\253\255\254\255\255\255\
    \018\000\252\255\253\255\254\255\014\000\255\255\016\000\255\255\
    \011\001\005\000\253\255\023\000\254\255\017\000\255\255\046\000\
    \253\255\254\255\042\000\052\000\053\000\255\255\053\000\048\000\
    \091\000\092\000\255\255\027\001\250\255\251\255\137\000\104\000\
    \089\000\088\000\106\000\255\255\143\000\137\000\174\000\254\255\
    \181\000\168\000\166\000\183\000\002\000\253\255\177\000\171\000\
    \186\000\004\000\252\255\053\002\251\255\252\255\253\255\103\001\
    \255\255\248\002\254\255\006\003\030\003\252\255\253\255\254\255\
    \255\255\040\003\050\003\074\003\252\255\253\255\254\255\255\255\
    \061\003\084\003\108\003\249\255\250\255\251\255\242\000\120\003\
    \142\003\179\000\193\000\014\000\255\255\189\000\187\000\183\000\
    \191\000\181\000\177\000\254\255\190\000\199\000\198\000\195\000\
    \201\000\191\000\187\000\253\255\157\003\095\003\174\003\196\003\
    \206\003\216\003\228\003\239\003\059\000\253\255\254\255\255\255\
    \012\004\252\255\253\255\087\004\255\255\145\004\252\255\253\255\
    \221\004\255\255\222\000\253\255\254\255\255\255\229\000\253\255\
    \254\255\255\255\001\000\255\255\016\001\252\255\253\255\254\255\
    \255\255\186\000\253\255\254\255\255\255\187\000\253\255\254\255\
    \255\255\193\000\255\255\039\001\252\255\253\255\254\255\255\255\
    \013\001\253\255\254\255\255\255\026\001\253\255\254\255\255\255\
    \034\001\253\255\254\255\255\255\232\000\253\255\254\255\255\255\
    \231\000\253\255\254\255\255\255\079\005\239\255\240\255\010\000\
    \242\255\024\000\245\255\246\255\043\001\002\004\249\255\045\005\
    \209\000\228\000\211\000\232\000\216\000\216\000\233\000\255\255\
    \227\000\223\000\239\000\254\255\233\000\234\000\253\255\017\000\
    \252\255\244\000\242\000\239\000\002\001\248\000\246\000\251\255\
    \020\001\029\001\027\001\023\001\029\001\019\001\021\001\250\255\
    \110\005\012\004\123\005\155\005\165\005\177\005\187\005\197\005\
    \243\255\151\001\199\001\253\255\255\255\003\002\222\005\209\005\
    \004\002\239\005\053\006\076\006\114\006\152\006\252\255\253\255\
    \227\006\255\255\085\007\246\255\247\255\011\000\249\255\120\001\
    \252\255\253\255\254\255\120\001\243\005\051\007\055\001\087\001\
    \068\001\091\001\076\001\090\001\107\001\255\255\101\001\095\001\
    \110\001\104\001\105\001\018\000\128\001\163\001\161\001\168\001\
    \158\001\160\001\182\001\204\001\225\001\221\001\227\001\217\001\
    \213\001\142\006\152\006\116\007\170\007\180\007\190\007\200\007\
    \210\007\250\255\120\002\077\002\253\255\255\255\154\002\082\007\
    \220\007\155\002\244\007\058\008\081\008\119\008\157\008\252\255\
    \253\255\232\008\255\255\135\002\110\002\253\255\078\002\254\255\
    \182\002\255\255\210\001\255\255\183\002\252\255\253\255\254\255\
    \255\255\251\001\255\255\178\002\252\255\253\255\254\255\255\255\
    \074\002\253\255\254\255\255\255\065\002\253\255\254\255\255\255\
    \184\002\252\255\253\255\254\255\015\000\255\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\015\000\255\255\017\000\255\255\255\255\
    \007\000\007\000\255\255\017\000\017\000\017\000\017\000\017\000\
    \017\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\008\000\008\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \009\000\255\255\009\000\255\255\009\000\255\255\255\255\012\000\
    \255\255\255\255\002\000\255\255\255\255\255\255\255\255\002\000\
    \255\255\255\255\255\255\255\255\255\255\007\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\001\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\001\000\001\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\003\000\255\255\001\000\255\255\
    \004\000\003\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\001\000\255\255\255\255\255\255\001\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\004\000\004\000\
    \004\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\003\000\
    \255\255\000\000\255\255\001\000\255\255\255\255\255\255\255\255\
    \255\255\000\000\002\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\002\000\255\255\255\255\255\255\255\255\003\000\003\000\
    \005\000\005\000\005\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\003\000\255\255\
    \003\000\255\255\003\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\001\000\255\255\255\255\255\255\255\255\
    \001\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\001\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\001\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\014\000\
    \255\255\016\000\255\255\255\255\007\000\007\000\255\255\016\000\
    \016\000\016\000\016\000\016\000\016\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\008\000\255\255\008\000\255\255\008\000\255\255\
    \255\255\011\000\255\255\255\255\255\255\001\000\001\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \001\000\255\255\255\255\255\255\255\255\007\000\255\255\009\000\
    \255\255\255\255\255\255\000\000\000\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\000\000\255\255\000\000\
    \255\255\255\255\004\000\255\255\255\255\255\255\001\000\001\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\001\000\255\255\004\000\003\000\255\255\255\255\255\255\
    \255\255\255\255\001\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\001\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\003\000\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\255\255\000\000\255\255\000\000\000\000\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\000\000\
    \255\255\255\255\000\000\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\055\000\
    \058\000\000\000\058\000\000\000\000\000\063\000\000\000\063\000\
    \000\000\000\000\068\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\255\255\255\255\000\000\
    \082\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\092\000\000\000\000\000\095\000\255\255\255\255\095\000\
    \255\255\255\255\255\255\255\255\102\000\000\000\000\000\000\000\
    \106\000\000\000\000\000\000\000\110\000\000\000\000\000\000\000\
    \113\000\000\000\000\000\000\000\255\255\000\000\255\255\000\000\
    \255\255\255\255\000\000\255\255\000\000\125\000\000\000\129\000\
    \000\000\000\000\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\000\000\141\000\000\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\000\000\165\000\000\000\000\000\000\000\255\255\
    \000\000\255\255\000\000\255\255\174\000\000\000\000\000\000\000\
    \000\000\255\255\255\255\181\000\000\000\000\000\000\000\000\000\
    \255\255\255\255\188\000\000\000\000\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\222\000\000\000\000\000\000\000\
    \226\000\000\000\000\000\255\255\000\000\231\000\000\000\000\000\
    \255\255\000\000\236\000\000\000\000\000\000\000\240\000\000\000\
    \000\000\000\000\255\255\000\000\246\000\000\000\000\000\000\000\
    \000\000\251\000\000\000\000\000\000\000\255\000\000\000\000\000\
    \000\000\255\255\000\000\005\001\000\000\000\000\000\000\000\000\
    \010\001\000\000\000\000\000\000\014\001\000\000\000\000\000\000\
    \018\001\000\000\000\000\000\000\022\001\000\000\000\000\000\000\
    \026\001\000\000\000\000\000\000\029\001\000\000\000\000\255\255\
    \000\000\255\255\000\000\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\000\000\255\255\255\255\000\000\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\081\001\085\001\000\000\000\000\088\001\255\255\255\255\
    \088\001\255\255\255\255\255\255\255\255\095\001\000\000\000\000\
    \255\255\000\000\099\001\000\000\000\000\255\255\000\000\255\255\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\146\001\150\001\000\000\000\000\153\001\255\255\
    \255\255\153\001\255\255\255\255\255\255\255\255\160\001\000\000\
    \000\000\255\255\000\000\255\255\255\255\000\000\255\255\000\000\
    \168\001\000\000\255\255\000\000\174\001\000\000\000\000\000\000\
    \000\000\255\255\000\000\181\001\000\000\000\000\000\000\000\000\
    \186\001\000\000\000\000\000\000\190\001\000\000\000\000\000\000\
    \193\001\000\000\000\000\000\000\255\255\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\004\000\000\000\003\000\003\000\121\000\000\000\
    \003\000\000\000\121\000\031\001\101\001\255\255\000\000\031\001\
    \101\001\000\000\000\000\126\000\114\000\000\000\000\000\000\000\
    \003\000\000\000\010\000\003\000\157\000\121\000\162\000\000\000\
    \000\000\000\000\031\001\101\001\000\000\011\000\047\000\005\000\
    \008\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\054\000\111\000\116\000\117\000\197\001\055\000\
    \000\000\124\000\080\001\047\000\000\000\046\000\125\000\081\001\
    \060\000\012\000\103\000\107\000\000\000\047\000\013\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\028\000\046\000\006\000\196\000\223\000\243\000\056\001\
    \117\001\027\000\020\000\255\255\046\000\046\000\015\000\019\000\
    \023\000\030\000\031\000\033\000\021\000\025\000\014\000\029\000\
    \026\000\032\000\017\000\022\000\016\000\024\000\018\000\034\000\
    \039\000\035\000\046\000\007\000\040\000\041\000\042\000\043\000\
    \044\000\045\000\059\000\083\000\046\000\036\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\047\000\
    \065\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\084\000\130\000\255\255\038\000\131\000\
    \132\000\133\000\053\000\135\000\053\000\136\000\046\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\255\255\046\000\137\000\
    \138\000\148\000\064\000\145\000\051\000\146\000\051\000\147\000\
    \049\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\152\000\
    \049\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\149\000\150\000\153\000\091\000\255\255\
    \002\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\255\255\075\000\105\000\101\000\109\000\
    \119\000\126\000\115\000\151\000\121\000\122\000\158\000\091\000\
    \121\000\075\000\154\000\155\000\156\000\159\000\160\000\161\000\
    \213\000\197\000\195\000\198\000\199\000\200\000\057\000\081\000\
    \201\000\202\000\203\000\121\000\205\000\206\000\128\000\207\000\
    \091\000\208\000\209\000\210\000\211\000\252\000\000\001\212\000\
    \093\000\237\000\123\000\221\000\248\000\142\000\002\001\057\001\
    \241\000\255\255\015\001\023\001\027\001\055\001\075\000\011\001\
    \052\001\048\001\045\001\019\001\074\000\046\001\047\001\049\001\
    \073\000\096\000\050\001\007\001\051\001\053\001\054\001\212\000\
    \072\000\073\001\058\001\059\001\071\000\060\001\070\000\069\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\076\000\076\000\
    \076\000\076\000\096\000\061\001\062\001\247\000\062\000\063\001\
    \072\001\076\000\076\000\076\000\076\000\076\000\076\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\143\000\065\001\066\001\067\001\068\001\069\001\070\001\
    \077\000\077\000\077\000\077\000\077\000\077\000\071\001\144\000\
    \072\001\076\000\076\000\076\000\076\000\076\000\076\000\170\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \171\000\255\255\145\001\255\255\006\001\124\001\138\001\146\001\
    \077\000\077\000\077\000\077\000\077\000\077\000\078\000\078\000\
    \078\000\078\000\078\000\078\000\078\000\078\000\078\000\078\000\
    \123\001\121\001\250\000\254\000\118\001\137\001\115\001\078\000\
    \078\000\078\000\078\000\078\000\078\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\116\001\
    \117\001\119\001\120\001\117\001\122\001\117\001\079\000\079\000\
    \079\000\079\000\079\000\079\000\090\000\137\001\235\000\078\000\
    \078\000\078\000\078\000\078\000\078\000\239\000\125\001\025\001\
    \021\001\084\001\067\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\255\255\079\000\079\000\
    \079\000\079\000\079\000\079\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\126\001\009\001\255\255\127\001\
    \245\000\128\001\129\001\086\000\086\000\086\000\086\000\086\000\
    \086\000\117\001\013\001\140\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\017\001\086\001\131\001\084\001\084\001\004\001\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\132\001\086\000\086\000\086\000\086\000\086\000\
    \086\000\087\000\087\000\087\000\087\000\087\000\087\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\095\000\133\001\134\001\135\001\136\001\117\001\171\001\
    \088\000\088\000\088\000\088\000\088\000\088\000\095\000\166\000\
    \178\001\087\000\087\000\087\000\087\000\087\000\087\000\089\001\
    \089\001\000\000\167\000\000\000\000\000\168\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\149\001\
    \088\000\088\000\088\000\088\000\088\000\088\000\000\000\164\001\
    \167\001\000\000\095\000\164\001\000\000\168\001\000\000\191\001\
    \000\000\000\000\255\255\095\000\187\001\000\000\000\000\095\000\
    \000\000\095\000\000\000\000\000\000\000\095\000\164\001\000\000\
    \164\001\165\001\000\000\000\000\164\001\095\000\000\000\255\255\
    \000\000\095\000\000\000\095\000\094\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\164\001\
    \000\000\151\001\000\000\000\000\000\000\000\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\095\000\000\000\166\001\000\000\
    \000\000\000\000\095\000\000\000\149\001\149\001\095\000\000\000\
    \169\001\000\000\194\001\000\000\000\000\000\000\095\000\083\001\
    \000\000\000\000\095\000\000\000\095\000\094\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\183\001\000\000\
    \000\000\000\000\196\001\176\001\000\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\000\000\154\001\154\001\
    \000\000\000\000\000\000\000\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\000\000\255\255\255\255\098\000\098\000\098\000\
    \098\000\098\000\098\000\000\000\000\000\000\000\000\000\182\001\
    \000\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\000\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\000\000\000\000\175\001\164\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \175\000\189\001\095\000\095\000\095\000\095\000\095\000\095\000\
    \000\000\000\000\185\001\178\000\000\000\148\001\176\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\177\000\
    \177\000\177\000\176\000\177\000\177\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\182\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\185\000\
    \255\255\000\000\183\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\183\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\189\000\214\000\
    \214\000\214\000\214\000\214\000\214\000\214\000\214\000\214\000\
    \214\000\192\000\255\255\255\255\190\000\191\000\191\000\191\000\
    \191\000\191\000\191\000\191\000\191\000\191\000\213\000\000\000\
    \191\000\191\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\180\001\000\000\000\000\193\000\169\001\173\001\
    \195\001\000\000\194\000\000\000\000\000\212\000\190\000\191\000\
    \191\000\191\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \219\000\000\000\219\000\000\000\000\000\218\000\218\000\218\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\218\000\204\000\
    \000\000\000\000\000\000\000\000\000\000\212\000\214\000\214\000\
    \214\000\214\000\214\000\214\000\214\000\214\000\214\000\214\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\217\000\
    \000\000\217\000\000\000\215\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\216\000\216\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\216\000\216\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\216\000\216\000\216\000\216\000\216\000\216\000\
    \216\000\216\000\000\000\215\000\218\000\218\000\218\000\218\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\173\000\218\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\218\000\218\000\
    \218\000\000\000\000\000\000\000\000\000\000\000\228\000\000\000\
    \073\001\000\000\037\001\037\001\037\001\037\001\037\001\037\001\
    \037\001\037\001\037\001\037\001\074\001\074\001\074\001\074\001\
    \074\001\074\001\074\001\074\001\074\001\074\001\000\000\072\001\
    \000\000\000\000\180\000\000\000\000\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\072\001\
    \000\000\000\000\000\000\227\000\187\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\000\000\233\000\000\000\000\000\227\000\000\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\000\000\000\000\000\000\000\000\
    \232\000\000\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\225\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \000\000\000\000\000\000\000\000\232\000\000\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \031\001\032\001\000\000\000\000\031\001\036\001\037\001\037\001\
    \037\001\037\001\037\001\037\001\037\001\037\001\037\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\031\001\
    \000\000\038\001\000\000\000\000\000\000\000\000\064\001\000\000\
    \000\000\000\000\000\000\000\000\039\001\000\000\033\001\036\001\
    \037\001\037\001\037\001\037\001\037\001\037\001\037\001\037\001\
    \037\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\230\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \040\001\079\001\000\000\079\001\000\000\041\001\078\001\078\001\
    \078\001\078\001\078\001\078\001\078\001\078\001\078\001\078\001\
    \000\000\000\000\034\001\074\001\074\001\074\001\074\001\074\001\
    \074\001\074\001\074\001\074\001\074\001\043\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\042\001\000\000\000\000\
    \075\001\000\000\000\000\044\001\000\000\000\000\077\001\000\000\
    \077\001\000\000\035\001\076\001\076\001\076\001\076\001\076\001\
    \076\001\076\001\076\001\076\001\076\001\076\001\076\001\076\001\
    \076\001\076\001\076\001\076\001\076\001\076\001\076\001\000\000\
    \075\001\076\001\076\001\076\001\076\001\076\001\076\001\076\001\
    \076\001\076\001\076\001\078\001\078\001\078\001\078\001\078\001\
    \078\001\078\001\078\001\078\001\078\001\078\001\078\001\078\001\
    \078\001\078\001\078\001\078\001\078\001\078\001\078\001\000\000\
    \088\001\090\001\090\001\090\001\090\001\090\001\090\001\090\001\
    \090\001\090\001\090\001\000\000\000\000\088\001\000\000\000\000\
    \000\000\088\001\090\001\090\001\090\001\090\001\090\001\090\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\088\001\000\000\
    \000\000\138\001\000\000\108\001\108\001\108\001\108\001\108\001\
    \108\001\108\001\108\001\108\001\108\001\000\000\000\000\000\000\
    \000\000\000\000\090\001\090\001\090\001\090\001\090\001\090\001\
    \137\001\000\000\088\001\000\000\000\000\000\000\000\000\000\000\
    \088\001\000\000\000\000\000\000\088\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\088\001\088\001\000\000\000\000\030\001\
    \088\001\088\001\088\001\087\001\000\000\088\001\000\000\000\000\
    \137\001\000\000\000\000\000\000\000\000\088\001\000\000\000\000\
    \000\000\088\001\000\000\088\001\087\001\091\001\091\001\091\001\
    \091\001\091\001\091\001\091\001\091\001\091\001\091\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\091\001\091\001\
    \091\001\091\001\091\001\091\001\092\001\092\001\092\001\092\001\
    \092\001\092\001\092\001\092\001\092\001\092\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\092\001\092\001\092\001\
    \092\001\092\001\092\001\000\000\000\000\000\000\091\001\091\001\
    \091\001\091\001\091\001\091\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\088\001\088\001\088\001\088\001\088\001\088\001\
    \088\001\088\001\088\001\088\001\000\000\092\001\092\001\092\001\
    \092\001\092\001\092\001\088\001\088\001\088\001\088\001\088\001\
    \088\001\144\001\097\001\144\001\000\000\000\000\143\001\143\001\
    \143\001\143\001\143\001\143\001\143\001\143\001\143\001\143\001\
    \139\001\139\001\139\001\139\001\139\001\139\001\139\001\139\001\
    \139\001\139\001\000\000\088\001\088\001\088\001\088\001\088\001\
    \088\001\096\001\096\001\096\001\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\096\001\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\096\001\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\000\000\000\000\000\000\000\000\096\001\
    \000\000\096\001\096\001\096\001\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\096\001\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\096\001\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\096\001\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\096\001\096\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\096\001\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\096\001\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\096\001\096\001\096\001\000\000\000\000\
    \000\000\000\000\096\001\000\000\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\096\001\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\096\001\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\096\001\096\001\096\001\101\001\102\001\
    \000\000\000\000\101\001\107\001\108\001\108\001\108\001\108\001\
    \108\001\108\001\108\001\108\001\108\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\153\001\101\001\000\000\106\001\
    \000\000\000\000\000\000\000\000\130\001\000\000\000\000\000\000\
    \000\000\153\001\109\001\000\000\103\001\107\001\108\001\108\001\
    \108\001\108\001\108\001\108\001\108\001\108\001\108\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \094\001\000\000\000\000\000\000\000\000\000\000\110\001\000\000\
    \000\000\000\000\000\000\111\001\139\001\139\001\139\001\139\001\
    \139\001\139\001\139\001\139\001\139\001\139\001\153\001\000\000\
    \104\001\000\000\000\000\000\000\153\001\000\000\000\000\000\000\
    \153\001\140\001\000\000\113\001\000\000\000\000\000\000\000\000\
    \153\001\000\000\000\000\112\001\153\001\000\000\153\001\152\001\
    \000\000\114\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \105\001\000\000\000\000\000\000\000\000\142\001\000\000\142\001\
    \000\000\140\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \143\001\143\001\143\001\143\001\143\001\143\001\143\001\143\001\
    \143\001\143\001\143\001\143\001\143\001\143\001\143\001\143\001\
    \143\001\143\001\143\001\143\001\155\001\155\001\155\001\155\001\
    \155\001\155\001\155\001\155\001\155\001\155\001\153\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\155\001\155\001\155\001\
    \155\001\155\001\155\001\153\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\155\001\155\001\155\001\
    \155\001\155\001\155\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \153\001\000\000\000\000\000\000\000\000\100\001\153\001\000\000\
    \000\000\000\000\153\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\153\001\000\000\000\000\000\000\153\001\000\000\
    \153\001\152\001\156\001\156\001\156\001\156\001\156\001\156\001\
    \156\001\156\001\156\001\156\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\156\001\156\001\156\001\156\001\156\001\
    \156\001\157\001\157\001\157\001\157\001\157\001\157\001\157\001\
    \157\001\157\001\157\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\157\001\157\001\157\001\157\001\157\001\157\001\
    \000\000\000\000\000\000\156\001\156\001\156\001\156\001\156\001\
    \156\001\000\000\000\000\000\000\000\000\000\000\000\000\153\001\
    \153\001\153\001\153\001\153\001\153\001\153\001\153\001\153\001\
    \153\001\000\000\157\001\157\001\157\001\157\001\157\001\157\001\
    \153\001\153\001\153\001\153\001\153\001\153\001\000\000\162\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \153\001\153\001\153\001\153\001\153\001\153\001\161\001\161\001\
    \161\001\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \161\001\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \161\001\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \000\000\000\000\000\000\000\000\161\001\000\000\161\001\161\001\
    \161\001\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \161\001\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \161\001\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \161\001\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \161\001\161\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \161\001\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \161\001\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \161\001\161\001\161\001\000\000\000\000\000\000\000\000\161\001\
    \000\000\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \161\001\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \161\001\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \161\001\161\001\161\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\159\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\003\000\000\000\121\000\255\255\
    \003\000\255\255\121\000\031\001\101\001\055\000\255\255\031\001\
    \101\001\255\255\255\255\125\000\112\000\255\255\255\255\255\255\
    \000\000\255\255\000\000\003\000\156\000\121\000\161\000\255\255\
    \255\255\255\255\031\001\101\001\255\255\000\000\008\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\005\000\108\000\112\000\116\000\196\001\005\000\
    \255\255\123\000\033\001\036\000\255\255\008\000\123\000\033\001\
    \056\000\000\000\100\000\104\000\255\255\009\000\000\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\027\000\036\000\000\000\195\000\220\000\242\000\055\001\
    \123\001\013\000\015\000\058\000\009\000\008\000\000\000\018\000\
    \022\000\029\000\030\000\032\000\020\000\024\000\000\000\012\000\
    \025\000\031\000\016\000\021\000\000\000\014\000\017\000\033\000\
    \038\000\034\000\036\000\000\000\039\000\040\000\041\000\042\000\
    \043\000\044\000\056\000\080\000\009\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\037\000\
    \061\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\083\000\127\000\058\000\011\000\130\000\
    \131\000\132\000\046\000\134\000\046\000\135\000\037\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\063\000\037\000\136\000\
    \137\000\143\000\061\000\144\000\049\000\145\000\049\000\146\000\
    \048\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\142\000\
    \048\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\148\000\149\000\142\000\089\000\063\000\
    \000\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\055\000\066\000\104\000\100\000\108\000\
    \118\000\125\000\112\000\150\000\120\000\120\000\152\000\092\000\
    \120\000\066\000\153\000\154\000\155\000\158\000\159\000\160\000\
    \190\000\193\000\194\000\197\000\198\000\199\000\056\000\080\000\
    \200\000\201\000\202\000\120\000\204\000\205\000\127\000\206\000\
    \095\000\207\000\208\000\209\000\210\000\249\000\253\000\190\000\
    \089\000\234\000\120\000\220\000\244\000\139\000\001\001\040\001\
    \238\000\058\000\012\001\020\001\024\001\041\001\066\000\008\001\
    \042\001\043\001\044\001\016\001\066\000\045\001\046\001\048\001\
    \066\000\092\000\049\001\003\001\050\001\052\001\053\001\190\000\
    \066\000\036\001\057\001\058\001\066\000\059\001\066\000\066\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\095\000\060\001\061\001\244\000\061\000\062\001\
    \036\001\069\000\069\000\069\000\069\000\069\000\069\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\076\000\076\000\
    \076\000\139\000\064\001\065\001\066\001\067\001\068\001\069\001\
    \076\000\076\000\076\000\076\000\076\000\076\000\070\001\139\000\
    \036\001\069\000\069\000\069\000\069\000\069\000\069\000\167\000\
    \167\000\167\000\167\000\167\000\167\000\167\000\167\000\167\000\
    \167\000\081\001\103\001\063\000\003\001\110\001\107\001\103\001\
    \076\000\076\000\076\000\076\000\076\000\076\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \111\001\112\001\249\000\253\000\113\001\107\001\114\001\077\000\
    \077\000\077\000\077\000\077\000\077\000\078\000\078\000\078\000\
    \078\000\078\000\078\000\078\000\078\000\078\000\078\000\115\001\
    \116\001\118\001\119\001\120\001\121\001\122\001\078\000\078\000\
    \078\000\078\000\078\000\078\000\089\000\107\001\234\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\238\000\124\001\024\001\
    \020\001\082\001\066\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\092\000\078\000\078\000\
    \078\000\078\000\078\000\078\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\125\001\008\001\095\000\126\001\
    \244\000\127\001\128\001\085\000\085\000\085\000\085\000\085\000\
    \085\000\129\001\012\001\139\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\016\001\082\001\130\001\085\001\088\001\003\001\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\131\001\085\000\085\000\085\000\085\000\085\000\
    \085\000\086\000\086\000\086\000\086\000\086\000\086\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\093\000\132\001\133\001\134\001\135\001\136\001\170\001\
    \087\000\087\000\087\000\087\000\087\000\087\000\093\000\163\000\
    \177\001\086\000\086\000\086\000\086\000\086\000\086\000\085\001\
    \088\001\255\255\163\000\255\255\255\255\163\000\163\000\163\000\
    \163\000\163\000\163\000\163\000\163\000\163\000\163\000\147\001\
    \087\000\087\000\087\000\087\000\087\000\087\000\255\255\164\001\
    \166\001\255\255\096\000\164\001\255\255\166\001\255\255\188\001\
    \255\255\255\255\146\001\093\000\184\001\255\255\255\255\096\000\
    \255\255\093\000\255\255\255\255\255\255\093\000\164\001\255\255\
    \163\001\163\001\255\255\255\255\163\001\093\000\255\255\081\001\
    \255\255\093\000\255\255\093\000\093\000\094\000\094\000\094\000\
    \094\000\094\000\094\000\094\000\094\000\094\000\094\000\163\001\
    \255\255\147\001\255\255\255\255\255\255\255\255\094\000\094\000\
    \094\000\094\000\094\000\094\000\096\000\255\255\163\001\255\255\
    \255\255\255\255\096\000\255\255\150\001\153\001\096\000\255\255\
    \168\001\255\255\192\001\255\255\255\255\255\255\096\000\082\001\
    \255\255\255\255\096\000\255\255\096\000\096\000\094\000\094\000\
    \094\000\094\000\094\000\094\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\179\001\255\255\
    \255\255\255\255\192\001\172\001\255\255\097\000\097\000\097\000\
    \097\000\097\000\097\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\255\255\150\001\153\001\
    \255\255\255\255\255\255\255\255\098\000\098\000\098\000\098\000\
    \098\000\098\000\255\255\085\001\088\001\097\000\097\000\097\000\
    \097\000\097\000\097\000\255\255\255\255\255\255\255\255\179\001\
    \255\255\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\255\255\098\000\098\000\098\000\098\000\
    \098\000\098\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\255\255\255\255\172\001\163\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \172\000\188\001\099\000\099\000\099\000\099\000\099\000\099\000\
    \255\255\255\255\184\001\172\000\255\255\147\001\172\000\172\000\
    \172\000\172\000\172\000\172\000\172\000\172\000\172\000\172\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\177\000\
    \177\000\177\000\178\000\178\000\178\000\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\179\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\179\000\
    \146\001\255\255\179\000\179\000\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\185\000\185\000\185\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\185\000\186\000\213\000\
    \213\000\213\000\213\000\213\000\213\000\213\000\213\000\213\000\
    \213\000\186\000\150\001\153\001\186\000\186\000\186\000\186\000\
    \186\000\186\000\186\000\186\000\186\000\186\000\191\000\255\255\
    \191\000\191\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\179\001\255\255\255\255\186\000\168\001\172\001\
    \192\001\255\255\186\000\255\255\255\255\191\000\192\000\192\000\
    \192\000\192\000\192\000\192\000\192\000\192\000\192\000\192\000\
    \212\000\255\255\212\000\255\255\255\255\212\000\212\000\212\000\
    \212\000\212\000\212\000\212\000\212\000\212\000\212\000\192\000\
    \255\255\255\255\255\255\255\255\255\255\191\000\214\000\214\000\
    \214\000\214\000\214\000\214\000\214\000\214\000\214\000\214\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\215\000\
    \255\255\215\000\255\255\214\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\215\000\215\000\216\000\216\000\
    \216\000\216\000\216\000\216\000\216\000\216\000\216\000\216\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\255\255\214\000\218\000\218\000\218\000\218\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\172\000\219\000\
    \219\000\219\000\219\000\219\000\219\000\219\000\219\000\219\000\
    \219\000\255\255\255\255\255\255\255\255\255\255\224\000\255\255\
    \037\001\255\255\037\001\037\001\037\001\037\001\037\001\037\001\
    \037\001\037\001\037\001\037\001\073\001\073\001\073\001\073\001\
    \073\001\073\001\073\001\073\001\073\001\073\001\255\255\037\001\
    \255\255\255\255\179\000\255\255\255\255\224\000\224\000\224\000\
    \224\000\224\000\224\000\224\000\224\000\224\000\224\000\224\000\
    \224\000\224\000\224\000\224\000\224\000\224\000\224\000\224\000\
    \224\000\224\000\224\000\224\000\224\000\224\000\224\000\037\001\
    \255\255\255\255\255\255\224\000\186\000\224\000\224\000\224\000\
    \224\000\224\000\224\000\224\000\224\000\224\000\224\000\224\000\
    \224\000\224\000\224\000\224\000\224\000\224\000\224\000\224\000\
    \224\000\224\000\224\000\224\000\224\000\224\000\224\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\255\255\229\000\255\255\255\255\227\000\255\255\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\227\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\255\255\255\255\255\255\255\255\
    \229\000\255\255\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\224\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \255\255\255\255\255\255\255\255\232\000\255\255\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \028\001\028\001\255\255\255\255\028\001\039\001\039\001\039\001\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\028\001\
    \255\255\028\001\255\255\255\255\255\255\255\255\039\001\255\255\
    \255\255\255\255\255\255\255\255\028\001\255\255\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\229\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \028\001\072\001\255\255\072\001\255\255\028\001\072\001\072\001\
    \072\001\072\001\072\001\072\001\072\001\072\001\072\001\072\001\
    \255\255\255\255\028\001\074\001\074\001\074\001\074\001\074\001\
    \074\001\074\001\074\001\074\001\074\001\028\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\028\001\255\255\255\255\
    \074\001\255\255\255\255\028\001\255\255\255\255\075\001\255\255\
    \075\001\255\255\028\001\075\001\075\001\075\001\075\001\075\001\
    \075\001\075\001\075\001\075\001\075\001\076\001\076\001\076\001\
    \076\001\076\001\076\001\076\001\076\001\076\001\076\001\255\255\
    \074\001\077\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\078\001\078\001\078\001\078\001\078\001\
    \078\001\078\001\078\001\078\001\078\001\079\001\079\001\079\001\
    \079\001\079\001\079\001\079\001\079\001\079\001\079\001\255\255\
    \086\001\087\001\087\001\087\001\087\001\087\001\087\001\087\001\
    \087\001\087\001\087\001\255\255\255\255\086\001\255\255\255\255\
    \255\255\089\001\087\001\087\001\087\001\087\001\087\001\087\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\089\001\255\255\
    \255\255\108\001\255\255\108\001\108\001\108\001\108\001\108\001\
    \108\001\108\001\108\001\108\001\108\001\255\255\255\255\255\255\
    \255\255\255\255\087\001\087\001\087\001\087\001\087\001\087\001\
    \108\001\255\255\086\001\255\255\255\255\255\255\255\255\255\255\
    \086\001\255\255\255\255\255\255\086\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\089\001\086\001\255\255\255\255\028\001\
    \086\001\089\001\086\001\086\001\255\255\089\001\255\255\255\255\
    \108\001\255\255\255\255\255\255\255\255\089\001\255\255\255\255\
    \255\255\089\001\255\255\089\001\089\001\090\001\090\001\090\001\
    \090\001\090\001\090\001\090\001\090\001\090\001\090\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\090\001\090\001\
    \090\001\090\001\090\001\090\001\091\001\091\001\091\001\091\001\
    \091\001\091\001\091\001\091\001\091\001\091\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\091\001\091\001\091\001\
    \091\001\091\001\091\001\255\255\255\255\255\255\090\001\090\001\
    \090\001\090\001\090\001\090\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\092\001\092\001\092\001\092\001\092\001\092\001\
    \092\001\092\001\092\001\092\001\255\255\091\001\091\001\091\001\
    \091\001\091\001\091\001\092\001\092\001\092\001\092\001\092\001\
    \092\001\137\001\093\001\137\001\255\255\255\255\137\001\137\001\
    \137\001\137\001\137\001\137\001\137\001\137\001\137\001\137\001\
    \138\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\255\255\092\001\092\001\092\001\092\001\092\001\
    \092\001\093\001\093\001\093\001\093\001\093\001\093\001\093\001\
    \093\001\093\001\093\001\093\001\093\001\093\001\093\001\093\001\
    \093\001\093\001\093\001\093\001\093\001\093\001\093\001\093\001\
    \093\001\093\001\093\001\255\255\255\255\255\255\255\255\093\001\
    \255\255\093\001\093\001\093\001\093\001\093\001\093\001\093\001\
    \093\001\093\001\093\001\093\001\093\001\093\001\093\001\093\001\
    \093\001\093\001\093\001\093\001\093\001\093\001\093\001\093\001\
    \093\001\093\001\093\001\096\001\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\096\001\096\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\096\001\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\096\001\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\096\001\096\001\096\001\255\255\255\255\
    \255\255\255\255\096\001\255\255\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\096\001\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\096\001\096\001\096\001\096\001\096\001\
    \096\001\096\001\096\001\096\001\096\001\096\001\098\001\098\001\
    \255\255\255\255\098\001\109\001\109\001\109\001\109\001\109\001\
    \109\001\109\001\109\001\109\001\109\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\151\001\098\001\255\255\098\001\
    \255\255\255\255\255\255\255\255\109\001\255\255\255\255\255\255\
    \255\255\151\001\098\001\255\255\098\001\098\001\098\001\098\001\
    \098\001\098\001\098\001\098\001\098\001\098\001\098\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \093\001\255\255\255\255\255\255\255\255\255\255\098\001\255\255\
    \255\255\255\255\255\255\098\001\139\001\139\001\139\001\139\001\
    \139\001\139\001\139\001\139\001\139\001\139\001\151\001\255\255\
    \098\001\255\255\255\255\255\255\151\001\255\255\255\255\255\255\
    \151\001\139\001\255\255\098\001\255\255\255\255\255\255\255\255\
    \151\001\255\255\255\255\098\001\151\001\255\255\151\001\151\001\
    \255\255\098\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \098\001\255\255\255\255\255\255\255\255\140\001\255\255\140\001\
    \255\255\139\001\140\001\140\001\140\001\140\001\140\001\140\001\
    \140\001\140\001\140\001\140\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\142\001\142\001\
    \142\001\142\001\142\001\142\001\142\001\142\001\142\001\142\001\
    \143\001\143\001\143\001\143\001\143\001\143\001\143\001\143\001\
    \143\001\143\001\144\001\144\001\144\001\144\001\144\001\144\001\
    \144\001\144\001\144\001\144\001\152\001\152\001\152\001\152\001\
    \152\001\152\001\152\001\152\001\152\001\152\001\154\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\152\001\152\001\152\001\
    \152\001\152\001\152\001\154\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\152\001\152\001\152\001\
    \152\001\152\001\152\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \154\001\255\255\255\255\255\255\255\255\098\001\154\001\255\255\
    \255\255\255\255\154\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\154\001\255\255\255\255\255\255\154\001\255\255\
    \154\001\154\001\155\001\155\001\155\001\155\001\155\001\155\001\
    \155\001\155\001\155\001\155\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\155\001\155\001\155\001\155\001\155\001\
    \155\001\156\001\156\001\156\001\156\001\156\001\156\001\156\001\
    \156\001\156\001\156\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\156\001\156\001\156\001\156\001\156\001\156\001\
    \255\255\255\255\255\255\155\001\155\001\155\001\155\001\155\001\
    \155\001\255\255\255\255\255\255\255\255\255\255\255\255\157\001\
    \157\001\157\001\157\001\157\001\157\001\157\001\157\001\157\001\
    \157\001\255\255\156\001\156\001\156\001\156\001\156\001\156\001\
    \157\001\157\001\157\001\157\001\157\001\157\001\255\255\158\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \157\001\157\001\157\001\157\001\157\001\157\001\158\001\158\001\
    \158\001\158\001\158\001\158\001\158\001\158\001\158\001\158\001\
    \158\001\158\001\158\001\158\001\158\001\158\001\158\001\158\001\
    \158\001\158\001\158\001\158\001\158\001\158\001\158\001\158\001\
    \255\255\255\255\255\255\255\255\158\001\255\255\158\001\158\001\
    \158\001\158\001\158\001\158\001\158\001\158\001\158\001\158\001\
    \158\001\158\001\158\001\158\001\158\001\158\001\158\001\158\001\
    \158\001\158\001\158\001\158\001\158\001\158\001\158\001\158\001\
    \161\001\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \161\001\161\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \161\001\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \161\001\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \161\001\161\001\161\001\255\255\255\255\255\255\255\255\161\001\
    \255\255\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \161\001\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \161\001\161\001\161\001\161\001\161\001\161\001\161\001\161\001\
    \161\001\161\001\161\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\158\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec read_json v lexbuf =
   __ocaml_lex_read_json_rec v lexbuf 0
and __ocaml_lex_read_json_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 169 "lib/read.mll"
                ( `Bool true )
# 987 "lib/read.ml"

  | 1 ->
# 170 "lib/read.mll"
                ( `Bool false )
# 992 "lib/read.ml"

  | 2 ->
# 171 "lib/read.mll"
                ( `Null )
# 997 "lib/read.ml"

  | 3 ->
# 172 "lib/read.mll"
                (
                    `Floatlit "NaN"
                )
# 1008 "lib/read.ml"

  | 4 ->
# 179 "lib/read.mll"
                (
                    `Floatlit "Infinity"
                )
# 1019 "lib/read.ml"

  | 5 ->
# 186 "lib/read.mll"
                (
                    `Floatlit "-Infinity"
                )
# 1030 "lib/read.ml"

  | 6 ->
# 193 "lib/read.mll"
                (
                    `Stringlit (finish_stringlit v lexbuf)
                )
# 1042 "lib/read.ml"

  | 7 ->
# 201 "lib/read.mll"
                         ( make_positive_int v lexbuf )
# 1047 "lib/read.ml"

  | 8 ->
# 202 "lib/read.mll"
                         ( make_negative_int v lexbuf )
# 1052 "lib/read.ml"

  | 9 ->
# 203 "lib/read.mll"
                (
                    `Floatlit (Lexing.lexeme lexbuf)
                 )
# 1063 "lib/read.ml"

  | 10 ->
# 211 "lib/read.mll"
                 ( let acc = ref [] in
                   try
                     read_space v lexbuf;
                     read_object_end lexbuf;
                     let field_name = read_ident v lexbuf in
                     read_space v lexbuf;
                     read_colon v lexbuf;
                     read_space v lexbuf;
                     acc := (field_name, read_json v lexbuf) :: !acc;
                     while true do
                       read_space v lexbuf;
                       read_object_sep v lexbuf;
                       read_space v lexbuf;
                       let field_name = read_ident v lexbuf in
                       read_space v lexbuf;
                       read_colon v lexbuf;
                       read_space v lexbuf;
                       acc := (field_name, read_json v lexbuf) :: !acc;
                     done;
                     assert false
                   with Common.End_of_object ->
                     `Assoc (List.rev !acc)
                 )
# 1090 "lib/read.ml"

  | 11 ->
# 235 "lib/read.mll"
                 ( let acc = ref [] in
                   try
                     read_space v lexbuf;
                     read_array_end lexbuf;
                     acc := read_json v lexbuf :: !acc;
                     while true do
                       read_space v lexbuf;
                       read_array_sep v lexbuf;
                       read_space v lexbuf;
                       acc := read_json v lexbuf :: !acc;
                     done;
                     assert false
                   with Common.End_of_array ->
                     `List (List.rev !acc)
                 )
# 1109 "lib/read.ml"

  | 12 ->
# 251 "lib/read.mll"
                 ( read_json v lexbuf )
# 1114 "lib/read.ml"

  | 13 ->
# 252 "lib/read.mll"
                 ( finish_comment v lexbuf; read_json v lexbuf )
# 1119 "lib/read.ml"

  | 14 ->
# 253 "lib/read.mll"
                 ( newline v lexbuf; read_json v lexbuf )
# 1124 "lib/read.ml"

  | 15 ->
# 254 "lib/read.mll"
                 ( read_json v lexbuf )
# 1129 "lib/read.ml"

  | 16 ->
# 255 "lib/read.mll"
                 ( custom_error "Unexpected end of input" v lexbuf )
# 1134 "lib/read.ml"

  | 17 ->
# 256 "lib/read.mll"
                 ( long_error "Invalid token" v lexbuf )
# 1139 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_json_rec v lexbuf __ocaml_lex_state

and finish_string v lexbuf =
   __ocaml_lex_finish_string_rec v lexbuf 56
and __ocaml_lex_finish_string_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 260 "lib/read.mll"
                  ( Buffer.contents v.buf )
# 1151 "lib/read.ml"

  | 1 ->
# 261 "lib/read.mll"
                  ( finish_escaped_char v lexbuf;
                    finish_string v lexbuf )
# 1157 "lib/read.ml"

  | 2 ->
# 263 "lib/read.mll"
                  ( add_lexeme v.buf lexbuf;
                    finish_string v lexbuf )
# 1163 "lib/read.ml"

  | 3 ->
# 265 "lib/read.mll"
                  ( custom_error "Unexpected end of input" v lexbuf )
# 1168 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_string_rec v lexbuf __ocaml_lex_state

and map_string v f lexbuf =
   __ocaml_lex_map_string_rec v f lexbuf 61
and __ocaml_lex_map_string_rec v f lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 268 "lib/read.mll"
                  ( let b = v.buf in
                    f (Buffer.contents b) 0 (Buffer.length b) )
# 1181 "lib/read.ml"

  | 1 ->
# 270 "lib/read.mll"
                  ( finish_escaped_char v lexbuf;
                    map_string v f lexbuf )
# 1187 "lib/read.ml"

  | 2 ->
# 272 "lib/read.mll"
                  ( add_lexeme v.buf lexbuf;
                    map_string v f lexbuf )
# 1193 "lib/read.ml"

  | 3 ->
# 274 "lib/read.mll"
                  ( custom_error "Unexpected end of input" v lexbuf )
# 1198 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_map_string_rec v f lexbuf __ocaml_lex_state

and finish_escaped_char v lexbuf =
   __ocaml_lex_finish_escaped_char_rec v lexbuf 66
and __ocaml_lex_finish_escaped_char_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 279 "lib/read.mll"
           c
# 1211 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 279 "lib/read.mll"
             ( Buffer.add_char v.buf c )
# 1215 "lib/read.ml"

  | 1 ->
# 280 "lib/read.mll"
         ( Buffer.add_char v.buf '\b' )
# 1220 "lib/read.ml"

  | 2 ->
# 281 "lib/read.mll"
         ( Buffer.add_char v.buf '\012' )
# 1225 "lib/read.ml"

  | 3 ->
# 282 "lib/read.mll"
         ( Buffer.add_char v.buf '\n' )
# 1230 "lib/read.ml"

  | 4 ->
# 283 "lib/read.mll"
         ( Buffer.add_char v.buf '\r' )
# 1235 "lib/read.ml"

  | 5 ->
# 284 "lib/read.mll"
         ( Buffer.add_char v.buf '\t' )
# 1240 "lib/read.ml"

  | 6 ->
let
# 285 "lib/read.mll"
                a
# 1246 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 285 "lib/read.mll"
                           b
# 1251 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and
# 285 "lib/read.mll"
                                      c
# 1256 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3)
and
# 285 "lib/read.mll"
                                                 d
# 1261 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 4) in
# 286 "lib/read.mll"
         ( let x =
             (hex a lsl 12) lor (hex b lsl 8) lor (hex c lsl 4) lor hex d
           in
           if x >= 0xD800 && x <= 0xDBFF then
             finish_surrogate_pair v x lexbuf
           else
             Codec.utf8_of_code v.buf x

         )
# 1273 "lib/read.ml"

  | 7 ->
# 295 "lib/read.mll"
         ( long_error "Invalid escape sequence" v lexbuf )
# 1278 "lib/read.ml"

  | 8 ->
# 296 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )
# 1283 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_escaped_char_rec v lexbuf __ocaml_lex_state

and finish_surrogate_pair v x lexbuf =
   __ocaml_lex_finish_surrogate_pair_rec v x lexbuf 80
and __ocaml_lex_finish_surrogate_pair_rec v x lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 299 "lib/read.mll"
                  a
# 1296 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and
# 299 "lib/read.mll"
                             b
# 1301 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3)
and
# 299 "lib/read.mll"
                                        c
# 1306 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 4)
and
# 299 "lib/read.mll"
                                                   d
# 1311 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 5) in
# 300 "lib/read.mll"
         ( let y =
             (hex a lsl 12) lor (hex b lsl 8) lor (hex c lsl 4) lor hex d
           in
           if y >= 0xDC00 && y <= 0xDFFF then
             Codec.utf8_of_surrogate_pair v.buf x y
           else
             long_error "Invalid low surrogate for code point beyond U+FFFF"
               v lexbuf
         )
# 1323 "lib/read.ml"

  | 1 ->
# 309 "lib/read.mll"
         ( long_error "Missing escape sequence representing low surrogate \
                       for code point beyond U+FFFF" v lexbuf )
# 1329 "lib/read.ml"

  | 2 ->
# 311 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )
# 1334 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_surrogate_pair_rec v x lexbuf __ocaml_lex_state

and finish_stringlit v lexbuf =
   __ocaml_lex_finish_stringlit_rec v lexbuf 89
and __ocaml_lex_finish_stringlit_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 316 "lib/read.mll"
         ( let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
           let s = Bytes.create (len+1) in
           Bytes.set s 0 '"';
           Bytes.blit lexbuf.lex_buffer lexbuf.lex_start_pos s 1 len;
           Bytes.to_string s
         )
# 1351 "lib/read.ml"

  | 1 ->
# 322 "lib/read.mll"
         ( long_error "Invalid string literal" v lexbuf )
# 1356 "lib/read.ml"

  | 2 ->
# 323 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )
# 1361 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_stringlit_rec v lexbuf __ocaml_lex_state

and read_lt v lexbuf =
   __ocaml_lex_read_lt_rec v lexbuf 100
and __ocaml_lex_read_lt_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 326 "lib/read.mll"
             ( () )
# 1373 "lib/read.ml"

  | 1 ->
# 327 "lib/read.mll"
             ( long_error "Expected '<' but found" v lexbuf )
# 1378 "lib/read.ml"

  | 2 ->
# 328 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 1383 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lt_rec v lexbuf __ocaml_lex_state

and read_gt v lexbuf =
   __ocaml_lex_read_gt_rec v lexbuf 104
and __ocaml_lex_read_gt_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 331 "lib/read.mll"
         ( () )
# 1395 "lib/read.ml"

  | 1 ->
# 332 "lib/read.mll"
         ( long_error "Expected '>' but found" v lexbuf )
# 1400 "lib/read.ml"

  | 2 ->
# 333 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )
# 1405 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_gt_rec v lexbuf __ocaml_lex_state

and read_comma v lexbuf =
   __ocaml_lex_read_comma_rec v lexbuf 108
and __ocaml_lex_read_comma_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 336 "lib/read.mll"
         ( () )
# 1417 "lib/read.ml"

  | 1 ->
# 337 "lib/read.mll"
         ( long_error "Expected ',' but found" v lexbuf )
# 1422 "lib/read.ml"

  | 2 ->
# 338 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )
# 1427 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_comma_rec v lexbuf __ocaml_lex_state

and finish_comment v lexbuf =
   __ocaml_lex_finish_comment_rec v lexbuf 112
and __ocaml_lex_finish_comment_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 341 "lib/read.mll"
         ( () )
# 1439 "lib/read.ml"

  | 1 ->
# 342 "lib/read.mll"
         ( long_error "Unterminated comment" v lexbuf )
# 1444 "lib/read.ml"

  | 2 ->
# 343 "lib/read.mll"
         ( newline v lexbuf; finish_comment v lexbuf )
# 1449 "lib/read.ml"

  | 3 ->
# 344 "lib/read.mll"
         ( finish_comment v lexbuf )
# 1454 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_comment_rec v lexbuf __ocaml_lex_state

and read_eof lexbuf =
   __ocaml_lex_read_eof_rec lexbuf 118
and __ocaml_lex_read_eof_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 352 "lib/read.mll"
              ( true )
# 1466 "lib/read.ml"

  | 1 ->
# 353 "lib/read.mll"
              ( false )
# 1471 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_eof_rec lexbuf __ocaml_lex_state

and read_space v lexbuf =
   __ocaml_lex_read_space_rec v lexbuf 120
and __ocaml_lex_read_space_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 356 "lib/read.mll"
                             ( newline v lexbuf; read_space v lexbuf )
# 1483 "lib/read.ml"

  | 1 ->
# 357 "lib/read.mll"
                             ( finish_comment v lexbuf; read_space v lexbuf )
# 1488 "lib/read.ml"

  | 2 ->
# 358 "lib/read.mll"
                             ( newline v lexbuf; read_space v lexbuf )
# 1493 "lib/read.ml"

  | 3 ->
# 359 "lib/read.mll"
                             ( read_space v lexbuf )
# 1498 "lib/read.ml"

  | 4 ->
# 360 "lib/read.mll"
                             ( () )
# 1503 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_space_rec v lexbuf __ocaml_lex_state

and read_null v lexbuf =
   __ocaml_lex_read_null_rec v lexbuf 127
and __ocaml_lex_read_null_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 363 "lib/read.mll"
              ( () )
# 1515 "lib/read.ml"

  | 1 ->
# 364 "lib/read.mll"
              ( long_error "Expected 'null' but found" v lexbuf )
# 1520 "lib/read.ml"

  | 2 ->
# 365 "lib/read.mll"
              ( custom_error "Unexpected end of input" v lexbuf )
# 1525 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_null_rec v lexbuf __ocaml_lex_state

and read_null_if_possible v lexbuf =
   __ocaml_lex_read_null_if_possible_rec v lexbuf 134
and __ocaml_lex_read_null_if_possible_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 368 "lib/read.mll"
              ( true )
# 1537 "lib/read.ml"

  | 1 ->
# 369 "lib/read.mll"
              ( false )
# 1542 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_null_if_possible_rec v lexbuf __ocaml_lex_state

and read_bool v lexbuf =
   __ocaml_lex_read_bool_rec v lexbuf 139
and __ocaml_lex_read_bool_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 372 "lib/read.mll"
                ( true )
# 1554 "lib/read.ml"

  | 1 ->
# 373 "lib/read.mll"
                ( false )
# 1559 "lib/read.ml"

  | 2 ->
# 376 "lib/read.mll"
                ( true )
# 1564 "lib/read.ml"

  | 3 ->
# 377 "lib/read.mll"
                ( false )
# 1569 "lib/read.ml"

  | 4 ->
# 379 "lib/read.mll"
                ( long_error "Expected 'true' or 'false' but found" v lexbuf )
# 1574 "lib/read.ml"

  | 5 ->
# 380 "lib/read.mll"
                ( custom_error "Unexpected end of input" v lexbuf )
# 1579 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_bool_rec v lexbuf __ocaml_lex_state

and read_int v lexbuf =
   __ocaml_lex_read_int_rec v lexbuf 163
and __ocaml_lex_read_int_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 383 "lib/read.mll"
                         ( try extract_positive_int lexbuf
                           with Int_overflow ->
                             lexer_error "Int overflow" v lexbuf )
# 1593 "lib/read.ml"

  | 1 ->
# 386 "lib/read.mll"
                         ( try extract_negative_int lexbuf
                           with Int_overflow ->
                             lexer_error "Int overflow" v lexbuf )
# 1600 "lib/read.ml"

  | 2 ->
# 389 "lib/read.mll"
                         ( (* Support for double-quoted "ints" *)
                           Buffer.clear v.buf;
                           let s = finish_string v lexbuf in
                           try
                             (* Any OCaml-compliant int will pass,
                                including hexadecimal and octal notations,
                                and embedded underscores *)
                             int_of_string s
                           with _ ->
                             custom_error
                               "Expected an integer but found a string that \
                                doesn't even represent an integer"
                               v lexbuf
                         )
# 1618 "lib/read.ml"

  | 3 ->
# 403 "lib/read.mll"
                         ( long_error "Expected integer but found" v lexbuf )
# 1623 "lib/read.ml"

  | 4 ->
# 404 "lib/read.mll"
                         ( custom_error "Unexpected end of input" v lexbuf )
# 1628 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_int_rec v lexbuf __ocaml_lex_state

and read_int32 v lexbuf =
   __ocaml_lex_read_int32_rec v lexbuf 172
and __ocaml_lex_read_int32_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 407 "lib/read.mll"
                         ( try Int32.of_string (Lexing.lexeme lexbuf)
                           with _ ->
                             lexer_error "Int32 overflow" v lexbuf )
# 1642 "lib/read.ml"

  | 1 ->
# 410 "lib/read.mll"
                         ( (* Support for double-quoted "ints" *)
                           Buffer.clear v.buf;
                           let s = finish_string v lexbuf in
                           try
                             (* Any OCaml-compliant int will pass,
                                including hexadecimal and octal notations,
                                and embedded underscores *)
                             Int32.of_string s
                           with _ ->
                             custom_error
                               "Expected an int32 but found a string that \
                                doesn't even represent an integer"
                               v lexbuf
                         )
# 1660 "lib/read.ml"

  | 2 ->
# 424 "lib/read.mll"
                         ( long_error "Expected int32 but found" v lexbuf )
# 1665 "lib/read.ml"

  | 3 ->
# 425 "lib/read.mll"
                         ( custom_error "Unexpected end of input" v lexbuf )
# 1670 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_int32_rec v lexbuf __ocaml_lex_state

and read_int64 v lexbuf =
   __ocaml_lex_read_int64_rec v lexbuf 179
and __ocaml_lex_read_int64_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 428 "lib/read.mll"
                         ( try Int64.of_string (Lexing.lexeme lexbuf)
                           with _ ->
                             lexer_error "Int32 overflow" v lexbuf )
# 1684 "lib/read.ml"

  | 1 ->
# 431 "lib/read.mll"
                         ( (* Support for double-quoted "ints" *)
                           Buffer.clear v.buf;
                           let s = finish_string v lexbuf in
                           try
                             (* Any OCaml-compliant int will pass,
                                including hexadecimal and octal notations,
                                and embedded underscores *)
                             Int64.of_string s
                           with _ ->
                             custom_error
                               "Expected an int64 but found a string that \
                                doesn't even represent an integer"
                               v lexbuf
                         )
# 1702 "lib/read.ml"

  | 2 ->
# 445 "lib/read.mll"
                         ( long_error "Expected int64 but found" v lexbuf )
# 1707 "lib/read.ml"

  | 3 ->
# 446 "lib/read.mll"
                         ( custom_error "Unexpected end of input" v lexbuf )
# 1712 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_int64_rec v lexbuf __ocaml_lex_state

and read_number v lexbuf =
   __ocaml_lex_read_number_rec v lexbuf 186
and __ocaml_lex_read_number_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 449 "lib/read.mll"
                ( nan )
# 1724 "lib/read.ml"

  | 1 ->
# 450 "lib/read.mll"
                ( infinity )
# 1729 "lib/read.ml"

  | 2 ->
# 451 "lib/read.mll"
                ( neg_infinity )
# 1734 "lib/read.ml"

  | 3 ->
# 452 "lib/read.mll"
                ( float_of_string (Lexing.lexeme lexbuf) )
# 1739 "lib/read.ml"

  | 4 ->
# 453 "lib/read.mll"
                ( Buffer.clear v.buf;
                  let s = finish_string v lexbuf in
                  try
                    (* Any OCaml-compliant float will pass,
                       including hexadecimal and octal notations,
                       and embedded underscores. *)
                    float_of_string s
                  with _ ->
                    match s with
                        "NaN" -> nan
                      | "Infinity" -> infinity
                      | "-Infinity" -> neg_infinity
                      | _ ->
                          custom_error
                            "Expected a number but found a string that \
                             doesn't even represent a number"
                            v lexbuf
                )
# 1761 "lib/read.ml"

  | 5 ->
# 471 "lib/read.mll"
                ( long_error "Expected number but found" v lexbuf )
# 1766 "lib/read.ml"

  | 6 ->
# 472 "lib/read.mll"
                ( custom_error "Unexpected end of input" v lexbuf )
# 1771 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_number_rec v lexbuf __ocaml_lex_state

and read_string v lexbuf =
   __ocaml_lex_read_string_rec v lexbuf 220
and __ocaml_lex_read_string_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 475 "lib/read.mll"
             ( Buffer.clear v.buf;
               finish_string v lexbuf )
# 1784 "lib/read.ml"

  | 1 ->
# 477 "lib/read.mll"
             ( long_error "Expected '\"' but found" v lexbuf )
# 1789 "lib/read.ml"

  | 2 ->
# 478 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 1794 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_string_rec v lexbuf __ocaml_lex_state

and read_ident v lexbuf =
   __ocaml_lex_read_ident_rec v lexbuf 224
and __ocaml_lex_read_ident_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 481 "lib/read.mll"
             ( Buffer.clear v.buf;
               finish_string v lexbuf )
# 1807 "lib/read.ml"

  | 1 ->
let
# 483 "lib/read.mll"
             s
# 1813 "lib/read.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 484 "lib/read.mll"
             ( s )
# 1817 "lib/read.ml"

  | 2 ->
# 485 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )
# 1822 "lib/read.ml"

  | 3 ->
# 486 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 1827 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_ident_rec v lexbuf __ocaml_lex_state

and map_ident v f lexbuf =
   __ocaml_lex_map_ident_rec v f lexbuf 229
and __ocaml_lex_map_ident_rec v f lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 489 "lib/read.mll"
             ( Buffer.clear v.buf;
               map_string v f lexbuf )
# 1840 "lib/read.ml"

  | 1 ->
# 492 "lib/read.mll"
             ( map_lexeme f lexbuf )
# 1845 "lib/read.ml"

  | 2 ->
# 493 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )
# 1850 "lib/read.ml"

  | 3 ->
# 494 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 1855 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_map_ident_rec v f lexbuf __ocaml_lex_state

and read_sequence read_cell init_acc v lexbuf =
   __ocaml_lex_read_sequence_rec read_cell init_acc v lexbuf 234
and __ocaml_lex_read_sequence_rec read_cell init_acc v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 497 "lib/read.mll"
             ( let acc = ref init_acc in
               try
                 read_space v lexbuf;
                 read_array_end lexbuf;
                 acc := read_cell !acc v lexbuf;
                 while true do
                   read_space v lexbuf;
                   read_array_sep v lexbuf;
                   read_space v lexbuf;
                   acc := read_cell !acc v lexbuf;
                 done;
                 assert false
               with Common.End_of_array ->
                 !acc
             )
# 1881 "lib/read.ml"

  | 1 ->
# 512 "lib/read.mll"
             ( long_error "Expected '[' but found" v lexbuf )
# 1886 "lib/read.ml"

  | 2 ->
# 513 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 1891 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_sequence_rec read_cell init_acc v lexbuf __ocaml_lex_state

and read_list_rev read_cell v lexbuf =
   __ocaml_lex_read_list_rev_rec read_cell v lexbuf 238
and __ocaml_lex_read_list_rev_rec read_cell v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 516 "lib/read.mll"
             ( let acc = ref [] in
               try
                 read_space v lexbuf;
                 read_array_end lexbuf;
                 acc := read_cell v lexbuf :: !acc;
                 while true do
                   read_space v lexbuf;
                   read_array_sep v lexbuf;
                   read_space v lexbuf;
                   acc := read_cell v lexbuf :: !acc;
                 done;
                 assert false
               with Common.End_of_array ->
                 !acc
             )
# 1917 "lib/read.ml"

  | 1 ->
# 531 "lib/read.mll"
             ( long_error "Expected '[' but found" v lexbuf )
# 1922 "lib/read.ml"

  | 2 ->
# 532 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 1927 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_list_rev_rec read_cell v lexbuf __ocaml_lex_state

and read_array_end lexbuf =
   __ocaml_lex_read_array_end_rec lexbuf 242
and __ocaml_lex_read_array_end_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 535 "lib/read.mll"
             ( raise Common.End_of_array )
# 1939 "lib/read.ml"

  | 1 ->
# 536 "lib/read.mll"
             ( () )
# 1944 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_array_end_rec lexbuf __ocaml_lex_state

and read_array_sep v lexbuf =
   __ocaml_lex_read_array_sep_rec v lexbuf 244
and __ocaml_lex_read_array_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 539 "lib/read.mll"
             ( () )
# 1956 "lib/read.ml"

  | 1 ->
# 540 "lib/read.mll"
             ( raise Common.End_of_array )
# 1961 "lib/read.ml"

  | 2 ->
# 541 "lib/read.mll"
             ( long_error "Expected ',' or ']' but found" v lexbuf )
# 1966 "lib/read.ml"

  | 3 ->
# 542 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 1971 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_array_sep_rec v lexbuf __ocaml_lex_state

and read_abstract_fields read_key read_field init_acc v lexbuf =
   __ocaml_lex_read_abstract_fields_rec read_key read_field init_acc v lexbuf 249
and __ocaml_lex_read_abstract_fields_rec read_key read_field init_acc v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 546 "lib/read.mll"
             ( let acc = ref init_acc in
               try
                 read_space v lexbuf;
                 read_object_end lexbuf;
                 let field_name = read_key v lexbuf in
                 read_space v lexbuf;
                 read_colon v lexbuf;
                 read_space v lexbuf;
                 acc := read_field !acc field_name v lexbuf;
                 while true do
                   read_space v lexbuf;
                   read_object_sep v lexbuf;
                   read_space v lexbuf;
                   let field_name = read_key v lexbuf in
                   read_space v lexbuf;
                   read_colon v lexbuf;
                   read_space v lexbuf;
                   acc := read_field !acc field_name v lexbuf;
                 done;
                 assert false
               with Common.End_of_object ->
                 !acc
             )
# 2005 "lib/read.ml"

  | 1 ->
# 569 "lib/read.mll"
             ( long_error "Expected '{' but found" v lexbuf )
# 2010 "lib/read.ml"

  | 2 ->
# 570 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 2015 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_abstract_fields_rec read_key read_field init_acc v lexbuf __ocaml_lex_state

and read_lcurl v lexbuf =
   __ocaml_lex_read_lcurl_rec v lexbuf 253
and __ocaml_lex_read_lcurl_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 573 "lib/read.mll"
             ( () )
# 2027 "lib/read.ml"

  | 1 ->
# 574 "lib/read.mll"
             ( long_error "Expected '{' but found" v lexbuf )
# 2032 "lib/read.ml"

  | 2 ->
# 575 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 2037 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lcurl_rec v lexbuf __ocaml_lex_state

and read_object_end lexbuf =
   __ocaml_lex_read_object_end_rec lexbuf 257
and __ocaml_lex_read_object_end_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 578 "lib/read.mll"
             ( raise Common.End_of_object )
# 2049 "lib/read.ml"

  | 1 ->
# 579 "lib/read.mll"
             ( () )
# 2054 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_object_end_rec lexbuf __ocaml_lex_state

and read_object_sep v lexbuf =
   __ocaml_lex_read_object_sep_rec v lexbuf 259
and __ocaml_lex_read_object_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 582 "lib/read.mll"
             ( () )
# 2066 "lib/read.ml"

  | 1 ->
# 583 "lib/read.mll"
             ( raise Common.End_of_object )
# 2071 "lib/read.ml"

  | 2 ->
# 584 "lib/read.mll"
             ( long_error "Expected ',' or '}' but found" v lexbuf )
# 2076 "lib/read.ml"

  | 3 ->
# 585 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 2081 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_object_sep_rec v lexbuf __ocaml_lex_state

and read_colon v lexbuf =
   __ocaml_lex_read_colon_rec v lexbuf 264
and __ocaml_lex_read_colon_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 588 "lib/read.mll"
             ( () )
# 2093 "lib/read.ml"

  | 1 ->
# 589 "lib/read.mll"
             ( long_error "Expected ':' but found" v lexbuf )
# 2098 "lib/read.ml"

  | 2 ->
# 590 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 2103 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_colon_rec v lexbuf __ocaml_lex_state

and read_lpar v lexbuf =
   __ocaml_lex_read_lpar_rec v lexbuf 268
and __ocaml_lex_read_lpar_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 593 "lib/read.mll"
             ( () )
# 2115 "lib/read.ml"

  | 1 ->
# 594 "lib/read.mll"
             ( long_error "Expected '(' but found" v lexbuf )
# 2120 "lib/read.ml"

  | 2 ->
# 595 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 2125 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lpar_rec v lexbuf __ocaml_lex_state

and read_rpar v lexbuf =
   __ocaml_lex_read_rpar_rec v lexbuf 272
and __ocaml_lex_read_rpar_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 598 "lib/read.mll"
             ( () )
# 2137 "lib/read.ml"

  | 1 ->
# 599 "lib/read.mll"
             ( long_error "Expected ')' but found" v lexbuf )
# 2142 "lib/read.ml"

  | 2 ->
# 600 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 2147 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rpar_rec v lexbuf __ocaml_lex_state

and read_lbr v lexbuf =
   __ocaml_lex_read_lbr_rec v lexbuf 276
and __ocaml_lex_read_lbr_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 603 "lib/read.mll"
             ( () )
# 2159 "lib/read.ml"

  | 1 ->
# 604 "lib/read.mll"
             ( long_error "Expected '[' but found" v lexbuf )
# 2164 "lib/read.ml"

  | 2 ->
# 605 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 2169 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lbr_rec v lexbuf __ocaml_lex_state

and read_rbr v lexbuf =
   __ocaml_lex_read_rbr_rec v lexbuf 280
and __ocaml_lex_read_rbr_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 608 "lib/read.mll"
             ( () )
# 2181 "lib/read.ml"

  | 1 ->
# 609 "lib/read.mll"
             ( long_error "Expected ']' but found" v lexbuf )
# 2186 "lib/read.ml"

  | 2 ->
# 610 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 2191 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rbr_rec v lexbuf __ocaml_lex_state

and skip_json v lexbuf =
   __ocaml_lex_skip_json_rec v lexbuf 284
and __ocaml_lex_skip_json_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 616 "lib/read.mll"
                ( () )
# 2203 "lib/read.ml"

  | 1 ->
# 617 "lib/read.mll"
                ( () )
# 2208 "lib/read.ml"

  | 2 ->
# 618 "lib/read.mll"
                ( () )
# 2213 "lib/read.ml"

  | 3 ->
# 619 "lib/read.mll"
                ( () )
# 2218 "lib/read.ml"

  | 4 ->
# 620 "lib/read.mll"
                ( () )
# 2223 "lib/read.ml"

  | 5 ->
# 621 "lib/read.mll"
                ( () )
# 2228 "lib/read.ml"

  | 6 ->
# 622 "lib/read.mll"
                ( finish_skip_stringlit v lexbuf )
# 2233 "lib/read.ml"

  | 7 ->
# 623 "lib/read.mll"
                          ( () )
# 2238 "lib/read.ml"

  | 8 ->
# 624 "lib/read.mll"
                ( () )
# 2243 "lib/read.ml"

  | 9 ->
# 626 "lib/read.mll"
                 ( try
                     read_space v lexbuf;
                     read_object_end lexbuf;
                     skip_ident v lexbuf;
                     read_space v lexbuf;
                     read_colon v lexbuf;
                     read_space v lexbuf;
                     skip_json v lexbuf;
                     while true do
                       read_space v lexbuf;
                       read_object_sep v lexbuf;
                       read_space v lexbuf;
                       skip_ident v lexbuf;
                       read_space v lexbuf;
                       read_colon v lexbuf;
                       read_space v lexbuf;
                       skip_json v lexbuf;
                     done;
                     assert false
                   with Common.End_of_object ->
                     ()
                 )
# 2269 "lib/read.ml"

  | 10 ->
# 649 "lib/read.mll"
                 ( try
                     read_space v lexbuf;
                     read_array_end lexbuf;
                     skip_json v lexbuf;
                     while true do
                       read_space v lexbuf;
                       read_array_sep v lexbuf;
                       read_space v lexbuf;
                       skip_json v lexbuf;
                     done;
                     assert false
                   with Common.End_of_array ->
                     ()
                 )
# 2287 "lib/read.ml"

  | 11 ->
# 664 "lib/read.mll"
                 ( skip_json v lexbuf )
# 2292 "lib/read.ml"

  | 12 ->
# 665 "lib/read.mll"
                 ( finish_comment v lexbuf; skip_json v lexbuf )
# 2297 "lib/read.ml"

  | 13 ->
# 666 "lib/read.mll"
                 ( newline v lexbuf; skip_json v lexbuf )
# 2302 "lib/read.ml"

  | 14 ->
# 667 "lib/read.mll"
                 ( skip_json v lexbuf )
# 2307 "lib/read.ml"

  | 15 ->
# 668 "lib/read.mll"
                 ( custom_error "Unexpected end of input" v lexbuf )
# 2312 "lib/read.ml"

  | 16 ->
# 669 "lib/read.mll"
                 ( long_error "Invalid token" v lexbuf )
# 2317 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_skip_json_rec v lexbuf __ocaml_lex_state

and finish_skip_stringlit v lexbuf =
   __ocaml_lex_finish_skip_stringlit_rec v lexbuf 338
and __ocaml_lex_finish_skip_stringlit_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 675 "lib/read.mll"
         ( () )
# 2329 "lib/read.ml"

  | 1 ->
# 676 "lib/read.mll"
         ( long_error "Invalid string literal" v lexbuf )
# 2334 "lib/read.ml"

  | 2 ->
# 677 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )
# 2339 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_skip_stringlit_rec v lexbuf __ocaml_lex_state

and skip_ident v lexbuf =
   __ocaml_lex_skip_ident_rec v lexbuf 349
and __ocaml_lex_skip_ident_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 680 "lib/read.mll"
             ( finish_skip_stringlit v lexbuf )
# 2351 "lib/read.ml"

  | 1 ->
# 681 "lib/read.mll"
             ( () )
# 2356 "lib/read.ml"

  | 2 ->
# 682 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )
# 2361 "lib/read.ml"

  | 3 ->
# 683 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 2366 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_skip_ident_rec v lexbuf __ocaml_lex_state

and buffer_json v lexbuf =
   __ocaml_lex_buffer_json_rec v lexbuf 354
and __ocaml_lex_buffer_json_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 696 "lib/read.mll"
                ( add_lexeme v.buf lexbuf )
# 2378 "lib/read.ml"

  | 1 ->
# 698 "lib/read.mll"
                ( finish_buffer_stringlit v lexbuf )
# 2383 "lib/read.ml"

  | 2 ->
# 699 "lib/read.mll"
                 ( try
                     Buffer.add_char v.buf '{';
                     buffer_space v lexbuf;
                     buffer_object_end v lexbuf;
                     buffer_ident v lexbuf;
                     buffer_space v lexbuf;
                     buffer_colon v lexbuf;
                     buffer_space v lexbuf;
                     buffer_json v lexbuf;
                     while true do
                       buffer_space v lexbuf;
                       buffer_object_sep v lexbuf;
                       buffer_space v lexbuf;
                       buffer_ident v lexbuf;
                       buffer_space v lexbuf;
                       buffer_colon v lexbuf;
                       buffer_space v lexbuf;
                       buffer_json v lexbuf;
                     done;
                     assert false
                   with Common.End_of_object ->
                     ()
                 )
# 2410 "lib/read.ml"

  | 3 ->
# 723 "lib/read.mll"
                 ( try
                     Buffer.add_char v.buf '[';
                     buffer_space v lexbuf;
                     buffer_array_end v lexbuf;
                     buffer_json v lexbuf;
                     while true do
                       buffer_space v lexbuf;
                       buffer_array_sep v lexbuf;
                       buffer_space v lexbuf;
                       buffer_json v lexbuf;
                     done;
                     assert false
                   with Common.End_of_array ->
                     ()
                 )
# 2429 "lib/read.ml"

  | 4 ->
# 739 "lib/read.mll"
                 ( add_lexeme v.buf lexbuf; buffer_json v lexbuf )
# 2434 "lib/read.ml"

  | 5 ->
# 740 "lib/read.mll"
                 ( Buffer.add_string v.buf "/*";
                   finish_buffer_comment v lexbuf;
                   buffer_json v lexbuf )
# 2441 "lib/read.ml"

  | 6 ->
# 743 "lib/read.mll"
                 ( Buffer.add_char v.buf '\n';
                   newline v lexbuf;
                   buffer_json v lexbuf )
# 2448 "lib/read.ml"

  | 7 ->
# 746 "lib/read.mll"
                 ( add_lexeme v.buf lexbuf; buffer_json v lexbuf )
# 2453 "lib/read.ml"

  | 8 ->
# 747 "lib/read.mll"
                 ( custom_error "Unexpected end of input" v lexbuf )
# 2458 "lib/read.ml"

  | 9 ->
# 748 "lib/read.mll"
                 ( long_error "Invalid token" v lexbuf )
# 2463 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_json_rec v lexbuf __ocaml_lex_state

and finish_buffer_stringlit v lexbuf =
   __ocaml_lex_finish_buffer_stringlit_rec v lexbuf 403
and __ocaml_lex_finish_buffer_stringlit_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 754 "lib/read.mll"
         ( Buffer.add_char v.buf '"';
           add_lexeme v.buf lexbuf
         )
# 2477 "lib/read.ml"

  | 1 ->
# 757 "lib/read.mll"
         ( long_error "Invalid string literal" v lexbuf )
# 2482 "lib/read.ml"

  | 2 ->
# 758 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )
# 2487 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_buffer_stringlit_rec v lexbuf __ocaml_lex_state

and buffer_ident v lexbuf =
   __ocaml_lex_buffer_ident_rec v lexbuf 414
and __ocaml_lex_buffer_ident_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 761 "lib/read.mll"
             ( finish_buffer_stringlit v lexbuf )
# 2499 "lib/read.ml"

  | 1 ->
# 762 "lib/read.mll"
             ( add_lexeme v.buf lexbuf )
# 2504 "lib/read.ml"

  | 2 ->
# 763 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )
# 2509 "lib/read.ml"

  | 3 ->
# 764 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 2514 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_ident_rec v lexbuf __ocaml_lex_state

and buffer_space v lexbuf =
   __ocaml_lex_buffer_space_rec v lexbuf 419
and __ocaml_lex_buffer_space_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 767 "lib/read.mll"
                             (
    add_lexeme v.buf lexbuf;
    newline v lexbuf;
    buffer_space v lexbuf )
# 2529 "lib/read.ml"

  | 1 ->
# 771 "lib/read.mll"
                             (
    Buffer.add_string v.buf "/*";
    finish_buffer_comment v lexbuf;
    buffer_space v lexbuf )
# 2537 "lib/read.ml"

  | 2 ->
# 775 "lib/read.mll"
                             (
    Buffer.add_char v.buf '\n';
    newline v lexbuf;
    buffer_space v lexbuf )
# 2545 "lib/read.ml"

  | 3 ->
# 779 "lib/read.mll"
                             (
    add_lexeme v.buf lexbuf;
    buffer_space v lexbuf )
# 2552 "lib/read.ml"

  | 4 ->
# 782 "lib/read.mll"
                             ( () )
# 2557 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_space_rec v lexbuf __ocaml_lex_state

and buffer_object_end v lexbuf =
   __ocaml_lex_buffer_object_end_rec v lexbuf 426
and __ocaml_lex_buffer_object_end_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 785 "lib/read.mll"
             (
      Buffer.add_char v.buf '}';
      raise Common.End_of_object )
# 2571 "lib/read.ml"

  | 1 ->
# 788 "lib/read.mll"
             ( () )
# 2576 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_object_end_rec v lexbuf __ocaml_lex_state

and buffer_object_sep v lexbuf =
   __ocaml_lex_buffer_object_sep_rec v lexbuf 428
and __ocaml_lex_buffer_object_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 791 "lib/read.mll"
             ( Buffer.add_char v.buf ',' )
# 2588 "lib/read.ml"

  | 1 ->
# 792 "lib/read.mll"
             ( Buffer.add_char v.buf '}'; raise Common.End_of_object )
# 2593 "lib/read.ml"

  | 2 ->
# 793 "lib/read.mll"
             ( long_error "Expected ',' or '}' but found" v lexbuf )
# 2598 "lib/read.ml"

  | 3 ->
# 794 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 2603 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_object_sep_rec v lexbuf __ocaml_lex_state

and buffer_array_end v lexbuf =
   __ocaml_lex_buffer_array_end_rec v lexbuf 433
and __ocaml_lex_buffer_array_end_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 797 "lib/read.mll"
             ( Buffer.add_char v.buf ']'; raise Common.End_of_array )
# 2615 "lib/read.ml"

  | 1 ->
# 798 "lib/read.mll"
             ( () )
# 2620 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_array_end_rec v lexbuf __ocaml_lex_state

and buffer_array_sep v lexbuf =
   __ocaml_lex_buffer_array_sep_rec v lexbuf 435
and __ocaml_lex_buffer_array_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 801 "lib/read.mll"
             ( Buffer.add_char v.buf ',' )
# 2632 "lib/read.ml"

  | 1 ->
# 802 "lib/read.mll"
             ( Buffer.add_char v.buf ']'; raise Common.End_of_array )
# 2637 "lib/read.ml"

  | 2 ->
# 803 "lib/read.mll"
             ( long_error "Expected ',' or ']' but found" v lexbuf )
# 2642 "lib/read.ml"

  | 3 ->
# 804 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 2647 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_array_sep_rec v lexbuf __ocaml_lex_state

and buffer_colon v lexbuf =
   __ocaml_lex_buffer_colon_rec v lexbuf 440
and __ocaml_lex_buffer_colon_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 807 "lib/read.mll"
             ( Buffer.add_char v.buf ':' )
# 2659 "lib/read.ml"

  | 1 ->
# 808 "lib/read.mll"
             ( long_error "Expected ':' but found" v lexbuf )
# 2664 "lib/read.ml"

  | 2 ->
# 809 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 2669 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_colon_rec v lexbuf __ocaml_lex_state

and buffer_gt v lexbuf =
   __ocaml_lex_buffer_gt_rec v lexbuf 444
and __ocaml_lex_buffer_gt_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 812 "lib/read.mll"
         ( Buffer.add_char v.buf '>' )
# 2681 "lib/read.ml"

  | 1 ->
# 813 "lib/read.mll"
         ( long_error "Expected '>' but found" v lexbuf )
# 2686 "lib/read.ml"

  | 2 ->
# 814 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )
# 2691 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_gt_rec v lexbuf __ocaml_lex_state

and finish_buffer_comment v lexbuf =
   __ocaml_lex_finish_buffer_comment_rec v lexbuf 448
and __ocaml_lex_finish_buffer_comment_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 817 "lib/read.mll"
         ( Buffer.add_string v.buf "*/" )
# 2703 "lib/read.ml"

  | 1 ->
# 818 "lib/read.mll"
         ( long_error "Unterminated comment" v lexbuf )
# 2708 "lib/read.ml"

  | 2 ->
# 819 "lib/read.mll"
         ( Buffer.add_char v.buf '\n';
           newline v lexbuf;
           finish_buffer_comment v lexbuf )
# 2715 "lib/read.ml"

  | 3 ->
# 822 "lib/read.mll"
         ( add_lexeme v.buf lexbuf; finish_buffer_comment v lexbuf )
# 2720 "lib/read.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_buffer_comment_rec v lexbuf __ocaml_lex_state

;;

# 824 "lib/read.mll"
 
  let _ = (read_json : lexer_state -> Lexing.lexbuf -> t)

  let read_t = read_json

  let read_int8 v lexbuf =
    let n = read_int v lexbuf in
    if n < 0 || n > 255 then
      lexer_error "Int8 overflow" v lexbuf
    else
      char_of_int n

  let read_list read_cell v lexbuf =
    List.rev (read_list_rev read_cell v lexbuf)

  let array_of_rev_list l =
    match l with
        [] -> [| |]
      | x :: tl ->
          let len = List.length l in
          let a = Array.make len x in
          let r = ref tl in
          for i = len - 2 downto 0 do
            a.(i) <- List.hd !r;
            r := List.tl !r
          done;
          a

  let read_array read_cell v lexbuf =
    let l = read_list_rev read_cell v lexbuf in
    array_of_rev_list l

  (* Read a JSON object, reading the keys into OCaml strings
     (provided for backward compatibility) *)
  let read_fields read_field init_acc v =
    read_abstract_fields read_ident read_field init_acc v

  let finish v lexbuf =
    read_space v lexbuf;
    if not (read_eof lexbuf) then
      long_error "Junk after end of JSON value:" v lexbuf

  let init_lexer = Common.init_lexer

  let from_lexbuf v ?(stream = false) lexbuf =
    read_space v lexbuf;

    let x =
      if read_eof lexbuf then
        raise Common.End_of_input
      else
        read_json v lexbuf
    in

    if not stream then
      finish v lexbuf;

    x


  let from_string ?buf ?fname ?lnum s =
    try
      let lexbuf = Lexing.from_string s in
      let v = init_lexer ?buf ?fname ?lnum () in
      from_lexbuf v lexbuf
    with Common.End_of_input ->
      Common.json_error "Blank input data"

  let from_channel ?buf ?fname ?lnum ic =
    try
      let lexbuf = Lexing.from_channel ic in
      let v = init_lexer ?buf ?fname ?lnum () in
      from_lexbuf v lexbuf
    with Common.End_of_input ->
      Common.json_error "Blank input data"

  let from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    try
      let x = from_channel ?buf ?fname ?lnum ic in
      close_in ic;
      x
    with e ->
      close_in_noerr ic;
      raise e

  exception Finally of exn * exn

  let seq_from_lexbuf v ?(fin = fun () -> ()) lexbuf =
    let stream = Some true in
    let rec f () =
      try Seq.Cons (from_lexbuf v ?stream lexbuf, f)
      with
          Common.End_of_input ->
            fin ();
            Seq.Nil
        | e ->
            (try fin () with fin_e -> raise (Finally (e, fin_e)));
            raise e
    in
    f

  let seq_from_string ?buf ?fname ?lnum s =
    let v = init_lexer ?buf ?fname ?lnum () in
    seq_from_lexbuf v (Lexing.from_string s)

  let seq_from_channel ?buf ?fin ?fname ?lnum ic =
    let lexbuf = Lexing.from_channel ic in
    let v = init_lexer ?buf ?fname ?lnum () in
    seq_from_lexbuf v ?fin lexbuf

  let seq_from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    let fin () = close_in ic in
    let fname =
      match fname with
          None -> Some file
        | x -> x
    in
    let lexbuf = Lexing.from_channel ic in
    let v = init_lexer ?buf ?fname ?lnum () in
    seq_from_lexbuf v ~fin lexbuf

  type json_line = [ `Json of t | `Exn of exn ]

  let lineseq_from_channel
      ?buf ?(fin = fun () -> ()) ?fname ?lnum:(lnum0 = 1) ic =
    let buf =
      match buf with
          None -> Some (Buffer.create 256)
        | Some _ -> buf
    in
    let rec f lnum = fun () ->
      try
        let line = input_line ic in
        Seq.Cons (`Json (from_string ?buf ?fname ~lnum line), f (lnum + 1))
      with
          End_of_file -> fin (); Seq.Nil
        | e -> Seq.Cons (`Exn e, f (lnum + 1))
    in
    f lnum0

  let lineseq_from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    let fin () = close_in ic in
    let fname =
      match fname with
          None -> Some file
        | x -> x
    in
    lineseq_from_channel ?buf ~fin ?fname ?lnum ic

  let prettify ?std s =
    pretty_to_string ?std (from_string s)

  let compact ?std:_ s =
    to_string (from_string s)

# 2886 "lib/read.ml"

module Util = struct
exception Type_error of string * t

let typeof = function
  | `Assoc _ -> "object"
  | `Bool _ -> "bool"
  | `Float _ -> "float"
  | `List _ -> "array"
  | `Null -> "null"
  | `String _ -> "string"
  | `Intlit _ -> "intlit"
  | `Floatlit _ -> "floatlit"
  | `Stringlit _ -> "stringlit"

let typerr msg js = raise (Type_error (msg ^ typeof js, js))

exception Undefined of string * t

let assoc name obj = try List.assoc name obj with Not_found -> `Null

let member name = function
  | `Assoc obj -> assoc name obj
  | js -> typerr ("Can't get member '" ^ name ^ "' of non-object type ") js

let rec path l obj =
  match l with
  | [] -> Some obj
  | key :: l -> (
      match obj with
      | `Assoc assoc -> (
          match List.assoc key assoc with
          | obj -> path l obj
          | exception Not_found -> None)
      | _ -> None)

let index i = function
  | `List l as js ->
      let len = List.length l in
      let wrapped_index = if i < 0 then len + i else i in
      if wrapped_index < 0 || wrapped_index >= len then
        raise (Undefined ("Index " ^ string_of_int i ^ " out of bounds", js))
      else List.nth l wrapped_index
  | js ->
      typerr ("Can't get index " ^ string_of_int i ^ " of non-array type ") js

let map f = function
  | `List l -> `List (List.map f l)
  | js -> typerr "Can't map function over non-array type " js

let to_assoc = function
  | `Assoc obj -> obj
  | js -> typerr "Expected object, got " js

let to_option f = function `Null -> None | x -> Some (f x)
let to_bool = function `Bool b -> b | js -> typerr "Expected bool, got " js

let to_bool_option = function
  | `Bool b -> Some b
  | `Null -> None
  | js -> typerr "Expected bool or null, got " js

let to_number = function
  | js -> typerr "Expected number, got " js

let to_number_option = function
  | `Null -> None
  | js -> typerr "Expected number or null, got " js

let to_float = function
  | js -> typerr "Expected float, got " js

let to_float_option = function
  | `Null -> None
  | js -> typerr "Expected float or null, got " js

let to_int = function
  | js -> typerr "Expected int, got " js

let to_int_option = function
  | `Null -> None
  | js -> typerr "Expected int or null, got " js

let to_list = function `List l -> l | js -> typerr "Expected array, got " js

let to_string = function
  | js -> typerr "Expected string, got " js

let to_string_option = function
  | `Null -> None
  | js -> typerr "Expected string or null, got " js

let convert_each f = function
  | `List l -> List.map f l
  | js -> typerr "Can't convert each element of non-array type " js

let rec rev_filter_map f acc l =
  match l with
  | [] -> acc
  | x :: tl -> (
      match f x with
      | None -> rev_filter_map f acc tl
      | Some y -> rev_filter_map f (y :: acc) tl)

let filter_map f l = List.rev (rev_filter_map f [] l)

let rec rev_flatten acc l =
  match l with
  | [] -> acc
  | x :: tl -> (
      match x with
      | `List l2 -> rev_flatten (List.rev_append l2 acc) tl
      | _ -> rev_flatten acc tl)

let flatten l = List.rev (rev_flatten [] l)

let filter_index i l =
  filter_map
    (function
      | `List l -> ( try Some (List.nth l i) with _ -> None) | _ -> None)
    l

let filter_list l = filter_map (function `List l -> Some l | _ -> None) l

let filter_member k l =
  filter_map
    (function
      | `Assoc l -> ( try Some (List.assoc k l) with _ -> None) | _ -> None)
    l

let filter_assoc l = filter_map (function `Assoc l -> Some l | _ -> None) l
let filter_bool l = filter_map (function `Bool x -> Some x | _ -> None) l
let filter_int l =
  filter_map (
      function
      | _ -> None
    ) l

let filter_float l =
  filter_map (
    function
      | _ -> None
  ) l

let filter_number l =
  filter_map (
    function
      | _ -> None
  ) l

let filter_string l =
  filter_map (
    function
      | _ -> None
  ) l

let keys o =
  to_assoc o |> List.map (fun (key, _) -> key)

let values o =
  to_assoc o |> List.map (fun (_, value) -> value)

let combine (first : t) (second : t) =
  match (first, second) with
  | `Assoc a, `Assoc b -> (`Assoc (a @ b) : t)
  | a, b -> raise (Invalid_argument "Expected two objects, check inputs")
end

