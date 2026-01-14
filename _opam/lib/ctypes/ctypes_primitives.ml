(*
 * Copyright (c) 2016 whitequark
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes_primitive_types
let sizeof : type a. a prim -> int = function
 | Char -> 1
 | Schar -> 1
 | Uchar -> 1
 | Bool -> 1
 | Short -> 2
 | Int -> 4
 | Long -> 8
 | Llong -> 8
 | Ushort -> 2
 | Sint -> 4
 | Uint -> 4
 | Ulong -> 8
 | Ullong -> 8
 | Size_t -> 8
 | Int8_t -> 1
 | Int16_t -> 2
 | Int32_t -> 4
 | Int64_t -> 8
 | Uint8_t -> 1
 | Uint16_t -> 2
 | Uint32_t -> 4
 | Uint64_t -> 8
 | Float -> 4
 | Double -> 8
 | LDouble -> 16
 | Complex32 -> 8
 | Complex64 -> 16
 | Complexld -> 32
 | Nativeint -> 8
 | Camlint -> 8
let alignment : type a. a prim -> int = function
 | Char -> 1
 | Schar -> 1
 | Uchar -> 1
 | Bool -> 1
 | Short -> 2
 | Int -> 4
 | Long -> 8
 | Llong -> 8
 | Ushort -> 2
 | Sint -> 4
 | Uint -> 4
 | Ulong -> 8
 | Ullong -> 8
 | Size_t -> 8
 | Int8_t -> 1
 | Int16_t -> 2
 | Int32_t -> 4
 | Int64_t -> 8
 | Uint8_t -> 1
 | Uint16_t -> 2
 | Uint32_t -> 4
 | Uint64_t -> 8
 | Float -> 4
 | Double -> 8
 | LDouble -> 16
 | Complex32 -> 4
 | Complex64 -> 8
 | Complexld -> 16
 | Nativeint -> 8
 | Camlint -> 8
let name : type a. a prim -> string = function
 | Char -> "char"
 | Schar -> "signed char"
 | Uchar -> "unsigned char"
 | Bool -> "_Bool"
 | Short -> "short"
 | Int -> "int"
 | Long -> "long"
 | Llong -> "long long"
 | Ushort -> "unsigned short"
 | Sint -> "int"
 | Uint -> "unsigned int"
 | Ulong -> "unsigned long"
 | Ullong -> "unsigned long long"
 | Size_t -> "size_t"
 | Int8_t -> "int8_t"
 | Int16_t -> "int16_t"
 | Int32_t -> "int32_t"
 | Int64_t -> "int64_t"
 | Uint8_t -> "uint8_t"
 | Uint16_t -> "uint16_t"
 | Uint32_t -> "uint32_t"
 | Uint64_t -> "uint64_t"
 | Float -> "float"
 | Double -> "double"
 | LDouble -> "long double"
 | Complex32 -> "float _Complex"
 | Complex64 -> "double _Complex"
 | Complexld -> "long double _Complex"
 | Nativeint -> "intnat"
 | Camlint -> "intnat"
let format_string : type a. a prim -> string option = function
 | Char -> Some "%d"
 | Schar -> Some "%d"
 | Uchar -> Some "%d"
 | Bool -> Some "%d"
 | Short -> Some "%hd"
 | Int -> Some "%d"
 | Long -> Some "%ld"
 | Llong -> Some "%lld"
 | Ushort -> Some "%hu"
 | Sint -> Some "%d"
 | Uint -> Some "%u"
 | Ulong -> Some "%lu"
 | Ullong -> Some "%llu"
 | Size_t -> Some "%zu"
 | Int8_t -> Some "%d"
 | Int16_t -> Some "%d"
 | Int32_t -> Some "%d"
 | Int64_t -> Some "%ld"
 | Uint8_t -> Some "%u"
 | Uint16_t -> Some "%u"
 | Uint32_t -> Some "%u"
 | Uint64_t -> Some "%lu"
 | Float -> Some "%.12g"
 | Double -> Some "%.12g"
 | LDouble -> Some "%.12Lg"
 | Complex32 -> None
 | Complex64 -> None
 | Complexld -> None
 | Nativeint -> Some "%ld"
 | Camlint -> Some "%ld"
let pointer_size = 8
let pointer_alignment = 8
