(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Support for various ABIs *)

[@@@warning "-37"]

type abi = Code of int | Unsupported of string

let abi_code = function
   Code c -> c
 | Unsupported sym -> raise (Ctypes.Unsupported sym)

let aix = Unsupported "FFI_AIX"
let darwin = Unsupported "FFI_DARWIN"
let eabi = Unsupported "FFI_EABI"
let fastcall = Unsupported "FFI_FASTCALL"
let gcc_sysv = Unsupported "FFI_GCC_SYSV"
let linux = Unsupported "FFI_LINUX"
let linux64 = Unsupported "FFI_LINUX64"
let linux_soft_float = Unsupported "FFI_LINUX_SOFT_FLOAT"
let ms_cdecl = Unsupported "FFI_MS_CDECL"
let n32 = Unsupported "FFI_N32"
let n32_soft_float = Unsupported "FFI_N32_SOFT_FLOAT"
let n64 = Unsupported "FFI_N64"
let n64_soft_float = Unsupported "FFI_N64_SOFT_FLOAT"
let o32 = Unsupported "FFI_O32"
let o32_soft_float = Unsupported "FFI_O32_SOFT_FLOAT"
let osf = Unsupported "FFI_OSF"
let pa32 = Unsupported "FFI_PA32"
let stdcall = Unsupported "FFI_STDCALL"
let sysv = Unsupported "FFI_SYSV"
let thiscall = Unsupported "FFI_THISCALL"
let unix = Unsupported "FFI_UNIX"
let unix64 = Code 2
let v8 = Unsupported "FFI_V8"
let v8plus = Unsupported "FFI_V8PLUS"
let v9 = Unsupported "FFI_V9"
let vfp = Unsupported "FFI_VFP"
let default_abi = Code 2
