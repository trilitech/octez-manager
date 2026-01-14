# 0.6 2023/08/05 hide startup log

* Use system category to log "Loading Sdl_ttf" message. (We now
  log only for dev versions.)

* Fix typo environment lookup of `LIBSLD2_PATH` to be correct
  `LIBSDL2_PATH`.
* Locate `SDL2_ttf.dll` on Windows.

# 0.5 2022/11/30 trying to autodetect library path

And add workflow for github actions for testing dynamic libraries from
bytecode.

# 0.4 2022/11/22 Bug fix (calling ocaml on an *.ml file)

You can now (again) directly run a "toplevel file" with `ocaml`, for
instance

```
#use "topfind";;
#thread;;
#require "tsdl-ttf";;
Tsdl_ttf.Ttf.init () |> ignore
```

# 0.3 Change library name (BREAKING)

Starting from 0.3, the library name is the same as the opam package
name `tsdl-ttf`. (The library name used to be `tsdl_ttf`, which was
confusing).

# 2021 new maintainer is sanette
https://github.com/sanette/tsdl-ttf
