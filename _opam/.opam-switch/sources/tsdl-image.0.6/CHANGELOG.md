# Pending

* Fix typo environment lookup of `LIBSLD2_PATH` to be correct `LIBSDL2_PATH`
* Locate `SDL2_image.dll` on Windows
* Use SDL "system" category to log "Loading Sdl_image" message (we now
  log only for dev versions)

# 0.5 2022/11/30 trying to autodetect library path

And add workflow for github actions for testing dynamic libraries from
bytecode.

# 0.4 2022/11/22 Bug fix (calling ocaml on an *.ml file)

You can now (again) directly run a "toplevel file" with `ocaml`, for
instance

```
#use "topfind";;
#thread;;
#require "tsdl-image";;
Tsdl_image.Image.(init Init.empty) |> ignore
```
Thanks @anentropic for reporting on MacOS.

# 0.3 Change library name (BREAKING)

Starting from 0.3, the library name is the same as the opam package
name `tsdl-image`. (The library name used to be `tsdl_image`, which was
confusing).

# 2021 new maintainer is sanette
https://github.com/sanette/tsdl-image
