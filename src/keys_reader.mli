type key_info = {name : string; value : string}

val read_public_key_hashes :
  base_dir:string -> (key_info list, [`Msg of string]) result
