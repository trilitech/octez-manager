type t = Node | Dal_node | Baker | Accuser | Signer

let to_string = function
  | Node -> "node"
  | Dal_node -> "dal-node"
  | Baker -> "baker"
  | Accuser -> "accuser"
  | Signer -> "signer"

let of_string s =
  match String.trim @@ String.lowercase_ascii s with
  | "node" -> Node
  | "baker" -> Baker
  | "accuser" -> Accuser
  | "dal-node" -> Dal_node
  | "signer" -> Signer
  | s -> raise (Invalid_argument s)
