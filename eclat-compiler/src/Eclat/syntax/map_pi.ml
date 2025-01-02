open Ast

let map (f : e -> e) (pi: pi) : pi =
  let main = f pi.main in
  { pi with main }

