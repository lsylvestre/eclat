open CustomStdlib

external length : string -> int = "%string_length"
external get : string -> int -> char = "%string_safe_get"
external unsafe_get : string -> int -> char = "%string_unsafe_get"
