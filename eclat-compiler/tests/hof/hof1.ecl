let main x =
  let f x = x in
  let g = if true then f else f in
  g 5 ;;