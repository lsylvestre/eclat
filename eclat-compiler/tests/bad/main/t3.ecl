
let main <<'s>>() = 
  (42:int<'s>) ;;

   (* =================
   $ ./eclat tests/bad/main/t2.ecl
     Error: The type of the program input should be a basic type. 
   ==================== *)