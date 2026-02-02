
let main a = 
  exec get(a,0) default 0 ;;

   (* =================
   $ ./eclat tests/bad/main/t1.ecl
     Error: The type of the program input should be a basic type. 
   ==================== *)