
let main () =
  parfor i = 0 to ?p do 
    print_int i
  done ;;

  (* ====================
     file tests/bad/macros/parfor.ecl, from line 3, characters 2, to line 5 characters 6:
     Error: 
     Cannot statically determine size ?2412
     ==================== *)