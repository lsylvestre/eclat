type ~a option = None | Some of ~a;;

let main o = 
  match o with
  | None -> ()
  | Some x -> print_int x ;;
