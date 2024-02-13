open CustomStdlib

let a = Array.init 10 (fun x -> x) ;;

for i = 0 to Array.length a - 1 do
  print_int (Array.get a i)
done
