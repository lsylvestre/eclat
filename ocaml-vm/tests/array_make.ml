open CustomStdlib

let a = Array.make 10 100 ;;

Array.set a 6 42;;

for i = 0 to Array.length a - 1 do
  print_int (Array.get a i)
done
