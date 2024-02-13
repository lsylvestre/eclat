open CustomStdlib ;;
open Platform ;;


let rec wait(n) =
	if n < 1 then () else wait(n-1) ;;

while true do
  led_off();
  wait(1_000_000);
  led_on();
  wait(3_000_000)
done ;;