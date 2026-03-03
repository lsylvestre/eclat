> let f k =
  k(create<5>()) ;;
val f : forall ~v1 $v2 ~v3  . ((~v3 array<5>[l16] -[$v2]-> ~v1) -[$v2]-> ~v1) | 0


> let h  () =
  f (fun a -> f (fun b ->
      (get(a,0) || get(b,0)))) ;;
val h : forall ~v1 ~v2  . (unit -[1]-> (~v1 * ~v2)) | 0


> h ;;
file %stdin>2, from line 1, characters 4, to line 3 characters 30:
Error: 
response time 2 should be less than or equal to 1

file %stdin>2, from line 1, characters 4, to line 3 characters 30:
Error: 
response time 2 should be less than or equal to 1


> h;;
file %stdin>3, line 1, characters 0-1:
Error:  (unbound variable h)