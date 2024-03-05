(* ./eclat tests/matrix/life.ecl -yosys-ecp5 -clk-top="clk48" -top="i:1|o:1" *)


let static m1 = false^256^50;;
let static m2 = false^256^50;;

let neighbourhood (m,x,y) =
  let get (i,j) : int<5> = 
    if m.(i).(j) then 1 else 0 in
  get(x-1,y-1) + get(x-1,y) + get(x-1,y+1) + 
  get(x,y-1)   + get(x,y+1) +
  get(x+1,y-1) + get(x+1,y) + get(x+1,y+1) ;;

let next_cell(m1,m2,x,y) =
  let alive = m1.(x).(y) in
  let n = neighbourhood(m1,x,y) in
  let become = if alive then (n = 2 or n = 3) else (n = 3) in
  m2.(x).(y) <- become ;;
 
let next_matrix(m1,m2) =
  let rec loop(j) = (* ignore la bordure *)
    if j >= m1[0].length-2 then () else
    (for i = 1 to m1[1].length-2 do 
    next_cell(m1,m2,i,j)
    done ; loop(j+1))
  in loop(1) ;;

let counter () =
  reg (fun c -> c + 1) last 0 ;;

let main () =
  let cy = counter () in
  exec 
    next_matrix(m1,m2);
    print_string "cy:"; print_int cy; print_newline ();
    next_matrix(m2,m1)
  default ()  ;;