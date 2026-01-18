(* ./eclat ../proc/queue.ecl *)

let queue ((_,in_data, in_valid, out_rdy) :  (int<'N> * `a * bool * bool)) =

  let queue_step (inputs : (`a vect<'N> * int<32> * int<32> * int<32> * 
                            (bool * bool * `a))) =
    let (content, cpt, rd_ptr, wr_ptr, 
          (in_rdy, out_valid, out_data)) = inputs in
    let next_in_rdy = (cpt < vect_size content)  in  (* FIFO not full *)
    let next_out_valid = (cpt > 0)               in  (* FIFO not empty *)
    let next_out_data = vect_nth(content,rd_ptr) in
    let push = in_valid & in_rdy in
    let pop = out_valid & out_rdy in

    let (next_content, next_wr_ptr) =
      if push then let next_content = vect_update(content,wr_ptr,in_data) in
                   let next_wr_ptr = wr_ptr + 1 in
                   (next_content, next_wr_ptr)
      else (content, wr_ptr) in

    let next_rd_ptr = if pop then rd_ptr + 1 else rd_ptr in

    let next_cpt = if push & not(pop) then cpt + 1 else
                   if not(push) & pop then cpt - 1 else cpt in
    
    (next_content, next_cpt, next_rd_ptr, next_wr_ptr, 
        (next_in_rdy, next_out_valid, next_out_data))
  in
  let next_s = reg queue_step 
               init (vect_create<'N>(in_data), 0, 0, 0, (false, false, in_data)) 
  in
  let (_, _, _, _, (next_in_rdy, next_out_valid, next_out_data)) = next_s in
  (next_in_rdy, next_out_valid, next_out_data) ;;

let main () =
  queue((0:int<16>), (0:int<64>), true, true) ;;