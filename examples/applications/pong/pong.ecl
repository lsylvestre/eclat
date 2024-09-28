let rec wait() = () ;;

external interface_accelerometer : (int<2> * std_logic * std_logic) => (int<16> * int<16> * int<16> * std_logic * bool * std_logic * std_logic) ;;

type box = int<11>*int<11>*int<11>*int<11>*(int<4>*int<4>*int<4>) ;; (* [| x;y;w;h;r;g;b |] *)

let (bg_r,bg_g,bg_b) = (0,0,1) ;;

let ball_color = (-1,0,0) ;;
let player_color = (-1,-1,-1) ;;

let (player_w,player_h) = (20,100) ;;

let ball_size = 14 ;;

let collision((x1,y1,w1,h1),(x2,y2,w2,h2)) =
    (x1 <= x2 + w2) && (x1 + w1 >= x2) && (y1 <= y2 + h2) && (y1 + h1 >= y2) ;;

let pll(f : unit -> (bool*bool*int<4>*int<4>*int<4>)) =
    reg (fun v -> let (v,_) = exec f() default v in v) init (true,true,0,0,0);;
    (* let (_,v) = reg (fun (indic,v) -> if indic then (false,v) else (true, f())) last (true,(true,true,0,0,0)) in v;;*)

let hvpos() =
    reg (fun (h,v) -> if h < 799 then (h+1,v) else if v < 525 then (0,v+1) else (0,0)) last (-1,0) ;;

let display(ball,player1,player2) =
    let (h_offset,v_offset) = (160,45) in
    let (hpos,vpos) = hvpos() in

    let color_to_draw(i,j) =
        let (x1,y1,w1,h1,(r1,g1,b1)) = ball in
        let (x2,y2,w2,h2,(r2,g2,b2)) = player1 in
        let (x3,y3,w3,h3,(r3,g3,b3)) = player2 in
        wait();
        if (vpos >= y1+v_offset && vpos < y1+h1+v_offset && hpos >= x1+h_offset && hpos < x1+w1+h_offset) then
            (r1,g1,b1)
        else if (vpos >= y2+v_offset && vpos < y2+h2+v_offset && hpos >= x2+h_offset && hpos < x2+w2+h_offset) then
            (r2,g2,b2)
        else if (vpos >= y3+v_offset && vpos < y3+h3+v_offset && hpos >= x3+h_offset && hpos < x3+w3+h_offset) then
            (r3,g3,b3)
        else
            if hpos = 480 then (-3,-3,-3) else (bg_r,bg_g,bg_b) (* 640/2 + h_offset *)
    in
    let (r,g,b) : (int<4> * int<4> * int<4>) = color_to_draw(hpos,vpos) in

    let hsync = hpos <= 16 or hpos >= 112 in
    let vsync = vpos <= 10 or vpos >= 12 in

    let (r,g,b) =
        if ((hpos < h_offset) or (vpos < v_offset)) then
            (0,0,0)
        else
            (r,g,b)
    in
    (hsync,vsync,r,g,b) ;;

let move((ball,player1,player2,p1_dy,score) : box * box * box * int<11> * int<5>) : box * box * box * int<5> =
    let player_pot_delta = (0,3) in
    let ball_pot_delta = (2,2) in

    let (ball_x,ball_y,ball_w,ball_h,(_,_,_)) = ball in
    let (player1_x,player1_y,player1_w,player1_h,(_,_,_)) = player1 in
    let (player2_x,player2_y,player2_w,player2_h,(_,_,_)) = player2 in

    
    let player1_delta(top_col,bot_col) = let (_,dy,_) = reg (fun (dy,r_dy,cpt) -> if p1_dy = 0 then (dy,0,cpt) else if cpt = 500000 then let dy = (if (top_col && p1_dy < 0) or (bot_col && p1_dy > 0) then 0 else p1_dy) in (dy,dy,0) else (dy,0,cpt+1)) last (0,0,0) in dy in
    let player2_delta(top_col,bot_col) = let (_,dy,_) = reg (fun (old_dy,r_dy,cpt) ->
        if cpt = 3000000 then
            let delta = if ball_y + ball_h / 2 > player2_y + player2_h / 2 then 1 else -1 in
            let dy = (if (top_col && delta < 0) or (bot_col && delta > 0) then 0 else delta) in
            (dy,dy,0)
        else
            (old_dy,0,cpt+1)
    ) last (0,0,0) in dy in

    let (player1,player1_dy) =
        let (pot_dx,pot_dy) = player_pot_delta in
        let (ball_pot_dx,ball_pot_dy) = ball_pot_delta in
        let collision_top_wall = player1_y - pot_dy < 0 in
        let collision_bottom_wall = player1_y + player1_h + pot_dy >= 480 in
        let collision_ball = collision((player1_x, player1_y + pot_dy, player1_w,player1_h), (ball_x + ball_pot_dx,ball_y - ball_pot_dy,ball_w,ball_h)) or collision((player1_x, player1_y - pot_dy, player1_w,player1_h), (ball_x + ball_pot_dx,ball_y + ball_pot_dy,ball_w,ball_h)) in
        
        let top_col = (collision_top_wall or (collision_ball && (ball_y+ball_h-ball_pot_dy <= player1_y+pot_dy))) in (* -v pour deplacement *)
        let bot_col = (collision_bottom_wall or (collision_ball && (ball_y+ball_pot_dy >= player1_y+player1_h-pot_dy))) in (* +v pour deplacement *)
        let dy = player1_delta(top_col,bot_col) in
        (player1_x,player1_y + dy,player_w,player_h,player_color),p1_dy
    in
    let (player2,player2_dy) =
        let (pot_dx,pot_dy) = (1,1) in
        let (ball_pot_dx,ball_pot_dy) = ball_pot_delta in
        let collision_top_wall = player2_y - pot_dy < 0 in
        let collision_bottom_wall = player2_y + player2_h + pot_dy >= 480 in
        let collision_ball = collision((player2_x, player2_y + pot_dy, player2_w,player2_h), (ball_x + ball_pot_dx,ball_y - ball_pot_dy,ball_w,ball_h)) or collision((player2_x, player2_y - pot_dy, player2_w,player2_h), (ball_x + ball_pot_dx,ball_y + ball_pot_dy,ball_w,ball_h)) in
        
        let top_col = (collision_top_wall or (collision_ball && (ball_y+ball_h-ball_pot_dy <= player2_y+pot_dy))) in (* -v pour deplacement *)
        let bot_col = (collision_bottom_wall or (collision_ball && (ball_y+ball_pot_dy >= player2_y+player2_h-pot_dy))) in (* +v pour deplacement *)
        let dy = player2_delta(top_col,bot_col) in
        (player2_x,player2_y+dy,player_w,player_h,player_color), let delta = if ball_y + ball_h / 2 > player2_y + player2_h / 2 then 1 else -1 in (if (top_col && delta < 0) or (bot_col && delta > 0) then 0 else delta)
    in

    let (player1_x,player1_y,player1_w,player1_h,(_,_,_)) = player1 in
    let (player2_x,player2_y,player2_w,player2_h,(_,_,_)) = player2 in

    let rec ball_delta(left_col,right_col,top_col,bot_col,score_delta,col_p1,col_p2) =
        let rec up(x,y) =
            let f1(xx,yy) = (xx,yy-1) in
            if x = -2 then
                if y = -2 then (-1,-2)
                else f1(x,y)
            else if x = -1 then
                if y = -2 then (-1,-2)
                else if y = 2 then (-2,2)
                else (0,0) (* can't happen *)
            else if x = 1 then
                if y = -2 then (1,-2)
                else if y = 2 then (2,2)
                else (0,0) (* can't happen *)
            else if x = 2 then
                if y = -2 then (1,-2)
                else f1(x,y)
            else (0,0) (* can't happen *)
        in
        let rec down(x,y) =
            let f1(xx,yy) = (xx,yy+1) in
            if x = -2 then
                if y = 2 then (-1,2)
                else f1(x,y)
            else if x = -1 then
                if y = -2 then (-2,-2)
                else if y = 2 then (-1,2)
                else (0,0) (* can't happen *)
            else if x = 1 then
                if y = -2 then (2,-2)
                else if y = 2 then (1,2)
                else (0,0) (* can't happen *)
            else if x = 2 then
                if y = 2 then (1,2)
                else f1(x,y)
            else (0,0) (* can't happen *)
        in
        let ((_,_),(dx,dy),_,score) = reg (fun ((old_dx,old_dy),(r_dx,r_dy),cpt,score) ->
            if cpt = 50000 then (
                let (z,_) = exec 
                  let u = up(old_dx,old_dy) in
                  let d = down(old_dx, old_dy) in
                  let o = (old_dx,old_dy) in
                  let (dx,dy) =
                    if col_p1 && right_col then
                        if player1_dy < 0 then u else if player1_dy > 0 then d else o
                    else if col_p2 && left_col then
                        if player2_dy < 0 then u else if player2_dy > 0 then d else o
                    else o
                  in
                  let dx = if left_col or right_col then -dx else dx in
                  let dy = if top_col or bot_col then -dy else dy in
                  print_int (player1_dy); print_string ", "; print_int(dx); print_string ", "; print_int(dy); print_newline ();
                  ((dx,dy),(if left_col or right_col then -old_dx else old_dx,if top_col or bot_col then -old_dy else old_dy),0,score+score_delta)
              default ((old_dx,old_dy),(0,0),cpt,score) in z)
             else ((old_dx,old_dy),(0,0),cpt+1,score)
        ) last ((2,1),(0,0),0,0)
        in (dx,dy,score)
    in
    let ((ball,score),_) =
      exec
        let (pot_dx,pot_dy) = ball_pot_delta in
         wait();
        let collision_top_wall = ball_y - pot_dy < 0 in
         wait();
        let collision_bottom_wall = ball_y + ball_h + pot_dy >= 480 in
         wait();
        let collision_left_wall = ball_x - pot_dx < 0 in
         wait();
        let collision_right_wall = ball_x + ball_w + pot_dx >= 640 in
         wait();
        let collision_player1 = collision((ball_x + pot_dx, ball_y + pot_dy, ball_w,ball_h), (player1_x,player1_y,player1_w,player1_h)) or collision((ball_x + pot_dx, ball_y - pot_dy, ball_w,ball_h), (player1_x,player1_y,player1_w,player1_h)) in
         wait();
        let collision_player2 = collision((ball_x - pot_dx, ball_y + pot_dy, ball_w,ball_h), (player2_x,player2_y,player2_w,player2_h)) or collision((ball_x - pot_dx, ball_y - pot_dy, ball_w,ball_h), (player2_x,player2_y,player2_w,player2_h)) in
        wait();
        let left_col = collision_left_wall or ((* (collision_player1 && ball_x + pot_dx >= player1_x + player1_w) or *) (collision_player2 && ball_x - pot_dx <= player2_x + player2_w)) in
        wait();
        let right_col = collision_right_wall or ((collision_player1 && ball_x+ball_w+pot_dx >= player1_x) (* or (collision_player2 && ball_x+ball_w-pot_dx <= player2_x) *)) in
        wait();
        let top_col = collision_top_wall or ((collision_player1 && ball_y >= player1_y + player1_h) or (collision_player2 && ball_y >= player2_y + player2_h)) in
        wait();
        let bot_col = collision_bottom_wall or ((collision_player1 && ball_y+ball_h <= player1_y) or (collision_player2 && ball_y+ball_h <= player2_y)) in
        wait();
        let (dx,dy,score) = ball_delta(left_col,right_col,top_col,bot_col,if collision_right_wall then -1 else if collision_left_wall then 1 else 0,collision_player1,collision_player2) in
        wait();
        (ball_x+dx,ball_y+dy,ball_size,ball_size,ball_color),score
      default (ball,score)
    in
    
    (ball,player1,player2,score) ;;

let minus = (true,true,true,true,true,true,false,true) ;;

let none = (true,true,true,true,true,true,true,true) ;;

let pause = (none,displayE,displayS,displayU,displayA,displayP) ;;

let map_digit(n: int<5>) =
    match n with
    | 0 -> display0
    | 1 -> display1
    | 2 -> display2
    | 3 -> display3
    | 4 -> display4
    | 5 -> display5
    | 6 -> display6
    | 7 -> display7
    | 8 -> display8
    | 9 -> display9
    | 10 -> displayA
    | 11 -> displayB
    | 12 -> displayC
    | 13 -> displayD
    | 14 -> displayE
    | 15 -> displayF
    | _ -> displayL
    end ;;

let calibrate_accel(b,offset) =
    let (_,delta) = reg (fun (cpt,delta) -> if b then (if cpt = 100000000 then (0,offset) else (cpt+1,delta)) else (cpt,delta)) last (0,0) in
    offset - delta ;;

let control_levels(accel) =
    if accel < -64 then -3
    else if accel < -32 then -2
    else if accel < -16 then -1
    else if accel > 64 then 3
    else if accel > 32 then 2
    else if accel > 16 then 1
    else 0 ;;

let main(((s0,s1,s2,s3,s4,s5,s6,s7,s8,s9), (k0,k1), gsensor_int, gsensor_sdo, gsensor_sdi) : ((bool*bool*bool*bool*bool*bool*bool*bool*bool*bool) * (bool*bool) * int<2> * std_logic * std_logic)) =
    let (accel_x,accel_y,accel_z,gsensor_cs_n,gsensor_sclk,gsensor_sdo,gsensor_sdi) =
        run Interface_accelerometer(gsensor_int, gsensor_sdo, gsensor_sdi) in
    
    let (ball,player1,player2,score) = reg (fun (ball,player1,player2,score) ->
        let accel_y = calibrate_accel(not(k0), accel_y) in
        if s0 then (ball,player1,player2,score)
        else (
            let player1_dy = control_levels(accel_y) in
            move(ball,player1,player2, player1_dy,score)
        )
    ) last (
        let player1 = (610,190,player_w,player_h,(-1,-1,-1)) in
        let player2 = (10,190,player_w,player_h,(15,15,15)) in
        let ball = (313,233,ball_size,ball_size,(-1,0,0)) in (* 313,233 *)
        (ball,player1,player2,0)
    ) in
    
    let (hsync,vsync,r,g,b) = pll(fun () -> display(ball,player1,player2)) in

    let n = map_digit(if score < 0 then abs(score) else score) in
    let digs = (n,if score < 0 then minus else none,none,none,none,none) in

    ((s0,s1,s2,s3,s4,s5,s6,s7,s8,s9), if s0 then pause else digs, gsensor_cs_n, gsensor_sclk, hsync,vsync,r,g,b, gsensor_sdo,gsensor_sdi) ;;
