let half_add(a,b) = 
  let s = a xor b in
  let co = a & b in
  (s,co) ;;

let full_add(a,b,ci) =
  let (s1,c1) = half_add(a,b) in
  let (s,c2) = half_add(ci,s1) in
  let co = c1 or c2 in
  (s, co) ;;

let add_n((x, y, ci) : int<'N> * int<'N> * bool) : int<'N> =
  let (result,co) = 
    iterate<1 to 'N> (fun (i,(res,ci)) ->
      let z = size_val res in
      let a = get_bit(x,z-i) in
      let b = get_bit(y,z-i) in 
      let (s,co) = full_add(a,b,ci) in
      (update_bit(res,z-i,s),co)
    ) init (x,ci)
 in result ;;

let addsub_n(neg_b, a, b) =
  let b_prepare = if neg_b then lnot b else b in
  add_n(a, b_prepare, neg_b) ;;

let main () =
  addsub_n(false,(0:int<32>),0);;