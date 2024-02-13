open CustomStdlib

type 'a t = 'a list

let cons a l = a::l

let hd = function
    [] -> failwith "hd"
  | a::_ -> a

let tl = function
    [] -> failwith "tl"
  | _::l -> l

let nth l n =
  if n < 0 then invalid_arg "List.nth" else
  let rec nth_aux l n =
    match l with
    | [] -> failwith "nth"
    | a::l -> if n = 0 then a else nth_aux l (n-1)
  in nth_aux l n

let init n f =
  let rec aux n acc =
    if n = 0 then acc
    else
      aux (n-1) ((f n)::acc)
  in
  aux n []

let rec fold_left f accu l =
  match l with
    [] -> accu
  | a::l -> fold_left f (f accu a) l

let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> rev_append l (a :: l2)

let rev l = rev_append l [] 

let length l =
  let rec aux l acc =
    match l with 
    | [] -> acc
    | _::t -> aux t (acc+1) 
  in aux l 0

let rec map f l =
  match l with
    [] -> []
  | x::t -> 
     let y = f x in y :: map f t

let filter p l =
  let rec aux l acc =
    match l with
    | [] -> rev acc
    | x::l -> if p x then aux l (x::acc) else aux l acc 
  in aux l []

let iter f l =
  let rec aux l =
    match l with
    | [] -> ()
    | x::t -> f x; aux t 
  in aux l
