let f (x,y) =  
  [set(x,0,42) || set(y,0,44)] 
wcet 1;;

(* Error: Expect max({&v2:1},{&v1:1}) <= 1.
   It is not always the case
   For example, max({%l:1},{%l:1}) > 1 *)