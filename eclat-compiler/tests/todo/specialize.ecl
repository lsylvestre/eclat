> let g(f)= f(f true);;
val g : forall $v1  . ((bool -($v1)-> bool) -($v1 + $v1)-> bool) | 0


> let shared g f = f(f true);;
val g : forall $v1  . ((bool -($v1)-> bool) -($v1 + $v1 + 1)-> bool) | 0


> let main = g (fun y -> pause(); y or true);;
val main : bool | 3