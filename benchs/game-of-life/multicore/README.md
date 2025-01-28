## Game of Life, multicore adaptation of v1 in OCaml 

using the `Task.parallel_for` from `Domainslib`.

*The world is represented by an array of booleans.*


```
$ make compile FLAGS=-O3
real	0m0.021s
user	0m0.008s
sys	0m0.009s
$ time make run N=1
real	0m23.999s
user	0m22.678s
sys	0m0.114s
$ time make run N=2
real	0m12.270s
user	0m24.315s
sys	0m0.024s
$ time make run N=3
real	0m8.604s
user	0m23.011s
sys	0m0.039s
$ time make run N=4
real	0m6.843s
user	0m24.220s
sys	0m0.053s
$ time make run N=5
real	0m7.248s
user	0m29.986s
sys	0m0.090s
$ time make run N=6
real	0m9.213s
user	0m45.187s
sys	0m0.187s
$ 
$ make clean
```
