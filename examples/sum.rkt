#lang forthqk
: sum dup 0 = if drop else over over + rot rot swap drop 1 - sum then ;
0 30000 sum
.
cr
