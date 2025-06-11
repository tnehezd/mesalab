## Test cases 
This directory contains some test cases.

# MIST_synthetic:
Ebben a mappában a MIST oldalon található szintetikus csillagfejlődési pályák találhatóak:
v/vcrit = 0
tömeg 2.1 - 12.6 Msun
fe/h = -1 és 0.5
ez átszámolva Z-re az alábbi képlettel:

blabla

mivel a program a "history.data" file-okra van optimalizálva, ezért a MIST oldalára kinyerhető eep file-okat átkonvertáltam kvázi-history file-okká:
szerkezet, header megegyezik a MESA által kiadott file-okéval (kapott egy plusz oszlopot model_number-ként, amelyet önkényesen feltöltöttem számokkal 1-gyel kezdődően)
mivel az oldal csak eep file-okat ad, ezért dummy inlist file-okat hoztam létre, melyben kizárólag a csillag tömege és kezdeti fémessége szerepel.



