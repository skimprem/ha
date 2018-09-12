#!/bin/bash

bin=./build/ha.exe
#data=./data/GAOP2018_2m.nc
#data=./data/gao2012.nc
data=./data/DTU10GRA_2min.nc
og=grid.tst
vm="[verbose_mode]"
hm=dh
oc=coef.tst

$bin\
  -nf $data\
  -hm $hm\
  -nm stdout\
  -oc $oc\
  -og $og\
  #--verbose $vm
