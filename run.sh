#!/bin/bash

bin=./build/ha.exe
#data=./data/GAOP2018_7.5m.nc
#data=./data/gao2012.nc
#data=./data/DTU10GRA_2min.nc
data=./data/DTU10GRA_1min.nc
og=grid.tst
vm="[verbose_mode]"
hm=dh
oc=coef.tst

$bin\
  -nf $data\
  --verbose $vm
  #-hm $hm\
  #-nm stdout\
  #-oc $oc\
  #-og $og\
