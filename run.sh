#!/bin/bash

bin=./build/ha.exe
data=./data/GAOP2018_2m.nc
og=test.tst
vm="[verbose_mode]"
hm=dh
oc=$data"_coef"

$bin\
  -nf $data\
  -hm $hm\
  -oc $oc\
  #-og $og\
  #-nm stdout\
  #--verbose $vm
