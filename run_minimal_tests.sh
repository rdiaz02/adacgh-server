#!/bin/bash

#### BEWARE: if you compare output wth version 1,
#### recall the "ok" fr wavelets and cghseg use
#### MAD, whereas those for version 1
#### use mergelevels. 

rm -r -f ./runs-tmp/tmp-3/*
rm -r -f ./runs-tmp/tmp-3-older-format/*

for i in $(ls ./test-cases-3/); do cp -a ./test-cases-3/$i ./runs-tmp/tmp-3/.; done


for i in $(ls ./runs-tmp/tmp-3/);
do echo "*********"; echo; echo; echo $i; 
    /http/adacgh-server/runADaCGHserver-3.py /http/adacgh-server/runs-tmp/tmp-3/$i;
done





### Now use the older format, but test everything!!!
## the following is ugly. could improve it

cp -a ./test-cases/dnacopy-ok ./runs-tmp/tmp-3-older-format/.
cp -a ./test-cases/haarseg-ok ./runs-tmp/tmp-3-older-format/.

for i in $(ls ./runs-tmp/tmp-3-older-format/);
do echo "*********"; echo; echo; echo $i; 
    /http/adacgh-server/runADaCGHserver-3.py /http/adacgh-server/runs-tmp/tmp-3-older-format/$i;
done

echo
echo  "****************"
echo "**************         Verify output older format"
echo  "****************"


for i in $(ls ./runs-tmp/tmp-3-older-format/); 
do cd ./runs-tmp/tmp-3-older-format/$i; 
    echo "*********"; echo; echo; echo $i; 
    cat R_Status.txt; 
    cd /http/adacgh-server; 
done



