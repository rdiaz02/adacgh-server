#!/bin/bash

#### BEWARE: if you compare output wth version 1,
#### recall the "ok" fr wavelets and cghseg use
#### MAD, whereas those for version 1
#### use mergelevels. 

rm -r -f ./runs-tmp/tmp-2/*
rm -r -f ./runs-tmp/tmp-2-several-identical/*


cp -a ./test-cases/0-two-chroms ./runs-tmp/tmp-2/.
cp -a ./test-cases/dnacopy-ok ./runs-tmp/tmp-2/.
cp -a ./test-cases/biohmm-ok ./runs-tmp/tmp-2/.
cp -a ./test-cases/hmm-ok ./runs-tmp/tmp-2/.
cp -a ./test-cases/glad-ok ./runs-tmp/tmp-2/.
cp -a ./test-cases/wavelets-ok* ./runs-tmp/tmp-2/.
cp -a ./test-cases/cghseg-ok* ./runs-tmp/tmp-2/.
cp -a ./test-cases/haarseg-ok ./runs-tmp/tmp-2/.
cp -a ./test-cases/140-one-master ./runs-tmp/tmp-2/140-one
cp -a ./test-cases/140-two-master ./runs-tmp/tmp-2/140-two
cp -a ./test-cases/several-large ./runs-tmp/tmp-2/several-large

cp -a ./test-cases/dnacopy-ok ./runs-tmp/tmp-2-several-identical/.
cp -a ./test-cases/biohmm-ok ./runs-tmp/tmp-2-several-identical/.
cp -a ./test-cases/hmm-ok ./runs-tmp/tmp-2-several-identical/.
cp -a ./test-cases/glad-ok ./runs-tmp/tmp-2-several-identical/.
cp -a ./test-cases/wavelets-ok* ./runs-tmp/tmp-2-several-identical/.
cp -a ./test-cases/cghseg-ok* ./runs-tmp/tmp-2-several-identical/.
cp -a ./test-cases/haarseg-ok ./runs-tmp/tmp-2-several-identical/.

for i in $(ls ./runs-tmp/tmp-2-several-identical/); do cp ./test-cases/inputData.RData.9.arrays.some.identical ./runs-tmp/tmp-2-several-identical/$i/inputData.RData; done


# /http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/dnacopy-ok
# /http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/biohmm-ok
# /http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/hmm-ok
# /http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/glad-ok
# /http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/wavelets-ok
# /http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/wavelets-ok-mergelevels
# /http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/wavelets-ok-no-merge
# /http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/cghseg-ok
# /http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/cghseg-ok-no-merge
# /http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/cghseg-ok-mergelevels
# /http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/haarseg-ok
# /http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/140-one
# /http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/140-two


for i in $(ls ./runs-tmp/tmp-2/);
do echo "*********"; echo; echo; echo $i; 
    /http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/$i;
done


echo 
echo
echo
echo "Running Several identical"

for i in $(ls ./runs-tmp/tmp-2-several-identical/);
do echo "*********"; echo; echo; echo $i; 
    /http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2-several-identical/$i;
done



echo 
echo
echo
echo "Verify output"
echo  "****************"


for i in $(ls ./runs-tmp/tmp-2/); 
do cd ./runs-tmp/tmp-2/$i; 
    echo "*********"; echo; echo; echo $i; 
    cat R_Status.txt; 
    cd /http/adacgh-server; 
done

echo
echo "Verify output several identical"
echo  "****************"


for i in $(ls ./runs-tmp/tmp-2-several-identical/); 
do cd ./runs-tmp/tmp-2-several-identical/$i; 
    echo "*********"; echo; echo; echo $i; 
    cat R_Status.txt; 
    cd /http/adacgh-server; 
done




