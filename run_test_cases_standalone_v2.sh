#!/bin/bash

#### BEWARE: if you compare output wth version 1,
#### recall the "ok" fr wavelets and cghseg use
#### MAD, whereas those for version 1
#### use mergelevels. 


rm -f -r ./runs-tmp/tmp-standalone/dnacopy-ok
rm -f -r ./runs-tmp/tmp-standalone/cghseg-ok
rm -f -r ./runs-tmp/tmp-standalone/cghseg-ok-no-merge
rm -f -r ./runs-tmp/tmp-standalone/cghseg-ok-mergelevels
rm -f -r ./runs-tmp/tmp-standalone/hmm-ok
rm -f -r ./runs-tmp/tmp-standalone/biohmm-ok
rm -f -r ./runs-tmp/tmp-standalone/wavelets-ok
rm -f -r ./runs-tmp/tmp-standalone/wavelets-ok-no-merge
rm -f -r ./runs-tmp/tmp-standalone/wavelets-ok-mergelevels
rm -f -r ./runs-tmp/tmp-standalone/glad-ok
rm -f -r ./runs-tmp/tmp-standalone/haarseg-ok
rm -f -r ./runs-tmp/tmp-standalone/140-one
rm -f -r ./runs-tmp/tmp-standalone/140-two
rm -f -r ./runs-tmp/tmp-standalone/several-large


cp -a ./test-cases/dnacopy-ok ./runs-tmp/tmp-standalone/.
cp -a ./test-cases/biohmm-ok ./runs-tmp/tmp-standalone/.
cp -a ./test-cases/hmm-ok ./runs-tmp/tmp-standalone/.
cp -a ./test-cases/glad-ok ./runs-tmp/tmp-standalone/.
cp -a ./test-cases/wavelets-ok* ./runs-tmp/tmp-standalone/.
cp -a ./test-cases/cghseg-ok* ./runs-tmp/tmp-standalone/.
cp -a ./test-cases/haarseg-ok ./runs-tmp/tmp-standalone/.
cp -a ./test-cases/140-one-master ./runs-tmp/tmp-standalone/140-one
cp -a ./test-cases/140-two-master ./runs-tmp/tmp-standalone/140-two
cp -a ./test-cases/several-large ./runs-tmp/tmp-standalone/several-large


cp f2.R f2-standalone.R

sed -i 's/assign(".__ADaCGH_SERVER_APPL", TRUE)/assign(".__ADaCGH_SERVER_APPL", FALSE)/' f2-standalone.R


for i in $(ls ./runs-tmp/tmp-standalone/); do cp f2-standalone.R ./runs-tmp/tmp-standalone/$i/.; done


for i in $(ls ./runs-tmp/tmp-standalone/); do cd ./runs-tmp/tmp-standalone/$i; echo "*********"; echo; echo; echo $i; R-2.10 --no-readline --no-save --slave < f2-standalone.R >>f2.Rout 2>>f2status; cd /http/adacgh-server; done

echo 
echo
echo
echo "Verify output"


for i in $(ls ./runs-tmp/tmp-standalone/); do cd ./runs-tmp/tmp-standalone/$i; echo "*********"; echo; echo; echo $i; cat R_Status.txt; cd /http/adacgh-server; done
