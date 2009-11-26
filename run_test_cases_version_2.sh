#!/bin/bash

#### BEWARE: if you compare output wth version 1,
#### recall the "ok" fr wavelets and cghseg use
#### MAD, whereas those for version 1
#### use mergelevels. 


rm -f -r ./runs-tmp/tmp-2/dnacopy-ok
rm -f -r ./runs-tmp/tmp-2/cghseg-ok
rm -f -r ./runs-tmp/tmp-2/cghseg-ok-no-merge
rm -f -r ./runs-tmp/tmp-2/cghseg-ok-mergelevels
rm -f -r ./runs-tmp/tmp-2/hmm-ok
rm -f -r ./runs-tmp/tmp-2/biohmm-ok
rm -f -r ./runs-tmp/tmp-2/wavelets-ok
rm -f -r ./runs-tmp/tmp-2/wavelets-ok-no-merge
rm -f -r ./runs-tmp/tmp-2/wavelets-ok-mergelevels
rm -f -r ./runs-tmp/tmp-2/glad-ok
rm -f -r ./runs-tmp/tmp-2/haarseg-ok
rm -f -r ./runs-tmp/tmp-2/140-one
rm -f -r ./runs-tmp/tmp-2/140-two
rm -f -r ./runs-tmp/tmp-2/several-large




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


/http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/dnacopy-ok
/http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/biohmm-ok
/http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/hmm-ok
/http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/glad-ok
/http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/wavelets-ok
/http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/wavelets-ok-mergelevels
/http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/wavelets-ok-no-merge
/http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/cghseg-ok
/http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/cghseg-ok-no-merge
/http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/cghseg-ok-mergelevels
/http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/haarseg-ok
/http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/140-one
/http/adacgh-server/runADaCGHserver-2.py /http/adacgh-server/runs-tmp/tmp-2/140-two



