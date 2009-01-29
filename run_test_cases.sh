#!/bin/bash
rm -f -r ./runs-tmp/tmp/dnacopy-ok
rm -f -r ./runs-tmp/tmp/cghseg-ok
rm -f -r ./runs-tmp/tmp/hmm-ok
rm -f -r ./runs-tmp/tmp/biohmm-ok
rm -f -r ./runs-tmp/tmp/wavelets-ok
rm -f -r ./runs-tmp/tmp/glad-ok

cp -a ./test-cases/dnacopy-ok ./runs-tmp/tmp/.
cp -a ./test-cases/biohmm-ok ./runs-tmp/tmp/.
cp -a ./test-cases/hmm-ok ./runs-tmp/tmp/.
cp -a ./test-cases/glad-ok ./runs-tmp/tmp/.
cp -a ./test-cases/wavelets-ok ./runs-tmp/tmp/.
cp -a ./test-cases/cghseg-ok ./runs-tmp/tmp/.


/http/adacgh-server/runADaCGHserver.py /http/adacgh-server/runs-tmp/tmp/dnacopy-ok
/http/adacgh-server/runADaCGHserver.py /http/adacgh-server/runs-tmp/tmp/biohmm-ok
/http/adacgh-server/runADaCGHserver.py /http/adacgh-server/runs-tmp/tmp/hmm-ok
/http/adacgh-server/runADaCGHserver.py /http/adacgh-server/runs-tmp/tmp/glad-ok
/http/adacgh-server/runADaCGHserver.py /http/adacgh-server/runs-tmp/tmp/wavelets-ok
/http/adacgh-server/runADaCGHserver.py /http/adacgh-server/runs-tmp/tmp/cghseg-ok


