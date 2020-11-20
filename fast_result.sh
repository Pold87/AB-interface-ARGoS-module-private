RESULTDIR=$(ls -td ./data/*/ | head -1)
RESULTFILE=$(ls -t ./${RESULTDIR}/1000000/* | head -1)
cat $RESULTFILE
