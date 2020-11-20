 # Iterate over experimental settings and start experiments
 
# Create data folder for that experiment
DATADIRBASE="data/${EXPERIMENT}_${DECISIONRULE}"
w=1	
  while [[ -d "${DATADIRBASE}-$w" ]] ; do
    let w++
  done
DATADIRBASE="${DATADIRBASE}-${w}/"
for i in `seq 1 $REPETITIONS`; do

     for y in "${NUMBYZANTINE[@]}"; do

	 for TAU in "${TAUS[@]}"; do

             DATADIR="${DATADIRBASE}${TAU}/"
             mkdir -p $DATADIR

	     for MIXING in "${MIXINGS[@]}"; do

		 for BYZANTINESWARMSTYLE in "${BYZANTINESWARMSTYLES[@]}"; do
	     
	     for k in "${NUMROBOTS[@]}"; do

	     R0=$k
	     B0=0

	 for p in "${PERCENT_BLACKS[@]}"; do

	PERCENT_BLACK=$p
	PERCENT_WHITE=$(expr 100 - $PERCENT_BLACK)
	
	RADIX=$(printf 'num%d_black%d_byz%d_run%d' $k $PERCENT_BLACK $y $i)

        # Create smart contract with specified tau (threshold)
	  rm -f ${SCOUTFILE} ${CONTRACT}          
	  #sed -e "s|TAU|$TAU|g" ${SCTEMPLATE} > ${SCOUTFILE}
          cp ${SCTEMPLATE} ${CONTRACT}

	  solc --overwrite --abi --bin-runtime ${CONTRACT} -o ${DOCKERBASE}/geth/shared/contract_files

	  # Generate the genesis block; first: read in compiled smart contract
	  BINDATA=`cat ${DOCKERBASE}/geth/shared/contract_files/Estimation.bin-runtime`

	  # Create genesis using puppeth
	  bash ${DOCKERBASE}/geth/files/reset-genesis ${NUMROBOTS}
	  
	  # Insert the smart contract into the genesis 
	  sed -ie "s|123\": {|123\": {\n\"code\": \"0xBINDATA\",|g" ${GENESIS}
	  
	  sed -ie "s|BINDATA|$BINDATA|g" ${GENESIS}

	  # Change the gas limit
	  sed -ie "s|0x47b760|0x9000000000000|g" ${GENESIS}

	  # Change the value of the pre-funded accounts
	  sed -ie "s|\"0x200000000000000000000000000000000000000000000000000000000000000\"|\"0xde0b6b3a7640000\"|g" ${GENESIS}

	  # Undo for the contract account (first match)
	  sed -ie "0,/\"0xde0b6b3a7640000\"/s//\"0x200000000000000000000000000000000000000000000000000000000000000\"/" ${GENESIS}
	  
	  # Update the contract ABI to make it compatible with the
	  # geth console (latest solc and latest geth console are
	  # incompatibe)
	  OLDABI="${DOCKERBASE}/geth/shared/contract_files/Estimation.abi"
	  sed -e "s|\"stateMutability\":\"payable\"|\"stateMutability\":\"payable\",\"payable\":\"true\"|g" $OLDABI > $CONTRACTABI
	  
	  echo "I created the contract"
	  
	# Create ARGoS template
	  sed -e "s|BASEDIR|$BASEDIR|g"\
	    -e "s|CONTRACTADDRESS|$CONTRACTADDRESS|g"\
	    -e "s|CONTRACTABI|$CONTRACTABI|g"\
	    -e "s|NUMRUNS|$NUMRUNS|g"\
	    -e "s|DATADIR|$DATADIR|g"\
	    -e "s|RADIX|$RADIX|g"\
	    -e "s|NUMROBOTS|$k|g"\
	    -e "s|R0|$R0|g"\
	    -e "s|B0|$B0|g"\
	    -e "s|PERCENT_BLACK|$PERCENT_BLACK|g"\
	    -e "s|PERCENT_WHITE|$PERCENT_WHITE|g"\
	    -e "s|DECISIONRULE|$DECISIONRULE|g"\
	    -e "s|USEMULTIPLENODES|$USEMULTIPLENODES|g"\
	    -e "s|MININGDIFF|$MININGDIFF|g"\
	    -e "s|BLOCKCHAINPATH|$BLOCKCHAINPATH|g"\
	    -e "s|THREADS|$THREADS|g"\
	    -e "s|USECLASSICALAPPROACH|$USECLASSICALAPPROACH|g"\
	    -e "s|NUMBYZANTINE|$y|g"\
	    -e "s|LENGTHOFRUNS|$LENGTHOFRUNS|g"\
	    -e "s|BYZANTINESWARMSTYLE|$BYZANTINESWARMSTYLE|g"\
	    -e "s|SUBSWARMCONSENSUS|$SUBSWARMCONSENSUS|g"\
	    -e "s|REGENERATEFILE|$REGENERATEFILE|g"\
	    -e "s|REALTIME|$REALTIME|g"\
	    -e "s|FLOODINGATTACK|$FLOODINGATTACK|g"\
	    -e "s|MAXFLOODING|$MAXFLOODING|g"\
	    -e "s|MIXING|$MIXING|g"\
	    -e "s|DETERMINECONSENSUS|$DETERMINECONSENSUS|g"\
	    -e "s|DISTRIBUTEETHER|$DISTRIBUTEETHER|g"\
	    -e "s|CONTAINERNAMEBASE|$CONTAINERNAMEBASE|g"\
	    -e "s|ARENASIZEDIM|$ARENASIZEDIM|g"\
	    -e "s|ARENASIZEPLUSLARGE|$ARENASIZEPLUSLARGE|g"\
	    -e "s|ARENASIZEPLUSSMALL|$ARENASIZEPLUSSMALL|g"\
	    -e "s|ARENASIZEHALF|$ARENASIZEHALF|g"\
	    -e "s|ARENASIZEMINUS|$ARENASIZEMINUS|g"\
	    -e "s|CELLDIMENSION|$CELLDIMENSION|g"\
	    -e "s|VISUALIZATION|$VISUALIZATION|g"\
	      $TEMPLATE > $OUTFILE
	      
	bash ${DOCKERBASE}/local_scripts/stop_network.sh $k

	sleep 5
        sudo systemctl restart docker.service
	# Restart network
	bash ${DOCKERBASE}/local_scripts/start_network.sh $k
	# Start experiment
	argos3 -c $OUTFILE
	
	bash ${DOCKERBASE}/local_scripts/stop_network.sh $k

	 done
	     done
	 
		 done

	     done

	 done
    
     done
done

