#!/bin/sh

USERNAME=`whoami`
mailto='volker.strobel87@gmail.com'
DOCKERBASE='/home/vstrobel/Documents/docker-geth-network/'
TEMPLATE='experiments/epuck_EC_locale_template.argos'
CONTRACT="${DOCKERBASE}/geth/shared/smart_contract_threshold.sol"
SCTEMPLATE="${DOCKERBASE}/geth/shared/3_Threshold.sol_template"
OUTFILE="experiments/epuck.argos"
SCOUTFILE="${DOCKERBASE}/geth/shared/3_Threshold.sol"
BASEDIR="$PWD/controllers/epuck_environment_classification/"
BLOCKCHAINPATH="$HOME/eth_data_para/data" # always without '/' at the end!!

DECISIONRULE=$1
NUMROBOTS=(20)
REPETITIONS=30
TAUS=(1000000)
LENGTHOFRUNS=(1000)

MIXINGS=1

MININGDIFF=1000000
USEMULTIPLENODES=true
USEBACKGROUNDGETHCALLS=true
MAPPINGPATH="$HOME/Documents/blockchain-journal-bc/experiments/config.txt"
CHANGEDIFFIULTY=""
NUMRUNS=1
THREADS=1
NOW=`date +"%d-%m-%Y"`
# The miner node is the first of the used nodes
USECLASSICALAPPROACH=false
DISTRIBUTEETHER="false"
CONTAINERNAMEBASE="ethereum_eth."
CONTRACTADDRESS="${DOCKERBASE}/geth/deployed_contract/contractAddress.txt"
CONTRACTABI="${DOCKERBASE}/geth/deployed_contract/contractABI.abi"
MAXFLOODING=20
REALTIME="true"

# 1: Always send 0.0 as value
# 2: Always send 1.0 as value
# 3: Send 0.0 with probability 0.5, send 1.0 else
# 4: Send a random number between 0.0 and 1.0
# 5: Send the true value but apply Gaussian noise to the sensor readings
# 11: Perform a Sybil and flooding attack, always send 0.0 as value
# 12: Perform a Sybil and flooding attack, always send 1.0 as value
# 13: Perform a Sybil and flooding attack, send 0.0 with probabiity 0.5, send 1.0 else
# 14: Perform a Sybil and flooding attack, send a random number between 0.0 and 1.0
# 15: Perform a Sybil and flooding attack, send the true value but with some Gaussian noise
# 20: Perform a jamming attack


