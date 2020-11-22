#!/bin/sh

. ./global_config.sh

EXPERIMENT="1_Increasing_Byz"
MIXINGS=5
PERCENT_BLACKS=(68)
DETERMINECONSENSUS="true"
NUMROBOTS=(10)
NUMBYZANTINE=(0 1 2 3 4 5)
BYZANTINESWARMSTYLES=( 1 )
MAXFLOODING=20
LENGTHOFRUNS=2500
SUBSWARMCONSENSUS=false # Determines if all N robots have to agree or
		       # only the beneficial subswarm.

. ./run_experiment.sh
