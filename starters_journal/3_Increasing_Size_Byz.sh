#!/bin/sh

. ./global_config.sh

EXPERIMENT="3_Increasing_Size_Byz"
PERCENT_BLACKS=(68)
DETERMINECONSENSUS="true"
NUMROBOTS=(5)
NUMBYZANTINE=(1)
BYZANTINESWARMSTYLES=( 1 )
MAXFLOODING=20
LENGTHOFRUNS=2500
SUBSWARMCONSENSUS=false # Determines if all N robots have to agree or
		       # only the beneficial subswarm.

. ./run_experiment.sh
