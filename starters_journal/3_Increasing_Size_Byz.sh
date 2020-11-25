#!/bin/sh

. ./global_config.sh

EXPERIMENT="3_Increasing_Size_Byz"
PERCENT_BLACKS=(68)
MIXINGS=5
DETERMINECONSENSUS="true"
NUMROBOTS=(10)
NUMBYZANTINE=(2)
BYZANTINESWARMSTYLES=( 1 )
MAXFLOODING=20
LENGTHOFRUNS=2500
SUBSWARMCONSENSUS=false # Determines if all N robots have to agree or
		       # only the beneficial subswarm.

. ./run_experiment.sh