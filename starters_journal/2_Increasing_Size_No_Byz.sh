#!/bin/sh

. ./global_config.sh

EXPERIMENT="2_Increasing_Size_No_Byz"
PERCENT_BLACKS=(68)
MIXINGS=5
DETERMINECONSENSUS="true"
NUMROBOTS=(5)
NUMBYZANTINE=(0)
BYZANTINESWARMSTYLES=( 0 )
MAXFLOODING=20
LENGTHOFRUNS=2500
SUBSWARMCONSENSUS=false # Determines if all N robots have to agree or
		       # only the beneficial subswarm.

. ./run_experiment.sh
