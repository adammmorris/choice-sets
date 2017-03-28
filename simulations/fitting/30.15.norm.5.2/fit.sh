#!/bin/bash
#
#SBATCH -p serial_requeue
#SBATCH -t 0-23:59
#SBATCH --mem 350
#SBATCH -o /dev/null
#SBATCH -e /dev/null
source new-modules.sh; module load matlab

simsName=${1}
fitName=${2}
fixedParams=${3}
numStarts=${4}
numFnEvals=${5} # should be an even #

priorPDFs="{@(x) log(unifpdf(x, 0, 10)), @(x) log(exppdf(x, 1)), @(x) log(unifpdf(x, 0, 1))}" # don't forget to update the beta prior with max!

dataname="sims.mat"
envname="env_30_15_5_2"
savename="params"
outputname="${SLURM_ARRAY_TASK_ID}.txt"

datapath="fitting/$envname/$simsName"
envpath="env"
savepath="fitting/$envname/$simsName/$fitName"

homedir="/users/amorris/Documents/choicesets/with_sam/simulations";
scratchdir="/n/regal/cushman_lab/amorris/$SLURM_JOBID/$SLURM_ARRAY_TASK_ID"

if [ ! -f "$homedir/$datapath/$savename/$outputname" ]; then

	if [ ! -d "$scratchdir/$savename/" ]; then
		mkdir -p "$scratchdir/$savename/"
	fi
	if [ ! -d "$homedir/$savepath/$savename/" ]; then
		mkdir "$homedir/$savepath/$savename/"
	fi

	cp "$homedir/$datapath/$dataname" "$scratchdir/"
	cp "$homedir/$envpath/${envname}.mat" "$scratchdir/"

	matlab -nojvm -nodisplay -nosplash -nodesktop -r "addpath $homedir; addpath '$homedir/utilities'; fitModel_cont('$scratchdir/$dataname', '$scratchdir/${envname}.mat', '$scratchdir/$savename/', $numStarts, $SLURM_ARRAY_TASK_ID, $fixedParams, $priorPDFs, $numFnEvals); exit;"

	mv "$scratchdir/$savename/$outputname" "$homedir/$savepath/$savename/$outputname"
fi