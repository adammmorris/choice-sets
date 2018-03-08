#!/bin/bash
#
#SBATCH -p general
#SBATCH -t 0-01:00
#SBATCH --mem 512
#SBATCH -o /dev/null
#SBATCH -e /dev/null
source new-modules.sh; module load matlab

nStarts="20"
fixedParams="[5 -1 0]"
nBetaSteps="20"
#priorPDFs="{@(x) log(gampdf(x, 20, .25)), @(x) log(betapdf(x, 2, 2)), @(x) log(unifpdf(x, 0, 1))}"
priorPDFs="{@(x) log(unifpdf(x, 0, 10)), @(x) log(unifpdf(x, 0, 1)), @(x) log(unifpdf(x, 0, 1))}"

dataname="sims.mat"
envname="env1.mat"
savename="params"
outputname="Params_Subj${SLURM_ARRAY_TASK_ID}.txt"

datapath="fitting/env1/sims_mixture"
envpath="env"
savepath="fitting/env1/sims_mixture/fit_preferred"

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
	cp "$homedir/$envpath/$envname" "$scratchdir/"

	matlab -nojvm -nodisplay -nosplash -nodesktop -r "addpath $homedir; fitModel_bayes('$scratchdir/$dataname', '$scratchdir/$envname', '$scratchdir/$savename/', $nStarts, $SLURM_ARRAY_TASK_ID, $fixedParams, $nBetaSteps, $priorPDFs); exit;"

	mv "$scratchdir/$savename/$outputname" "$homedir/$savepath/$savename/$outputname"
fi