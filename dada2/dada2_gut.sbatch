#!/bin/bash

#SBATCH --job-name=gut_dada2
#SBATCH --mem=250G
#SBATCH --time=12:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH -p general
#SBATCH -A r00324
#SBATCH --output %j_guts_dada2.txt
#SBATCH --err %j_guts_dada2.err
#SBATCH --mail-user emamuell@iu.edu
#SBATCH --mail-type BEGIN,END,FAIL

cd /N/u/emamuell/Quartz/github/Gut/dada2
module load r

Rscript ./Gut_ASV.R
