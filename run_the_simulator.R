#Run the Simulator
source("anticoagulation_therapy_simulator.R")

#Read in the avatars table
avatars = read.table("avatars", sep="\t", header=T)

#Distribute the job
av_per = 5  # Number of avatars per file (per run)
block = av_per - 1

av_index = 1

to = av_index*av_per
from = to - block

av_sub = avatars[from:to,]

#Create random seeds
# need to create a global set of random seeds so when the jobs are distributed the values are random and
# don't repeat with every block
set.seed(4321)
numReplicates = 1
randomValues = array(round(abs(rnorm(nrow(avatars)*numReplicates)*nrow(avatars)*numReplicates)), dim=c(nrow(avatars), numReplicates))
rand_sub = randomValues[from:to,]

#Run function anticoagulation_therapy_simulator
av_out = processAvatar(avatars=av_sub, protocol="coumagen_pharm", initialDose=16, numDaysToSimulate=22, maxDose=15, numReplicates=numReplicates, maxTime=24, rseed=rand_sub)

