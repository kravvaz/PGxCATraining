
#This function takes the following arguments and simulates a course of anticoagulation therapy.
#avatars: A file includes avatars with assigned initial warfarin doses.
#protocols: A desired dosing protocol for warfarin maintenance dosing.
#initialDose: A relevant code for the dosing algorithm used for initial dosing.
#numDaysToSimulate: A desired number of days for simulation.
#maxDose: The upper limit of a range of warfarin dose.
#numReplicates: Number of replication of a simulation.
#maxTime: The time interval for getting INR and Dose calculations outputs.
#rseed: random seeds.

##################################
###    simulation function     ###
##################################
processAvatar = function(avatars, protocol, initialDose, numDaysToSimulate, maxDose, numReplicates, maxTime, rseed) {

require(deSolve)
source("hamberg_2007.R")
source("protocols.R")
    
    ####### associate the protocol dose with the avatar group
    avatars = cbind(avatars, "Protocol" = rep(protocol, nrow(avatars)))

    ####### associate the initial dose with the avatar group
    if (initialDose <= 15) {
        avatars = cbind(avatars, "InitialDose" = rep(initialDose, nrow(avatars)))
    } else if (initialDose == 16) {
        avatars = cbind(avatars, "InitialDose" = avatars$DOSE_AND*2)
    } else if (initialDose == 17) {
        avatars = cbind(avatars, "InitialDose" = avatars$DOSE_GAGE)
    } else if (initialDose == 18) {
        avatars = cbind(avatars, "InitialDose" = avatars$DOSE_SCONCE)
    }

    numAvatars = nrow(avatars)

    INR.array = array(NA, dim=c(numDaysToSimulate, numReplicates, numAvatars))
    INR.check.array = array(0, dim=c(numDaysToSimulate, numReplicates, numAvatars))
    dose.array = INR.array
    big_array = c()
    INRerrorDist = array(rnorm(numDaysToSimulate*numAvatars*numReplicates,0,0.1), dim=c(numDaysToSimulate, numReplicates, numAvatars))   # INR error distribution
    
	for(a in 1:numAvatars) {
    		
    		for (r in 1:numReplicates) {
    		    
    		    # configure the random seed
    		    if (is.vector(rseed) & numReplicates == 1) {
    		        if (numReplicates == 1) {
    		            seed = rseed[a]
    		        } else {
    		            seed = rseed[r]
    		        }
    		    } else if (is.matrix(rseed)) {
    		        seed = rseed[a, r]
		        } else {
		            stop("Your random seed isn't working like you think it should!")
		        }
		        set.seed(seed)
        	    rnums = round(abs(rnorm(numDaysToSimulate)*100*numDaysToSimulate)) # just a list of random numbers that will be used as seeds for some protocols

                INRs = array(NA, numDaysToSimulate)
        		INR.check = array(0, numDaysToSimulate)
			    Cs_super_out = array(0, dim=c(maxTime*numDaysToSimulate+1, numReplicates))     # keep track of the dose superpositioning for each avatar
        		doses = INRs
        		INRtimePoints = c(1:numDaysToSimulate) # this is in days
		        INRtimePoints = INRtimePoints * 24
                
			    # set the initial dose
			    doses[1] = avatars$InitialDose[a]
    
		        Cs_rows = maxTime*numDaysToSimulate+1
		        Cs = matrix(0, nrow = Cs_rows, ncol=numDaysToSimulate)
		        Cs_super = 0
		        cat(c("Simulating avatar:", a, "for replicate:", r, "\n"))

		        for (i in 1:numDaysToSimulate) {
		            pill = dose_cat(doses[i], maxDose)    # converts dose into closest pill form
		            testPatient = hamberg_2007(pill, Cs_super, avatars$AGE[a], as.character(avatars$CYP2C9[a]), as.character(avatars$VKORC1G[a]), 1, maxTime*numDaysToSimulate, seed)
		            INRerror = INRerrorDist[i, r, a]
		            measuredINR = round(testPatient$INRv[i*24+1] + INRerror, digits=1)  # round this to 1 decimal, add 1 b/c time is zero based
	            
		            # keep track of the dose and INR values per person over time
		            if (is.na(measuredINR)) {
		                cat(c("Aw Snap...INR is NA:", "avatar:", avatarID, "day:", i, "\n"))
		                print(testPatient$parameters)
		                doses[i] = NA
		                INRs[i] = NA
		            } else {                
		                Cs[((i-1)*24 + 1):Cs_rows, i] = testPatient$Cs[1:length(((i-1)*24 + 1):Cs_rows)]
		                Cs_super = apply(Cs, 1, sum)[i*24+2]    
                  
		                INRs[i] = measuredINR
		                
		                # pick a protocol
		                if (is.na(doses[min(i+1, numDaysToSimulate)])) {
		                    INR.check[i] = i  # checked INR on this day (due to looping we are off by one day when we check INRs so 2,4,7 days are clinically 3,5,8 but don't worry it all works out)
		                    if (avatars$Protocol[a] == "coumagen_pharm") {
		                        doses = coumagen_pharm(measuredINR, doses, i, maxDose)
		                    } else if (avatars$Protocol[a] == "coumagen_standard") {
		                        doses = coumagen_standard(measuredINR, doses, i, maxDose)
		                    } else if (avatars$Protocol[a] == "wilson") {
		                        doses = wilson(measuredINR, doses, i, maxDose)		                    
    		                } else if (avatars$Protocol[a] == "fixed_dose") {
        		                doses = fixed_dose(doses, i) 
        		            } else if (avatars$Protocol[a] == "wilson_coumagen_pharm") {
        		                doses = wilson_coumagen_pharm(measuredINR, doses, i, maxDose)
        		            } else if (avatars$Protocol[a] == "wilson_coumagen_standard") {
        		                doses = wilson_coumagen_standard(measuredINR, doses, i, maxDose)
        		            } else {
    		                    stop("Hey buddy, you forgot to add the protocol to the simulator - duh...")
    		                }                
			            }
		            }
		    }

		    Cs_super_out[, r] = apply(Cs, 1, sum)
            INR.array[, r, a] = INRs
            dose.array[, r, a] = doses
            INR.check.array[, r, a] = INR.check		    
		}		
	}	
	as.data.frame(list("INR"=INR.array, "Dose"=dose.array, "Check"=INR.check.array))
}






