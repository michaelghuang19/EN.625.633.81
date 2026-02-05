differentArrivalProbs = function(n, uppLimit, nmbSimulations) {

	### Simulating the different probabilities of arrival
	
	arrivalProbs = runif(n,0,uppLimit);
	totArrivalsInSim = rep(0,nmbSimulations);

	for (jSim in 1:nmbSimulations) {
	
		### Generating arrival variates
	
			arriveOrNot = rbinom(n,1,arrivalProbs);
			totArrivals = sum(arriveOrNot);
			totArrivalsInSim[jSim] = totArrivals;
			
		}
		
	par(mfrow = c(2,1))	
	hist(totArrivalsInSim, density = 50);
	
	
	##### Now generating and displaying Poisson Sample
	
	Lambda = sum(arrivalProbs)
	PoissonSample = rpois(nmbSimulations, Lambda)
	hist(PoissonSample, density = 50)
	
	}

# Higher max probability of arrival / uppLimit, so approximation should be worse
# n, uppLimit, nmbSimulations
differentArrivalProbs(100, 0.75, 1000)

# Lower max probability of arrival / uppLimit, so approximation should be better
differentArrivalProbs(100, 0.075, 1000)
