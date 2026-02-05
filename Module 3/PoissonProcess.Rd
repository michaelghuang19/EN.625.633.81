


poissonProcess = function(tot, n, lambda, s, nmbSim) {
	
    #  initiating vector where I will store all of my estimates
    	vectorOfFractions = c(rep(0,nmbSim))
    
    
    #  Simulating the Poisson process nmbSim times
	    for (j in 1:nmbSim) {
	
			#  simulating little t values (incremental arrival times of customers)
				tVec = rexp(tot,lambda);
			# calculating big T values   (summing up the times)
				TVec = cumsum(tVec)
			#  Calculating the fraction of customers that arrive before time s
				fracLessThenS = length(TVec[TVec < s])
			#  Storing it in 'vectorOfFractions'
				if (fracLessThenS == n) {
					vectorOfFractions[j] = 1;
				}
		}
		
		estProb = mean(vectorOfFractions);
		paste("estimated Probability = ", estProb);
}

lambda = 0.2
s = 10
n = 5
tot = 20
simulations = 1000
simulated_prob = poissonProcess(tot, n, lambda, s, simulations)

real_prob = dpois(n, lambda * s)

paste("Simulated Probability P(N(5)=3):", simulated_prob)
paste("Real Probability using dpois:", real_prob)