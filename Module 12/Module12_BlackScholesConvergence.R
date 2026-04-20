
blackScholesConvergence = function(T,N, lambda, mu, nmbSimulations) {
	
	##   loop over time steps


vectorOfFinalDifferences = rep(0, nmbSimulations)

for (iSim in 1:nmbSimulations) {


	timeIncrements = seq(0, T,T/N);
	W_vec = rep(0, N+1);
	X0 = 1;
	EM_soln = rep(0,N+1);
	EM_soln[1] = X0;

	for (j in 0:N) {
		
		if (j == 0) {
			W = 0;
			EM = X0;
		}
		if (j > 0) {
			dW = rnorm(1,0, sqrt(T/N));
			W = W_vec[j] + dW;
			EM = EM_soln[j] + lambda*EM_soln[j]*T/N + mu*EM_soln[j]*dW;
		}
		W_vec[j+1] = W;
		EM_soln[j+1] = EM;
	
	}
	Xt = X0*exp((lambda-mu^2/2)*timeIncrements + mu*W_vec);
	
	vectorOfFinalDifferences[iSim] = Xt[N+1] - EM_soln[N+1]

}

averageFinalDifference = mean(vectorOfFinalDifferences)
print(paste("average deviation = ", averageFinalDifference, ", and the delta t = ", T/N))

}