


limitTheorem = function(n,epsilon) {
	
	vecOfAverages = c(rep(0,10000))
	for (i in 1:10000) {
		smpl = rpois(n,4.2)
		avg = mean(smpl);
		vecOfAverages[i] = avg;
	}
	hist(vecOfAverages);
	vecOfDifferences = vecOfAverages - 4.2*c(rep(1,10000));
	nmbCloseToTruth = length(vecOfDifferences[abs(vecOfDifferences) <= epsilon]);
	print(nmbCloseToTruth/10000);
}