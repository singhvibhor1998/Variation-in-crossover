#Commands of TSP
tspresult1=runGA(noRuns =30,problem='tsp')
tspresult2=runGA(noRuns=30,problem = 'tsp')
tspresult3=runGA(noRuns=30,problem = 'tsp')



p1 <- parseData(tspresult1, 2,200)
p2 <- parseData(tspresult2, 2,200)
p3 <- parseData(tspresult3, 2,200)

plotbars(p1,p2,p3,"pmxCrossover","oxCrossover","pbxCrossover")

