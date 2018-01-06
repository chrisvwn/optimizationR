source("hopfield.R")

weights = matrix(c(
				0,	1,	-2,
				1,	0,	1,
				-2,	1,	0),
			3,3)

hopnet = list(weights = weights)

init.y = c(1,-1,1)

run.hopfield(hopnet, init.y, stepbystep=T, topo=c(3,1))	
				
