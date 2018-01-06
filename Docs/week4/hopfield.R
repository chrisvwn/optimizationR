require(grDevices)

sign <- function(z) {
	return( (z>=0) * 2 - 1 )
}

# hopfields network
# with Hebbian learning
# input
#	n_nodes: number of nodes
#	patterns: a list of patterns used to train the net
# output
#	hopfields net
hopfield <- function(patterns) {
	n_nodes = length(patterns[[1]])
	weights <- matrix(rep(0,n_nodes*n_nodes), n_nodes, n_nodes)
	for (i in 1:length(patterns)) {
		weights <- weights + patterns[[i]] %o% patterns[[i]]
	}
	diag(weights) <- 0
	weights <- weights / length(patterns)

	return(list(weights = weights))
}

# compute activation of a hopnet, given a pattern
# input
#	hopnet: Hopfields net given by the functin hopfields()
#	patter: input pattern
# output
#	net state after converging
run.hopfield <- function(hopnet, pattern, maxit=100, 
					replace=T, stepbystep=F, topo=c()) {
	if (stepbystep == T) {
		cat("weights = \n")
		print(hopnet$weights)
		cat("input pattern = ", pattern, "\n")
		img = pattern
		dim(img) = topo

		## set up the plot region:
		plot(c(1, topo[1]), c(1, topo[2]), type = "n", xlab = "", ylab = "")
		image <- as.raster((img+1)/2)
		rasterImage(image, 1, 1, topo[1], topo[2], interpolate = FALSE)
		readline()
	}

	y = pattern
	n_nodes = dim(hopnet$weights)[1]
	
	order = c()
	converge = F

	for(it in 0:(maxit-1)) {
		if (it %% n_nodes == 0) {
			if (replace == F && converge == T) {
				print('reach a stable state!!!')
				break
			}
			order = sample(1:(n_nodes),replace=replace)
			converge = T
		}
	
		i = order[it %% n_nodes + 1]
		yi.old = y[i]
		z = hopnet$weights[i,] %*% y
		y[i] = sign(z)
		if (yi.old != y[i]) {
			converge = F
		}

		if (stepbystep == T) {
			img = y
			dim(img) = topo
			## set up the plot region:
			plot(c(1, topo[1]), c(1, topo[2]), type = "n", xlab = "", ylab = "")
			image <- as.raster((img+1)/2)
			rasterImage(image, 1, 1, topo[1], topo[2], interpolate = FALSE)
			
			cat('iter ', it+1, '\n')
			cat('pick neuron ', i, '\n')
			cat('input to this neuron ', z, '\n')
			cat('ouput of this neuron ', y[i], '\n')
			cat('new state ', y, '\n')
			readline() 
		}
	}
	return(y)
}
