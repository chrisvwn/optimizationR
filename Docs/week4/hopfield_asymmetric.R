source("hopfield.R")

model = list(
	n_nodes = 2,
	weights = matrix(c(
			0,	1,
			-1,	0), 
		2, 2))

init.y = c(1,-1)	# bias = 0
intermediate.y = activate(model, init.y)[[2]]
for (i in 1:length(intermediate.y)) {
	print(intermediate.y[[i]])
}
