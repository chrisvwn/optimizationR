library(png)
digits = c(1,2,3,4,5,6,7,8,9,0)

############## load patterns #############
patterns <- list()
orgdim <- NULL
for (i in digits) {
	img <- readPNG(paste(as.character(i),'.png',sep=''))
	orgdim <- dim(img)
	dim(img) <- NULL
	img <- img*2 - 1
	patterns[[length(patterns)+1]] <- c(img,1)	# the last 1 for bias
}

similar = matrix(rep(0,length(digits) * length(digits)), length(digits), length(digits))
for (i in 1:length(patterns)) {
	for (j in 1:length(patterns)) {
		similar[i,j] = patterns[[i]] %*% patterns[[j]] / (sqrt(sum(patterns[[i]] ^ 2)) * sqrt(sum(patterns[[j]] ^ 2)))
	}
}

print("similarities between images (cosin distance)")
print(similar)

