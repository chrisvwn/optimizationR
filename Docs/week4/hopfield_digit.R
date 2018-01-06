source("hopfield.R")
library(png)

digits = c(1,3,5,7,9)
noise_rate = 0

############## load patterns #############
patterns <- list()
orgdim <- NULL
for (i in digits) {
	img <- readPNG(paste(as.character(i),'.png',sep=''))
	orgdim <- dim(img)
	dim(img) <- NULL
	img <- img*2 - 1
	patterns[[length(patterns)+1]] <- img
}

############# train a Hopfield net ############
hopnet <- hopfield(patterns)

############ visual test ###########
plotdim = 2*orgdim
plot(c(1,(plotdim[1]+5)*length(digits)), c(1,(plotdim[2]+5)*3), type="n", xlab="", ylab="")
x = 1

for (i in 1:length(patterns)) {
	digit = digits[i]
	pattern = patterns[[i]]

	noise = (runif(length(pattern), 0, 1) > noise_rate ) * 2. - 1	
	input = pattern * noise
	ret <- run.hopfield(hopnet, input, maxit = 1000, 
						replace=F, stepbystep=F, topo=orgdim)

	img <- pattern; dim(img) <- orgdim
	image <- as.raster((img+1)/2)
	rasterImage(image, x, 1, x + plotdim[1], plotdim[2], interpolate=F)
	#writePNG(img, paste(as.character(digit), '.in.png',sep=''))

	img <- input; dim(img) <- orgdim
	image <- as.raster((img+1)/2)
	rasterImage(image, x, 1+2*(plotdim[2]+5), 
					x + plotdim[1], 1+2*(plotdim[2]+5)+plotdim[2], interpolate=F)
	#writePNG(img, paste(as.character(digit), '.in.png',sep=''))

	img <- ret; dim(img) <- orgdim
	image <- as.raster((img+1)/2)
	rasterImage(image, x, 1+plotdim[2]+5, x + plotdim[1], 1+2*plotdim[2]+5, interpolate=F)
	#writePNG(img, paste(as.character(digit), '.out.png',sep=''))


	x = x + plotdim[1]+5
}
