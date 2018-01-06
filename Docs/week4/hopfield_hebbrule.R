source("hopfield.R")
pattern1 = rep(1, 10*10)
pattern2 = rep(-1, 10*10)
weights = matrix(rep(0,10*10*10*10),10*10, 10*10)

weights = weights + pattern1 %o% pattern1 + pattern2 %o% pattern2 - 2*diag(10*10)

pattern3 = matrix(pattern1, 10, 10)
for (i in 1:3)
      pattern3[i,] = -1
dim(pattern3) = NULL
pattern4 = matrix(pattern1, 10, 10)
for (i in 1:5)
      pattern4[i,] = -1
dim(pattern4) = NULL

weights = weights + pattern3 %o% pattern3 - diag(10*10)

weights = weights - pattern3 %o% pattern3 + diag(10*10)

run.hopfield(list(weights=weights), pattern4,
                    stepbystep=T, topo=c(10,10), replace=F)

