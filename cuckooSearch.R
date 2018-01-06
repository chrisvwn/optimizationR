cuckoo.search <- function(n = 5) {
  message("Running cuckoo search")
  # discovery rate of alien eggs/solutions
  pa <- 0.25

  ## Change this if you want to get better results
  # Tolerance
  tol <- -0.8
  ## Simple bounds of the search domain
  nd <- 11 #number of eggs per nest
  nrange <- 10 #max value per egg
  lb <- matrix(3, 1, nd)
  ub <- matrix(nrange, 1, nd)

  # Random initial solutions
  nest <- matrix(0, n, nd)
  for (i in 1:n) {
    nest[i,] <- ceiling(abs(lb + (ub - lb) * runif(nd)))
  }

  # Get the current best
  fitness <- 10^10 * matrix(1, n, 1)
  fgenome <- vector()
  current <- cuckoos.best.nest(nest, nest, fitness, fgenome)
  fmin <- current$fmin
  bestnest <- current$best
  nest <- current$nest
  fitness <- current$fitness
  fgenome <- current$fgenome
  bestgenome <- fgenome

  iter <- 0
  # Start iterations
  while (fmin > tol) {
    # Generate new solutions (but keep the current best)
    new_nest <- cuckoos.nest(nest, bestnest, lb, ub)
    new_best <- cuckoos.best.nest(nest, new_nest, fitness, fgenome)
    fnew <- new_best$fmin
    best <- new_best$best
    nest <- new_best$nest
    fitness <- new_best$fitness
    fgenome <- new_best$fgenome

    # Update the counter
    iter <- iter + n
    
    # Discovery and randomization
    new_nest <- empty.nests(nest, lb, ub, pa)
    
    # Evaluate this set of solutions
    new_best <- cuckoos.best.nest(nest, new_nest, fitness, fgenome)
    fnew <- new_best$fmin
    best <- new_best$best
    nest <- new_best$nest
    fitness <- new_best$fitness
    fgenome <- new_best$fgenome
    iter <- iter + n
    
    # find the best objective so far
    if (fnew<fmin) {
      fmin <- fnew
      bestnest <- best
      bestgenome <- fgenome
    }
    print(paste0('iter:',iter, 'fitness:', fmin))
  }

  # Post optimization and processing
  print(paste0('Total number of iterations=', iter))
  return(list('fmin' = fmin, 'bestnest' = bestnest, 'nest' = nest, 'bestgenome' = bestgenome))
}

# Get cuckoos by random walk
cuckoos.nest <- function(nest, best, lb, ub) {
  # Levy flights
  n <- dim(nest)[1]

  # Levy exponent and coefficient
  # For details, see equation (2.21), Page 16 (chapter 2) of the book
  # X. S. Yang, Nature-Inspired Metaheuristic Algorithms, 2nd Edition, Luniver Press, (2010).
  beta <- 3/2
  sigma <- (gamma(1+beta)*sin(pi*beta/2)/(gamma((1+beta)/2)*beta*2^((beta-1)/2)))^(1/beta)
  for (i in 1:n) {
    s <- nest[i,]
    size <- dim(nest)[2]
    # This is a simple way of implementing Levy flights
    # For standard random walks, use step = 1
    ## Levy flights by Mantegnas's algorithm
    u <- rnorm(size)*sigma
    v <- rnorm(size)
    step <- u/abs(v)^(1/beta)
    # In the next equation, the difference factor (s-best) means that
    # When the solution is the best solution, it remains unchanged.
    stepsize <- 0.01*step*(s-best)
    # Here the factor 0.01 comes from the fact that L/100 should be typical
    # step size of walks/flights where L is the typical lenghtscale;
    # otherwise, Levy flights may become too aggresive/efficient.
    # which makes new solutions (even) jump out side of the design domain
    # (and thus wasting evaluations).
    # Now the actual random walks or flights
    s <- s+stepsize*rnorm(size)
    # Apply simple bounds/limits
    nest[i,] <- ceiling(abs(simple.bounds(s, lb, ub)))
  }

  return(nest)
}

cuckoos.best.nest <- function(nest, newnest, fitness, fgenome) {
  for (i in 1:dim(nest)[1]) {
    fret <- fobj(newnest[i,])
    fnew <- fret$bestcost
    if (fnew <= fitness[i]) {
      fitness[i] <- fnew
      nest[i,] <- newnest[i,]
      fgenome <- fret$fgenome
    }
  }

  # Find the current best
  fmin <- min(fitness)
  best <- nest[which.min(fitness)]
  bestgenome <- fgenome
  return(list('fmin' = fmin, 'best' = best, 'nest' = nest, 'fitness' = fitness, 'fgenome' = fgenome, 'bestgenome' = bestgenome))
}

## Replace some nests by constructing new solutions/nests
empty.nests <- function(nest, lb, ub, pa) {
  # A fraction of worse nests are discovered with a probability pa
  n <- dim(nest)[1]
  o <- dim(nest)[2]
  # Discovery or not -- a status vector
  k <- matrix(runif(n*o), n, o)>pa
  # In the real world,  if a cuckoo's egg is very similar to host's eggs, then
  # this cuckoo's egg is less likely to be discovered, thus the fitness should
  # be related to the difference in solutions. Therefore, it is a good idea
  # to do a random walk in a biased way with some random step sizes.
  ## New solution by biased/selective random walks
  stepsize <- runif(1)*(nest[sample.int(n),]-nest[sample.int(n),])
  return(nest*stepsize*k)
}

# Application of simple constraints
simple.bounds <- function(s, lb, ub) {
  # Apply the lower bound
  ns_tmp <- s
  i <- ns_tmp<lb
  ns_tmp[i] <- lb[i]
  # Apply the upper bounds
  j <- ns_tmp>ub
  ns_tmp[j] = ub[j]
  # Update this new move
  return(ns_tmp)
}

# You can replace the following by your own functions
# A d-dimensional objective functions
fobj <- function(u) {
  ## d-dimensional sphere function sum_j=1^d (u_j-1)^2.
  # with a minimum at (1,1, ...., 1);
  #return(sum((u-1)^2));
  
  params <- ceiling(abs(u))

  message("params=", paste(params, collapse=","))
  
  if(params[1] >= length(u)-1 || any(params<=0))
    return(list("bestgenome" = 0, "bestcost"=10000))
  
  message("Running genalgo ")
  #genalgo <- gaoptim::GAPerm(FUN = fnFitnessNet, n = params[1], popSize = 5)
  #genalgo$evolve(1)
  
  message("min=", paste(rep(1, params[1]), collapse=","), "max=", paste(params[2:(1+params[1])], collapse=","))
    # genalgo <- GA::ga(type = "real-valued",
    #                   fitness = fnFitnessNet,
    #                   min=c(rep(1, params[1])), max=params[2:(1+params[1])],
    #                   #parallel = T,
    #                   #nBits = 20,
    #                   keepBest = T,
    #                   maxiter = 2
    #                   #mutation = fnMutations,
    #                   #population = fnPopulation,
    #                   #popSize = 50
    #                   )
  
    genalgo <- GeneticAlg.int(genomeLen = params[1],
                              codonMin = 1,
                              codonMax = 10,
                              evalFunc = fnFitnessNet,
                              iterations = 1,
                              monitorFunc = monitorFunc,
                              genomeMin=c(rep(1, params[1])), 
                              genomeMax=params[2:(1+params[1])],
                              terminationCost = -0.9)

    message("in cuckoo genalgo: best cost= ", genalgo$best$cost, " bestgenome=", paste(genalgo$best$genome, collapse=','))
    
    return(list("bestgenome" = genalgo$best$genome, "bestcost"=genalgo$best$cost))
}


