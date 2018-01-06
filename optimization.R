if(!require(arules))
  install.packages("arules")

if(!require(arulesViz))
  install.packages("arulesViz")

if(!require(GA))
  install.packages("GA")

if(!require(neuralnet))
  install.packages("neuralnet")

message("Reading in data")
#read in the data for exploration
#stringsAsFactors false to read original strings for pre-processing
dset <- read.csv("samp_data.csv", header = T, stringsAsFactors = F)

message("Data preparation")
#Data Preparation

#remove the reviews column until NLP can be used
removeCols <- which(names(dset) == "Reviews")

dset <- dset[,-removeCols]

#rename products just to give shorter names for better visual inspection
dset[grep("Galaxy", dset$Product.Name),"Product.Name"] <- "Galaxy SPH-D700"
dset[grep("Nokia", dset$Product.Name),"Product.Name"] <- "Nokia Asha 302"

dset$Product.Name <- as.factor(dset$Product.Name)
dset$Brand.Name <- as.factor(dset$Brand.Name)

#Convert price to factor low, mid, high
dset$Price <- cut(dset$Price, breaks = 3, labels = c("low-price","mid-price","high-price"))

#convert Votes to factor low, mid, high
dset$Review.Votes <- cut(dset$Review.Votes, breaks = 3, labels = c("low-nvotes","mid-nvotes","high-nvotes"))

#convert Votes to factor low, mid, high
dset$Rating <- cut(dset$Rating, breaks = 3, labels = c("rating1","rating2","rating3"))

nBits = 2

message("Creating transactions")
#write to file to read back as transactions
#write.csv(dset, "dset.csv", row.names=F)

#tr <- read.transactions("dset.csv", format="single", sep=",", skip = 1)

tr <- as(dset, 'transactions')

#visually inspect binary incidence matrices
#image(tr)

#plot the frequency of items sets
#itemFrequencyPlot(tr)

message("Creating apriori rules")
#mine frequent itemsets, association rules or association hyperedges using the Apriori algorithm
#use defaults (support=0.1, confidence=0.8, maxlen=10)
#rules <- apriori(tr, parameter = list(support=0.5, confidence=0.5))
rules <- apriori(tr, parameter=list(minlen=2))

#remove duplicates if they exist
#subsetRules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector

#rules <- rules[-subsetRules] # remove subset rules.

#visualize rules
#inspect(rules)

#summary of the rules’ characteristics
#summary(rules)

#interestMeasure(rules, c("support", "chiSquare", "confidence", "conviction",
#                         "cosine", "coverage", "leverage", "lift", "oddsRatio"), tr)

#scatter plot
#plot(rules)

#plot(rules,method="graph",engine="interactive",shading=NA)

#Look at a few rules
#nokiaRules <-  apriori(tr, appearance=list(default="lhs", rhs="Brand.Name=Nokia"), control=list(verbose=T))

#inspect(nokiaRules)

message("Creating rules data frame")
rulesDF <- DATAFRAME(rules, separate=T)

fnRuleSide2Bin <- function(ruleCol) #supply LHS or RHS col from rules
{
  data.frame(t(sapply(ruleCol, FUN=function(rule) #for each rule
  {
    a <- strsplit(gsub("\\{|\\}", "", as.character(rule)), ",") #remove the curly braces
    b <- sapply(a, FUN=function(y) #for each rule
    {
      c <- strsplit(y, "=") #split on equals sign
      d <- unlist( #lookup col and factor level and concat
        sapply(c, 
               FUN=function(z) 
                 paste0(
                   which(names(dset)==z[1]), 
                   as.numeric(dset[which(dset[[z[1]]] == z[2]), z[1]])[1]
                   ) #concat colname and factor level
               )
        )
      
      e <- c( #concat values
        sapply(1:ncol(dset), #for each column in the orig dset
               function(colNum)
                  if(sum(grep(paste0("^", colNum), d)) > 0) #if the col value is non-zero convert to binary equiv
                  {
                      decimal2binary(substr(grep(paste0("^", colNum), d, value=T),2,2), nBits)
                  }
                  else 
                    decimal2binary(0,2) #if zero convert to zero in bin
               )
            )
        })
    })), 
  row.names = NULL #remove row names
  )
}


fnRuleSide2Dec <- function(ruleCol) #supply LHS or RHS col from rules
{
  data.frame(t(sapply(ruleCol, FUN=function(rule) #for each rule
  {
    a <- strsplit(gsub("\\{|\\}", "", as.character(rule)), ",") #remove the curly braces
    b <- sapply(a, FUN=function(y) #for each rule
    {
      c <- strsplit(y, "=") #split on equals sign
      d <- unlist( #lookup col and factor level and concat
        sapply(c, 
               FUN=function(z) 
                 paste0(
                   which(names(dset)==z[1]), 
                   as.numeric(dset[which(dset[[z[1]]] == z[2]), z[1]])[1]
                 ) #concat colname and factor level
        )
      )
      
      e <- c( #concat values
        sapply(1:ncol(dset), #for each column in the orig dset
               function(colNum)
                 if(sum(grep(paste0("^", colNum), d)) > 0) #if the col value is non-zero convert to binary equiv
                 {
                   substr(grep(paste0("^", colNum), d, value=T),2,2)
                 }
               else 
                 0
        )
      )
    })
  })), 
  row.names = NULL #remove row names
  )
}

fnRules2Dec <- function(rules)
{
  rulesDec <- cbind(fnRuleSide2Dec(rules$LHS), fnRuleSide2Dec(rules$RHS))
  
  names(rulesDec) <- c(paste0(c("L"), names(rulesDec)[1:5]),paste0(c("R"), names(rulesDec)[6:10]))
  
  rulesDec
  
}

fnRules2Bin <- function(rules)
{
  rulesBin <- cbind(fnRuleSide2Bin(rules$LHS), fnRuleSide2Bin(rules$RHS))
  
  
  names(rulesBin) <- c(paste0(c("L"), names(rulesBin)[1:10]),paste0(c("R"), names(rulesBin)[1:10]))
  
  rulesBin
}

fnBin2Dec <- function(rulesBin)
{
  #for each binary vector row in rules
  t(apply(rulesBin, 1,
         FUN=function(x)
           sapply(1:(length(x)/nBits), #divide into 2 bit pieces
                  function(y) 
                    binary2decimal(x[((y-1)*nBits+1):((y-1)*nBits+nBits)]) #convert each piece to dec
                  )
        )
  )
}

fnDecSide2Rule <- function(rulesDecSide)
{
  if(is.null(dim(rulesDecSide)))
    rulesDecSide <- as.data.frame(t(rulesDecSide))
  
  apply(rulesDecSide, 1, 
        FUN=function(ruleDec)
        {
          a <- ruleDec
          n <- ncol(dset)
          r <- ""
          for(i in 1:length(a))
          {
            if(a[i]!=0)
              r <- paste0(r, 
                          ifelse(r=="","",","), #comma only if appending
                          names(dset)[((i-1)%%n)+1], #append the name of the col from dset
                          "=", #append equality sign
                          dset[which(as.numeric(dset[,((i-1)%%n)+1]) == a[i])[1], ((i-1)%%n)+1] #lookup and append value
                          )
          }
          
          r #return the rule text
        }
    )
}

fnDec2Rules <- function(rulesDec)
{
  n <- ncol(rulesDec)
  idxLHS <- n/2 #divide into LHS and RHS
  
  rDecLHS <- rulesDec[,1:idxLHS]
  rDecRHS <- rulesDec[,(idxLHS+1):n]
  
  rLHS <- data.frame(fnDecSide2Rule(rDecLHS), row.names = NULL)
  rRHS <- data.frame(fnDecSide2Rule(rDecRHS), row.names = NULL)
  
  setNames(cbind(rLHS, rRHS), c("LHS", "RHS"))
}

fnBin2Rules <- function(rulesBin)
{
  rulesDec <- fnBin2Dec(rulesBin)
  
  n <- ncol(rulesDec)
  idxLHS <- n/2 #divide into LHS and RHS

  rDecLHS <- rulesDec[,1:idxLHS]
  rDecRHS <- rulesDec[,(idxLHS+1):n]
  
  rLHS <- data.frame(fnDecSide2Rule(rDecLHS), row.names = NULL)
  rRHS <- data.frame(fnDecSide2Rule(rDecRHS), row.names = NULL)
  
  setNames(cbind(rLHS, rRHS), c("LHS", "RHS"))
}

fnFitnessBin <- function(ruleBin)
{
  #print("fnFitnessBin")
  
  if(is.character(ruleBin))
    ruleBin <- as.numeric(ruleBin)
  
  #find corresponding row in the binary rules
  rulePos <- sapply(1:nrow(rulesBin), 
                    FUN=function(i) 
                      all(sapply(1:ncol(rulesBin), 
                                 FUN=function(j)  
                                   rulesBin[i,j] == ruleBin[j]
                                 )
                          )
                    )

  fitness <- if(any(rulePos))
    rulesDF[rulePos, "lift"]
  else
    0
  
  #if(fitness > 0)
    #print(paste0("received:", paste0(ruleBin, collapse = ""), " fit:", fitness))
  
  fitness
}

fnFitnessDec <- function(ruleBin)
{
  #print("fnFitnessDec")
  
  if(is.character(ruleBin))
    ruleBin <- as.numeric(ruleBin)
  
  #find corresponding row in the binary rules
  rulePos <- sapply(1:nrow(rulesBin), 
                    FUN=function(i) 
                      all(sapply(1:ncol(rulesBin), 
                                 FUN=function(j)  
                                   rulesBin[i,j] == ruleBin[j]
                      )
                      )
  )
  
  fitness <- if(any(rulePos))
    rulesDF[rulePos, "lift"]
  else
    0
  
  #if(fitness > 0)
    #print(paste0("received:", paste0(ruleBin, collapse = ""), " fit:", fitness))
  
  fitness
}

fnMutation <- function(rBin)
{
  #perform mutation on a given chromosome
  #to speed up the algo we reduce the randomness of mutations
  #to avoid mutating to values that are impossible e.g. if item1 uses 2 bits of the 4 possible
  #mutate within 2 bits
  
  #print("fnMutation")
  
  #print(rBin)
  if(is.character(rBin))
    rBin <- as.numeric(rBin)
  
  nGenes <- ncol(dset)
  
  randGene <- (round(rnorm(1,0.5,0.5)*10)%%(nGenes-1))+1

  randValBinOld <- rBin[(randGene*nBits-1):(randGene*nBits)]
  
  randValBin <- randValBinOld
  
  while(all(randValBin == randValBinOld))
  {
    #print("loop1")
    randVal <- (round(rnorm(1,0.5,0.5)*10)%%(nlevels(dset[,randGene])+1))

    randValBin <- decimal2binary(randVal, nBits)
  }
  
  rBin[(randGene*nBits-1):(randGene*nBits)] <- randValBin
  
  #print((randGene*nBits-1):(randGene*nBits))
  
  #if RHS of this gene is set unset it
  rBin[11:20] = 0

  #rBin[(randGene*nBits-1+10):(randGene*nBits+10)] = c(0,0)

  prevRandGene = randGene

  #mutate RHS
  while(randGene == prevRandGene)# || any(rBin[(randGene*nBits-1):(randGene*nBits)] == 1))
  {
    #print(randGene)
    randGene <- (round(rnorm(1,0.5,0.5)*10)%%(nGenes-1))+1
  }
  
  #print((randGene*nBits-1+10):(randGene*nBits+10))
  
  randVal <- (round(rnorm(1,0.5,0.5)*10)%%(nlevels(dset[,randGene])-1))+1
  
  randValBin <- decimal2binary(randVal, nBits)    
  
  rBin[(randGene*nBits-1+10):(randGene*nBits+10)] = randValBin

  rBin
}

fnMutations <- function(rulesBin, i)
{
  #perform mutation on a single chromosome
  #print("fnMutations")
  
  #get the population of chromosomes
  rBin <- rulesBin@population

  #perform mutation on the given index
  fnMutation(rBin[i,])
}

fnPopulation <- function(object)
{
  #return an initial population to be considered
  
  #print("fnPopulation")
  #get the number of bits in the object
  nc <-  object@nBits
  
  #create a random sample of indices in rulesBin
  samp <- sample(1:nrow(rulesBin), object@popSize)
  
  #return corresponding chromosomes/rows from rulesBin
  as.matrix(rulesBin[samp,])
}

message("Converting rules to binary")
#encode rules to binary
rulesBin <- fnRules2Bin(rulesDF)

#decode binary to rules
rulesBack <- fnBin2Rules(rulesBin)

#genalgo <- genalg::rbga.bin(size = 20, suggestions = as.matrix(rulesBin), popSize = 500, iters = 100, evalFunc = fnFitness, showSettings = T, verbose = T)

############# neural net
# 
# rulesDec <- fnRules2Dec(rulesDF)
# 
# fmlDec <- paste0(paste0(names(rulesDec)[6:10], collapse = "+"), "~", paste0(names(rulesDec)[1:5], collapse = "+"))
# 
# rulesNet <- neuralnet(fmlDec, rulesDec)
# 
# res <- nnet::compute(nnet, rulesBin[,1:10])
# 
# resResult <- res$net.result
# 
# pr.nn_ <- pr.nn$net.result
# # Accuracy (test set)
# original_values <- max.col(test_cv[, 14:16])
# pr.nn_2 <- max.col(pr.nn_)
# outs[i] <- mean(pr.nn_2 == original_values)

###

monitorFunc <- function(result) {
  cat("Best of gen: ", min(result$best$cost), "\n")
}

fnFitnessNet <- function(layers)
{
  message("Training neural net with layers = ", paste(layers, collapse=","))
  rulesBin <- fnRules2Bin(rulesDF)
  
  fmlBin <- paste0(paste0(names(rulesBin)[11:20], collapse = "+"), "~", paste0(names(rulesBin)[1:10], collapse = "+"))
  
  rulesBinNet <- tryCatch(neuralnet(formula = fmlBin,
                                    data = rulesBin,
                                    lifesign = "minimal",
                                    hidden = layers,
                                    stepmax = 30000),
           warning=function(w)
           {
             message(w)
             return(10)
           },
           error=function(e)
           {
             message(e)
             return(10)
           }
             )

  if(is.numeric(rulesBinNet))
  {
    message("Did not converge")
    return(0)
  }
  
  message("Computing neural net accuracy")
  resBin <- compute(rulesBinNet, rulesBin[,1:10])
  
  resBinResult <- resBin$net.result
  
  bestAccuracy <- 0
  thresh <- 0
  bestThresh <- thresh
  
  for(thresh in seq(0, 1, by = 0.01))
  {
    originalValues <- rulesBin[, 11:20]
    predValues <- resBinResult > thresh
    accuracy <- sum(sapply(1:nrow(originalValues), FUN=function(i) all(originalValues[i,] == predValues[i,]))/nrow(originalValues))
  
    if (accuracy > bestAccuracy)
    {
      bestAccuracy <- accuracy
      bestThresh <- thresh
    }
    
    #message(bestThresh, ":", bestAccuracy)
  }
  
  message("bestthreshold:bestaccuracy = ", paste0(bestThresh, ":", bestAccuracy))
  
  return(-1 * bestAccuracy)
}

#################### Gen Algo
#maxIter
#popSize
#layers
# nLayers <- 3
# maxLayers <- 1
# nNodes <- 1
# maxNodes <- 1

# genalgo <- GA::ga(type = "real-valued",
#                   fitness = fnFitnessNet,
#                   min=c(rep(0, nLayers)), max=rep(maxNodes, nLayers),
#                   #parallel = T,
#                   #nBits = 20,
#                   keepBest = T,
#                   maxiter = 2,
#                   #mutation = fnMutations,
#                   #population = fnPopulation,
#                   #popSize = 50
#                   )

#genalgo <- gaoptim::GAPerm(FUN = fnFitnessNet, n = nLayers)

#genalgo$evolve(1)
####################
#cuckoo

source("cuckooSearch.R")
nestSize=1
a <- cuckoo.search(nestSize)
