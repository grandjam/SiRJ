require(runjags)
require(mcmcplots)
require(mcmcse)
require(parallelMCMCcombine)
require(tidyr)

source(paste(getwd(),"plotPost.R",sep="/"))
source(paste(getwd(),"HDIofMCMC.R",sep="/"))
# Code for plotPost.R and HDIofMCMC.R are from Kruschke, J.K. (2010). Doing Bayesian data analysis: A Tutorial with R and BUGS. Academic Press / Elsevier.

#------------------------------------------------------------------------------
# THE MODEL.
modelstring = "
model {

## Likelihood -- distribution for observations of Lvl-1 DV and Lvl-1 regression equation
### Equation uses matrix notation; requires that 'X' be initialized as an array(nLvl1, nLvl1Preds) in which nLvl1Preds includes intercept
### This model stores the Lvl-1 beta coefficients including the constant/intercept in an array B(nLvl2,nLvl1Preds)
## This model is using predictors of the rate (inverse scale) parameter, per Stander et al.
  for (i in 1:nLvl1) {
    censored[i] ~ dinterval(rt[i], censoring_limits[i])

    rt[i] ~ dweib(shape, rate[i])

    # Lvl-1 regression equation    
    rate[i] <- exp(inprod(B[subj[i],], X[i,]))
    #shape[i] <- a[subj[i]] # allows shape parameter to vary for each person
  }

## Priors -- distribtion for estimated Lvl-1 regression coefficients and Lvl-2 regression equation
### Equation uses matrix notation; inprod requires that 'U' be initialized as an array(nLvl2, nLvl2Preds) in which nLvl2Preds includes intercept
### This model stores the Lvl-2 beta coefficients including the constant/intercept in a 2D array G(nLvl1Preds, nLvl2Preds).
### In this this instance, because nLvl2Preds = 1 (there are only intercepts in the Lvl-2 equation), both G and U reduce to a vector instead of an array
  # for (k in 1:nLvl1Preds) {
  #   for (j in 1:nLvl2) {
  #     B[j,k] <- xi[k]*B.raw[j,k]    # Scaling of Lvl-1 regression coefficients
  #   }
  #   xi[k] ~ dunif(0,100)
  # }
  
  shape ~ dexp(lambda) # models exponential distribution over shape parameter
  
  for (j in 1:nLvl2) {
    #a[j] ~ dexp(lambda) # models exponential distribution over shape parameter

    ## Model distribution over Lvl-1 parameters as multivariate normal to allow for correlation between intercepts and slopes
    B[j,1:nLvl1Preds] ~ dmnorm(B.hat[j,], Tau.B[,])

    ## Lvl-2 regression equation
    for (k in 1:nLvl1Preds) {
      B.hat[j,k] <- inprod(G[k], U[j])
    }
  }

  for (k in 1:nLvl1Preds) {
    #G[k] <- xi[k]*G.raw[k]     # Scaling of Lvl-2 regression coefficients
    G[k] ~ dnorm(0,.1) # Prior distribution over Lvl-2 regression coefficients
  }

  lambda ~ dunif(0, 20)  # prediction of shape parameter in sample

  Tau.B[1:nLvl1Preds,1:nLvl1Preds] ~ dwish(W[,], df)
  df <- nLvl1Preds + 1
  Sigma.BMat[1:nLvl1Preds,1:nLvl1Preds] <- inverse(Tau.B[,])     # Models distribution of (unscaled) variance/covariance matrix of Lvl-1 coefficients using inverse Wishart distribution

  for (k in 1:nLvl1Preds) {
    for (k.prime in 1:nLvl1Preds) {
      rho.B[k,k.prime] <- Sigma.BMat[k,k.prime]/sqrt(Sigma.BMat[k,k]*Sigma.BMat[k.prime,k.prime])     # Correlations among Lvl-1 parameters 
    }
    sigma.B[k] <- sqrt(Sigma.BMat[k,k])     # Random effects for Lvl-1 parameters expressed as standard deviations
  }
}

" # close quote for modelstring
writeLines(modelstring,con="2Lvl_HLMWeibullDVCorrelatedParams.txt")

#------------------------------------------------------------------------------
# THE DATA.

## Read in data
load("sirjSJTEval.RData")

## Transform data to correct format
dat <- sjtEval[,c(1,2,11,12,22,23)]
dat$agentID[dat$agentID %in% 10001:40000] = rep(dat$agentID[dat$agentID %in% 1:10000], 3) # renumber agents to reflect they are the same person
dat <- gather(dat, key = "rspType", value = "rt", 3:4)
dat$rspType <- factor(dat$rspType, labels = c(0,1)) # 0 = LE, 1 = ME
dat$rspType <- as.numeric(levels(dat$rspType))[dat$rspType]
dat$itemStem <- factor(dat$itemStem, labels = c(0,1)) # 0 = Absent, 1 = Present
dat$itemStem <- as.numeric(levels(dat$itemStem))[dat$itemStem]
dat$compare <- dat$compare <- factor(dat$compare, labels = c(0,1)) # 0 = rspCxt, 1 = rsp
dat$compare <- as.numeric(levels(dat$compare))[dat$compare]
### Right censor responses that don't finish by 10 iterations
dat$censored <- 0
dat$censored[dat$rt == 10] <- 1
dat$rt[dat$rt == 10] <- NA

## Run model on subsets of data (1000 agents per model)
m = 50 # number of subsets
k = length(unique(dat$agentID))/m # number of agents per subset
results <- vector("list", m)
codaSamples <- vector("list", m)

for (i in 1:m) { #### HAD TO RESTART MODEL AT ITERATION 47, CHANGE BACK TO 1:M
  ## Specify data for the model
  rt <- dat$rt[dat$agentID %in% ((i-1)*k+1):(i*k)]
  x1 <- dat$rspType[dat$agentID %in% ((i-1)*k+1):(i*k)] # 0 = LE, 1 = ME
  x2 <- dat$itemStem[dat$agentID %in% ((i-1)*k+1):(i*k)] # 0 = Absent, 1 = Present
  x3 <- dat$compare[dat$agentID %in% ((i-1)*k+1):(i*k)] # 0 = rspCxt, 1 = rsp
  censored <- dat$censored[dat$agentID %in% ((i-1)*k+1):(i*k)]
  censoring_limits <- rep(15, length(rt))
  censoring_limits[is.na(dat$rt[dat$agentID %in% ((i-1)*k+1):(i*k)])] <- 10
  rtInits <- rt
  rtNdx <- is.na(rt)
  rtInits[rtNdx] <- 10.1
  rtInits[!rtNdx] <- NA
  
  ## Specify indexing values for JAGS loops
  subjCount <- as.numeric(table(dat$agentID[dat$agentID %in% ((i-1)*k+1):(i*k)]))
  subj <- rep(1:length(subjCount), subjCount)
  nLvl1 = length(rt)
  nLvl2 = length(subjCount)
  
  ## Put data into matrices for JAGS model
  X <- cbind(rep(1, nLvl1), x1, x2, x3)
  U <- rep(1, nLvl2)
  
  ## Create data list for JAGS
  nLvl1Preds = ncol(X)   # Number of Lvl-1 predictor variables + intercept
  dataList <- list(
    rt = rt ,
    X = X ,
    U = U ,
    censored = censored,
    censoring_limits = censoring_limits,
    W = diag(nLvl1Preds) ,
    subj = subj ,
    nLvl1 = nLvl1 , 
    nLvl2 = nLvl2 ,
    nLvl1Preds = nLvl1Preds
  )
  
  # Create random initialization for parameters ##
  initsList <- replicate(3, list(
    rt = rtInits,
    B = array(rnorm(nLvl2*nLvl1Preds), c(nLvl2, nLvl1Preds)) ,
    #xi = runif(nLvl1Preds) ,
    G = rnorm(nLvl1Preds) ,
    Tau.B = matrix(rWishart(1, nLvl1Preds + 1, diag(nLvl1Preds)), nrow = nLvl1Preds, ncol = nLvl1Preds)
  ), simplify = F)
  
  
  #------------------------------------------------------------------------------
  # RUN THE CHAINS
  
  parameters = c("B", "shape", "lambda", "G", "sigma.B", "rho.B")
  adaptSteps = 1000         # Number of steps to "tune" the samplers.
  burnInSteps = 5000      # Number of steps to "burn-in" the samplers.
  nChains = 3              # Number of chains to run.
  numSavedSteps = 20000    # Total number of steps in chains to save.image     
  thinSteps= 3             # Number of steps to "thin" (1=keep every step).   
  
  # Parallel processing code
  ## Perform adaptation, burn-in, and run final MCMC chains
  Start_time <- Sys.time()  
  runJagsOut <- run.jags( method = "parallel",
                          model = "2Lvl_HLMWeibullDVCorrelatedParams.txt", 
                          monitor = parameters,
                          data = dataList, 
                          inits = initsList,
                          n.chains = nChains, 
                          adapt = adaptSteps,
                          burnin = burnInSteps,
                          sample = ceiling(numSavedSteps/nChains),
                          thin = thinSteps,
                          summarise = F,
                          plots = F
  )
  Run_time <- Sys.time() - Start_time
  ## Convert JAGS object into a mcmc.list that can be accessed by coda package
  codaSamples[[i]] <- as.mcmc.list(runJagsOut)
  #codaSamplesMat <- as.mcmc(runJagsOut)
  results[[i]] <- t(as.matrix( codaSamples[[i]] ))
  save(codaSamples, results, i, file = "sirjRTResults.RData")
  
  cat("######### Finished subset", i, print(Run_time), "#########")
}

# Combine results using parallelMCMCcombine
## Transform list into array with dims c(parameters, MCMC samples, subsets)
resultsArray <- array(NA, dim = c(826, 20001, length(results)))

for (i in 1:length(results)) {
  resultsArray[,,i] <- results[[i]]
}

mcmcChain <- consensusMCindep(subchain = resultsArray)
dimnames(mcmcChain) <- dimnames(results[[1]])

# Check MCMC convergence
mcmcplot(codaSamples[[i-2]], parms = c("lambda", "shape", "G[1]", "G[2]", "G[3]", "G[4]"))
#traplot(codaSamples, parms = c("g20"))

gelmanDiag <- gelman.diag(codaSamples, multivariate = F)
geweke.diag(codaSamples)
heidel.diag(codaSamples)
ESS <- effectiveSize(codaSamples)
apply(codaSamples, 2, sd) / ESS # Monte Carlo standard error for each parameter
#multiESS(codaSamplesMat)


#------------------------------------------------------------------------------
# EXAMINE THE RESULTS

# Convert coda-object codaSamples to matrix object for easier handling.
# But note that this concatenates the different chains into one long chain.
# Result is mcmcChain[ stepIdx , paramIdx ]


# Extract Lvl-2 regression estimates from MCMC chain #
g00Samp = mcmcChain["G[1]",]
g10Samp = mcmcChain["G[2]",]
g20Samp = mcmcChain["G[3]",]
g30Samp = mcmcChain["G[4]",]
lambdaSamp = mcmcChain["lambda",]

# Plot posterior #
plotPost( g00Samp , xlab="Intercept" , compVal=0.0 , HDItextPlace=0.9, xlim = c(-4.55,-4.4))
## proportional hazard interpretation: making ME response increases "hazard" of event by a factor of 1.81 (6.1 times more likely to answer sooner than making LE response)
plotPost( g10Samp , xlab=paste("Response Type") , compVal=0.0 , HDItextPlace=0.9, xlim = c(1.75, 1.9))
## proportional hazard interpretation: situation-present response increases "hazard" of event by a factor of .09 (1.09 times more likely to answer sooner than in situation-absent)
plotPost( g20Samp , xlab="Item Stem" , compVal=0.0 , HDItextPlace=0.9)
## proportional hazard interpretation: evaluating response increases "hazard" of event by a factor of 1.2 (3.32 times more likely to answer sooner than when evaluating consequences)
plotPost( g30Samp , xlab=paste("Evaluation Criteria") , compVal=0.0 , HDItextPlace=0.9, xlim = c(1.15, 1.25))
plotPost( 1/lambdaSamp , xlab=paste("Shape parameter") , compVal=0.0 , HDItextPlace=0.9) # 1/lambda = shape parameter in sample
