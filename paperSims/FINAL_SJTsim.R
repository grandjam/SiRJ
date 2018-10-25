# ~~~~~~~~~~~~~~ #
# Run Simulation #
# ~~~~~~~~~~~~~~ #
library(parallel)
library(snowfall)
library(rlecuyer)
library(data.table)

# (1) Initialize numer of parallel processors to use
## Option 1: For running on local computer with multiple cores. Warning: This will use the maximum number of cores available on the computer if possible!
#sfInit(parallel = T, cpus = min(nrow(conds), detectCores()), type = "SOCK")

## Option 2: For running on cores distributed across multiple computers/nodes
sfSetMaxCPUs(number = 1000)
sfInit(parallel = T, cpus = 250, type = "MPI")

## (2) Specify experimental conditions to run and create SJT that will be used for all simulations
conds <- expand.grid(exptLvl = 1:5, expStrength = 5:8)

# (3) Load model scripts and initialization code into R; make sure model code is in the same directory in which this simulation code is run
source("modelFuncs.R") # General functions for making/searching memory structures, running HyGene, and answering an SJT item
source("environInits.R") # Initialize environment for all simulated individuals
source("SJTinits.R") # Create SJT that will be taken by all simulated individuals
source("answerSJT.R") # Main model that creates a person and completes SJT
nPersons <- 500 # number of test takers per condition

# (4) Export all data necessary to run the model and initialize the random number generator for each core
sfExportAll()

# (5) Run simulation
Start_time <- Sys.time()
condsDat <- sfApply(conds, 1, function(x) replicate(nPersons, takeSJT(x[1], x[2])))
Run_time <- Sys.time() - Start_time
Run_time

# (6) Stop cluster and return R to running on single-core
sfStop()

# ~~~~~~~~~~~~ #
# Extract Data #
# ~~~~~~~~~~~~ #
agentDat <- lapply(condsDat, function(x) {rbind(x[2,])})
agentChars <- lapply(agentDat, function(x) {
  d <- t(sapply(1:length(x), function(y) {
    x[y][[1]][2:10]
  }))
})
agentChars <- do.call("rbind", agentChars)
agentChars <- as.data.frame(apply(agentChars, 2, function(x) as.numeric(x)))
agentChars$agentID <- 1:(nrow(conds)*nPersons)

sjtDat <- lapply(condsDat, function(x) {rbind(x[1,])})
sjtDat <- do.call("rbind", sjtDat)
sjtDat <- do.call("rbind", sjtDat)
sjtDat$cond <- rep(rep(1:nrow(conds), each = max(sjtDat$sjtItem)), times = nPersons)
sjtDat <- sjtDat[order(sjtDat$cond),]
sjtDat$agentID <- rep(1:(nrow(conds)*nPersons), each = max(sjtDat$sjtItem))

finalDat <- merge(sjtDat, agentChars, by = "agentID")
finalDat <- as.data.frame(finalDat[,c(1,15,2:14,16:24)])
finalDat <- as.data.frame(apply(finalDat, 2, as.numeric))

save.image(file = "sjtSimDatAll_051217.RData")
save(finalDat, file = "sjtSimDatFile_051217.RData")



# ~~~~~~~~~~~~~~~~~~ #
# Random plots, etc. #
# ~~~~~~~~~~~~~~~~~~ #
# To get cumulative activation levels for leEvalRslt data, need to add where meEvalRst left off
# sapply(1:ncol(test$rspEval$leEvalRslt[,,5]), function(x) cumsum(test$rspEval$leEvalRslt[,x,5]) + sum(test$rspEval$meEvalRslt[,x,5], na.rm=T))


# ~~~~~~~~ #
# Analyses #
# ~~~~~~~~ #
# Create frequency tables for ME & LE selections by exptLvl and expStrength
meTabDat <- table(finalDat$meRspSlct, finalDat$exptLvl, finalDat$expStrength, finalDat$sjtItem)
leTabDat <- table(finalDat$leRspSlct, finalDat$exptLvl, finalDat$expStrength, finalDat$sjtItem)

# Extract "answer key" for each exptLvl (when expStrength = 8) [SHOULD RUN A NEW SIMULATION TO GENERATE THE ANSWER KEY!]
meKey <- t(sapply(1:5, function(x) {apply(meTabDat[,x,4,], 2, which.max)}))
leKey <- t(sapply(1:5, function(x) {apply(leTabDat[,x,4,], 2, which.max)}))
## Most commonly endorsed response for each item (when exptStrength = 8)
meRspSums <- sapply(1:20, function(x) rowSums(meTabDat[,,4,x]))
leRspSums <- sapply(1:20, function(x) rowSums(leTabDat[,,4,x]))

# Score SJTs using answer keys
## 1 pt for picking keyed ME, 1 pt for picking keyed LE
classicScoring <- function(meRsps, leRsps, meKey, leKey, numItems) {
  meCrct <- meRsps == meKey
  leCrct <- leRsps == leKey
  meScore <- colSums(matrix(meCrct, nrow = numItems))
  leScore <- colSums(matrix(leCrct, nrow = numItems))
  out <- meScore + leScore
  return(out)
}

## Compute scoring
scoreKeys <- sapply(1:5, function(x) classicScoring(finalDat$meRspSlct, finalDat$leRspSlct, meKey[x,], leKey[x,], 20))

## SJT performance based on scoring keys
sjtScore <- data.frame(
  agentID = unname(tapply(finalDat$agentID, finalDat$agentID, unique)),
  exptLvl = unname(tapply(finalDat$exptLvl, finalDat$agentID, unique)),
  expStrength = unname(tapply(finalDat$expStrength, finalDat$agentID, unique)),
  scoreKey1 = scoreKeys[,1],
  scoreKey2 = scoreKeys[,2],
  scoreKey3 = scoreKeys[,3],
  scoreKey4 = scoreKeys[,4],
  scoreKey5 = scoreKeys[,5]
)


# Latent profile analysis
library(tidyr)
meTemp <- spread(finalDat[,c(1,2,18,17,3,4)], key = sjtItem, value = meRspSlct)
leTemp <- spread(finalDat[,c(1,2,18,17,3,5)], key = sjtItem, value = leRspSlct)

library(poLCA)
meRsps <- as.matrix(meTemp[,5:24]) ~ 1
leRsps <- as.matrix(leTemp[,5:24]) ~ 1

meRsps <- as.matrix(meTemp[,5:14]) ~ 1
leRsps <- as.matrix(leTemp[,5:14]) ~ 1

lca <- list()
lca$ME.1 <- poLCA(meRsps, meTemp, nclass = 1, nrep = 50, verbose = F)
lca$ME.2 <- poLCA(meRsps, meTemp, nclass = 2, nrep = 50, verbose = F)
lca$ME.3 <- poLCA(meRsps, meTemp, nclass = 3, nrep = 50, verbose = F)
lca$ME.4 <- poLCA(meRsps, meTemp, nclass = 4, nrep = 50, verbose = F)
lca$ME.5 <- poLCA(meRsps, meTemp, nclass = 5, nrep = 50, verbose = F)
lca$ME.6 <- poLCA(meRsps, meTemp, nclass = 6, nrep = 50, verbose = F)
lca$ME.7 <- poLCA(meRsps, meTemp, nclass = 7, nrep = 50, verbose = F)
lca$ME.8 <- poLCA(meRsps, meTemp, nclass = 8, nrep = 50, verbose = F)
lca$ME.9 <- poLCA(meRsps, meTemp, nclass = 9, nrep = 50, verbose = F)
lca$ME.10 <- poLCA(meRsps, meTemp, nclass = 10, nrep = 50, verbose = F)

fitStats <- array(0, dim=c(5,9))
dimnames(fitStats)[[2]] <- c("NumProfiles","LogLikelihood","AIC","BIC","Gsq","Chisq","Entropy","npar","2LLDiff")

# Unable to compute entropy values because there are too many data points...R can't handle it
getFit <- function(x) {
  cls <- length(x$P)
  ll <- x$llik
  AIC <- x$aic
  BIC <- x$bic
  Gsq <- x$Gsq
  Chisq <- x$Chisq
  #entropy <- poLCA.entropy(x)/log(prod(sapply(x$probs,ncol)))
  npar <- x$npar
  #out <- c(cls, ll, AIC, BIC, Gsq, Chisq, entropy, npar)
  out <- c(cls, ll, AIC, BIC, Gsq, Chisq, npar)
  return(out)
}

llikTest <- function(x,y) {
  lr = 2*(x - y)
  return(lr)
}

fitStats[,1:7] <- t(sapply(lca, getFit))

for (i in 1:(nrow(fitStats)-1)) {
  fitStats[i+1,8] <- llikTest(fitStats[i+1,2], fitStats[i,2])
}
