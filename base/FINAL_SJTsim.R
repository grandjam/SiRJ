# ~~~~~~~~~~~~~~ #
# Run Simulation #
# ~~~~~~~~~~~~~~ #
library(parallel)
library(snowfall)
library(rlecuyer)
library(data.table)

# (1) Initialize numer of parallel processors to use
## Option 1: For running on local computer with multiple cores. Warning: This will use the maximum number of cores available on the computer if possible!
sfInit(parallel = T, cpus = min(nrow(conds), detectCores()), type = "SOCK")

## Option 2: For running on cores distributed across multiple computers/nodes
# sfSetMaxCPUs(number = 1000)
# sfInit(parallel = T, cpus = 250, type = "MPI")

## (2) Create experimental conditions to run. To make the call to use parallel processing easier, conds contains as many rows as there are test takers to be simulated
### Set number of test takers to create per condition for a single simulation
### To help speed up computing time, it's best to split the simulation up into smaller jobs by setting nPersons = (total number of agents desired / number of conditions created) / (total number of simulation jobs)
### In this case, I want to run 10000 agents and split it across 10 simulation jobs. So nPersons = 10000/20/10 = 50
# nPersons <- 50
nPersons <- 500 # number of test takers per condition
### Create conditions and replicate rows to create correct number of individuals to simulate
conds <- expand.grid(exptLvl = 1:5, expStrength = 5:8)
conds <- conds[rep(seq(nrow(conds)), nPersons),]
conds <- conds[order(conds$exptLvl),]
rownames(conds) <- 1:nrow(conds)
condsList <- split(conds, seq(nrow(conds))) # Turns condition data into list so that it can be used for parallel processing
### sitPresent can be set to either 0 (no situational item stems provided for any SJT items) or 1 (situational item stems provided for all SJT items)
### sitPresent cannot currently be manipulated within or between conditions (i.e., can't have some items within a given test present situations while other items do not)
### If the user wishes to create/simulate the same SJT with and without item stems, initialize a common seed in the SJTInits.R file
sitPresent = 1
### If user wants to create the same random test takers, specify a vector of values for seedNdx that is equal to nrow(conds)
#conds$seedNdx <- 1:nrow(conds)

# (3) Load model scripts and initialization code into R; make sure model code is in the same directory in which this simulation code is run
source("modelFuncs.R") # General functions for making/searching memory structures, running HyGene, and answering an SJT item
source("environInits.R") # Initialize environment for all simulated individuals
source("SJTinits.R") # Create SJT that will be taken by all simulated individuals
source("answerSJT.R") # Main model that creates a person and completes SJT

# (4) Export all data necessary to run the model and initialize the random number generator for each core
sfLibrary(data.table)
sfExportAll()

# (5) Run simulation
Start_time <- Sys.time()
condsDat <- sfClusterApplyLB(condsList, function(x) takeSJT(exptLvl = as.numeric(x[1]), 
                                                            expStrength = as.numeric(x[2]))) 
                                                            #seedNdx = as.numeric(x[3]))) # If not using a seedNdx, remove this argument from the function call
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

save.image(file = "sjtSimDatAll.RData")
save(finalDat, file = "sjtSimDatFinal.RData")
