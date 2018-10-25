# ~~~~~~~~~~~~~~ #
# Run Simulation #
# ~~~~~~~~~~~~~~ #
library(snowfall)
#library(rlecuyer)

# (1) Initialize numer of parallel processors to use
## Option 1: For running on local computer with multiple cores. Warning: This will use the maximum number of cores available on the computer if possible!
# library(parallel)
# sfInit(parallel = T, cpus = detectCores(), type = "SOCK")

## Option 2: For running on cores distributed across multiple computers/nodes
sfSetMaxCPUs(number = 1000)
sfInit(parallel = T, cpus = 250, type = "MPI")

## (2) Create experimental conditions to run. To make the call to use parallel processing easier, conds contains as many rows as there are test takers to be simulated
### Set number of test takers to create per condition for a single simulation
### To help speed up computing time, it's best to split the simulation up into smaller jobs by setting nPersons = (total number of agents desired / number of conditions created) / (total number of simulation jobs)
### In this case, I want to run 10000 agents and split it across 10 simulation jobs. So nPersons = 10000/20/10 = 50
nPersons <- 50
### Create conditions and replicate rows to create correct number of individuals to simulate
conds <- expand.grid(exptLvl = 1:5, expStrength = 5:8)
conds <- conds[rep(seq(nrow(conds)), nPersons),]
conds <- conds[order(conds$expStrength),]
rownames(conds) <- 1:nrow(conds)
condsList <- split(conds, seq(nrow(conds))) # Turns condition data into list so that it can be used for parallel processing

# (3) Load model scripts and initialization code into R; make sure model code is in the same directory in which this simulation code is run
# source("modelFuncs.R") # General functions for making/searching memory structures, running HyGene, and answering an SJT item
# source("environInits.R") # Initialize environment for all simulated individuals
# source("SJTinits.R") # Create SJT that will be taken by all simulated individuals
load("simInits.RData") # Loads in the same environment and SJT for all simulated individuals
source("readItemSitAbsent.R") # Loads in algorithm in which item stems are presented & interpreted by SJT respondents
source("answerSJT.R") # Overall SiRJ model that creates a single person who completes the SJT

# (4) Export all data and pacakges necessary to run the model
sfLibrary(data.table)
sfExportAll()

# (5) Run simulation
Start_time <- Sys.time()
condsDat <- sfClusterApplyLB(condsList, function(x) takeSJT(as.numeric(x[1]), as.numeric(x[2])))
Run_time <- Sys.time() - Start_time

# (6) Stop cluster and return R to running on single-core
sfStop()

### 7 Save data
id <- Sys.getenv("PBS_ARRAYID")
save(conds, file = paste("condsNoSit",id,".RData",sep = ""))
save(condsDat, file = paste("condsDatNoSit",id,".RData",sep = ""))