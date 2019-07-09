# ~~~~~~~~~~~~~~ #
# Run Simulation #
# ~~~~~~~~~~~~~~ #
library(parallel)
library(snowfall)
library(rlecuyer)
library(data.table)

# (1) Initialize numer of parallel processors to use
## Option 1: For running on local computer with multiple cores. Warning: This will use the maximum number of cores available on the computer if possible!
# sfInit(parallel = T, cpus = min(nrow(conds), detectCores()), type = "SOCK")

## Option 2: For running on cores distributed across multiple computers/nodes
sfSetMaxCPUs(number = 1000)
sfInit(parallel = T, cpus = as.numeric(Sys.getenv("SLURM_NTASKS")[1])-1, type = "MPI")
id <- Sys.getenv("SLURM_ARRAY_TASK_ID")

## (2) Create experimental conditions to run. To make the call to use parallel processing easier, conds contains as many rows as there are test takers to be simulated
### Set number of test takers to create per condition for a single simulation
### To help speed up computing time, it's best to split the simulation up into smaller jobs by setting nPersons = (total number of agents desired / number of conditions created) / (total number of simulation jobs) or alternatively nPersons = number of agents per condition / number of simulation jobs
### In this case, I want to run 10000 agents and split it across 25 simulation jobs. So nPersons = 10000/20/25 = 20
nPersons <- 20
### Create conditions and replicate rows to create correct number of individuals to simulate
conds <- expand.grid(exptLvl = 1:5, expStrength = 5:8)
conds <- conds[rep(seq(nrow(conds)), nPersons),]
conds <- conds[order(conds$exptLvl),]
rownames(conds) <- 1:nrow(conds)
### sitPresent can be set to either 0 (no situational item stems provided for any SJT items) or 1 (situational item stems provided for all SJT items)
### sitPresent cannot currently be manipulated within or between conditions (i.e., can't have some items within a given test present situations while other items do not)
### If the user wishes to create/simulate the same SJT with and without item stems, initialize a common seed in the SJTInits.R file
sitPresent = 1
### If user wants to create the same random test takers, specify a vector of values for seedNdx that is equal to nrow(conds)
conds$seedNdx <- (nrow(conds)*(as.numeric(id)-1)+1):(nrow(conds)*as.numeric(id))
#conds$seedNdx <- 1:nrow(conds)
condsList <- split(conds, seq(nrow(conds))) # Turns condition data into list so that it can be used for parallel processing

# (3) Load model scripts and initialization code into R; make sure model code is in the same directory in which this simulation code is run
set.seed(8312) # Sets seed to ensure that environment and SJT initialization are the same across all simluated conditions
source("modelFuncs.R") # General functions for making/searching memory structures and running HyGene
source("environInits.R") # Initialize environment for all simulated individuals
source("SJTInits.R") # Create SJT that will be taken by all simulated individuals
source("answerSJT_rsp.R") # Main model that creates a person and completes SJT -- person evaluates items based on similarity of response

# (4) Export all data necessary to run the model and initialize the random number generator for each core
sfLibrary(data.table)
sfExportAll()

# (5) Run simulation
Start_time <- Sys.time()
condsDat <- sfClusterApplyLB(condsList, function(x) takeSJT(exptLvl = as.numeric(x[1]), 
                                                            expStrength = as.numeric(x[2]), 
                                                            seedNdx = as.numeric(x[3]))) # If not using a seedNdx, remove this argument from the function call
Run_time <- Sys.time() - Start_time
Run_time


### (6) Save data
save(conds, file = paste("conds_compRsp_sitPres",id,".RData",sep = ""))
save(condsDat, file = paste("condsDat_compRsp_sitPres",id,".RData",sep = ""))

# (7) Stop cluster and return R to running on single-core
sfStop()
