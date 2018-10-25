# Code for replicating intial environment and SJT conditions used in simulations
set.seed(8312)
source("modelFuncs.R") # General functions for making/searching memory structures and running HyGene
source("environInits.R") # Initialize environment for all simulated individuals
source("SJTinits.R") # Create SJT that will be taken by all simulated individuals
save.image("simInits.RData")