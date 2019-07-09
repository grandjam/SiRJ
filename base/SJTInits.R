# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Create SJT items and response options #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# set.seed(912103) # Uncomment this line and set seed to any value to always have the same SJT created
# SJT parameters
numSjtItems = 20 # Number of SJT items to create and simulate
numSjtRsp = 4 # Number of response given for each SJT item
sjtSitSim = .9 # Similarity between an sjt item stem and the prototypical specific situation it reflected
sjtRspSim = .9 # Similarity between the expertise level of responses and response options on the SJT

# Create situations for SJT items and specify whether test will present situations (sjtPresent = 1) or not (sjtPresent = 0) for each item
## Randomly create items of broad and specific situations; numSJTitems does not have to equal numSitType*numSitSpec
sjtSitNdx <- data.frame("sitType" = sample(1:numSitType, size = numSjtItems, replace = T), 
                        "sitSpec" = sample(1:numSpecSit, size = numSjtItems, replace = T),
                        "sitPresent" = sitPresent) # Sets all items within a given test to either present or not present situations as part of item based on condition
## Create one item per broad and specific situation; numSJTitems must equal numSitType*numSitSpec for this one to work
# sjtSitNdx <- expand.grid("sitType" = 1:numSitType, 
#                          "sitSpec" = 1:numSpecSit, 
#                          "sitPresent" = sitPresent)
# sjtSitNdx <- order(sjtSitNdx$sitType)

# Create features for each SJT item that shares some % of features with prototypical specific situations
sjtSit <- t(sapply(1:nrow(sjtSitNdx), function(x) { 
  sitNdx <- as.numeric(sjtSitNdx[x,])
  sjtSitFeats <- sitSpecProto[(sitSpecProto[,1] == sitNdx[1] & sitSpecProto[,2] == sitNdx[2]), 3:ncol(sitSpecProto)]
  makeSim(sjtSitFeats, sjtSitSim)
}))

# Create 4 response options for each SJT item
## Each response option is made to reflect a situational response resembling a particular expertise level (% similar to expertise level response)
## Which expertise levels are represented in each SJT response is currently selected randomly (recorded in sjtRspLvls)
sjtRspLvls <- matrix(replicate(numSjtItems, expr = sample(1:numExptLvls, numSjtRsp, replace = F)), nrow = numSjtItems, byrow = T)
## Return list of length sitSpecProto, each element containing 4 response options of varying expertise levels (sjtRspLvls)
sjtRsp <- sapply(1:numSjtItems, function (x) {
  tempLvl <- sjtRspLvls[x,]
  sitNdx <- as.numeric(sjtSitNdx[x,])
  tempRsp <- t(sapply(tempLvl, function (y) rspExptLvls[[y]][(rspExptLvls[[y]][,1] == sitNdx[1] & rspExptLvls[[y]][,2] == sitNdx[2]),(3:ncol(rspExptLvls[[y]]))]))
  rspOpt <- t(sapply(1:nrow(tempRsp), function (z) makeSim(tempRsp[z,], sjtRspSim)))
  return(rspOpt)
}, simplify = F)
