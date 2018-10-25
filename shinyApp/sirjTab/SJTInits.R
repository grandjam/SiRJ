#########################################
# Create SJT items and response options #
#########################################
createSJT <- function(sjtSitNdx, sjtRspLvls, sitSpecProto, rspExptLvls) {
  # SJT parameters
  numSjtItems = 5 # Number of SJT items to create and simulate
  numSjtRsp = 4 # Number of response given for each SJT item
  
  # Create situations for SJT items
  # sjtSitNdx <- data.frame("sitType" = rep(1:numSitType, each = numSpecSit), "sitSpec" = rep(1:numSpecSit, numSitType)) # Create one item per broad and specific situation; numSJTitems must equal numSitType*numSitSpec for this one to work
  #sjtSitNdx <- data.frame("sitType" = sample(1:numSitType, size = numSjtItems, replace = T), "sitSpec" = sample(1:numSpecSit, size = numSjtItems, replace = T)) # Randomly create items of broad and specific situations; numSJTitems does not have to equal numSitType*numSitSpec
  
  # Create features for each SJT item that shares some % of features with prototypical specific situations
  sjtSit <- t(sapply(1:nrow(sjtSitNdx), function(x) { 
    sitNdx <- as.numeric(sjtSitNdx[x,])
    sjtSitFeats <- sitSpecProto[(sitSpecProto[,1] == sitNdx[1] & sitSpecProto[,2] == sitNdx[2]), 3:ncol(sitSpecProto)]
    makeSim(sjtSitFeats, .9)
  }))
  
  # Create 4 response options for each SJT item
  ## Each response option is made to reflect a situational response resembling a particular expertise level (% similar to expertise level response)
  ## Which expertise levels are represented in each SJT response is currently selected randomly (recorded in sjtRspLvls)
  #sjtRspLvls <- matrix(replicate(numSjtItems, expr = sample(1:numExptLvls, numSjtRsp, replace = F)), nrow = numSjtItems, byrow = T)
  ## Return list of length sitSpecProto, each element containing 4 response options of varying expertise levels (sjtRspLvls)
  sjtRsp <- sapply(1:numSjtItems, function (x) {
    tempLvl <- sjtRspLvls[x,]
    sitNdx <- as.numeric(sjtSitNdx[x,])
    tempRsp <- t(sapply(tempLvl, function (y) rspExptLvls[[y]][(rspExptLvls[[y]][,1] == sitNdx[1] & rspExptLvls[[y]][,2] == sitNdx[2]),(3:ncol(rspExptLvls[[y]]))]))
    rspOpt <- t(sapply(1:nrow(tempRsp), function (z) makeSim(tempRsp[z,], .9)))
    return(rspOpt)
  }, simplify = F)
  
  return(list("numSjtItems" = numSjtItems,
              "numSjtRsp" = numSjtRsp,
              "sjtSit" = sjtSit,
              "sjtRsp" = sjtRsp))
}