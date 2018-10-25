# ~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Initialize the environment #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Memory structure
mvLength = 9 # Number of features in a single minivector (situations, responses, and context)
numSitMv = numRspMv = 2 # Number of cue minivectors for situations and responses
numSitCxtMv = numRspCxtMv = 2 # Number of context minivectors for sits and rsps. sitCxtMvs represent interpretation of needs/demands associated with sits, rspCxtMVs represent interpretatin of goals/needs satisfied by rsps
numSitType = 3 # Number of broadly different types of situations to include in semantic memory for simulation
numSpecSit = 5 # Number of unique situations within each different situation type to include in semantic memory
sitTypeProb <- c(.8, .6, .5) # Prior probability of different situation types in the environment
simSitType = .2 # 0-1; degree of similarity between broadly different types of situations
simSpecSit = .75 # 0-1; degree of similarity between unique situations within each broadly different type of situation
tLen = 1:(mvLength*(numSitMv+numSitCxtMv+numRspMv+numRspCxtMv)) # convenience index of 1:total length of a memory trace; total length of memory trace includes the situational features, the situation interpretation/context, the response, and the response interpretation/context

# Situation ecology (i.e., nature and prevalence of situations) 
brSitVar = 20 # 2-Inf; degree of variability in specific situation base rates, larger numbers will make specific situations with different base rates more distinctive
numExptLvls = 5 # Number of qualitatively different expertise levels

## Specify base rate frequency of prototypical specific situations within each broad situation type
sitSpecProb <- t(sapply(1:numSitType, function(x) {
  rbeta(numSpecSit, (sitTypeProb[x]*brSitVar), (1-sitTypeProb[x])*brSitVar)
}))
## Create protypical situation traces for each sitType and specSit
sitRandVec <- sample(c(-1,0,1), length(tLen[1:(mvLength*(numSitMv))]), replace = T)
sitTypeProto <- t(replicate(numSitType, expr = makeSim(sitRandVec, simSitType)))
sitSpecProto <- sapply(1:numSitType, function(x) {
  t(replicate(numSpecSit, makeSim(sitTypeProto[x,], simSpecSit)))
}, simplify = "array")
sitSpecProto <- apply(sitSpecProto, 2, rbind) # Collapses sitSpecProto into matrix[numSitType*numSpecSit, numFeatures in data vector]
sitSpecProto <- cbind(matrix(c(rep(1:numSitType, each = numSpecSit),rep(1:numSpecSit, numSitType)), nrow = numSpecSit*numSitType, ncol = 2), sitSpecProto) # Adds identifier for broad situation type (col 1) and specific situation type (col 2)

## Create prototypical situational context traces for each specific situation that vary by expertise level
### sitTypeCxtExpt is list of length numExptLvls; each element contains numSitType x (mvLength*numSitCxtMV) matrix representing prototypical interpretation for each broad situation type at each expertise level
sitRandCxtExpt <- t(replicate(numExptLvls, sample(c(-1,0,1), length(1:(mvLength*numSitCxtMv)), replace = T))) # Creates matrix with qualitatively different sitCxt vectors for each expertise level; ppl with different expt perceive situations differently
sitTypeCxtExpt <- sapply(1:nrow(sitRandCxtExpt), function(x) t(replicate(numSitType, makeSim(sitRandCxtExpt[x,], simSitType))), simplify = F) # Within each matrix, rows share simSitType% of features; broad situations can be perceived more or less similarly, but degree of similarity is constant across exptLvls
### List of length numExptLvls; each element contains (numSpecSit*numSitType) x (mvLength*numSitCxtMV) matrix representing prototypical interpretation for each specific situation at each expertise level
### Context traces are clustered such that interpretations of specific situations within a broad situation type share simSpecSit% of features for a given expertise level
sitCxtExptLvls <- lapply(sitTypeCxtExpt, function(x) {
  matrix(sapply(1:numSitType, function(y) replicate(numSpecSit, makeSim(x[y,], simSpecSit))), nrow = numSpecSit*numSitType, byrow = T)
})
sitCxtExptLvls <- lapply(sitCxtExptLvls, function(x) {
  cbind(matrix(c(rep(1:numSitType, each = numSpecSit),rep(1:numSpecSit, numSitType)), nrow = numSpecSit*numSitType, ncol = 2), x) # Adds identifier for broad situation type (col 1) and specific situation type (col 2)
})

## Create prototypical responses to specific situations that vary by expertise level
### rspTypeExpt is list of length numExptLvls; each element contains numSitType x mvLength matrix representing prototypical responses for each broad situation type at each expertise level
rspRandExpt <- t(replicate(numExptLvls, sample(c(-1,0,1), length(1:(mvLength*numRspMv)), replace = T))) # Creates matrix with qualitatively different rsp vectors for each expertise level; ppl with different expterise respond to situations differently
rspTypeExpt <- sapply(1:nrow(rspRandExpt), function(x) t(replicate(numSitType, makeSim(rspRandExpt[x,], simSitType))), simplify = F) # Within each matrix, rows share simSitType% of features; responses can be perceived more or less similarly, but degree of similarity is constant across exptLvls
### List of length numExptLvls; each element contains (numSpecSit*numSitType) x mvLength matrix representing responses for each specific situation at each expertise level
### Response traces are clustered such that responses to specific situations within a broad situation type share simSpecSit% of features for a given expertise level
rspExptLvls <- lapply(rspTypeExpt, function(x) {
  matrix(sapply(1:numSitType, function(y) replicate(numSpecSit, makeSim(x[y,], simSpecSit))), nrow = numSpecSit*numSitType, byrow = T)
})
rspExptLvls <- lapply(rspExptLvls, function(x) {
  cbind(matrix(c(rep(1:numSitType, each = numSpecSit),rep(1:numSpecSit, numSitType)), nrow = numSpecSit*numSitType, ncol = 2), x) # Adds identifier for broad situation type (col 1) and specific situation type (col 2)
})

## Create prototypical response context traces for each specific situation that vary by expertise level
### rspTypeCxtExpt is list of length numExptLvls; each element contains numSitType x (mvLength*numSitCxtMV) matrix representing prototypical interpretation of the goals fulfilled by responses to each broad situation type at each expertise level
rspRandCxtExpt <- t(replicate(numExptLvls, sample(c(-1,0,1), length(1:(mvLength*numRspCxtMv)), replace = T))) # Creates matrix with qualitatively different rspCxt vectors for each expertise level; ppl with different expt perceive goals achieved by responses differently
rspTypeCxtExpt <- sapply(1:nrow(rspRandCxtExpt), function(x) t(replicate(numSitType, makeSim(rspRandCxtExpt[x,], simSitType))), simplify = F) # Within each matrix, rows share simSitType% of features; interpretations of goals accomplished by responses to broad situations can be perceived more or less similarly, but degree of similarity is constant across exptLvls
### List of length numExptLvls; each element contains (numSpecSit*numSitType) x (mvLength*numSitCxtMV) matrix representing prototypical interpretation for each specific situation at each expertise level
### Response context traces are clustered such that interpretations of responses to specific situations within a broad situation type share simSpecSit% of features for a given expertise level
rspCxtExptLvls <- lapply(rspTypeCxtExpt, function(x) {
  matrix(sapply(1:numSitType, function(y) replicate(numSpecSit, makeSim(x[y,], simSpecSit))), nrow = numSpecSit*numSitType, byrow = T)
})
rspCxtExptLvls <- lapply(rspCxtExptLvls, function(x) {
  cbind(matrix(c(rep(1:numSitType, each = numSpecSit),rep(1:numSpecSit, numSitType)), nrow = numSpecSit*numSitType, ncol = 2), x) # Adds identifier for broad situation type (col 1) and specific situation type (col 2)
})
