#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Create an individual -- memory structures and data frame to hold SJT responses #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Initialize person parameters
expStrength = expStrength # 0-Inf; Scaling factor representing person's degree of experience, determines probability of having experienced a specific situation given the base rate of its situational type and the relative number of times related events are represented in episodic memory. expStrength of person is exponetially related to number of experiences stored in memory to make diffs between experience lvl greater (so don't make too big or model runs really slow!!)
exptLvl = exptLvl # integer value between 1 and 5 representing different expertise levels
exptLvlProb <- exptProb(exptLvl, numExptLvls)
L = .85 # encoding fidelity (range 0-1; higher numbers = traces encoded in epMemory are more accurate)
S = .9 # Strength of association between a particular response|interpretation of situation; larger numbers make it more likely that same response gets paired with a given interpretation. Could represent strength of normative response for given expertise lvl
Ac = .216 # threshold for trace activation in episodic memory; sets criteria that cosine similarity between two traces must exceed to be activated. Best boundaries are between [0,1] where 1 = traces are identical and 0 = traces are orthogonal (but not necessarily uncorrelated)
MESim = .85 # threshold for evaluating whether givRsp is similar enough to genRsp to be selected as ME rsp
LESim = -.85 # threshold for evaluating whether givRsp is dissimilar enough to genRsp to be selected as LE rsp
phi = 4 # WM capacity; determines maximum number of hypotheses that can be maintained in SOC at any given time
Tmax = 10 # Number of times person will search semantic memory for hypotheses to include in SOC and evaluate SJT rsps; related to motivation and/or time pressure
sjtMax = 10 # Number of times person will go through SJT item evaluation process to identify ME and LE responses; related to motivation and/or time pressure

# Create freqency of experienced events based on event probabilities and expStrength of person
tFreqSamp <- matrix(sapply(1:nrow(sitSpecProb), function(x) {
  sapply(1:ncol(sitSpecProb), function(y) {
    sample(c(0,1), size = 2^expStrength, replace = T, prob = c(1-sitSpecProb[x,y], sitSpecProb[x,y]))
  })
}), nrow = 2^expStrength, ncol = numSitType*numSpecSit)
## Partition specific situations by expertise level
tFreqExp <- c(0,colSums(tFreqSamp)) # Total number of times exemplars of specific situation have been experienced and thus appear in episodic memory; 0 added to front of vector to facilitate indexing for epMemory construction
epMemCnt <- matrix(tFreqExp[2:length(tFreqExp)], nrow = numSpecSit) # Tabulate total number of specific situations representations (row) by situation type (col)

# Initialize memory matrices
## Episodic memory
epMemory <- as.data.frame(matrix(NA, nrow = sum(tFreqExp), ncol = 5+length(tLen)))
## Semantic memory
semMemory <- as.data.frame(matrix(NA, nrow = sum(epMemCnt > 0), ncol = 5+length(tLen)))
## Label columns and add situation identifiers
names(epMemory) <- names(semMemory) <- c(paste(paste("S", rep(1:numSitMv, each = mvLength), sep = ""), 1:mvLength, sep = "_"), 
                                         paste(paste("SCxt", rep(1:numSitCxtMv, each = mvLength), sep = ""), 1:mvLength, sep = "_"), 
                                         paste(paste("R", rep(1:numRspMv, each = mvLength), sep = ""), 1:mvLength, sep = "_"),
                                         paste(paste("RCxt", rep(1:numRspCxtMv, each = mvLength), sep = ""), 1:mvLength, sep = "_"), 
                                         "sitType","sitSpec","sitCxtLvl","rspLvl","rspCxtLvl")
epMemory$sitType <- rep(1:numSitType, colSums(epMemCnt))
epMemory$sitSpec <- unlist(sapply(1:ncol(epMemCnt), function(x) rep(1:numSpecSit, epMemCnt[,x]), simplify = F))
semMemory$sitType <- rep(1:numSitType, apply(epMemCnt, 2, function(x) sum(x != 0)))
semMemory$sitSpec <- unlist(sapply(1:ncol(epMemCnt), function(x) which(epMemCnt[,x] != 0), simplify = F))

# Populate episodic memory and semantic memory based on experienced situations, responses, and interpretations
## Semantic memory is populated with prototypes of sitCxts (interpretations of needs) and associated rsps & rspCxts (actions & needs fulfilled). This operationally makes semMemory store beliefs about how and why needs perceived in the environment should be addressed
## sitCxts are associated with sits according to exptLvlProb. For a given exptLvl, specific rsps are associated with sitCxts according to S. For a given exptLvl, rsps are always associated with same rspCxt
## This representation of semMemory differs only slightly from original HyGene model where semMemory = representations of experienced hypotheses and the data with which they are typically associated (p. 158). That is, HyGene treats semantic memory as a list of prototypes, where prototypes are modeled as the expected values (i.e., average) of distribution of event traces in episodic memory (p. 163)
### ndx for looping through traces
ndxTrace <- which(tFreqExp[2:length(tFreqExp)] > 0)+1 
### Populate specific situations (sit) in episodic memory by encoding tFreqExp number of instances of each situation at fidelity lvl= L
epMemory[,1:(mvLength*(numSitMv))] <- do.call("rbind", sapply(ndxTrace, function(x) {
  epMemRow <- (cumsum(tFreqExp[1:x])[x-1]+1):(cumsum(tFreqExp[1:x])[x])
  temp <- t(sapply(epMemRow, function(y) { encode(sitSpecProto[x-1,(3:ncol(sitSpecProto))], L) }))
  return(temp)
}))
### Populate interpretations of specific situations (sitCxt) in epMemory by sampling from sitCxtLvls given distribution of interpretations across different expertise levels; interpretation encoded at fidelity = L
epMemory[,c((mvLength*numSitMv+1):((mvLength*numSitMv)+(mvLength*numSitCxtMv)),(ncol(epMemory)-2))] <- do.call("rbind", sapply(ndxTrace, function(x) {
  epMemRow <- (cumsum(tFreqExp[1:x])[x-1]+1):(cumsum(tFreqExp[1:x])[x])
  exptSamp <- sample(1:numExptLvls, length(epMemRow), replace = T, prob = exptLvlProb)
  temp <- t(sapply(exptSamp, function (y) { c(encode(sitCxtExptLvls[[y]][x-1,(3:ncol(sitCxtExptLvls[[y]]))], L), y) }))
  return(temp)
}))
### Populate responses (rsp) in episodic memory for each experience; rsp encoded at fidelity = L
#### exptSamp uses rspSit function -- provides rsp at same exptLvl as sitCxt S% of the time, and a rsp at a different lvl than sitCxt (1-S)% of the time
epMemory[,c(((mvLength*numSitMv)+(mvLength*numSitCxtMv)+1):(((mvLength*numSitMv)+(mvLength*numSitCxtMv))+(mvLength*numRspMv)),(ncol(epMemory)-1))] <- do.call("rbind", sapply(ndxTrace, function(x) {
  epMemRow <- (cumsum(tFreqExp[1:x])[x-1]+1):(cumsum(tFreqExp[1:x])[x])
  exptSamp <- sapply(epMemory$sitCxtLvl[epMemRow], function(z) rspSit(z, S))
  temp <- t(sapply(exptSamp, function (y) { c(encode(rspExptLvls[[y]][x-1,(3:ncol(rspExptLvls[[y]]))], L), y) }))
  return(temp)
}))
### Populate interpretations of responses (rspCxt) in episodic memory for each experience, where exptLvl of rsp interpretation = exptLvl of rsp; interpretation encoded at fidelity = L
epMemory[,c((((mvLength*numSitMv)+(mvLength*numSitCxtMv))+(mvLength*numRspMv)+1):(ncol(epMemory)-5),ncol(epMemory))] <- do.call("rbind", sapply(ndxTrace, function(x) {
  epMemRow <- (cumsum(tFreqExp[1:x])[x-1]+1):(cumsum(tFreqExp[1:x])[x])
  temp <- t(sapply(epMemory$rspLvl[epMemRow], function (y) { c(encode(rspCxtExptLvls[[y]][x-1,(3:ncol(rspCxtExptLvls[[y]]))], L), y) }))
  return(temp)
}))
### Populate semMemory as average of encoded memory traces
semMemory[,tLen] <- t(sapply(ndxTrace, function(x) {
  epMemRow <- (cumsum(tFreqExp[1:x])[x-1]+1):(cumsum(tFreqExp[1:x])[x])
  if (!is.null(nrow(epMemory[epMemRow,]))) {
    temp <- apply(epMemory[epMemRow,tLen], 2, mean) } else {
      temp <- epMemory[epMemRow,tLen] }
  return(temp)
}))
### Identify Euclidean distance between expertise level for sitCxt/rsp/rspCxt trace stored in semantic memory compared to prototypical sitCxt/rsp/rspCxt trace. Not very useful in practice...but descriptive
#### sitCxtLvl
semMemory$sitCxtLvl <- tDist(tMem = semMemory[,(mvLength*numSitMv+1):((mvLength*numSitMv)+(mvLength*numSitCxtMv))], 
                             tProto = lapply(sitCxtExptLvls, function(l) l[,(3:ncol(l))]),
                             epMemCnt = epMemCnt)
#### rspLvl
semMemory$rspLvl <- tDist(tMem = semMemory[,((mvLength*numSitMv)+(mvLength*numSitCxtMv)+1):(((mvLength*numSitMv)+(mvLength*numSitCxtMv))+(mvLength*numRspMv))],
                          tProto = lapply(rspExptLvls, function(l) l[,(3:ncol(l))]),
                          epMemCnt = epMemCnt)
#### rspCxtLvl
semMemory$rspCxtLvl <- tDist(tMem = semMemory[,((mvLength*numSitMv)+(mvLength*numSitCxtMv)+((mvLength*numRspMv)+1)):((mvLength*numSitMv)+(mvLength*numSitCxtMv)+(mvLength*numRspMv)+(mvLength*numRspCxtMv))],
                             tProto = lapply(rspCxtExptLvls, function(l) l[,(3:ncol(l))]),
                             epMemCnt = epMemCnt)

# Data frame to record responses to SJT items
sjtEval <- data.frame("sjtItem" = 1:numSjtItems,
                      "meRspSlct" = NA,
                      "leRspSlct" = NA,
                      "sitType" = sjtSitNdx[,1],
                      "sitSpec" = sjtSitNdx[,2],
                      "rsp1Sim" = NA, 
                      "rsp2Sim" = NA, 
                      "rsp3Sim" = NA, 
                      "rsp4Sim" = NA,
                      "meReEval" = NA,
                      "leReEval" = NA,
                      "meReEval2" = NA,
                      "meRspSlctInit" = NA)
