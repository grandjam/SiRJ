# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# (1) Functions for creating memory structures #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
## Function for making traces (trace) that share some % (sim) of features on average
makeSim <- function(trace, sim) {
  featSet <- c(-1,0,1)
  nReplace <- round(length(trace)*(1-sim))
  loc <- sample(1:length(trace), nReplace, replace = F)
  chgFeat <- sapply(1:nReplace, function(x) {
    sample(featSet[!featSet %in% trace[loc][x]], 1)
  })
  trace[loc] <- chgFeat
  return(trace)}
## Function for encoding true events (event) into episodic memory given encoding fidelity (L)
encode <- function(event, L) {
  loc <- which(event == 1 | event == -1)
  enc <- sample(c(0,1), length(loc), replace = T, prob = c(1-L,L))
  encLoc <- loc[which(enc == 0)]
  event[encLoc] <- 0
  return(event)}
## Function for computing distance (Euclidean) between trace stored in memory and prototypical trace
tDist <- function(tMem, tProto, epMemCnt) {
  dst <- sapply(1:sum(apply(epMemCnt, 2, function(y) sum(y != 0))), function (x) {
    tempMem <- as.matrix(tMem[x,])
    tempTgt <- matrix(unlist(lapply(tProto, "[" , x, )), nrow = numExptLvls)
    colnames(tempMem) <- colnames(tempTgt) <- rownames(tempMem) <- rownames(tempTgt) <- NULL
    temp <- rbind(tempMem, tempTgt)
    tempDist <- dist(temp)
    temp2 <- as.numeric(which.min(as.matrix(tempDist)[2:(numExptLvls+1),1]))
    return(temp2)
  })
  return(dst)}
## Function for associating rsps to sitCxts given a strength of association (S)
rspSit <- function(sitCxtExpt, S) {
  chgExptLvl <- sample(c(0,1), 1, prob = c(S, 1-S))
  if (chgExptLvl == 0) {
    rspExptLvl <- sitCxtExpt
  } else {
    rspExptLvl <- sample((1:numExptLvls)[-sitCxtExpt], 1, replace = T)
  }
  return(rspExptLvl)}
## Function for creating probability of traces at each exptLvl in memory given a person's exptLvl
exptProb <- function(exLvl, numExptLvls) {
  exRng <- 1:numExptLvls
  exProbVals <- quantile(0:1, probs = seq(0,1,length.out = numExptLvls))
  if (exLvl == min(exRng)) {
    exSD = .08
    exMean = exProbVals[min(exRng)]+.1
  } else {
    if (exLvl == max(exRng)) {
      exSD = .08
      exMean = exProbVals[max(exRng)]-.1
    } else {
      exSD = .15
      exMean = exProbVals[exLvl]
    }
  }
  probs <- dnorm(seq(0,1,length.out = numExptLvls), mean = exMean, sd = exSD)/sum(dnorm(seq(0,1,length.out = numExptLvls), mean = exMean, sd = exSD))
  return(probs)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# (2) Functions for searching memory structures #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
## Function for computing activation level between two traces
### Computes cubed cosine similiarity between tMem and dObs
tActLvl <- function(tMem, dObs) {
  if (length(tMem) != length(dObs)) {
    stop("Traces must be same length")
  } else {
    tMem <- as.numeric(tMem)
    dObs <- as.numeric(dObs)
    actLvl <- ((tMem %*% dObs) / (sqrt((tMem%*%tMem)*(dObs%*%dObs))))^3
    return(actLvl)}}
## Function for computing similarity between two traces (same as tActLvl, except final computation is not cubed; used only in consistency checking function, see Thomas et al., p. 164)
### Computes cosine similiarity between tMem and dObs
tSimLvl <- function(tMem, dObs) {
  if (length(tMem) != length(dObs)) {
    stop("Traces must be same length")
  } else {
    tMem <- as.numeric(tMem)
    dObs <- as.numeric(dObs)
    simLvl <- ((tMem %*% dObs) / (sqrt((tMem%*%tMem)*(dObs%*%dObs))))
    return(simLvl)}}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# (3) Overall HyGene function for searching and extracting memory traces #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
## Function requires four inputs:
###  1) epMemFeats = What are the epMemory features probed by the initial stimulus? Input is the vector of column numbers from epMemory to include in Step 1 activation search
###  2) memContentNdx = What are the epMemory features that should be included in the unspecified probe passed to semMemory? Input is the vector of column numbers in epMemory that specifies entire unspecified probe used in Steps 2 and 3
###  3) srchProbe = What is the stimulus trace used to probe epMemory? Input is a single vector representing the environmental trace that prompts epMemory search in Step 1
###  4) Ac = What is the activation level threshold in episodic memory that must be reached? Input is single value between 0-1.
## Function returns a named list with two elements: 
###  [1] SOC: data frame containing full final traces extraced from semMemory based on memory search
###  [2] srchResult: data frame containing sit, sitCxt, rsp, rspCxt, echo intensity (activation strength), and posterior probabilty of traces in SOC

HyGene <- function(epMemFeats, memContentNdx, srchProbe, Ac) {
###########################################################
# Step 0: Initialize SOC for memory search and extraction #
###########################################################  
## Set of leading contenders (SOC) in WM
SOC <- as.data.frame(matrix(NA, nrow = phi, ncol = 6+length(tLen)))  
names(SOC) <- c(paste(paste("S", rep(1:numSitMv, each = mvLength), sep = ""), 1:mvLength, sep = "_"), 
                paste(paste("SCxt", rep(1:numSitCxtMv, each = mvLength), sep = ""), 1:mvLength, sep = "_"), 
                paste(paste("R", rep(1:numRspMv, each = mvLength), sep = ""), 1:mvLength, sep = "_"), 
                paste(paste("RCxt", rep(1:numRspCxtMv, each = mvLength), sep = ""), 1:mvLength, sep = "_"), 
                "sitType","sitSpec","sitCxtLvl","rspLvl","rspCxtLvl","actLvl")

#############################################################################
# Step 1: Stimulus from environment used to probe traces in episodic memory #
#############################################################################
# Compute activation level of traces in epMemory given search probe
## Computes cosine similiarity between each epMem trace and srchProbe
actEpMemTraceLvl <- apply(epMemory[,epMemFeats], 1, function(x) {
  tActLvl(x, srchProbe)
})

#################################################################
# Step 2: Construct unspecified probe based on activated traces #
#################################################################
# Identify which traces in episodic memory exceed activation threshold
actEpMemTrace <- which(round(actEpMemTraceLvl, 3) >= Ac)
## If no episodic memory traces exceed threshold...
if (length(actEpMemTrace) == 0) {
  if (sum(actEpMemTraceLvl == max(actEpMemTraceLvl)) > 1) { # ...If multiple traces are tied for highest activation level, select them all
    actEpMemTrace <- which(actEpMemTraceLvl == max(actEpMemTraceLvl))
  } else { # ...If only one trace has highest activation level, select it
    actEpMemTrace <- which.max(actEpMemTraceLvl)
  }
}
# Extract unspecified probe and transform into conditional echo content; only considers memory elements specified in memContentNdx
## unspPrbAll & epMemCondEchoCntAll return entire unspPrb and epMemCondEchoCnt for use if consistency check fails
if (!is.null(nrow(epMemory[actEpMemTrace,]))) {
  unspPrb <- apply(epMemory[actEpMemTrace, memContentNdx], 2, function(x) {sum(x*actEpMemTraceLvl[actEpMemTrace])})
  unspPrbAll <- apply(epMemory[actEpMemTrace, ], 2, function(x) {sum(x*actEpMemTraceLvl[actEpMemTrace])})
} else {
  unspPrb <- epMemory[actEpMemTrace, memContentNdx]*actEpMemTraceLvl[actEpMemTrace]
  unspPrbAll <- epMemory[actEpMemTrace, 1:((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength)+(numRspCxtMv*mvLength))]*actEpMemTraceLvl[actEpMemTrace]
}
epMemCondEchoCnt <- unspPrb/max(abs(unspPrb))
epMemCondEchoCntAll <- unspPrbAll/max(abs(unspPrbAll))

###########################################################
# Step 3: Match unspecified probe against semantic memory #
###########################################################
# Compute activation level of traces in semMemory based on epMemCondEchoCnt
actSemMemTraceLvl <- apply(semMemory[,memContentNdx], 1, function(x) {tActLvl(x, epMemCondEchoCnt)})
## If no semantic memory traces exceed threshold...
if (all(actSemMemTraceLvl < 0)) {
  if (sum(actSemMemTraceLvl == max(actEpMemTraceLvl)) > 1) { # ...If multiple traces are tied for highest activation level, select them all
    actSemMemTrace <- which(actSemMemTraceLvl == max(actSemMemTraceLvl))
    probSemMemTrace <- actSemMemTraceLvl[actSemMemTrace]/sum(actSemMemTraceLvl[actSemMemTrace])
  } else { # ...If only one trace has highest activation level, select it
    actSemMemTrace <- which.max(actSemMemTraceLvl)
    probSemMemTrace <- 1
  }
## If semantic memory traces exceed threshold...
} else {
  actSemMemTrace <- which(actSemMemTraceLvl > 0)
  probSemMemTrace <- actSemMemTraceLvl[actSemMemTrace]/sum(actSemMemTraceLvl[actSemMemTrace])
}

########################################################################
# Step 4: Add traces from semMemory to set of leading contenders (SOC) #
########################################################################
# Populate SOC
if (length(actSemMemTrace) > 1) {
  Amin = 0 # initial activation threshold for hypothesis to enter set of leading contenders
  k = 1
  while (k < Tmax) {
    RSampled <- sample(actSemMemTrace, 1, prob = probSemMemTrace) # Sample trace from semMemory at rate proportional to activation level
    RSit <- semMemory[RSampled,c("sitType","sitSpec")]
    if (!interaction(RSit[,1], RSit[,2]) %in% interaction(SOC$sitType, SOC$sitSpec, drop = T) & actSemMemTraceLvl[RSampled] > Amin) { # Checks whether sampled response is NOT already in SOC and if activation level of sampled response is greater than current minimum activation threshold for SOC inclusion
      if (any(is.na(SOC$actLvl))) {
        SOCndx <- min(which(is.na(SOC$actLvl)))
      } else {
        SOCndx <- which.min(SOC$actLvl)
      }
      SOC[SOCndx,] <- c(as.numeric(semMemory[RSampled,]), actSemMemTraceLvl[RSampled])
      Amin <- min(SOC$actLvl, na.rm = T) # Update minimum activation threshold for SOC inclusion to activation level of sampled memory
    } else {
      k = k+1
    }
  }
} else {
  SOC[1,] <- c(semMemory[actSemMemTrace,], actSemMemTraceLvl[actSemMemTrace])
}

########################################################
# Step 5: Compute mean echo intensity for items in SOC #
########################################################
# Consistency check: Check if traces in SOC are consistent with initial search probe and remove if they are not
cCheck <- which(sapply(which(!is.na(SOC$actLvl)), function (x) { tSimLvl(srchProbe, as.numeric(SOC[x,epMemFeats])) }) > 0)
SOC <- SOC[cCheck,]

# If consistency check results in removal of all items from SOC, return NA reflecting that person is unable to draw any conclusions from the memory search
if (length(cCheck) == 0) {
  srchResult <- data.frame("sitType" = NA,
                           "sitSpec" = NA,
                           "sitCxtLvl" = NA,
                           "rspLvl" = NA,
                           "rspCxtLvl" = NA,
                           "meanSemMemEchoInt" = NA,
                           "prob" = NA)
} else {
  # Multiple traces in episodic memory activated
  if (length(actEpMemTrace) > 1) {
    semMemActInt <- sapply(1:nrow(SOC), function (y) {
      apply(epMemory[actEpMemTrace, setdiff(memContentNdx, epMemFeats)], 1, function (x) {
        tActLvl(x, SOC[y,setdiff(memContentNdx, epMemFeats)])
      })
    })
    meanSemMemEchoInt = colMeans(semMemActInt, na.rm = T)
  } else {
    # Only one trace in episodic memory activated
    semMemActInt <- sapply(1:nrow(SOC), function (y) { 
      tActLvl(epMemory[actEpMemTrace, setdiff(memContentNdx, epMemFeats)], SOC[y,setdiff(memContentNdx, epMemFeats)])
    })
    meanSemMemEchoInt = semMemActInt
  }
  if (any(meanSemMemEchoInt < 0 | is.na(meanSemMemEchoInt))) {
    meanSemMemEchoInt[which(meanSemMemEchoInt < 0 | is.na(meanSemMemEchoInt))] <- .001
  }
  srchResult <- data.frame("sitType" = SOC$sitType,
                           "sitSpec" = SOC$sitSpec,
                           "sitCxtLvl" = SOC$sitCxtLvl,
                           "rspLvl" = SOC$rspLvl,
                           "rspCxtLvl" = SOC$rspCxtLvl,
                           "meanSemMemEchoInt" = meanSemMemEchoInt,
                           "prob" = meanSemMemEchoInt/sum(meanSemMemEchoInt)) # Posterior probability of traces in SOC
}
  
##################
# Compile output #
##################
  output <- list("SOC" = SOC, "srchResult" = srchResult, "unspPrb" = epMemCondEchoCntAll)
  return(output)
}

# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# # Function for conditional reasoning steps in SiRJ when situation IS PRESENTED on SJT item #
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ## Function returns a named list of lists with following elements: 
# ### [1] sitCxt: list containing (a) sitCxts entered into SOC, (b) sitCxt selection, and (c) final sitCxt chosen
# ### [2] genRsp: list containing (a) genRsps entered into SOC, (b) genRsp selection, (c) final response chosen, and (d) rspCxt for genRsp
# ### [3] givRspCxt: list containing (a) rspCxt entered into SOC for each givRsp, (b) rspCxt selection for each givRsp, (c) final rspCxt chosen for each givRsp, and (d) final sitCxt identified for each givRsp
# 
# readItem <- function(sjtItem) {
#   ###############################################################################
#   # Step 1: Generate interpretation of situation (sitCxt) given situation (sit) #
#   ###############################################################################
#   # Specify indices for the features that should be compared against search probe in epMemory (in this case, sit)
#   sitFeats <- 1:(numSitMv*mvLength)
#   ## Optional code to make number of features that can be probed limited by WM (phi)
#   # if (phi < numSitMv) {
#   #   dSamp <- sort(sample(1:numSitMv, size = phi, replace = F))
#   #   sitFeats <- c(sapply(1:length(dSamp), function (x) {
#   #     ((dSamp[x]*mvLength)-(mvLength-1)):(dSamp[x]*mvLength)}))
#   # } else {
#   #   sitFeats <- 1:(numSitMv*mvLength)
#   # }
#   
#   # Specify indices of all the memory features that should be considered when interpreting situation; should include all features making up the conditional likelihood search, e.g., (hypothesis|data)
#   ## In this case, sit and sitCxt are considered (sitCxt|sit)
#   sitSitCxtNdx <- 1:((numSitMv*mvLength)+(numSitCxtMv*mvLength))
#   
#   # Generate sitCxts in SOC and probabilistically select interpretation based on activation strength for SJT item
#   sitCxtSOC <- HyGene(epMemFeats = sitFeats, memContentNdx = sitSitCxtNdx, srchProbe = sjtSit[sjtItem, sitFeats], Ac = Ac)
#   ## Check if memory search was unsuccessful
#   if (any(is.na(sitCxtSOC$srchResult$prob))) { # if unsuccessful, return results from unspecified probe (i.e., generate novel interpretation not stored in memory)
#     sitCxtSlct <- NA
#     genSitCxt <- sitCxtSOC$unspPrb[(numSitMv*mvLength+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength))]
#   } else { # if sucessful, select result from SOC with highest probability/activation level
#     #sitCxtSlct <- sample(1:nrow(sitCxtSOC$srchResult), 1, prob = sitCxtSOC$srchResult$prob)
#     sitCxtSlct <- which.max(sitCxtSOC$srchResult$prob)
#     genSitCxt <- sitCxtSOC$SOC[sitCxtSlct, sitSitCxtNdx[-(1:(numSitMv*mvLength))]]
#   }
#   
#   
#   ##########################################################################$$$$$$$$$$$$###################################
#   # Step 2 & 3: Generate response (genRsp) and response context (genRspCxt) based on interpretation of situation (sitCxt) #
#   ######################################################################################$$$$$$$$$$$$#######################
#   # Specify indices for the features that should be compared against search probe in epMemory (in this case, sitCxt)
#   sitCxtFeats <- (numSitMv*mvLength+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength))
#   
#   # Specify indices of all the memory features that should be considered when generating response; should include all features making up the conditional likelihood search, e.g., (hypothesis|data)
#   ## In this case, sitCxt and rsp are considered (rsp|sitCxt)
#   sitCxtRspNdx <- (numSitMv*mvLength+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength))
#   
#   # Generate responses in SOC and probabilistically select rsp based on activation strength
#   genRspSOC <- HyGene(epMemFeats = sitCxtFeats, memContentNdx = sitCxtRspNdx, srchProbe = genSitCxt, Ac = Ac)
#   if (any(is.na(genRspSOC$srchResult$prob))) { # if unsuccessful, return results from unspecified probe (i.e., generate novel interpretation not stored in memory)
#     rspSlct <- NA
#     genRsp <- genRspSOC$unspPrb[((numSitMv*mvLength)+(numSitCxtMv*mvLength)+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength))]
#     genRspCxt <- genRspSOC$unspPrb[((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength)+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength)+(numRspCxtMv*mvLength))]
#   } else { # if sucessful, probabilistically select result from SOC
#     #rspSlct <- sample(1:nrow(genRspSOC$srchResult), 1, prob = genRspSOC$srchResult$prob)
#     rspSlct <- which.max(genRspSOC$srchResult$prob)
#     genRsp <- genRspSOC$SOC[rspSlct, sitCxtRspNdx[-(1:(numSitCxtMv*mvLength))]]
#     genRspCxt <- genRspSOC$SOC[rspSlct, ((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength)+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength)+(numRspCxtMv*mvLength))]
#   }
#   
#   
#   ######################################################################################
#   # Step 4: Generate interpretation of each SJT rsp (givRspCxt) given SJT rsp (givRsp) #
#   ######################################################################################
#   # Specify indices for the features that should be compared against search probe in epMemory (in this case givRsp)
#   rspFeats <- ((numSitMv*mvLength)+(numSitCxtMv*mvLength)+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength))
#   ## Optional code to make number of features that can be probed limited by WM (phi)
#   # if (phi < numRspMv) {
#   #   dSamp <- sort(sample((numSitMv+numSitCxtMv+1):(numSitMv+numSitCxtMv+numRspMv), size = phi, replace = F))
#   #   rspFeats <- c(sapply(1:length(dSamp), function (x) {
#   #     ((dSamp[x]*mvLength)-(mvLength-1)):(dSamp[x]*mvLength)}))
#   # } else {
#   #   rspFeats <- ((numSitMv*mvLength)+(numSitCxtMv*mvLength)+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength))
#   # }
#   
#   # Specify indices of all the memory features that should be considered when interpreting response options; should include all features making up the conditional likelihood search, e.g., (hypothesis|data)
#   ## In this case, sitCxt, rsp, and rspCxt are considered (rspCxt|(givRsp & genSitCxt))
#   sitCxtRspRspCxtNdx <- ((numSitMv*mvLength)+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength)+(numRspCxtMv*mvLength))
#   
#   # Generate rspCxts in SOC and probabilistically select interpretation based on activation strength for EACH givRsp to SJT item
#   givRspCxtSOC <- sapply(1:nrow(sjtRsp[[sjtItem]]), function(x) {
#     HyGene(epMemFeats = rspFeats, memContentNdx = sitCxtRspRspCxtNdx, srchProbe = as.numeric(sjtRsp[[sjtItem]][x,]), Ac = Ac)
#   })
#   ## Check if traces survived in SOC for each givRsp
#   noneGen <- unlist(lapply(givRspCxtSOC[2,], function(x) {
#     if ((any(is.na(x$prob)))) {
#       chk <- TRUE
#     } else {
#       chk <- FALSE
#     }
#     return(chk)
#   }))
#   rspCxtSlct <- sapply(1:length(noneGen), function(x) {
#     if (noneGen[x] == T) { # if no SOC traces survived for a givRsp, select nothing
#       slct <- NA
#     } else { # if any SOC traces survived for a givRsp, probabilistically select result from SOC
#       #slct <- sample(1:nrow(givRspCxtSOC[2,][[x]]), 1, prob = givRspCxtSOC[2,][[x]]$prob)
#       slct <- which.max(givRspCxtSOC[2,][[x]]$prob)
#     }
#     return(slct)
#   })
#   givRspCxt <- t(sapply(1:length(noneGen), function(x) {
#     if (noneGen[x] == T) { # if no SOC traces survived for a givRsp, return results from unspecified probe (i.e., generate novel interpretation not stored in memory)
#       grc <- givRspCxtSOC[3,][[x]][sitCxtRspRspCxtNdx[-(1:((numSitCxtMv*mvLength)+(numRspMv*mvLength)))]]
#     } else { # if any SOC traces survived for a givRsp, probabilistically select result from SOC
#       grc <- givRspCxtSOC[1,][[x]][rspCxtSlct[x], sitCxtRspRspCxtNdx[-(1:((numSitCxtMv*mvLength)+(numRspMv*mvLength)))]] 
#     }
#     return(grc)
#   }))
#   givRspSitCxt <- t(sapply(1:length(noneGen), function(x) {
#     if (noneGen[x] == T) { # if no SOC traces survived for a givRsp, return results from unspecified probe (i.e., generate novel interpretation not stored in memory)
#       grsc <- givRspCxtSOC[3,][[x]][sitCxtFeats]
#     } else { # if any SOC traces survived for a givRsp, probabilistically select result from SOC
#       grsc <- givRspCxtSOC[1,][[x]][rspCxtSlct[x], sitCxtFeats]
#     }
#     return(grsc)
#   }))
#   
#   
#   ##################
#   # Compile output #
#   ##################
#   sitCxtOut <- list("sitCxtSOC" = sitCxtSOC,
#                     "sitCxtSlct" = sitCxtSlct,
#                     "genSitCxt" = genSitCxt)
#   genRspOut <- list("genRspSOC" = genRspSOC,
#                     "rspSlct" = rspSlct,
#                     "genRsp" = genRsp,
#                     "genRspCxt" = genRspCxt)
#   givRspCxtOut <- list("givRspCxtSOC" = givRspCxtSOC,
#                        "rspCxtSlct" = rspCxtSlct,
#                        "givRspCxt" = givRspCxt,
#                        "givRspSitCxt" = givRspSitCxt)
#   output <- list("sitCxt" = sitCxtOut, "genRsp" = genRspOut, "givRspCxt" = givRspCxtOut)
#   return(output)
# }