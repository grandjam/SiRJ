# Function for creating minivectors 
createTrace <- function(nSits, nMems, nFeats, encFid) {
  protoVecs <- rbind(sample(c(-1,0,1), nFeats, replace = T), matrix(NA, nrow = nSits-1, ncol = nFeats))
  for (i in 2:nrow(protoVecs)) {
      protoVecs[i,] = makeSim(protoVecs[i-1,], .8)
    }
  memVecs <- matrix(sapply(1:nrow(protoVecs), function(x) {
    replicate(nMems, expr = encode(protoVecs[x,], encFid))
  }, simplify = T), ncol = nFeats, byrow = T)
  if (any(apply(memVecs, 1, function(x) {all(x == 0)}))) { # Don't allow any trace to be formed that has all zeros
    ndx = apply(memVecs, 1, function(x) {all(x == 0)})
    memVecs[ndx, sample(1:nFeats, 2)] = sample(c(1,-1), 2) 
  }
  #sitProtoVecs <- t(replicate(nSits, sample(c(-1,0,1), nFeats, replace = T)))
  out <- list(memVecs, protoVecs)
  return(out)
}

# Function for creating/displaying episodic and semantic memory
createMem <- function(nSits, nMems, nFeats, encFid, MVs = c("Situation","Situation Context","Response","Response Context")) {
  ## Create episodic memory
  sitVec <- createTrace(nSits, nMems, nFeats, encFid) # sitVec computed separately so its randVec can be saved for creating probes
  epMemory <- cbind(sitVec[[1]], do.call("cbind", replicate(length(MVs)-1, createTrace(nSits, nMems, nFeats, encFid)[[1]], simplify = F)))

  ## Create data frame for plotting episodic memory
  epMemoryPlot <- as.data.frame(cbind(1:nrow(epMemory), epMemory))
  names(epMemoryPlot)[1] <- "rowID"
  epMemoryPlot$sitID <- rep(1:nSits, each = nMems)
  epMemoryPlot <- gather(epMemoryPlot, feature, value, -c(rowID, sitID))
  epMemoryPlot <- cbind(c(as.integer(factor(epMemoryPlot$feature, levels = unique(epMemoryPlot$feature)))), epMemoryPlot)
  names(epMemoryPlot)[1] <- "colID"
  epMemoryPlot <- epMemoryPlot[,c(2,1,3,5)]
  epMemoryPlot <- epMemoryPlot[order(epMemoryPlot$rowID),]
  epMemoryPlot$adj <- (epMemoryPlot$value - min(epMemoryPlot$value))/(max(epMemoryPlot$value) - min(epMemoryPlot$value)) # rescale values to be between 0-1
  epMemoryPlot$trace <- factor(findInterval(epMemoryPlot$colID, seq(0,max(epMemoryPlot$colID),nFeats), left.open = T)) # find location index for each information type (sit, sitCxt, rsp, rspCxt)
  levels(epMemoryPlot$trace) <- MVs
 
  ## Create data frame for coords of epMemory traces for plotting
  epYStrt <- rev(seq(.55, ((nSits-1)*nMems)+.55, nMems)) # reversed sequence order so that rectangle and legend colors match up
  epYEnd <- rev(seq(nMems+.45, (nSits*nMems)+.45, nMems)) # reversed sequence order so that rectangle and legend colors match up
  epXStrt <- .5
  epXEnd <- (nFeats*length(MVs))+.5
  epRectNdx <- data.frame("sits" = as.factor(1:nSits),
                          "xStrt" = epXStrt,
                          "xEnd" = epXEnd,
                          "yStrt" = epYStrt,
                          "yEnd" = epYEnd)
  
  ## Create semmantic memory and data frame for displaying
  semAvg <- aggregate(epMemoryPlot$value, by = list(interaction(epMemoryPlot$colID, epMemoryPlot$sitID, epMemoryPlot$trace)), mean) # average features across memory traces within each situation
  semLbls <- unlist(strsplit(as.character(semAvg$Group.1), "[.]"))
  semMemoryPlot <- data.frame("rowID" = as.numeric(semLbls[seq(2,length(semLbls),3)]),
                              "colID" = as.numeric(semLbls[seq(1,length(semLbls),3)]),
                              "sitID" = rep(1:nSits, each = nFeats*length(MVs)),
                              "value" = round(semAvg$x, 2),
                              "adj" = (semAvg$x - min(semAvg$x))/(max(semAvg$x) - min(semAvg$x)),
                              "trace" = as.factor(semLbls[seq(3,length(semLbls),3)]))
  semMemoryPlot <- semMemoryPlot[order(semMemoryPlot$rowID),]
  semMemory <-  matrix(semMemoryPlot$value, nrow = nSits, ncol = nFeats*length(MVs), byrow = T)
  
  ## Create data frame for coords of semMemory traces for plotting
  semYStrt <- rev(seq(1.45, (nSits+.45), 1)) # reversed sequence order so that rectangle and legend colors match up
  semYEnd <- rev(seq(0.55, nSits, 1)) # reversed sequence order so that rectangle and legend colors match up
  semXStrt <- .5
  semXEnd <- (nFeats*length(MVs))+.5
  semRectNdx <- data.frame("sits" = as.factor(1:nSits),
                          "xStrt" = semXStrt,
                          "xEnd" = semXEnd,
                          "yStrt" = semYStrt,
                          "yEnd" = semYEnd)
  
  return(list("epMemoryPlot" = epMemoryPlot, 
              "epRectNdx" = epRectNdx, 
              "epMemory" = epMemory, 
              "semMemoryPlot" = semMemoryPlot, 
              "semRectNdx" = semRectNdx, 
              "semMemory" = semMemory,
              "sitRandVec" = sitVec[[2]], 
              "nSits" = nSits, 
              "nMems" = nMems, 
              "nFeats" = nFeats,
              "MVs" = MVs))
}

# Function for creating memory probe trace
createProbe <- function(randVec, epMemory, probeSit, probeSim) {
  ## Create situation probe
  nSits = length(unique(epMemory$sitID))
  nMems = length(unique(epMemory$rowID))/nSits
  nFeats = unname(table(epMemory$trace)[1]/(nSits*nMems))
  if (as.numeric(probeSit) > 3) {
    sitProbe <- sample(c(-1,0,1), nFeats, replace = T)
  } else {
    sitProbe <- makeSim(randVec[as.numeric(probeSit),], probeSim)
  }
  if (all(sitProbe == 0)) {
    sitProbe[sample(1:nFeats, 2)] <- sample(c(1,-1), 2) # Don't allow a trace probe to be formed that has all zeros
  }
  
  ## Create data frame for plotting probe
  sitProbePlot <- data.frame("rowID" = rep((nSits*nMems)+1, nFeats),
                             "colID" = 1:nFeats,
                             "value" = sitProbe,
                             "adj" = (sitProbe - min(sitProbe))/(max(sitProbe) - min(sitProbe))) # rescale values to be between 0-1
  return(sitProbePlot)
}

# Function for computing activation levels, unspecified probe, and echo content based on memory probe
compAct <- function(memory, nSits, nMems, nFeats, MVs, probe, Ac, ep) {
  MVLength = length(MVs)
  ## Compute activation levels
  if (ep == T) { # for episodic memory
    act = apply(memory[,1:nFeats], 1, function(x) {tActLvl(x, probe)})
    rowID = 1:(nSits*nMems)
  } else { # for semantic memory
    act = apply(memory, 1, function(x) {tActLvl(x, probe)})
    rowID = 1:nSits
  }
  
  ## Computes color scaling for plots
  value = prob = round(act,2)
  if (is.factor(probe)) {
    value = prob = 0
    adj = -1
  } else {
    if (any(value < 0)) {
      posNdx = which(value > 0)
      prob = round(value/sum(value[posNdx]),2)
      prob[-posNdx] = 0
    } else {
      prob = round(value/sum(value),2)
    }
    adj = (value - min(value))/(max(value) - min(value))
  }
  
  ## Create data frame for plotting activation levels
  actLvlPlot = data.frame("rowID" = rowID,
                          "colID" = 1, #(memHyG()$nFeats*length(memHyG()$MVs))+1
                          "value" = value,
                          "prob" = prob,
                          "adj" = adj) # rescale values to be between 0-1
  
  ## Identify traces that do and do NOT exceeding activation levels (for whiting out unactivated traces on plot)
  trNoActNdx <- which(act < Ac)
  trNoActPlot <- data.frame("rowID" = rep(trNoActNdx, each = nFeats*MVLength),
                            "colID" = rep(1:(nFeats*MVLength), length(trNoActNdx)))
  
  ## Compute unspecified probe and conditional echo content
  trActNdx <- which(act >= Ac) # Identifies which traces DID exceed activation (helpful for computing conditional echo content)
  if (length(trActNdx) == 0) {
    value = "NA"
    adj = "NA"
  } else {
    if (length(trActNdx) > 1) {
      epMemCondEchoCnt <- apply(memory[trActNdx, 1:(nFeats*MVLength)], 2, function(x) {sum(x*act[trActNdx])})
      unspPrb <- epMemCondEchoCnt/max(abs(epMemCondEchoCnt))
      value = c(round(unspPrb,2), round(epMemCondEchoCnt,2))
      adj = (value - min(value))/(max(value) - min(value))
    } else {
      epMemCondEchoCnt <- memory[trActNdx,1:(nFeats*MVLength)]*act[trActNdx]
      unspPrb <- epMemCondEchoCnt/max(abs(epMemCondEchoCnt))
      value = c(round(unspPrb,2), round(epMemCondEchoCnt,2))
      adj = (value - min(value))/(max(value) - min(value))
    }
  }
  
  ## Create data frame for plotting unspecified probe
  unspPrbPlot <- data.frame("rowID" = rep(1:2, each = nFeats*MVLength),
                            "colID" = rep(1:(nFeats*MVLength), 2),
                            "value" = value,
                            "adj" = adj,
                            "trace" = rep(rep(MVs, each = nFeats),2))
  return(list("actLvlPlot" = actLvlPlot,
              "trNoActPlot" = trNoActPlot,
              "unspPrbPlot" = unspPrbPlot))
}

# Function for populating SOC
updateSOC <- function(soc, socPlot, socRectNdx, semMemory, semProbLvl, AMin, nSits, nFeats, MVs) { # semMemory has nrows = nSits, col1 = sit#, col2 = actLvl, col3-x = feature values; soc obj has nrows = WM, col1 = sit#, col2 = actLvl, col3-x = feature values
  semSamp = sample(semMemory[,1], 1, prob = semProbLvl)
  if ((!(semSamp %in% soc[,1])) & (semMemory[semSamp,2] > AMin)) {
    if (any(is.na(soc[,2]))) {
      socNdx = min(which(is.na(soc[,2])))
    } else {
      socNdx = which.min(soc[,2])
    }
    ## Update SOC obj
    soc[socNdx,] = semMemory[semMemory[,1] == semSamp,]
    
    ## Update socPlot obj
    value = semMemory[semSamp,3:ncol(semMemory)]
    socPlot$value[which(socPlot$rowID == socNdx)] = value
    socPlot$adj[which(socPlot$rowID == socNdx)] = (value - min(value))/(max(value) - min(value))
    socPlot$sitID[which(socPlot$rowID == socNdx)] = semSamp
    socPlot$trace[which(socPlot$rowID == socNdx)] = rep(MVs, each = nFeats)
    
    ## Update socRectNdx obj
    socYStrt <- seq(0.55, sum(!is.na(soc[,1])), 1)
    socYEnd <- seq(1.45, (sum(!is.na(soc[,1]))+.45), 1)
    socXStrt <- .5
    socXEnd <- (nFeats*length(MVs))+.5
    socRectNdx <- data.frame("sits" = factor((nSits+1) - na.omit(soc[,1])),
                             "xStrt" = socXStrt,
                             "xEnd" = socXEnd,
                             "yStrt" = socYStrt,
                             "yEnd" = socYEnd)
    AMin = min(soc[,2], na.rm = T)
    fail = F
  } else {
    fail = T
  }
  return(list("soc" = soc,
              "socPlot" = socPlot,
              "socRectNdx" = socRectNdx,
              "AMin" = AMin,
              "fail" = fail,
              "samp" = semSamp))
}

# Function for computing and plotting echo intensity
echoInt <- function(noActTr, socObj, memory, nSits, nMems, nFeats, MVs) {
  ## Compute echo intensity for each SOC trace against ALL traces in epMemory
  semMemActInt <- sapply(1:nrow(socObj$socRectNdx), function (y) {
    apply(memory[,(nFeats+1):ncol(memory)], 1, function (x) {
      tActLvl(x, socObj$soc[y,(nFeats+3):ncol(socObj$soc)])
    })
  }, simplify = T)
  ## Identify which epMemory traces exceed activation
  actNdx = setdiff((1:(nSits*nMems)), noActTr)
  ## Compute conditional echo intensity & posterior probability based only on traces that exceed activation in epMemory
  if (length(actNdx) == 1) { # if only one epMemory trace exceeds activation
    meanSemMemEchoInt = semMemActInt[actNdx,]
  } else {
    if (ncol(semMemActInt) > 1) { # if more than one epMemory trace activated and more than one trace in SOC
      meanSemMemEchoInt = colMeans(semMemActInt[actNdx,], na.rm = T)
    } else { # if more than one epMemory trace activated and only one trace in SOC
      meanSemMemEchoInt = mean(semMemActInt[actNdx], na.rm = T)
    }
  }
  prob = meanSemMemEchoInt/sum(meanSemMemEchoInt)
  if (any(meanSemMemEchoInt < 0 | is.na(meanSemMemEchoInt))) {
    meanSemMemEchoInt[which(meanSemMemEchoInt < 0 | is.na(meanSemMemEchoInt))] <- .001
  }
  
  ## Create data frames for plotting echo intensity for each trace in SOC; returns list of length = SOC
  semMemActIntPlot <- sapply(1:ncol(semMemActInt), function(x) {
    data.frame("rowID" = 1:(nSits*nMems),
               "colID" = 1,
               "value" = round(semMemActInt[,x], 2),
               "adj" = round((semMemActInt[,x] - min(semMemActInt[,x]))/(max(semMemActInt[,x]) - min(semMemActInt[,x])), 2),
               "sitID" = socObj$soc[x,1],
               "trace" = rep(MVs[2], nSits*nMems))
    }, simplify = F)
  names(semMemActIntPlot) = paste("SOCTrace", 1:ncol(semMemActInt), sep = "")  

  ## Create data for plotting SOC trace rectangles
  socTrRectNdx <- sapply(1:ncol(semMemActInt), function(x) {
    data.frame("sits" = socObj$socRectNdx[x,1],
               "xStrt" = .5,
               "xEnd" = nFeats+.5,
               "yStrt" = .55,
               "yEnd" = 1.45)
  }, simplify = F)
  names(socTrRectNdx) = paste("SOCTrace", 1:ncol(semMemActInt), sep = "")
  
  ## Create data frame for plotting which epMemory traces don't exceed activation (+1 in length to also cover up activation column)
  trNoActPlot <- data.frame("rowID" = rep(noActTr, each = nFeats+1),
                            "colID" = rep(1:(nFeats+1), length(noActTr)))
  return(list("echoIntPlot" = semMemActIntPlot,
              "meanEchoInt" = round(meanSemMemEchoInt, 2),
              "postProb" = round(prob, 2),
              "trNoActPlot" = trNoActPlot,
              "socTrRectNdx" = socTrRectNdx)
         )
}

