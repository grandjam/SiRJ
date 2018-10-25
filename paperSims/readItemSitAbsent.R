# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Function for conditional reasoning steps in SiRJ when situation IS NOT PRESENTED on SJT item #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
## Function returns a named list of lists with following elements: 
### [1] sitCxt: list containing (a) sitCxts entered into SOC, (b) sitCxt selection, and (c) final sitCxt chosen
### [2] genRsp: list containing (a) genRsps entered into SOC, (b) genRsp selection, (c) final response chosen, and (d) rspCxt for genRsp
### [3] givRspCxt: list containing (a) rspCxt entered into SOC for each givRsp, (b) rspCxt selection for each givRsp, (c) final rspCxt chosen for each givRsp, and (d) final sitCxt identified for each givRsp

readItem <- function(sjtItem) {
  ###################################################################################################
  # Step 1: Generate interpretation of situation (sitCxt) given available response options (givRsp) #
  ###################################################################################################
  # Specify indices for the features that should be compared against search probe in epMemory (in this case, rsp)
  rspFeats <- ((numSitMv*mvLength)+(numSitCxtMv*mvLength)+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength))
  
  # Specify indices of all the memory features that should be considered when interpreting situation; should include all features making up the conditional likelihood search, e.g., (hypothesis|data)
  ## In this case, rsp and sitCxt are considered (sitCxt|rsp)
  sitCxtRspNdx <- (numSitMv*mvLength+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength))
  
  # Generate sitCxts in SOC and probabilistically select interpretation based on activation strength for SJT item
  sitCxtSOC <- lapply(1:nrow(sjtRsp[[sjtItem]]), function(x) {
    HyGene(epMemFeats = rspFeats, memContentNdx = sitCxtRspNdx, srchProbe = as.numeric(sjtRsp[[sjtItem]][x,]), Ac = Ac)
  })
  
  # Check if memory search was unsuccessful
  sitCxtSlct <- sapply(sitCxtSOC, function(x) {which.max(x$srchResult$prob)})
  if(all(unlist(lapply(sitCxtSlct, function(x) length(x) == 0)))) { # if no sitCxts were generated for any rsp option, return resuts from unspecified probe (i.e., generate novel interpretation not stored in memory)
    genSitCxt = lapply(sitCxtSOC, "[[", "unspPrb")
  } else { # if at least one sitCxt was generated, select result from SOC for each rsp option with highest probability/activation level
    slctNdx = which(unlist(lapply(sitCxtSlct, function(x) length(x) > 0)))
    genSitCxt = lapply(slctNdx, function(x) {sitCxtSOC[[x]]$SOC[sitCxtSlct[[x]],]})
  }
  # Aggregate returned sitCxt vectors into single interpretation
  genSitCxt <- rbindlist(genSitCxt)
  genSitCxt <- colMeans(genSitCxt)[sitCxtRspNdx[(1:(numSitCxtMv*mvLength))]]
  
  
  ##########################################################################$$$$$$$$$$$$###################################
  # Step 2 & 3: Generate response (genRsp) and response context (genRspCxt) based on interpretation of situation (sitCxt) #
  ######################################################################################$$$$$$$$$$$$#######################
  # Specify indices for the features that should be compared against search probe in epMemory (in this case, sitCxt)
  sitCxtFeats <- (numSitMv*mvLength+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength))
  
  # Specify indices of all the memory features that should be considered when generating response; should include all features making up the conditional likelihood search, e.g., (hypothesis|data)
  ## In this case, sitCxt and rsp are considered (rsp|sitCxt)
  sitCxtRspNdx <- (numSitMv*mvLength+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength))
  
  # Generate responses in SOC and probabilistically select rsp based on activation strength
  genRspSOC <- HyGene(epMemFeats = sitCxtFeats, memContentNdx = sitCxtRspNdx, srchProbe = genSitCxt, Ac = Ac)
  if (any(is.na(genRspSOC$srchResult$prob))) { # if unsuccessful, return results from unspecified probe (i.e., generate novel interpretation not stored in memory)
    rspSlct <- NA
    genRsp <- genRspSOC$unspPrb[((numSitMv*mvLength)+(numSitCxtMv*mvLength)+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength))]
    genRspCxt <- genRspSOC$unspPrb[((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength)+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength)+(numRspCxtMv*mvLength))]
  } else { # if sucessful, probabilistically select result from SOC
    #rspSlct <- sample(1:nrow(genRspSOC$srchResult), 1, prob = genRspSOC$srchResult$prob)
    rspSlct <- which.max(genRspSOC$srchResult$prob)
    genRsp <- genRspSOC$SOC[rspSlct, sitCxtRspNdx[-(1:(numSitCxtMv*mvLength))]]
    genRspCxt <- genRspSOC$SOC[rspSlct, ((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength)+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength)+(numRspCxtMv*mvLength))]
  }
  
  
  ######################################################################################
  # Step 4: Generate interpretation of each SJT rsp (givRspCxt) given SJT rsp (givRsp) #
  ######################################################################################
  # Specify indices for the features that should be compared against search probe in epMemory (in this case givRsp)
  rspFeats <- ((numSitMv*mvLength)+(numSitCxtMv*mvLength)+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength))
  ## Optional code to make number of features that can be probed limited by WM (phi)
  # if (phi < numRspMv) {
  #   dSamp <- sort(sample((numSitMv+numSitCxtMv+1):(numSitMv+numSitCxtMv+numRspMv), size = phi, replace = F))
  #   rspFeats <- c(sapply(1:length(dSamp), function (x) {
  #     ((dSamp[x]*mvLength)-(mvLength-1)):(dSamp[x]*mvLength)}))
  # } else {
  #   rspFeats <- ((numSitMv*mvLength)+(numSitCxtMv*mvLength)+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength))
  # }
  
  # Specify indices of all the memory features that should be considered when interpreting response options; should include all features making up the conditional likelihood search, e.g., (hypothesis|data)
  ## In this case, sitCxt, rsp, and rspCxt are considered (rspCxt|(givRsp & genSitCxt))
  sitCxtRspRspCxtNdx <- ((numSitMv*mvLength)+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength)+(numRspCxtMv*mvLength))
  
  # Generate rspCxts in SOC and probabilistically select interpretation based on activation strength for EACH givRsp to SJT item
  givRspCxtSOC <- sapply(1:nrow(sjtRsp[[sjtItem]]), function(x) {
    HyGene(epMemFeats = rspFeats, memContentNdx = sitCxtRspRspCxtNdx, srchProbe = as.numeric(sjtRsp[[sjtItem]][x,]), Ac = Ac)
  })
  ## Check if traces survived in SOC for each givRsp
  noneGen <- unlist(lapply(givRspCxtSOC[2,], function(x) {
    if ((any(is.na(x$prob)))) {
      chk <- TRUE
    } else {
      chk <- FALSE
    }
    return(chk)
  }))
  rspCxtSlct <- sapply(1:length(noneGen), function(x) {
    if (noneGen[x] == T) { # if no SOC traces survived for a givRsp, select nothing
      slct <- NA
    } else { # if any SOC traces survived for a givRsp, probabilistically select result from SOC
      #slct <- sample(1:nrow(givRspCxtSOC[2,][[x]]), 1, prob = givRspCxtSOC[2,][[x]]$prob)
      slct <- which.max(givRspCxtSOC[2,][[x]]$prob)
    }
    return(slct)
  })
  givRspCxt <- t(sapply(1:length(noneGen), function(x) {
    if (noneGen[x] == T) { # if no SOC traces survived for a givRsp, return results from unspecified probe (i.e., generate novel interpretation not stored in memory)
      grc <- givRspCxtSOC[3,][[x]][sitCxtRspRspCxtNdx[-(1:((numSitCxtMv*mvLength)+(numRspMv*mvLength)))]]
    } else { # if any SOC traces survived for a givRsp, probabilistically select result from SOC
      grc <- givRspCxtSOC[1,][[x]][rspCxtSlct[x], sitCxtRspRspCxtNdx[-(1:((numSitCxtMv*mvLength)+(numRspMv*mvLength)))]] 
    }
    return(grc)
  }))
  givRspSitCxt <- t(sapply(1:length(noneGen), function(x) {
    if (noneGen[x] == T) { # if no SOC traces survived for a givRsp, return results from unspecified probe (i.e., generate novel interpretation not stored in memory)
      grsc <- givRspCxtSOC[3,][[x]][sitCxtFeats]
    } else { # if any SOC traces survived for a givRsp, probabilistically select result from SOC
      grsc <- givRspCxtSOC[1,][[x]][rspCxtSlct[x], sitCxtFeats]
    }
    return(grsc)
  }))
  
  
  ##################
  # Compile output #
  ##################
  sitCxtOut <- list("sitCxtSOC" = sitCxtSOC,
                    "sitCxtSlct" = sitCxtSlct,
                    "genSitCxt" = genSitCxt)
  genRspOut <- list("genRspSOC" = genRspSOC,
                    "rspSlct" = rspSlct,
                    "genRsp" = genRsp,
                    "genRspCxt" = genRspCxt)
  givRspCxtOut <- list("givRspCxtSOC" = givRspCxtSOC,
                       "rspCxtSlct" = rspCxtSlct,
                       "givRspCxt" = givRspCxt,
                       "givRspSitCxt" = givRspSitCxt)
  output <- list("sitCxt" = sitCxtOut, "genRsp" = genRspOut, "givRspCxt" = givRspCxtOut)
  return(output)
}