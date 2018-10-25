########################################
# Function for answering all SJT items #
########################################
takeSJT <- function(exptLvl, expStrength, mvLength, numSitMv, numSitCxtMv, numRspMv, numRspCxtMv, tLen, numSjtItems, numSjtRsp, sjtSit, sjtRsp, epMemory, semMemory, MESim, LESim, phi = 4, Ac, Tmax, sjtEval) {
  # Progress bar for answering items
  progressItem = Progress$new() # Initialize progress bar
  progressItem$set(value = 0) # Set progress bar to start at 0
  on.exit(progressItem$close()) # Tell progress bar to close when finished
  
  # Function for updating progress bar
  updateProgressItem <- function(value = NULL, message = NULL, detail = NULL, ndx, total) {
    if (is.null(value)) {
      value = ndx/total
    }
    progressItem$set(value = value, message = message, detail = detail)
  }
  
  # Set correct environments for readItem and HyGene functions
  environment(readItem) <- environment()
  environment(HyGene) <- environment()
  evalRslt <- array(NA, dim = c(Tmax, numSjtRsp, numSjtItems))


#####################
# Answer SJT items  #
#####################
  startTime <- Sys.time()
  for (sjtItem in 1:numSjtItems) {
    # Evaluate responses
    # Update progress bar to indicate that ME response is being selected for item
    if (is.function(updateProgressItem)) {
      msgTxt = paste("Answering item #", sjtItem, "...", sep = "")
      detailTxt = paste("Choosing ME and LE responses")
      updateProgressItem(message = msgTxt, detail = detailTxt, ndx = sjtItem, total = numSjtItems)
    }
    evalNum = 0
    sjt <- vector("list", Tmax)
    while (evalNum < Tmax) {
      # Sys.sleep(.01)
      # print(paste("Evaluating MErsp for item #",sjtItem," -- iteration: ",evalNum, sep = ""))
      sjt[[evalNum+1]] <- readItem(sjtItem = sjtItem)
      givRspCxt <- sjt[[evalNum+1]]$givRspCxt$givRspCxt
      genRspSOC <- sjt[[evalNum+1]]$genRsp$genRspSOC
      rspSlct <- sjt[[evalNum+1]]$genRsp$rspSlct
      ## Compute similarity between givRspCxt for each rsp and genRspCxt (i.e., evaluate whether interpretation of givRsp consequences is similar to interpretation of genRsp consequences)
      evalRslt[evalNum+1,,sjtItem] <- sapply(1:nrow(givRspCxt), function(x) {
        tSimLvl(givRspCxt[x,], genRspSOC$SOC[rspSlct, ((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength)+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength)+(numRspCxtMv*mvLength))])
      })
      ## Compute cumulative similarity strength for each rsp option
      cumRspEval <- colSums(evalRslt[,,sjtItem], na.rm = T)
      ## Check whether there is only one evaluation that exceeds ME threshold AND it is higher than all other response options
      if ((any(cumRspEval > MESim, na.rm = T)) & (sum(cumRspEval == max(cumRspEval)) == 1)) {
        ### Check whether person has not made response yet or if current response is not as good as new response. If yes, record option; if no, do nothing
        if ((is.na(sjtEval$meRspSlct[sjtItem])) | (sjtEval$meRspSlct[sjtItem] != which.max(cumRspEval))) {
          sjtEval$meRspSlct[sjtItem] <- which.max(cumRspEval)
          sjtEval$meReEval[sjtItem] <- evalNum + 1
        }
      }
      ## Check whether there is only one evaluation that exceeds LE threshold AND it is lower than all other response options
      if ((any(cumRspEval < LESim, na.rm = T)) & (sum(cumRspEval == min(cumRspEval)) == 1)) {
        ### Check whether person has not made response yet or if current response is not as good as new response. If yes, record option; if no, do nothing
        if ((is.na(sjtEval$leRspSlct[sjtItem])) | (sjtEval$leRspSlct[sjtItem] != which.min(cumRspEval))) {
          sjtEval$leRspSlct[sjtItem] <- which.min(cumRspEval)
          sjtEval$leReEval[sjtItem] <- evalNum + 1
        }
      }
      ## Check if both ME and LE responses have been made. If so, quit evaluating; if not continue evaluating if sjtMax there is time/motivation to do so
      if ((!is.na(sjtEval$meRspSlct[sjtItem])) & (!is.na(sjtEval$leRspSlct[sjtItem]))) {
        evalNum = evalNum + 1
        break
      }
      evalNum = evalNum + 1
    } # / Close while loop
    
    # Record results of evaluation
    sjtEval[sjtItem, 6:9] <- cumRspEval
    ## Check if ME response has been made. If no response selected, make selection. If response selected, do nothing
    if (is.na(sjtEval$meRspSlct[sjtItem])) {
      ### Check if more than one rsp tied for max. If so, randomly sample one of the responses. If not, select response with highest cumulative similarity
      if ((sum(cumRspEval == max(cumRspEval)) > 1)) {
        sjtEval$meRspSlct[sjtItem] <- sample(which(cumRspEval == max(cumRspEval)), 1)
        sjtEval$meReEval[sjtItem] <- evalNum
      } else {
        sjtEval$meRspSlct[sjtItem] <- which.max(cumRspEval)
        sjtEval$meReEval[sjtItem] <- evalNum
      }
    }
    ## Check if LE response has been made. If no response selected, make selection. If response selected, do nothing
    if (is.na(sjtEval$leRspSlct[sjtItem])) {
      ### Check if more than one rsp tied for min. If so, randomly sample one of the responses. If not, select response with lowest cumulative similarity
      ### The ME response cannot also be selected as the LE response
      if ((sum(cumRspEval[-sjtEval$meRspSlct[sjtItem]] == min(cumRspEval[-sjtEval$meRspSlct[sjtItem]])) > 1)) {
        sjtEval$leRspSlct[sjtItem] <- sample(which(cumRspEval == min(cumRspEval[-sjtEval$meRspSlct[sjtItem]])), 1)
        sjtEval$leReEval[sjtItem] <- evalNum
      } else {
        sjtEval$leRspSlct[sjtItem] <- which(cumRspEval == min(cumRspEval[-sjtEval$meRspSlct[sjtItem]]))
        sjtEval$leReEval[sjtItem] <- evalNum
      }
    }
  } # / Close item for loop  
  runTime <- Sys.time() - startTime
  output <- list("sjtEval" = sjtEval,
                 # "person" = list("epMemCnt" = epMemCnt,
                 #                 "epMemSize" = sum(tFreqExp),
                 #                 "expStrength" = expStrength,
                 #                 "exptLvl" = exptLvl,
                 #                 "L" = L,
                 #                 "Ac" = Ac,
                 #                 "MESim" = MESim,
                 #                 "LESim"= LESim,
                 #                 "phi" = phi,
                 #                 "Tmax" = Tmax),
                 "rspEval" = evalRslt)
                 #"sjtEvalRaw" = sjtEvalRaw,
                 # "sjt" = list("sjtSits" = sjtSit,
                 #              "sjtRsp" = sjtRsp,
                 #              "sjtRspLvls" = sjtRspLvls),
                 # "runTime" = runTime)
  return(output)
}