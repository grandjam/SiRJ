# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Overall function for creating a person and answering all SJT items #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Passing a value for the seedNdx argument will ensure the same random person is always created. If no value is passed for this argument, a different random person will be created each time
takeSJT <- function(exptLvl, expStrength, seedNdx = NULL) {
  environment(readItem) <- environment()
  environment(HyGene) <- environment()
# Create a person to take the SJT
  if (!is.null(seedNdx)) {
    set.seed(seedNdx)
  }
  source("personInits.R", local = T)
  evalRslt <- array(NA, dim = c(sjtMax, numSjtRsp, numSjtItems))
  sjtEvalRaw <- vector("list", numSjtItems)
  names(sjtEvalRaw) <- paste("Item", 1:numSjtItems, sep = "")


#####################
# Answer SJT items  #
#####################
  startTime <- Sys.time()
  for (sjtItem in 1:numSjtItems) {
  # Evaluate responses
    evalNum = 0
    sjt <- vector("list", sjtMax)
    while (evalNum < sjtMax) {
      # Sys.sleep(.01)
      # print(paste("Evaluating MErsp for item #",sjtItem," -- iteration: ",meEval, sep = ""))
      sjt[[evalNum+1]] <- readItem(sjtItem = sjtItem)
      givRspCxt <- sjt[[evalNum+1]]$givRspCxt$givRspCxt
      genRspSOC <- sjt[[evalNum+1]]$genRsp$genRspSOC
      rspSlct <- sjt[[evalNum+1]]$genRsp$rspSlct
      ##############################################################################################################################################################################################
      # Step 5: Compute similarity between givRspCxt for each rsp and genRspCxt (i.e., evaluate whether interpretation of givRsp consequences is similar to interpretation of genRsp consequences) #
      ##############################################################################################################################################################################################
      evalRslt[evalNum+1,,sjtItem] <- sapply(1:nrow(givRspCxt), function(x) {
        tSimLvl(givRspCxt[x,], genRspSOC$SOC[rspSlct, ((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength)+1):((numSitMv*mvLength)+(numSitCxtMv*mvLength)+(numRspMv*mvLength)+(numRspCxtMv*mvLength))])
      })
      ## Compute cumulative similarity strength for each rsp option
      cumRspEval <- colSums(evalRslt[,,sjtItem], na.rm = T)
      ######################################################################
      # Step 6: Determine if single ME and LE response has been identified #
      ######################################################################
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
    sjtEvalRaw[[sjtItem]] <- sjt[c(1:evalNum)]
    ## Check if ME response has been made. If no response selected, make selection. If response selected, do nothing
    if (is.na(sjtEval$meRspSlct[sjtItem])) {
      ### Check if more than one rsp exceeded ME threshold. If so, randomly sample one of the responses. If not, select response with highest cumulative similarity
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
      ### Check if more than one rsp exceeded LE threshold. If so, randomly sample one of the responses. If not, select response with lowest cumulative similarity
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
  
  # Sys.sleep(.01)
  # print(paste("Finished evaluating MErsp for item #",sjtItem," after ",meEval," iterations",sep = ""))

  runTime <- Sys.time() - startTime
  output <- list("sjtEval" = sjtEval,
                 "person" = list("epMemCnt" = epMemCnt,
                                 "epMemSize" = sum(tFreqExp),
                                 "expStrength" = expStrength,
                                 "exptLvl" = exptLvl,
                                 "L" = L,
                                 "Ac" = Ac,
                                 "MESim" = MESim,
                                 "LESim"= LESim,
                                 "phi" = phi,
                                 "sjtMax" = sjtMax),
                 "rspEval" = evalRslt,
                 #"sjtEvalRaw" = sjtEvalRaw, # Raw results of HyGene steps for each item. Huge file, only really helpful for confirming what happened
                 "sjt" = list("sjtSitNdx" = sjtSitNdx,
                              "sjtRspLvls" = sjtRspLvls,
                              "sjtSits" = sjtSit,
                              "sjtRsp" = sjtRsp),
                 "runTime" = runTime)
  return(output)
}