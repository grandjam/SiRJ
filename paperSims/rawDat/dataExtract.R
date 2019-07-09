library(data.table)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Combine data files from simulation runs #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Get file names of each simulation run
presRspCxtFileNames <- list.files(path = getwd(), pattern = 'condsDat_compRspCxt_sitPres')
absRspCxtFileNames <- list.files(path = getwd(), pattern = 'condsDat_compRspCxt_sitAbs')
presRspFileNames <- list.files(path = getwd(), pattern = 'condsDat_compRsp_sitPres')
absRspFileNames <- list.files(path = getwd(), pattern = 'condsDat_compRsp_sitAbs')

presRspCxtFileNames <- presRspCxtFileNames[order(as.numeric(gsub("\\D", "", presRspCxtFileNames)))] # Order files in ascending order of simulation run
absRspCxtFileNames <- absRspCxtFileNames[order(as.numeric(gsub("\\D", "", absRspCxtFileNames)))] # Order files in ascending order of simulation run
presRspFileNames <- presRspFileNames[order(as.numeric(gsub("\\D", "", presRspFileNames)))] # Order files in ascending order of simulation run
absRspFileNames <- absRspFileNames[order(as.numeric(gsub("\\D", "", absRspFileNames)))] # Order files in ascending order of simulation run

datFileNames <- c(presRspCxtFileNames, absRspCxtFileNames, presRspFileNames, absRspFileNames)
ndx <- 1:length(datFileNames)

# Initialize final combined dataset
sjtEval <- data.frame()
rspEval <- vector("list", 40000)
names(rspEval) <- paste("agentID", 1:40000, sep = "_")

# Run data extraction procedures on each simulation run
for (i in 1:length(datFileNames)) {
  ## Load in single data file
  load(datFileNames[i])
  
  ## Extract agent characteristics
  agentDat <- lapply(condsDat, function(x) {x$person[2:10]}) # Grabs all agent parameters from output
  agentDat <- rbindlist(agentDat)
  agentDat$agentID <- ((nrow(agentDat)*(i-1))+1):(nrow(agentDat)*i)
  
  ## Extract SJT results
  sjtEvalJob <- lapply(condsDat, function(x) {x$sjtEval})
  sjtEvalJob <- rbindlist(sjtEvalJob)
  sjtEvalJob$agentID <- rep(((nrow(agentDat)*(i-1))+1):(nrow(agentDat)*i), each = max(unique(sjtEvalJob$sjtItem)))
  
  ## Merge SJT and agent characteristic data
  finalJobDat <- merge(sjtEvalJob, agentDat, by = "agentID")
  finalJobDat <- finalJobDat[,-c(13,14)]
  
  ## Add condition (situation absent/situation present, compare response/compare response context) to data file
  if (grepl("sitAbs", datFileNames[i])) {
    finalJobDat$itemStem <- factor(rep("Absent", nrow(sjtEvalJob)), levels = c("Absent","Present"))
  } else {
    finalJobDat$itemStem <- factor(rep("Present", nrow(sjtEvalJob)), levels = c("Absent","Present"))
  }
  
  if (grepl("RspCxt", datFileNames[i])) {
    finalJobDat$compare <- factor(rep("rspCxt", nrow(sjtEvalJob)), levels = c("rspCxt","rsp"))
  } else {
    finalJobDat$compare <- factor(rep("rsp", nrow(sjtEvalJob)), levels = c("rspCxt","rsp"))
  }
  
  ## Extract cumulative similarity data
  rspEvalJob <- lapply(condsDat, function(x) {x$rspEval})
  
  # Save data into final object
  sjtEval <- rbind(sjtEval, finalJobDat)
  rspEval[((nrow(agentDat)*(i-1))+1):(nrow(agentDat)*i)] <- rspEvalJob
  
  # Clear environment
  rm(list = setdiff(ls(), c("datFileNames", "i", "sjtEval", "rspEval")))
}

save(sjtEval, file = "sirjSJTEval.RData")
save(rspEval, file = "sirjRspEval.RData")