library(data.table)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Combine data files from simulation runs #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Get file names of each simulation run
datSitFileNames <- list.files(path = getwd(), pattern = 'condsDatSit')
datNoSitFileNames <- list.files(path = getwd(), pattern = 'condsDatNoSit')
datSitFileNames <- datSitFileNames[order(as.numeric(gsub("\\D", "", datSitFileNames)))] # Order files in ascending order of simulation run
datNoSitFileNames <- datNoSitFileNames[order(as.numeric(gsub("\\D", "", datNoSitFileNames)))] # Order files in ascending order of simulation run
datFileNames <- c(datNoSitFileNames, datSitFileNames)
ndx <- 1:length(datFileNames)

# Initialize final combined dataset
sjtEval <- data.frame()
rspEval <- vector("list", 20000)
names(rspEval) <- paste("agentID", 1:20000, sep = "_")

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
  
  ## Add condition (situation absent, situation present) to data file
  if (grepl("NoSit", datFileNames[i])) {
    finalJobDat$itemStem <- factor(rep("Absent", nrow(sjtEvalJob)), levels = c("Absent","Present"))
  } else {
    finalJobDat$itemStem <- factor(rep("Present", nrow(sjtEvalJob)), levels = c("Absent","Present"))
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