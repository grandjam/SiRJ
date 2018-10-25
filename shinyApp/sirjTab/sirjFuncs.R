# Function for selecting persons to display when plotting memory distributions
personSelect = function(dat, slct) {
  tmp = dat[dat$ID %in% as.numeric(slct),]
  tmp$Freq[!tmp$sitCnt$ID %in% as.numeric(slct)] = NA
  return(tmp)
}

# Functions for processing SiRJ sjtEval data
sirjSJTEval = function(dat, pplAtt) {
  sjtEval = lapply(dat, "[[", 1)
  sjtEval = do.call("rbind", sjtEval)
  sjtEval$person = rep(1:5, each = 5)
  sjtEval$sitID = paste(sjtEval$sitType, ".", sjtEval$sitSpec, sep = "")
  sjtEval$meRspSlct = factor(LETTERS[sjtEval$meRspSlct], levels = LETTERS[1:4])
  sjtEval$leRspSlct = factor(LETTERS[sjtEval$leRspSlct], levels = LETTERS[1:4])
  sjtEval$expt = factor(rep(pplAtt$expt, each = 5), levels = 1:5)
  sjtEval$expLvl = factor(rep(pplAtt$expLvl, each = 5), levels = 1:4)
  return(sjtEval)
}

# Functions for processing SiRJ rspEval data
sirjRspEval = function(dat, Tmax, sjtEval) {
  ## Grab all rspEval data
  rspEval = lapply(dat, "[[", 2)
  
  ## Add row of zeroes into first row of rspEval for all individuals
  ### Result is list:people > list[1...5]:item 1...5
  rspEval = lapply(rspEval, function(x) {
    lapply(1:5, function(y) {
      rbind(rep(0,4), x[,,y])
    })
  })
  
  ## Computes cumulative sum of similarity levels and turns each person's data into a long data frame
  rspEval = lapply(1:5, function(x) {
    lapply(1:5, function(y) {
      out = as.data.frame(apply(rspEval[[x]][[y]], 2, cumsum))
      names(out) = c("A","B","C","D")
      out$iteration = 0:(Tmax[x])
      df = gather(out, key = rspOpt, value = cumSimilarity, A:D)
      return(df)
    })
  })
  
  ## Collapses person and item data into single overall data frame for plotting
  rspEval = do.call("rbind", lapply(rspEval, function(x) {do.call("rbind", x)}))
  
  ## Add columns to data frame for plotting
  ### Person ID
  rspEval$person = unlist(lapply(1:5, function(x) {rep(x, (Tmax[x]+1)*4*5)}))
  ### Item ID
  rspEval$item = unlist(lapply(1:5, function(x) {rep(1:5, each = (Tmax[x]+1)*4)}))
  ### ME rsp (puts T or F in rows for ME rsp)
  rspEval$meRspSlct = unlist(sapply(1:5, function(x) {
    unlist(sapply(1:5, function(y) {
      factor(rspEval[rspEval$person == x & rspEval$item == y, "rspOpt"] == sjtEval$meRspSlct[sjtEval$person == x & sjtEval$sjtItem == y])
    }, simplify = F))
  }, simplify = F))
  ### LE rsp (puts T or F in rows for LE rsp)
  rspEval$leRspSlct = unlist(sapply(1:5, function(x) {
    unlist(sapply(1:5, function(y) {
      factor(rspEval[rspEval$person == x & rspEval$item == y, "rspOpt"] == sjtEval$leRspSlct[sjtEval$person == x & sjtEval$sjtItem == y])
    }, simplify = F))
  }, simplify = F))
  ### Rsp type
  leRspSlctNdx = grep("FALSE.TRUE", interaction(rspEval$meRspSlct, rspEval$leRspSlct))
  meRspSlctNdx = grep("TRUE.FALSE", interaction(rspEval$meRspSlct, rspEval$leRspSlct))
  rspEval$rspType = "Not Chosen"
  rspEval$rspType[leRspSlctNdx] = "LE"
  rspEval$rspType[meRspSlctNdx] = "ME"
  rspEval$rspType = factor(rspEval$rspType, levels = c("ME", "LE", "Not Chosen"))
  ### ME rsp option (puts letter of rspOpt chosen as ME/LE in rows for ME/LE rsp)
  rspEval$rspSlct = "Not Chosen"
  rspEval$rspSlct[rspEval$rspType == "ME"] = rspEval$rspOpt[rspEval$rspType == "ME"]
  rspEval$rspSlct[rspEval$rspType == "LE"] = rspEval$rspOpt[rspEval$rspType == "LE"]
  rspEval$rspType = factor(rspEval$rspType, levels = c("ME", "LE", "Not Chosen"))
  ### Identify point at which final rsp was recorded
  rspEval$finalSlct = "Not Chosen"
  meFinalNdx = unlist(sapply(1:5, function(x) {
    unlist(sapply(1:5, function(y) {
      as.numeric(row.names(rspEval[rspEval$person == x & 
                                   rspEval$item == y & 
                                   rspEval$rspOpt == sjtEval$meRspSlct[sjtEval$person == x & sjtEval$sjtItem == y] & 
                                   rspEval$iteration == sjtEval$meReEval[sjtEval$person == x & sjtEval$sjtItem == y],]))
    }, simplify = F))
  }, simplify = F))
  leFinalNdx = unlist(sapply(1:5, function(x) {
    unlist(sapply(1:5, function(y) {
      as.numeric(row.names(rspEval[rspEval$person == x & 
                                   rspEval$item == y & 
                                   rspEval$rspOpt == sjtEval$leRspSlct[sjtEval$person == x & sjtEval$sjtItem == y] & 
                                   rspEval$iteration == sjtEval$leReEval[sjtEval$person == x & sjtEval$sjtItem == y],]))
    }, simplify = F))
  }, simplify = F))
  rspEval$finalSlct[meFinalNdx] <- "ME.s"
  rspEval$finalSlct[leFinalNdx] <- "LE.s"
  rspEval$finalSlct <- factor(rspEval$finalSlct, levels = c("ME.s", "LE.s", "Not Chosen"))
  # rspEval$finalSlct[meFinalNdx] <- paste(rspEval$rspOpt[meFinalNdx],"S", sep = "")
  # rspEval$finalSlct[leFinalNdx] <- paste(rspEval$rspOpt[leFinalNdx],"S", sep = "")
  
  return(rspEval)
}

# FOR COMPUTING RSP EVAL WHEN ME RSPS AND LE RSPS WERE EVALUATING SEQUENTIALLY VERSUS SIMULTANEOUSLY (E.G., USES FUNCTION IN ANSWERSJT2)
# sirjRspEval = function(data, Tmax, sjtEval) {
# # Grab all rspEval for both ME and LE 
# rspEval = lapply(data, "[[", 2)
# 
# # Add row of zeroes into first row of rspEval for all individuals
# # Result is list:people > list[1]:meRspEval & list[2]:leRspEval > list[1...5]:item 1...5
# rspEval = lapply(rspEval, function(x) {
#   lapply(x, function(y) {
#     lapply(1:5, function(z) {
#       rbind(rep(0,4), y[,,z])
#     })
#   })
# })
# 
# ## ME RSP DATA PROCESSING
# # Computes cumulative sum of similarity levels and turns each person's data into a long data frame
# meRspEval = lapply(rspEval, "[[", 1)
# meRspEval = lapply(1:5, function(x) {
#   lapply(1:5, function(y) {
#     out = as.data.frame(apply(meRspEval[[x]][[y]], 2, cumsum))
#     names(out) = c("A","B","C","D")
#     out$iteration = 0:(Tmax[x])
#     df = gather(out, key = rspOpt, value = cumSimilarity, A:D)
#     return(df)
#   })
# })
# 
# # Collapses person and item data into single overall data frame for plotting
# meRspEval = do.call("rbind", lapply(meRspEval, function(x) {do.call("rbind", x)}))
# meRspEval$person = unlist(lapply(1:5, function(x) {rep(x, (Tmax[x]+1)*4*5)}))
# meRspEval$item = unlist(lapply(1:5, function(x) {rep(1:5, each = (Tmax[x]+1)*4)}))
# meRspEval$rspSlct = unlist(sapply(1:5, function(x) { # Add which response was selected to data frame
#   unlist(sapply(1:5, function(y) {
#     factor(meRspEval[meRspEval$person == x & meRspEval$item == y, "rspOpt"] == sjtEval$meRspSlct[sjtEval$person == x & sjtEval$sjtItem == y])
#   }, simplify = F))
# }, simplify = F))
# 
# 
# ## LE RSP PROCESSING
# # Computes final cumulative similarity level at end of meRspEval process
# simEndMe = lapply(lapply(rspEval, "[[", 1), function(x) {
#   lapply(x, function(y) colSums(y, na.rm = T))
# })
# 
# # Adds cumulative similarity level at end of meRSPEval process to start of LE rsp process
# leRspEval = lapply(rspEval, "[[", 2)
# leRspEval = lapply(1:5, function(x) {
#   lapply(1:5, function(y) {
#     leRspEval[[x]][[y]][1,] <- simEndMe[[x]][[y]]
#     return(leRspEval[[x]][[y]])
#   })
# })
# 
# # Computes cumulative sum of similarity levels and turns each person's data into a long data frame
# # DOES NOT TAKE INTO ACCOUNT DIFFERNTIAL TMAX LEVELS -- SEE COMMENT BELOW
# leRspEval = lapply(1:5, function(x) {
#   lapply(1:5, function(y) {
#     out = as.data.frame(apply(leRspEval[[x]][[y]], 2, cumsum))
#     names(out) = c("A","B","C","D")
#     out$iteration = 0:(Tmax[x])
#     df = gather(out, key = rspOpt, value = cumSimilarity, A:D)
#     return(df)
#   })
# })
# 
# # Collapses person and item data into single overall data frame for plotting
# # DOES NOT TAKE INTO ACCOUNT DIFFERNTIAL TMAX LEVELS -- SEE COMMENT BELOW
# leRspEval = do.call("rbind", lapply(leRspEval, function(x) {do.call("rbind", x)}))
# leRspEval$person = meRspEval$person
# leRspEval$item = meRspEval$item
# leRspEval$rspSlct = unlist(sapply(1:5, function(x) { # Add which response was selected to data frame
#   unlist(sapply(1:5, function(y) {
#     factor(leRspEval[leRspEval$person == x & leRspEval$item == y, "rspOpt"] == sjtEval$leRspSlct[sjtEval$person == x & sjtEval$sjtItem == y])
#   }, simplify = F))
# }, simplify = F))
# 
# return(list("meRspEval" = meRspEval,
#             "leRspEval" = leRspEval))
# }