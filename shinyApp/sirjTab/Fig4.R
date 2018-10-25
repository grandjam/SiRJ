sirjRspTimeDist <- function(dat) {
  # Compute overall time dist data
  seAllDat = c(sd(dat$meReEval)/sqrt(length(dat$meReEval)), sd(dat$leReEval)/sqrt(length(dat$leReEval)))
  allDat = data.frame("title" = rep("Overall", 2),
                      "rspType" = factor(c("ME", "LE"), levels = c("LE", "ME")),
                      "meanIter" = c(mean(dat$meReEval), mean(dat$leReEval)),
                      "sdIter" = c(sd(dat$meReEval), sd(dat$leReEval)),
                      "CILower95" = c(mean(dat$meReEval) - 2*seAllDat[1], mean(dat$leReEval) - 2*seAllDat[2]),
                      "CIUpper95" = c(mean(dat$meReEval) + 2*seAllDat[1], mean(dat$leReEval) + 2*seAllDat[2])
                      )
  # Compute time dist data for each person
  meanPersonTime = aggregate(dat[,c("meReEval", "leReEval")], by = list(dat$person), mean)
  #sdPersonTime = aggregate(dat[,c("meReEval", "leReEval")], by = list(dat$person), sd)
  sePersonTime = sapply(1:5, function(x) {
    meRspSE = sd(dat$meReEval[dat$person == x])/sqrt(length(dat$meReEval[dat$person == x]))
    leRspSE = sd(dat$leReEval[dat$person == x])/sqrt(length(dat$leReEval[dat$person == x]))
    return(c(meRspSE, leRspSE))
  })
  meCI = sapply(1:5, function(x) {
    lower = meanPersonTime$meReEval[x] - 2*sePersonTime[1,x]
    upper = meanPersonTime$meReEval[x] + 2*sePersonTime[1,x]
    return(c(lower, upper))
  })
  leCI = sapply(1:5, function(x) {
    lower = meanPersonTime$leReEval[x] - 2*sePersonTime[2,x]
    upper = meanPersonTime$leReEval[x] + 2*sePersonTime[2,x]
    return(c(lower, upper))
  })
  if (any(meCI < 0)) { # Turn any CI less than 0 into 0 for plotting purposes
    meCI[which(meCI < 0)] = 0
  }
  if (any(leCI < 0)) { # Turn any CI less than 0 into 0 for plotting purposes
    leCI[which(leCI < 0)] = 0
  }
  personDat = data.frame("person" = rep(1:5, 2),
                         "rspType" = factor(rep(c("ME","LE"), each = 5), levels = c("LE", "ME")),
                         "meanIter" = c(meanPersonTime$meReEval, meanPersonTime$leReEval),
                         #"sdIter" = c(sdPersonTime$meReEval, sdPersonTime$leReEval),
                         "CILower95" = c(meCI[1,], leCI[1,]),
                         "CIUpper95" = c(meCI[2,], leCI[2,])
                         )

  allOut = 
    ggplot(data = allDat, aes(x = rspType, y = meanIter)) +
    geom_col(aes(fill = rspType)) +
    geom_hline(aes(color = rspType, yintercept = meanIter), linetype = 2) +
    geom_text(aes(label = meanIter, y = meanIter, color = rspType, group = rspType), nudge_x = .48, hjust = "right", nudge_y = -.1, vjust = 0, size = 4, show.legend = F, fontface = "bold") +
    geom_errorbar(aes(ymin = CILower95, ymax = CIUpper95), width = .1) +
    facet_grid(.~title) +
    coord_flip() +
    scale_fill_manual(values = c("ME" = "#18BC9C", "LE" = "#E74C3C")) +
    scale_color_manual(values = c("ME" = "#18BC9C", "LE" = "#E74C3C")) +
    scale_y_continuous(limits = c(0, ceiling(max(personDat$CIUpper95))), breaks = seq(0, ceiling(max(personDat$CIUpper95)), 2)) +
    guides(fill = FALSE, color = FALSE) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_text(size = 10),
          strip.text = element_text(size = 11, face = "bold"))
  
  pplOut = 
    ggplot(data = personDat, aes(x = rspType, y = meanIter)) +
    geom_col(aes(fill = rspType)) +
    geom_hline(aes(color = rspType, yintercept = meanIter), linetype = 2) +
    geom_text(aes(label = meanIter, y = meanIter, color = rspType, group = rspType), nudge_x = .48, hjust = "right", nudge_y = -.1, vjust = 0, size = 3, show.legend = F, fontface = "bold") +
    geom_errorbar(aes(ymin = CILower95, ymax = CIUpper95), width = .1) +
    facet_grid(.~person, labeller = as_labeller(setNames(paste("Person", 1:5, sep = " "), 1:5))) +
    coord_flip() +
    scale_fill_manual(values = c("ME" = "#18BC9C", "LE" = "#E74C3C")) +
    scale_color_manual(values = c("ME" = "#18BC9C", "LE" = "#E74C3C")) +
    scale_y_continuous(limits = c(0, ceiling(max(personDat$CIUpper95))), breaks = seq(0, ceiling(max(personDat$CIUpper95)), 2)) +
    guides(fill = FALSE, color = FALSE) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 10),
          strip.text = element_text(size = 11, face = "bold"))
  
  combinedPlots = 
    grid.arrange(allOut, pplOut, nrow = 1, ncol = 2,
                 top = textGrob("Figure 4: Average number of SiRJ iterations to make response selection", hjust = -.05, x = 0, gp = gpar(fontsize = 13, fontface = c("bold.italic"))),
                 bottom = textGrob("Iterations", gp = gpar(fontsize = 12, fontface = "bold")),
                 left = textGrob("Response Type", gp = gpar(fontsize = 12, fontface = "bold"), rot = 90, vjust = 1)
                 
    )  
  return(combinedPlots)
}