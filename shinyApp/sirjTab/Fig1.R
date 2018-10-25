envSitDistPlot = function(sitProb, brSitVar) {
  dat = data.frame("sit" = rep(1:3, each = 101),
                   "prob" = seq(0,1,.01),
                   "dens" = NA)
  dat$dens = as.vector(sapply(1:3, function(x) {
    dbeta(seq(0,1,.01), sitProb[x]*brSitVar, (1-sitProb[x])*brSitVar)*101
  }))

  out = 
    ggplot(data = dat, aes(x = prob, y = dens, group = factor(sit), fill = factor(sit), color = factor(sit))) + 
    geom_ribbon(aes(ymin = 0, ymax = dens), alpha = .4, size = 1) +
    scale_fill_manual(values = c("1" = "#18BC9C", "2" = "#F39C12", "3" = "#E74C3C")) + 
    scale_color_manual(values = c("1" = "#18BC9C", "2" = "#F39C12", "3" = "#E74C3C")) +
    guides(fill = guide_legend(keywidth = 3, keyheight = 1, override.aes = list(alpha = 1)), color = F) +
    labs(title = paste("Figure 1. Probability distributions of situations in environment", sep = ""),
         x = "Probabiltiy",
         y = "Density",
         fill = "Situation") +
    theme_bw() +
    theme(plot.title = element_text(size = 13, face = "bold.italic"),
          legend.justification = "top",
          legend.title = element_text(face = "bold", size = 12),
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 10),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(face = "bold", size = 12),
          axis.title.y = element_text(face = "bold", size = 12))
  return(out)
}