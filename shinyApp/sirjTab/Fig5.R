# Functions for creating response evaluation plots
## Make rspEval plot to extract legend
rspEvalLegend = function(dat) {
  scaleFUN <- function(x) sprintf("%.2f", x)
  out = 
    ggplot(data = dat, aes(x = iteration, y = cumSimilarity)) +
    geom_path(aes(size = rspType, color = rspType, group = rspOpt)) + #
    geom_vline(xintercept = dat$iteration[dat$finalSlct == "ME.s"], color = "#18BC9C", linetype = 2) +
    geom_vline(xintercept = dat$iteration[dat$finalSlct == "LE.s"], color = "#E74C3C", linetype = 2) +
    geom_point(aes(shape = rspOpt, color = rspType), fill = "transparent", stroke = 1.5, size = 5) +
    geom_point(data = dat[dat$finalSlct != "Not Chosen",], aes(shape = rspOpt, color = finalSlct, fill = finalSlct), stroke = 1.5, size = 5) +
    geom_text(data = dat[dat$finalSlct != "Not Chosen",], aes(label = rspOpt), color = "black", size = 3) +
    geom_hline(yintercept = .85) +
    geom_hline(yintercept = -.85) +
    { if (max(abs(dat$cumSimilarity), na.rm = T) < 1) {scale_y_continuous(limits = c(-1,1), labels = scaleFUN)} } +
    { if (max(abs(dat$cumSimilarity), na.rm = T) >= 1) {scale_y_continuous(limits = c((0 - max(abs(dat$cumSimilarity), na.rm = T)), (0 + max(abs(dat$cumSimilarity), na.rm = T))), labels = scaleFUN)} } +
    scale_color_manual(breaks = c("ME", "LE"), values = c("ME" = "#18BC9C", "LE" = "#E74C3C", "Not Chosen" = "gray", "ME.s" = "black", "LE.s" = "black")) +
    scale_fill_manual(values = c("ME.s" = "#18BC9C", "LE.s" = "#E74C3C", "Not Chosen" = "transparent")) +
    scale_shape_manual(values = c("A" = 21, "B" = 22, "C" = 24, "D" = 23)) +
    scale_size_manual(values = c("ME" = 2, "LE" = 2, "Not Chosen" = 1)) +
    guides(size = FALSE, 
           fill = FALSE, 
           color = guide_legend(override.aes = list(shape = rep(NA,2), size = 2)),
           shape = guide_legend(override.aes = list(size = 4))) +
    labs(color = "Response  \nType", shape = "Response  \nOption") +
    theme_bw() +
    theme(legend.justification = "top",
          legend.title = element_text(face = "bold", size = 11),
          legend.text = element_text(size = 11),
          axis.title = element_blank())
  return(out)
}

## Make response evaluation plots for a single person and single item (no legends)
rspEvalPlot <- function(dat, personNdx = NA, itemNdx = NA, METhr, LEThr) {
  scaleFUN <- function(x) sprintf("%.2f", x)
  out = 
    ggplot(data = dat, aes(x = iteration, y = cumSimilarity)) +
    geom_path(aes(size = rspType, color = rspType, group = rspOpt), show.legend = F) + # lines for cumulative similarity
    geom_vline(xintercept = dat$iteration[dat$finalSlct == "ME.s"], color = "#18BC9C", linetype = 2) + # vertical line for iteration where ME rsp made
    geom_vline(xintercept = dat$iteration[dat$finalSlct == "LE.s"], color = "#E74C3C", linetype = 2) + # vertical line for iteration where LE rsp made
    geom_hline(yintercept = METhr) + # horizontal line for ME threshold
    geom_hline(yintercept = LEThr) + # horizontal line for LE threshold
    geom_point(aes(shape = rspOpt, color = rspType), fill = "transparent", stroke = 1, size = 3, show.legend = F) + # points for cumSim for all rsp opts
    geom_point(data = dat[dat$finalSlct != "Not Chosen",], aes(shape = rspOpt, color = finalSlct, fill = finalSlct), stroke = 1.5, size = 5, show.legend = F) + # points for cumSim of final selected rsp opt
    geom_text(data = dat[dat$finalSlct != "Not Chosen",], aes(label = rspOpt), color = "black", size = 3) + # label for rspOpt selected
    { if (max(abs(dat$cumSimilarity), na.rm = T) < 1) {scale_y_continuous(limits = c(-1,1), labels = scaleFUN)} } +
    { if (max(abs(dat$cumSimilarity), na.rm = T) >= 1) {scale_y_continuous(limits = c((0 - max(abs(dat$cumSimilarity), na.rm = T)), (0 + max(abs(dat$cumSimilarity), na.rm = T))), labels = scaleFUN)} } +
    scale_color_manual(breaks = c("ME", "LE"), values = c("ME" = "#18BC9C", "LE" = "#E74C3C", "Not Chosen" = "gray", "ME.s" = "black", "LE.s" = "black")) +
    scale_fill_manual(values = c("ME.s" = "#18BC9C", "LE.s" = "#E74C3C", "Not Chosen" = "transparent")) +
    scale_shape_manual(values = c("A" = 21, "B" = 22, "C" = 24, "D" = 23)) +
    scale_size_manual(values = c("ME" = 2, "LE" = 2, "Not Chosen" = 1)) +
    theme_bw() +
    { if (!is.na(personNdx)) {labs(title = paste("Item", itemNdx, sep = " "))} } + #, y = "Cumulative Similarity"
    { if (!is.na(personNdx)) {theme(plot.title = element_text(size = 12, hjust = 0.5),
                                    axis.title = element_blank(),
                                    #axis.title.y = element_text(face = "bold", size = 12),
                                    axis.title.x = element_blank())} } +
    
    { if(is.na(personNdx)) {labs(title = paste("Item", itemNdx, sep = " "))} } +
    { if(is.na(personNdx)) {theme(plot.title = element_text(size = 12, hjust = 0.5),
                                  axis.title = element_blank())} }
  return(out)
}

## Combine rspEval plots together with a legend
grid_arrange_shared_legend <-  function(plotLeg, plots, ncol = length(plots), nrow = 1, position = c("bottom", "right")) {
  position <- match.arg(position)
  g <- ggplotGrob(plotLeg)$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) {x + theme(legend.position = "none")})
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(
    position,
    "bottom" = arrangeGrob(
      do.call(arrangeGrob, gl),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight)
    ),
    "right" = arrangeGrob(
      do.call(arrangeGrob, gl),
      legend,
      ncol = 2,
      widths = unit.c(unit(1, "npc") - lwidth, lwidth),
      top = textGrob("Figure 5: Response option evaluation for SJT items", hjust = -.1, x = 0, gp = gpar(fontsize = 13, fontface = c("bold.italic"))), 
      bottom = textGrob("Iterations", gp = gpar(fontsize = 12, fontface = "bold")),
      left = textGrob("Cumulative Similarity", gp = gpar(fontsize = 12, fontface = "bold"), rot = 90, vjust = 1)
    )
  )
  return(invisible(combined))
}