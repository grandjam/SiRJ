hygStep3 = 
  ggplot() +
  geom_tile(data = memHyG()$semMemoryPlot, aes(x = colID+2, y = rowID, alpha = adj, fill = trace), color = "white", height = 1, width = 1) +
  { if (is.factor(epMemHyGAct()$unspPrbPlot$value)) {
    geom_tile(data = epMemHyGAct()$unspPrbPlot[which(epMemHyGAct()$unspPrbPlot$rowID == 1),], aes(x = colID+2, y = rowID+memHyG()$nSits+.5), fill = "#2C3E50", color = "white", height = 1, width = 1) # blank/no extracted unspecified probe
  } else {
    geom_tile(data = epMemHyGAct()$unspPrbPlot[which(epMemHyGAct()$unspPrbPlot$rowID == 1),], aes(x = colID+2, y = rowID+memHyG()$nSits+.5, alpha = adj, fill = trace), color = "white", height = 1, width = 1) # unspecified probe
  }
  } +
  geom_tile(data = semMemHyGAct()$actLvlPlot, aes(x = colID, y = rowID, alpha = value), fill ="red", color = "white", height = 1, width = 1) + # raw activation level
  geom_tile(data = semMemHyGAct()$actLvlPlot, aes(x = colID+1, y = rowID, alpha = value), fill ="orange", color = "white", height = 1, width = 1) + # normalized activation level
  geom_text(data = memHyG()$semMemoryPlot, aes(x = colID+2, y = rowID, label = value), color = "white") +
  geom_text(data = epMemHyGAct()$unspPrbPlot[which(epMemHyGAct()$unspPrbPlot$rowID == 1),], aes(x = colID+2, y = rowID+memHyG()$nSits+.5, label = value), color = "white") +
  geom_text(data = semMemHyGAct()$actLvlPlot, aes(x = colID, y = rowID, label = value), color = "white") + # raw activation level
  geom_text(data = semMemHyGAct()$actLvlPlot, aes(x = colID+1, y = rowID, label = prob), color = "white") + # normalized activation level
  geom_rect(data = memHyG()$semRectNdx,
            aes(xmin = xStrt+2, xmax = xEnd+2, ymin = yStrt, ymax = yEnd, group = sits, color = sits),
            fill = "transparent", size = 1.5, inherit.aes = F) +
  coord_fixed() +
  scale_alpha(range = c(.4,.9), guide = "none") +
  scale_fill_manual(values = c("darkslategray","steelblue","indianred3","forestgreen")) +
  scale_color_manual(values = c("1" = "#18BC9C", "2" = "#F39C12", "3" = "#E74C3C", "4" = "#3498DB")) +
  scale_x_continuous(position = "top", breaks = 1:((memHyG()$nFeats*length(memHyG()$MVs))+2), labels = c("", "", paste("F", rep((1:memHyG()$nFeats), length(memHyG()$MVs)), sep = "")), expand = c(0,0)) +
  scale_y_continuous(position = "left", breaks = c(1:memHyG()$nSits, (seq(memHyG()$nSits, memHyG()$nSits+1))+.5), labels = c(paste("SmT", rep(1, memHyG()$nSits), sep = ""), "", "UnspPrb"), expand = c(0,.15)) +
  guides(color = guide_legend(keywidth = 3, keyheight = 1, order = 2)) +
  labs(title = "Figure 5: Activating semantic memory traces", color = "Situation", fill = "Information Type") +
  annotate(geom = "text",
           x = .5, #(memHyG()$nFeats*length(memHyG()$MVs))+.6
           y = (memHyG()$nSits)+.7,
           label = "ActLvl",
           hjust = "left",
           size = 3) +
  annotate(geom = "text",
           x = 1.5, #(memHyG()$nFeats*length(memHyG()$MVs))+.6
           y = (memHyG()$nSits)+.7,
           label = "NormAct",
           hjust = "left",
           size = 3) +
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 13, face = "bold.italic"),
        plot.margin = unit(c(0,0,0,0), "npc"),
        legend.justification = "top",
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12),
        axis.line = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(face = "bold", angle = 45),
        axis.text.y = element_text(face = "bold"),
        axis.ticks = element_blank(),
        axis.title = element_blank())