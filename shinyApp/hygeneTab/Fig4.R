hygStep2 = 
  ggplot() +
  geom_tile(data = memHyG()$epMemoryPlot, aes(x = colID+1, y = rowID+2.5, alpha = adj, fill = trace), color = "white", height = 1, width = 1) +
  geom_tile(data = prbHyG(), aes(x = colID+1, y = rowID+2.5, alpha = adj), fill ="mediumPurple", color = "white", height = 1, width = 1) +
  geom_tile(data = epMemHyGAct()$actLvlPlot, aes(x = colID, y = rowID+2.5, alpha = value), fill ="red", color = "white", height = 1, width = 1) +
  { if (dim(epMemHyGAct()$trNoActPlot)[1] != 0) {
    geom_tile(data = epMemHyGAct()$trNoActPlot, aes(x = colID+1, y = rowID+2.5, alpha = .8), fill ="white", color = "white", height = 1, width = 1)
  }
  } +
  { if (is.factor(epMemHyGAct()$unspPrbPlot$value)) {
    geom_tile(data = epMemHyGAct()$unspPrbPlot, aes(x = colID+1, y = rowID), fill = "#2C3E50", color = "white", height = 1, width = 1)
  } else {
    geom_tile(data = epMemHyGAct()$unspPrbPlot, aes(x = colID+1, y = rowID, alpha = adj, fill = trace), color = "white", height = 1, width = 1)
  }
  } +
  geom_text(data = memHyG()$epMemoryPlot, aes(x = colID+1, y = rowID+2.5, label = value), color = "white") +
  geom_text(data = prbHyG(), aes(x = colID+1, y = rowID+2.5, label = value), color = "white") +
  geom_text(data = epMemHyGAct()$actLvlPlot, aes(x = colID, y = rowID+2.5, label = value), color = "white") +
  geom_text(data = epMemHyGAct()$unspPrbPlot, aes(x = colID+1, y = rowID, label = value), color = "white") +
  geom_rect(data = memHyG()$epRectNdx,
            aes(xmin = xStrt+1, xmax = xEnd+1, ymin = yStrt+2.5, ymax = yEnd+2.5, group = sits, color = sits),
            fill = "transparent", size = 1.5, inherit.aes = F) +
  scale_alpha(range = c(.4,.9), guide = "none") +
  scale_fill_manual(values = c("darkslategray","steelblue","indianred3","forestgreen")) +
  scale_color_manual(values = c("1" = "#18BC9C", "2" = "#F39C12", "3" = "#E74C3C", "4" = "#3498DB")) +
  scale_x_continuous(position = "top", breaks = 1:((memHyG()$nFeats*length(memHyG()$MVs))+1), labels = c("", paste("F", rep((1:memHyG()$nFeats), length(memHyG()$MVs)), sep = "")), expand = c(0,0)) +
  scale_y_continuous(position = "left", breaks = c(1:2, (seq(2, ((memHyG()$nSits*memHyG()$nMems)+(3)))+.5)), labels = c("UnspPrb", "EchoCnt", "", paste("EpT", rep((memHyG()$nMems:1), memHyG()$nSits), sep = ""), "Probe"), expand = c(0,.15)) +
  coord_fixed() +
  guides(color = guide_legend(keywidth = 3, keyheight = 1, order = 2)) +
  labs(title = "Figure 4: Extracting unspecified probe from episodic memory", color = "Situation", fill = "Information Type") +
  annotate(geom = "text",
           x = .5, #(memHyG()$nFeats*length(memHyG()$MVs))+.6
           y = (memHyG()$nSits*memHyG()$nMems)+3.25,
           label = "ActLvl",
           hjust = "left",
           size = 3) +
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 13, face = "bold.italic"),
        legend.justification = "top",
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12),
        axis.line = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(face = "bold", angle = 45),
        axis.text.y = element_text(face = "bold"),
        axis.ticks = element_blank(),
        axis.title = element_blank())