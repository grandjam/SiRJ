hygStep5 =
  ggplot() +
  geom_tile(data = memHyG()$epMemoryPlot[memHyG()$epMemoryPlot$trace == "Situation Context",], aes(x = (colID-memHyG()$nFeats)+1, y = rowID, alpha = adj, fill = trace), color = "white", height = 1, width = 1) + # Episodic memory trace tiles
  geom_tile(data = na.omit(SOC$socPlot[SOC$socPlot$trace == "Situation Context" & SOC$socPlot$sitID == na.omit(SOC$soc[my_i,1]),]), aes(x = (colID-memHyG()$nFeats)+1, y = (memHyG()$nSits*memHyG()$nMems)+1.5, alpha = adj, fill = trace), color = "white", height = 1, width = 1) + # SOC trace tiles
  geom_tile(data = SOCechoInt()$echoIntPlot[[my_i]], aes(x = colID, y = rowID, alpha = value), fill ="red", color = "white", height = 1, width = 1) + # echo intensity activation level tiles
  { if (dim(SOCechoInt()$trNoActPlot)[1] != 0) {
    geom_tile(data = SOCechoInt()$trNoActPlot, aes(x = colID, y = rowID, alpha = .8), fill ="white", color = "white", height = 1, width = 1) # Gray out unactivated traces
  }
  } +
  geom_text(data = memHyG()$epMemoryPlot[memHyG()$epMemoryPlot$trace == "Situation Context",], aes(x = (colID-memHyG()$nFeats)+1, y = rowID, label = value), color = "white") + # Episodic memory trace text
  geom_text(data = na.omit(SOC$socPlot[SOC$socPlot$trace == "Situation Context" & SOC$socPlot$sitID == na.omit(SOC$soc[my_i,1]),]), aes(x = (colID-memHyG()$nFeats)+1, y = (memHyG()$nSits*memHyG()$nMems)+1.5, label = value), color = "white") + #SOC trace text
  geom_text(data = SOCechoInt()$echoIntPlot[[my_i]], aes(x = colID, y = rowID, label = value), color = "white") + # echo intensity activation level text
  geom_rect(data = SOCechoInt()$socTrRectNdx[[my_i]],
            aes(xmin = xStrt+1, xmax = xEnd+1, ymin = yStrt+((memHyG()$nSits*memHyG()$nMems)+.5), ymax = yEnd+((memHyG()$nSits*memHyG()$nMems)+.5), group = sits, color = sits),
            fill = "transparent", size = 1.5, inherit.aes = F) +
  geom_rect(data = memHyG()$epRectNdx,
            aes(xmin = xStrt+1, xmax = xEnd+1-memHyG()$nFeats, ymin = yStrt, ymax = yEnd, group = sits, color = sits),
            fill = "transparent", size = 1.5, inherit.aes = F) +
  scale_alpha(range = c(.4,.9), guide = "none") +
  scale_fill_manual(values = c("darkslategray","steelblue","indianred3","forestgreen")[2]) +
  scale_color_manual(values = c("1" = "#18BC9C", "2" = "#F39C12", "3" = "#E74C3C", "4" = "#3498DB")) +
  scale_x_continuous(position = "top", breaks = 1:(memHyG()$nFeats+1), labels = c("", paste("F", 1:memHyG()$nFeats, sep = "")), expand = c(0,0)) +
  scale_y_continuous(position = "left", breaks = c(1:(memHyG()$nSits*memHyG()$nMems), (memHyG()$nSits*memHyG()$nMems)+1.5), labels = c(paste("EpT", rep((memHyG()$nMems:1), memHyG()$nSits), sep = ""), paste("SOC", my_i, sep = "")), expand = c(0,.15)) +
  coord_fixed() +
  guides(color = guide_legend(keywidth = 3, keyheight = 1, order = 2)) +
  labs(title = "Figure 7: Computing conditional echo intensity", color = "Situation", fill = "Information Type") +
  annotate(geom = "text",
           x = .5, #(memHyG()$nFeats*length(memHyG()$MVs))+.6
           y = (memHyG()$nSits*memHyG()$nMems)+.75,
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