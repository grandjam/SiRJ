hygStep1 = 
  ggplot() +
  geom_tile(data = memHyG()$epMemoryPlot[(which(memHyG()$epMemoryPlot$trace == "Situation")),], aes(x = colID+1, y = rowID, alpha = adj, fill = trace), color = "white", height = 1, width = 1) +
  geom_tile(data = prbHyG(), aes(x = colID+1, y = rowID, alpha = adj), fill ="mediumPurple", color = "white", height = 1, width = 1) +
  geom_tile(data = epMemHyGAct()$actLvlPlot, aes(x = colID, y = rowID, alpha = value), fill ="red", color = "white", height = 1, width = 1) +
  geom_text(data = memHyG()$epMemoryPlot[(which(memHyG()$epMemoryPlot$trace == "Situation")),], aes(x = colID+1, y = rowID, label = value), color = "white") +
  geom_text(data = prbHyG(), aes(x = colID+1, y = rowID, label = value), color = "white") +
  geom_text(data = epMemHyGAct()$actLvlPlot, aes(x = colID, y = rowID, label = value), color = "white") +
  geom_rect(data = memHyG()$epRectNdx,
            aes(xmin = xStrt+1, xmax = xEnd-(memHyG()$nFeats)+1, ymin = yStrt, ymax = yEnd, group = sits, color = sits),
            fill = "transparent", size = 1.5, inherit.aes = F) +
  coord_fixed() +
  scale_alpha(range = c(.4,.9), guide = "none") +
  scale_fill_manual(values = c("darkslategray","steelblue","indianred3","forestgreen")) +
  scale_color_manual(values = c("1" = "#18BC9C", "2" = "#F39C12", "3" = "#E74C3C", "4" = "#3498DB")) +
  scale_x_continuous(position = "top", breaks = 1:((memHyG()$nFeats*length(which(memHyG()$MVs == "Situation")))+1), labels = c("", paste("F", rep((1:memHyG()$nFeats), length(which(memHyG()$MVs == "Situation"))), sep = "")), expand = c(0,0)) +
  scale_y_continuous(position = "left", breaks = 1:((memHyG()$nSits*memHyG()$nMems)+1), labels = c(paste("EpT", rep((memHyG()$nMems:1), memHyG()$nSits), sep = ""), "Probe"), expand = c(0,.15)) +
  guides(color = guide_legend(keywidth = 3, keyheight = 1, order = 2)) +
  labs(title = "Figure 3: Activating episodic memory traces", color = "Situation", fill = "Information Type") +
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