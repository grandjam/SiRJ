exMemTracePlot = 
  ggplot(data = memTrace$epMemoryPlot[which(memTrace$epMemoryPlot$sitID == 1),], aes(colID, rowID)) +
  geom_tile(aes(alpha = adj, fill = trace), color = "white", height = 1, width = 1) +
  geom_text(aes(label = value), color = "white") +
  coord_fixed() +
  scale_alpha(range = c(.4,.9), guide = "none") +
  scale_fill_manual(values = c("darkslategray","steelblue","indianred3","forestgreen")) +
  scale_x_continuous(position = "top", breaks = 1:(memTrace$nFeats*length(memTrace$MVs)), labels = c(paste("F", rep((1:memTrace$nFeats), length(memTrace$MVs)), sep = "")), expand = c(0,0)) +
  scale_y_continuous(position = "left", breaks = NULL, labels = c(paste("T", 1, sep = "")), expand = c(0,.15)) +
  labs(title = "Figure 1. Example memory trace in SiRJ", fill = "Information Type") +
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 13, face = "bold.italic"),
        legend.justification = "left",
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12),
        axis.line = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(face = "bold", angle = 45),
        axis.text.y = element_text(face = "bold"),
        axis.ticks = element_blank(),
        axis.title = element_blank())