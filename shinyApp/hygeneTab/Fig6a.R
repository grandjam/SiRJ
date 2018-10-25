hygStep4 =
  ggplot() +
  geom_tile(data = memHyG()$semMemoryPlot, aes(x = colID+2, y = rowID+nrow(SOC$soc)+.5, alpha = adj, fill = trace), color = "white", height = 1, width = 1) + # semantic memory trace tiles if blank SOC
  geom_tile(data = semMemHyGAct()$actLvlPlot, aes(x = colID, y = rowID+nrow(SOC$soc)+.5, alpha = value), fill ="red", color = "white", height = 1, width = 1) + # raw activation level tiles if blank SOC
  geom_tile(data = semMemHyGAct()$actLvlPlot, aes(x = colID+1, y = rowID+nrow(SOC$soc)+.5, alpha = value), fill ="orange", color = "white", height = 1, width = 1) + # normalized activation level tiles if blank SOC
  geom_tile(data = SOC$socPlot, aes(x = colID+2, y = rowID), fill = "#2C3E50", color = "white", height = 1, width = 1) + # blank SOC tiles
  geom_text(data = memHyG()$semMemoryPlot, aes(x = colID+2, y = rowID+nrow(SOC$soc)+.5, label = value), color = "white") + # semantic memory text values if updated SOC
  geom_text(data = semMemHyGAct()$actLvlPlot, aes(x = colID, y = rowID+nrow(SOC$soc)+.5, label = value), color = "white") + # raw activation level text values
  geom_text(data = semMemHyGAct()$actLvlPlot, aes(x = colID+1, y = rowID+nrow(SOC$soc)+.5, label = prob), color = "white") + # normalized activation level text values
  geom_text(data = na.omit(SOC$socPlot), aes(x = colID+2, y = rowID, label = value), color = "white") + # SOC text values
  geom_rect(data = memHyG()$semRectNdx, # semantic memory siuation rectangles if blank SOC
            aes(xmin = xStrt+2, xmax = xEnd+2, ymin = yStrt+nrow(SOC$soc)+.5, ymax = yEnd+nrow(SOC$soc)+.5, group = sits, color = sits),
            fill = "transparent", size = 1.5, inherit.aes = F) +
  coord_fixed() +
  scale_alpha(range = c(.4,.9), guide = "none") +
  scale_fill_manual(values = c("Situation" = "darkslategray", "Situation Context" = "steelblue", "Response" = "indianred3", "Response Context" = "forestgreen")) + # "NA" = "#2C3E50"
  scale_color_manual(values = c("1" = "#18BC9C", "2" = "#F39C12", "3" = "#E74C3C", "4" = "#3498DB")) +
  scale_x_continuous(position = "top", breaks = 1:((memHyG()$nFeats*length(memHyG()$MVs))+2), labels = c("", "", paste("F", rep((1:memHyG()$nFeats), length(memHyG()$MVs)), sep = "")), expand = c(0,0)) +
  scale_y_continuous(position = "left", breaks = c(1:nrow(SOC$soc), (seq(nrow(SOC$soc), memHyG()$nSits+nrow(SOC$soc)))+.5), labels = c(paste("SOC", 1:nrow(SOC$soc), sep = ""), "", paste("SmT", rep(1, memHyG()$nSits), sep = "")), expand = c(0,.15)) +
  guides(color = guide_legend(keywidth = 3, keyheight = 1, order = 2)) +
  labs(title = "Figure 6: Populating the Set of Leading Contenders (SOC)", color = "Situation", fill = "Information Type") +
  annotate(geom = "text",
           x = .7, #(memHyG()$nFeats*length(memHyG()$MVs))+.6
           y = memHyG()$nSits + nrow(SOC$soc) + 1.2,
           label = "ActLvl",
           hjust = "left",
           size = 3) +
  annotate(geom = "text",
           x = 1.7, #(memHyG()$nFeats*length(memHyG()$MVs))+.6
           y = memHyG()$nSits + nrow(SOC$soc) + 1.2,
           label = "NormAct",
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