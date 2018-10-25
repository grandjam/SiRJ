epMemPlot = 
  ggplot() +
  geom_tile(data = mem()$epMemoryPlot, aes(x = colID, y = rowID+mem()$nSits+.5, alpha = adj, fill = trace), color = "white", height = 1, width = 1) +
  geom_tile(data = mem()$semMemoryPlot, aes(x = colID, y = rowID, alpha = adj, fill = trace), color = "white", height = 1, width = 1) +
  geom_text(data = mem()$epMemoryPlot, aes(x = colID, y = rowID+mem()$nSits+.5, label = value), color = "white") +
  geom_text(data = mem()$semMemoryPlot, aes(x = colID, y = rowID, label = value), color = "white") +
  geom_rect(data = mem()$epRectNdx,
            aes(xmin = xStrt, xmax = xEnd, ymin = yStrt+mem()$nSits+.5, ymax = yEnd+mem()$nSits+.5, group = sits, color = sits),
            fill = "transparent", size = 1.5, inherit.aes = F) +
  geom_rect(data = mem()$semRectNdx,
            aes(xmin = xStrt, xmax = xEnd, ymin = yStrt, ymax = yEnd, group = sits, color = sits),
            fill = "transparent", size = 1.5, inherit.aes = F) +
  coord_fixed() +
  scale_alpha(range = c(.4,.9), guide = "none") +
  scale_fill_manual(values = c("darkslategray","steelblue","indianred3","forestgreen")) +
  scale_color_manual(values = c("1" = "#18BC9C", "2" = "#F39C12", "3" = "#E74C3C", "4" = "#3498DB")) +
  scale_x_continuous(position = "top", breaks = 1:(input$nFeats*length(mem()$MVs)), labels = c(paste("F", rep((1:input$nFeats), length(mem()$MVs)), sep = "")), expand = c(0,0)) +
  scale_y_continuous(position = "left", breaks = c(1:input$nSits, (seq(input$nSits, ((input$nSits*input$nMems)+(input$nSits)))+.5)), labels = c(paste("SmT", rep(1, input$nSits), sep = ""), "", paste("EpT", rep((input$nMems:1), input$nSits), sep = "")), expand = c(0,.15)) +
  guides(color = guide_legend(keywidth = 3, keyheight = 1, order = 2)) +
  labs(title = "Figure 2. Memory systems in SiRJ", color = "Situation", fill = "Information Type") +
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