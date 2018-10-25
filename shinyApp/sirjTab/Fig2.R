memLvlPlotA <- function(dat, titleText) {
  out = 
    ggplot(data = dat, aes(x = factor(Var1), y = Freq, fill = factor(ID))) +
    geom_col(position = position_dodge()) +
    geom_text(aes(label = Freq, y = Freq + .5, color = factor(ID)), position = position_dodge(.9), vjust = 0, size = 2.75, show.legend = F) +
    scale_fill_manual(values = c("1" = "#18BC9C", "2" = "#F39C12", "3" = "#E74C3C", "4" = "#3498DB", "5" = "#2C3E50")) + 
    scale_color_manual(values = c("1" = "#18BC9C", "2" = "#F39C12", "3" = "#E74C3C", "4" = "#3498DB", "5" = "#2C3E50")) + 
    guides(color = guide_legend(keywidth = 3, keyheight = 1)) +
    labs(title = paste("Fig 2", switch(titleText, "situation" = "a", "situation context" = "b", "response" = "c", "response context" = "d"), ". Distribution of ", titleText, " traces in episodic memory", sep = ""),
         x = "Situation ID",
         y = "Number of traces",
         fill = "Person") +
    theme_bw() +
    theme(plot.title = element_text(size = 13, face = "bold.italic"),
          legend.justification = "top",
          legend.title = element_text(face = "bold", size = 12),
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 10),
          axis.title.x = element_text(face = "bold", size = 12),
          axis.title.y = element_text(face = "bold", size = 12))
  return(out)
}

memLvlPlotB <- function(dat, titleText) {
  out = 
    ggplot(data = dat, aes(x = Var1, y = Freq, fill = factor(ID))) +
    geom_col(position = position_dodge()) +
    geom_text(aes(label = Freq, y = Freq + 1, color = factor(ID)), position = position_dodge(.9), vjust = -.5, size = 4, show.legend = F, fontface = "bold") +
    scale_fill_manual(values = c("1" = "#18BC9C", "2" = "#F39C12", "3" = "#E74C3C", "4" = "#3498DB", "5" = "#2C3E50")) + 
    scale_color_manual(values = c("1" = "#18BC9C", "2" = "#F39C12", "3" = "#E74C3C", "4" = "#3498DB", "5" = "#2C3E50")) + 
    guides(color = guide_legend(keywidth = 3, keyheight = 1)) +
    labs(title = paste("Figure 2", switch(titleText, "situation" = "a", "situation context" = "b", "response" = "c", "response context" = "d"), ". Distribution of ", titleText, " traces in episodic memory across expertise level", sep = ""),
         x = "Expertise level",
         y = "Number of traces",
         fill = "Person") +
    theme_bw() +
    theme(plot.title = element_text(size = 13, face = "bold.italic"),
          legend.justification = "top",
          legend.title = element_text(face = "bold", size = 12),
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 10),
          axis.title.x = element_text(face = "bold", size = 12),
          axis.title.y = element_text(face = "bold", size = 12))
  return(out)
}