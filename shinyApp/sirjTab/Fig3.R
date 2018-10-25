sirjRspDist <- function(data, barDisplay) {
  dat = gather(data, key = "rspType", value = "rspSlct", meRspSlct, leRspSlct)
  dat$rspType = factor(dat$rspType, levels = c("meRspSlct", "leRspSlct"), ordered = T)
  
  titleLabel = switch(barDisplay,
                      "1" = "expertise level",
                      "2" = "experience strength",
                      "3" = "person")

  legendLabel = switch(barDisplay,
                       "1" = "Expertise level",
                       "2" = "Experience\nstrength",
                       "3" = "Person")

  out = 
    ggplot(data = dat, aes(x = rspSlct)) +
    { if(barDisplay == 1) {geom_bar(aes(fill = as.factor(expt), size = 1.5))} }+
    { if(barDisplay == 2) {geom_bar(aes(fill = as.factor(expLvl), size = 1.5))} }+
    { if(barDisplay == 3) {geom_bar(aes(fill = as.factor(person), size = 1.5))} }+
    scale_fill_manual(values = c("1" = "#18BC9C", "2" = "#F39C12", "3" = "#E74C3C", "4" = "#3498DB", "5" = "#2C3E50"), drop = F) +
    guides(size = F) +
    labs(title = paste("Figure 3. Distribution of most and least effective response option selections by", titleLabel, sep = " "),
         y = "Number of people choosing response",
         x = "Response option",
         fill = legendLabel) +
    facet_grid(rspType~sjtItem, labeller = labeller(.cols = c('1' = "Item 1",
                                                              '2' = "Item 2",
                                                              '3' = "Item 3",
                                                              '4' = "Item 4",
                                                              '5' = "Item 5"),
                                                    .rows = c('meRspSlct' = "ME responses",
                                                              'leRspSlct' = "LE responses"))) +
    theme_bw() +
    theme(plot.title = element_text(size = 13, face = "bold.italic"),
          legend.justification = "top",
          legend.title = element_text(face = "bold", size = 12),
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 10),
          axis.title.x = element_text(face = "bold", size = 12),
          axis.title.y = element_text(face = "bold", size = 12),
          strip.text = element_text(size = 12))
  return(out)
}
