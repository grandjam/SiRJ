library(tidyr)
library(reshape2)
library(ggplot2)

load("sirjSJTEval.RData")
load("sirjRspEval.RData")

# ~~~~~~~~~~~ #
# Plots, etc. #
# ~~~~~~~~~~~ #
# ME & LE response selection distributions
## Transform data for plotting
### ME responses
#### Grouped by itemStem
meRspDistDat = dcast(sjtEval, sjtItem + itemStem ~ meRspSlct, fun.aggregate = length)
meRspRanks = t(apply(meRspDistDat[,3:6], 1, rank))
meRspRankCor = data.frame(x = 4, # Create separate data frame with correlations for plotting
                          y = 5500, 
                          rankCor = sapply(seq(1,nrow(meRspRanks),2), function(x) {
                            cor(meRspRanks[x,], meRspRanks[x+1,], method = "kendall")
                          }),
                          sjtItem = 1:max(meRspDistDat$sjtItem)
                          )
meRspDistDat$agree = rep(meRspRankCor$rankCor > 0, each = 2)
meRspDistDat = melt(meRspDistDat, id.vars = c("sjtItem", "itemStem", "agree"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
meRspDistDat$rspOption <- factor(meRspDistDat$rspOption, levels = 1:4, labels = LETTERS[1:4])

#### Grouped by exptLvl
meRspDistDatExpt = dcast(sjtEval, sjtItem + itemStem + exptLvl ~ meRspSlct, fun.aggregate = length)
meRspDistDatExpt = meRspDistDatExpt[order(meRspDistDatExpt$sjtItem, meRspDistDatExpt$exptLvl),]
meRspRanksExpt = t(apply(meRspDistDatExpt[,4:7], 1, rank))
meRspRankCorExpt = data.frame(x = 4, # Create separate data frame with correlations for plotting
                              y = 2000, 
                              rankCor = sapply(seq(1,nrow(meRspRanksExpt),2), function(x) {
                                cor(meRspRanksExpt[x,], meRspRanksExpt[x+1,], method = "kendall")
                              }),
                              sjtItem = rep(1:max(meRspDistDatExpt$sjtItem), each = max(meRspDistDatExpt$exptLvl)),
                              exptLvl = rep(1:max(meRspDistDatExpt$exptLvl), times = max(meRspDistDatExpt$sjtItem))
                              )
meRspDistDatExpt$agree = rep(meRspRankCorExpt$rankCor > 0, each = 2)
meRspDistDatExpt = melt(meRspDistDatExpt, id.vars = c("sjtItem", "itemStem", "exptLvl", "agree"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
meRspDistDatExpt$rspOption <- factor(meRspDistDatExpt$rspOption, levels = 1:4, labels = LETTERS[1:4])

#### Grouped by expStrength
meRspDistDatExpStr = dcast(sjtEval, sjtItem + itemStem + expStrength ~ meRspSlct, fun.aggregate = length)
meRspDistDatExpStr = meRspDistDatExpStr[order(meRspDistDatExpStr$sjtItem, meRspDistDatExpStr$expStrength),]
meRspRanksExpStr = t(apply(meRspDistDatExpStr[,4:7], 1, rank))
meRspRankCorExpStr = data.frame(x = 4, # Create separate data frame with correlations for plotting
                                y = 2000, 
                                rankCor = sapply(seq(1,nrow(meRspRanksExpStr),2), function(x) {
                                  cor(meRspRanksExpStr[x,], meRspRanksExpStr[x+1,], method = "kendall")
                                }),
                                sjtItem = rep(1:max(meRspDistDatExpStr$sjtItem), each = length(unique(meRspDistDatExpStr$expStrength))),
                                exptLvl = rep(1:length(unique(meRspDistDatExpStr$expStrength)), times = max(meRspDistDatExpStr$sjtItem))
                                )
meRspDistDatExpStr$agree = rep(meRspRankCorExpStr$rankCor > 0, each = 2)
meRspDistDatExpStr = melt(meRspDistDatExpStr, id.vars = c("sjtItem", "itemStem", "expStrength", "agree"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
meRspDistDatExpStr$rspOption <- factor(meRspDistDatExpStr$rspOption, levels = 1:4, labels = LETTERS[1:4])

### LE responses
#### Grouped by itemStem
leRspDistDat = dcast(sjtEval, sjtItem + itemStem ~ leRspSlct, fun.aggregate = length)
leRspRanks = t(apply(leRspDistDat[,3:6], 1, rank))
leRspRankCor = data.frame(x = 4, # Create separate data frame with correlations for plotting
                          y = 5500, 
                          rankCor = sapply(seq(1,nrow(leRspRanks),2), function(x) {
                            cor(leRspRanks[x,], leRspRanks[x+1,], method = "kendall")
                          }),
                          sjtItem = 1:max(leRspDistDat$sjtItem)
)
leRspDistDat$agree = rep(leRspRankCor$rankCor > 0, each = 2)
leRspDistDat = melt(leRspDistDat, id.vars = c("sjtItem", "itemStem", "agree"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
leRspDistDat$rspOption <- factor(leRspDistDat$rspOption, levels = 1:4, labels = LETTERS[1:4])

#### Grouped by exptLvl
leRspDistDatExpt = dcast(sjtEval, sjtItem + itemStem + exptLvl ~ leRspSlct, fun.aggregate = length)
leRspDistDatExpt = leRspDistDatExpt[order(leRspDistDatExpt$sjtItem, leRspDistDatExpt$exptLvl),]
leRspRanksExpt = t(apply(leRspDistDatExpt[,4:7], 1, rank))
leRspRankCorExpt = data.frame(x = 4, # Create separate data frame with correlations for plotting
                              y = 2000, 
                              rankCor = sapply(seq(1,nrow(leRspRanksExpt),2), function(x) {
                                cor(leRspRanksExpt[x,], leRspRanksExpt[x+1,], method = "kendall")
                              }),
                              sjtItem = rep(1:max(leRspDistDatExpt$sjtItem), each = max(leRspDistDatExpt$exptLvl)),
                              exptLvl = rep(1:max(leRspDistDatExpt$exptLvl), times = max(leRspDistDatExpt$sjtItem))
)
leRspDistDatExpt$agree = rep(leRspRankCorExpt$rankCor > 0, each = 2)
leRspDistDatExpt = melt(leRspDistDatExpt, id.vars = c("sjtItem", "itemStem", "exptLvl", "agree"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
leRspDistDatExpt$rspOption <- factor(leRspDistDatExpt$rspOption, levels = 1:4, labels = LETTERS[1:4])

#### Grouped by expStrength
leRspDistDatExpStr = dcast(sjtEval, sjtItem + itemStem + expStrength ~ leRspSlct, fun.aggregate = length)
leRspDistDatExpStr = leRspDistDatExpStr[order(leRspDistDatExpStr$sjtItem, leRspDistDatExpStr$expStrength),]
leRspRanksExpStr = t(apply(leRspDistDatExpStr[,4:7], 1, rank))
leRspRankCorExpStr = data.frame(x = 4, # Create separate data frame with correlations for plotting
                                y = 2000, 
                                rankCor = sapply(seq(1,nrow(leRspRanksExpStr),2), function(x) {
                                  cor(leRspRanksExpStr[x,], leRspRanksExpStr[x+1,], method = "kendall")
                                }),
                                sjtItem = rep(1:max(leRspDistDatExpStr$sjtItem), each = length(unique(leRspDistDatExpStr$expStrength))),
                                exptLvl = rep(1:length(unique(leRspDistDatExpStr$expStrength)), times = max(leRspDistDatExpStr$sjtItem))
)
leRspDistDatExpStr$agree = rep(leRspRankCorExpStr$rankCor > 0, each = 2)
leRspDistDatExpStr = melt(leRspDistDatExpStr, id.vars = c("sjtItem", "itemStem", "expStrength", "agree"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
leRspDistDatExpStr$rspOption <- factor(leRspDistDatExpStr$rspOption, levels = 1:4, labels = LETTERS[1:4])


## ME rsp plots
### Overall (no grouping)
ggplot(data = meRspDistDatExpt, aes(x = rspOption, y = count, size = 1.5)) +
  geom_col() +
  guides(size = F) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 8500)) +
  labs(y = "Number choosing response option",
       x = "Response option") +
  facet_wrap(~ sjtItem, 
             ncol = 10,
             labeller = labeller(.cols = c('1' = "Item 1", '2' = "Item 2", '3' = "Item 3", '4' = "Item 4", '5' = "Item 5",
                                           '6' = "Item 6", '7' = "Item 7", '8' = "Item 8", '9' = "Item 9", '10' = "Item 10",
                                           '11' = "Item 11", '12' = "Item 12", '13' = "Item 13", '14' = "Item 14", '15' = "Item 15",
                                           '16' = "Item 16", '17' = "Item 17", '18' = "Item 18", '19' = "Item 19", '20' = "Item 20"))) +
  theme_bw() +
  theme(legend.justification = "top",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 10))

### By expStrength only
ggplot(data = meRspDistDatExpStr) +
  geom_col(aes(x = rspOption, y = count, size = 1.5)) +
  guides(size = F) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 3900)) +
  labs(y = "Number choosing response option",
       x = "Response option") +
  facet_grid(expStrength ~ sjtItem, labeller = labeller(.cols = c('1' = "Item 1", '2' = "Item 2", '3' = "Item 3", '4' = "Item 4", '5' = "Item 5",
                                                              '6' = "Item 6", '7' = "Item 7", '8' = "Item 8", '9' = "Item 9", '10' = "Item 10",
                                                              '11' = "Item 11", '12' = "Item 12", '13' = "Item 13", '14' = "Item 14", '15' = "Item 15",
                                                              '16' = "Item 16", '17' = "Item 17", '18' = "Item 18", '19' = "Item 19", '20' = "Item 20"),
                                                        .rows = c('5' = "Exp Strength 1",
                                                                  '6' = "Exp Strength 2",
                                                                  '7' = "Exp Strength 3",
                                                                  '8' = "Exp Strength 4"))) +
  theme_bw() +
  theme(legend.justification = "top",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 10))

### By exptLvl only
ggplot(data = meRspDistDatExpt) +
  geom_col(aes(x = rspOption, y = count, size = 1.5)) +
  guides(size = F) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 3900)) +
  labs(y = "Number choosing response option",
       x = "Response option") +
  facet_grid(exptLvl ~ sjtItem, labeller = labeller(.cols = c('1' = "Item 1", '2' = "Item 2", '3' = "Item 3", '4' = "Item 4", '5' = "Item 5",
                                                              '6' = "Item 6", '7' = "Item 7", '8' = "Item 8", '9' = "Item 9", '10' = "Item 10",
                                                              '11' = "Item 11", '12' = "Item 12", '13' = "Item 13", '14' = "Item 14", '15' = "Item 15",
                                                              '16' = "Item 16", '17' = "Item 17", '18' = "Item 18", '19' = "Item 19", '20' = "Item 20"),
                                                    .rows = c('1' = "Expertise Lvl 1",
                                                              '2' = "Expertise Lvl 2",
                                                              '3' = "Expertise Lvl 3",
                                                              '4' = "Expertise Lvl 4",
                                                              '5' = "Expertise Lvl 5"))) +
  theme_bw() +
  theme(legend.justification = "top",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 10))

### By itemStem only
ggplot(data = meRspDistDat) +
  geom_rect(data = meRspDistDat[meRspDistDat$agree == F & meRspDistDat$itemStem == "Absent" & meRspDistDat$rspOption == "A",], aes(fill = agree), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .5, show.legend = F) +
  geom_col(aes(x = rspOption, y = count, fill = itemStem, size = 1.5), position = position_dodge()) +
  geom_text(data = meRspRankCor, aes(x = x, y = y, label = paste("tau == ", round(rankCor,2), sep = ""), hjust = 1), size = 3.5, parse = T) +
  scale_fill_manual(breaks = c("Absent", "Present"), values = c("Absent" = "#E74C3C", "Present" = "#18BC9C", "FALSE" = "#F39C12")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 5800)) +
  guides(size = F) +
  labs(y = "Number choosing response option",
       x = "Response option",
       fill = "SJT Item Stem") +
  facet_wrap(~ sjtItem, 
             ncol = 10,
             labeller = labeller(.cols = c('1' = "Item 1", '2' = "Item 2", '3' = "Item 3", '4' = "Item 4", '5' = "Item 5",
                                           '6' = "Item 6", '7' = "Item 7", '8' = "Item 8", '9' = "Item 9", '10' = "Item 10",
                                           '11' = "Item 11", '12' = "Item 12", '13' = "Item 13", '14' = "Item 14", '15' = "Item 15",
                                           '16' = "Item 16", '17' = "Item 17", '18' = "Item 18", '19' = "Item 19", '20' = "Item 20"))) +
  theme_bw() +
  theme(legend.position = "top",
        legend.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
        legend.background = element_rect(fill = "white", color = "black", size = 0.5, linetype = "solid"),
        legend.justification = "right",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 10))

### By exptLvl and itemStem
ggplot(data = meRspDistDatExpt) +
  geom_rect(data = meRspDistDatExpt[meRspDistDatExpt$agree == F & meRspDistDatExpt$itemStem == "Absent" & meRspDistDatExpt$rspOption == "A",], aes(fill = agree), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .5, show.legend = F) +
  geom_col(aes(x = rspOption, y = count, fill = itemStem, size = 1.5), position = position_dodge()) +
  geom_text(data = meRspRankCorExpt, aes(x = x, y = y, label = paste("tau == ", round(rankCor,2), sep = ""), hjust = 1), size = 3, parse = T) +
  scale_fill_manual(breaks = c("Absent", "Present"), values = c("Absent" = "#E74C3C", "Present" = "#18BC9C", "FALSE" = "#F39C12")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 2250)) +
  guides(size = F) +
  labs(y = "Number choosing response option",
       x = "Response option",
       fill = "SJT Item Stem") +
  facet_grid(exptLvl ~ sjtItem, labeller = labeller(.cols = c('1' = "Item 1", '2' = "Item 2", '3' = "Item 3", '4' = "Item 4", '5' = "Item 5",
                                                              '6' = "Item 6", '7' = "Item 7", '8' = "Item 8", '9' = "Item 9", '10' = "Item 10",
                                                              '11' = "Item 11", '12' = "Item 12", '13' = "Item 13", '14' = "Item 14", '15' = "Item 15",
                                                              '16' = "Item 16", '17' = "Item 17", '18' = "Item 18", '19' = "Item 19", '20' = "Item 20"),
                                                    .rows = c('1' = "Expertise Lvl 1",
                                                              '2' = "Expertise Lvl 2",
                                                              '3' = "Expertise Lvl 3",
                                                              '4' = "Expertise Lvl 4",
                                                              '5' = "Expertise Lvl 5"))) +
  theme_bw() +
  theme(legend.position = "top",
        legend.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
        legend.background = element_rect(fill = "white", color = "black", size = 0.5, linetype = "solid"),
        legend.justification = "right",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 10))

## LE rsp plots
### Overall (no grouping)
ggplot(data = leRspDistDatExpt, aes(x = rspOption, y = count, size = 1.5)) +
  geom_col() +
  guides(size = F) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 11000)) +
  labs(y = "Number choosing response option",
       x = "Response option") +
  facet_wrap(~ sjtItem, 
             ncol = 10,
             labeller = labeller(.cols = c('1' = "Item 1", '2' = "Item 2", '3' = "Item 3", '4' = "Item 4", '5' = "Item 5",
                                           '6' = "Item 6", '7' = "Item 7", '8' = "Item 8", '9' = "Item 9", '10' = "Item 10",
                                           '11' = "Item 11", '12' = "Item 12", '13' = "Item 13", '14' = "Item 14", '15' = "Item 15",
                                           '16' = "Item 16", '17' = "Item 17", '18' = "Item 18", '19' = "Item 19", '20' = "Item 20"))) +
  theme_bw() +
  theme(legend.justification = "top",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 10))

### By expStrength only
ggplot(data = leRspDistDatExpStr) +
  geom_col(aes(x = rspOption, y = count, size = 1.5)) +
  guides(size = F) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 3900)) +
  labs(y = "Number choosing response option",
       x = "Response option") +
  facet_grid(expStrength ~ sjtItem, labeller = labeller(.cols = c('1' = "Item 1", '2' = "Item 2", '3' = "Item 3", '4' = "Item 4", '5' = "Item 5",
                                                                  '6' = "Item 6", '7' = "Item 7", '8' = "Item 8", '9' = "Item 9", '10' = "Item 10",
                                                                  '11' = "Item 11", '12' = "Item 12", '13' = "Item 13", '14' = "Item 14", '15' = "Item 15",
                                                                  '16' = "Item 16", '17' = "Item 17", '18' = "Item 18", '19' = "Item 19", '20' = "Item 20"),
                                                        .rows = c('5' = "Exp Strength 1",
                                                                  '6' = "Exp Strength 2",
                                                                  '7' = "Exp Strength 3",
                                                                  '8' = "Exp Strength 4"))) +
  theme_bw() +
  theme(legend.justification = "top",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 10))

### By exptLvl only
ggplot(data = leRspDistDatExpt) +
  geom_col(aes(x = rspOption, y = count, size = 1.5)) +
  guides(size = F) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 3900)) +
  labs(y = "Number choosing response option",
       x = "Response option") +
  facet_grid(exptLvl ~ sjtItem, labeller = labeller(.cols = c('1' = "Item 1", '2' = "Item 2", '3' = "Item 3", '4' = "Item 4", '5' = "Item 5",
                                                              '6' = "Item 6", '7' = "Item 7", '8' = "Item 8", '9' = "Item 9", '10' = "Item 10",
                                                              '11' = "Item 11", '12' = "Item 12", '13' = "Item 13", '14' = "Item 14", '15' = "Item 15",
                                                              '16' = "Item 16", '17' = "Item 17", '18' = "Item 18", '19' = "Item 19", '20' = "Item 20"),
                                                    .rows = c('1' = "Expertise Lvl 1",
                                                              '2' = "Expertise Lvl 2",
                                                              '3' = "Expertise Lvl 3",
                                                              '4' = "Expertise Lvl 4",
                                                              '5' = "Expertise Lvl 5"))) +
  theme_bw() +
  theme(legend.justification = "top",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 10))

### By itemStem only
ggplot(data = leRspDistDat) +
  geom_rect(data = leRspDistDat[leRspDistDat$agree == F & leRspDistDat$itemStem == "Absent" & leRspDistDat$rspOption == "A",], aes(fill = agree), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .5, show.legend = F) +
  geom_col(aes(x = rspOption, y = count, fill = itemStem, size = 1.5), position = position_dodge()) +
  geom_text(data = leRspRankCor, aes(x = x, y = y, label = paste("tau == ", round(rankCor,2), sep = ""), hjust = 1), size = 3.5, parse = T) +
  scale_fill_manual(breaks = c("Absent", "Present"), values = c("Absent" = "#E74C3C", "Present" = "#18BC9C", "FALSE" = "#F39C12")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 5800)) +
  guides(size = F) +
  labs(y = "Number choosing response option",
       x = "Response option",
       fill = "SJT Item Stem") +
  facet_wrap(~ sjtItem, 
             ncol = 10,
             labeller = labeller(.cols = c('1' = "Item 1", '2' = "Item 2", '3' = "Item 3", '4' = "Item 4", '5' = "Item 5",
                                           '6' = "Item 6", '7' = "Item 7", '8' = "Item 8", '9' = "Item 9", '10' = "Item 10",
                                           '11' = "Item 11", '12' = "Item 12", '13' = "Item 13", '14' = "Item 14", '15' = "Item 15",
                                           '16' = "Item 16", '17' = "Item 17", '18' = "Item 18", '19' = "Item 19", '20' = "Item 20"))) +
  theme_bw() +
  theme(legend.position = "top",
        legend.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
        legend.background = element_rect(fill = "white", color = "black", size = 0.5, linetype = "solid"),
        legend.justification = "center",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 10))

### By exptLvl and itemStem
ggplot(data = leRspDistDatExpt) +
  geom_rect(data = leRspDistDatExpt[leRspDistDatExpt$agree == F & leRspDistDatExpt$itemStem == "Absent" & leRspDistDatExpt$rspOption == "A",], aes(fill = agree), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .5, show.legend = F) +
  geom_col(aes(x = rspOption, y = count, fill = itemStem, size = 1.5), position = position_dodge()) +
  geom_text(data = leRspRankCorExpt, aes(x = x, y = y, label = paste("tau == ", round(rankCor,2), sep = ""), hjust = 1), size = 3, parse = T) +
  scale_fill_manual(breaks = c("Absent", "Present"), values = c("Absent" = "#E74C3C", "Present" = "#18BC9C", "FALSE" = "#F39C12")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 2250)) +
  guides(size = F) +
  labs(y = "Number choosing response option",
       x = "Response option",
       fill = "SJT Item Stem") +
  facet_grid(exptLvl ~ sjtItem, labeller = labeller(.cols = c('1' = "Item 1", '2' = "Item 2", '3' = "Item 3", '4' = "Item 4", '5' = "Item 5",
                                                              '6' = "Item 6", '7' = "Item 7", '8' = "Item 8", '9' = "Item 9", '10' = "Item 10",
                                                              '11' = "Item 11", '12' = "Item 12", '13' = "Item 13", '14' = "Item 14", '15' = "Item 15",
                                                              '16' = "Item 16", '17' = "Item 17", '18' = "Item 18", '19' = "Item 19", '20' = "Item 20"),
                                                    .rows = c('1' = "Expertise Lvl 1",
                                                              '2' = "Expertise Lvl 2",
                                                              '3' = "Expertise Lvl 3",
                                                              '4' = "Expertise Lvl 4",
                                                              '5' = "Expertise Lvl 5"))) +
  theme_bw() +
  theme(legend.position = "top",
        legend.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
        legend.background = element_rect(fill = "white", color = "black", size = 0.5, linetype = "solid"),
        legend.justification = "right",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 10))




# Response time
## Create data frames for plotting survival rates
survDat = data.frame("rspType" = rep(factor(c("ME", "LE"), levels = c("LE", "ME")), each = 11),
                     "iterNum" = rep(0:10, 2),
                     "propChoose" = c(as.numeric(c(0,cumsum(table(sjtEval$meReEval))/400000)),
                                      as.numeric(c(0,0,cumsum(table(sjtEval$leReEval))/400000))
                     ))

survDatItemStem = data.frame("rspType" = rep(factor(c("ME", "LE"), levels = c("LE", "ME")), each = 22),
                             "itemStem" = rep(rep(factor(c("Absent", "Present"), levels = c("Absent", "Present")), each = 11), 2),
                             "iterNum" = rep(0:10, 4),
                             "propChoose" = c(as.numeric(c(0,cumsum(table(sjtEval$meReEval[sjtEval$itemStem == "Absent"]))/sum(table(sjtEval$meReEval[sjtEval$itemStem == "Absent"])))), 
                                              as.numeric(c(0,cumsum(table(sjtEval$meReEval[sjtEval$itemStem == "Present"]))/sum(table(sjtEval$meReEval[sjtEval$itemStem == "Present"])))),
                                              as.numeric(c(0,0,cumsum(table(sjtEval$leReEval[sjtEval$itemStem == "Absent"]))/sum(table(sjtEval$leReEval[sjtEval$itemStem == "Absent"])))), 
                                              as.numeric(c(0,0,cumsum(table(sjtEval$leReEval[sjtEval$itemStem == "Present"]))/sum(table(sjtEval$leReEval[sjtEval$itemStem == "Present"]))))
                             ))

propMEExpt = apply(table(sjtEval$meReEval, sjtEval$exptLvl), 2, function(x) cumsum(x)/80000)
propLEExpt = apply(table(sjtEval$leReEval, sjtEval$exptLvl), 2, function(x) cumsum(x)/80000)
propMEExpt <- rbind(0,propMEExpt)
propLEExpt <- rbind(0,0,propLEExpt)

survDatExpt = data.frame("rspType" = rep(factor(c("ME", "LE"), levels = c("LE", "ME")), each = 55),
                         "exptLvl" = rep(rep(factor(1:5), each = 11), 2),
                         "iterNum" = rep(0:10, 10),
                         "propChoose" = as.numeric(c(propMEExpt, propLEExpt)))

propMEExpStrength = apply(table(sjtEval$meReEval, sjtEval$expStrength), 2, function(x) cumsum(x)/100000)
propLEExpStrength = apply(table(sjtEval$leReEval, sjtEval$expStrength), 2, function(x) cumsum(x)/100000)
propMEExpStrength <- rbind(0,propMEExpStrength)
propLEExpStrength <- rbind(0,0,propLEExpStrength)

survDatExpStrength = data.frame("rspType" = rep(factor(c("ME", "LE"), levels = c("LE", "ME")), each = 44),
                                "expStrength" = rep(rep(factor(1:4), each = 11), 2),
                                "iterNum" = rep(0:10, 8),
                                "propChoose" = as.numeric(c(propMEExpStrength, propLEExpStrength)))

## Survival plots
### Overall
ggplot(data = survDat, aes(x = iterNum, y = 1-propChoose)) +
  geom_line(aes(color = rspType), size = 2) +
  scale_color_manual(values = c("LE" = "#E74C3C", "ME" = "#18BC9C")) +
  scale_y_continuous(breaks = seq(0,1, by = .1), labels = scales::percent) +
  scale_x_continuous(breaks = 0:10) +
  labs(y = "Survival",
       x = "Iteration number",
       color = "Response Type") +
  theme_bw() +
  theme(legend.position = "top",
        legend.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
        legend.background = element_rect(fill = "white", color = "black", size = 0.5, linetype = "solid"),
        legend.justification = "right",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 10))

### By item stem
ggplot(data = survDatItemStem, aes(x = iterNum, y = 1-propChoose)) +
  geom_line(aes(color = rspType, linetype = itemStem), size = 2) +
  scale_color_manual(values = c("LE" = "#E74C3C", "ME" = "#18BC9C")) +
  scale_linetype_manual(values = c("Present" = "solid", "Absent" = "11")) +
  scale_y_continuous(breaks = seq(0,1, by = .1), labels = scales::percent) +
  scale_x_continuous(breaks = 0:10) +
  labs(y = "Survival",
       x = "Iteration number",
       color = "Response Type",
       linetype = "SJT Item Stem") +
  theme_bw() +
  theme(legend.position = "top",
        legend.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
        legend.background = element_rect(fill = "white", color = "black", size = 0.5, linetype = "solid"),
        legend.justification = "right",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 10))

### By exptLvl
ggplot(data = survDatExpt, aes(x = iterNum, y = 1-propChoose)) +
  geom_line(aes(color = exptLvl, linetype = rspType), size = 2) +
  #scale_color_manual(values = c("LE" = "#E74C3C", "ME" = "#18BC9C")) +
  #scale_linetype_manual(values = c("Present" = "solid", "Absent" = "11")) +
  scale_y_continuous(breaks = seq(0,1, by = .1), labels = scales::percent) +
  scale_x_continuous(breaks = 0:10) +
  labs(y = "Survival",
       x = "Iteration number",
       color = "Expertise Lvl",
       linetype = "Response Type") +
  theme_bw() +
  theme(legend.position = "top",
        legend.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
        legend.background = element_rect(fill = "white", color = "black", size = 0.5, linetype = "solid"),
        legend.justification = "right",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 10))

### By expStrength
ggplot(data = survDatExpStrength, aes(x = iterNum, y = 1-propChoose)) +
  geom_line(aes(color = expStrength, linetype = rspType), size = 2) +
  #scale_color_manual(values = c("LE" = "#E74C3C", "ME" = "#18BC9C")) +
  #scale_linetype_manual(values = c("Present" = "solid", "Absent" = "11")) +
  scale_y_continuous(breaks = seq(0,1, by = .1), labels = scales::percent) +
  scale_x_continuous(breaks = 0:10) +
  labs(y = "Survival",
       x = "Iteration number",
       color = "Experience Strength",
       linetype = "Response Type") +
  theme_bw() +
  theme(legend.position = "top",
        legend.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
        legend.background = element_rect(fill = "white", color = "black", size = 0.5, linetype = "solid"),
        legend.justification = "right",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 10))



## Create data frame for mean response times
sePresent = c(sd(sjtEval$meReEval[sjtEval$itemStem == "Present"])/sqrt(length(sjtEval$meReEval[sjtEval$itemStem == "Present"])), sd(sjtEval$leReEval[sjtEval$itemStem == "Present"])/sqrt(length(sjtEval$leReEval[sjtEval$itemStem == "Present"])))
seAbsent = c(sd(sjtEval$meReEval[sjtEval$itemStem == "Absent"])/sqrt(length(sjtEval$meReEval[sjtEval$itemStem == "Absent"])), sd(sjtEval$leReEval[sjtEval$itemStem == "Absent"])/sqrt(length(sjtEval$leReEval[sjtEval$itemStem == "Absent"])))
allDat = data.frame("rspType" = rep(factor(c("ME", "LE"), levels = c("LE", "ME")), each = 2),
                    "itemStem" = rep(factor(c("Absent", "Present"), levels = c("Absent", "Present")), 2),
                    "meanIter" = c(round(mean(sjtEval$meReEval[sjtEval$itemStem == "Absent"]),2),
                                   round(mean(sjtEval$meReEval[sjtEval$itemStem == "Present"]),2), 
                                   round(mean(sjtEval$leReEval[sjtEval$itemStem == "Absent"]),2), 
                                   round(mean(sjtEval$leReEval[sjtEval$itemStem == "Present"]),2)), 
                    "medianIter" = c(median(sjtEval$meReEval[sjtEval$itemStem == "Absent"]),
                                     median(sjtEval$meReEval[sjtEval$itemStem == "Present"]), 
                                     median(sjtEval$leReEval[sjtEval$itemStem == "Absent"]), 
                                     median(sjtEval$leReEval[sjtEval$itemStem == "Present"])), 
                    "sdIter" = c(sd(sjtEval$meReEval[sjtEval$itemStem == "Absent"]),
                                 sd(sjtEval$meReEval[sjtEval$itemStem == "Present"]), 
                                 sd(sjtEval$leReEval[sjtEval$itemStem == "Absent"]), 
                                 sd(sjtEval$leReEval[sjtEval$itemStem == "Present"])), 
                    "CILower95" = c(mean(sjtEval$meReEval[sjtEval$itemStem == "Absent"]) - 2*seAbsent[1], 
                                    mean(sjtEval$meReEval[sjtEval$itemStem == "Present"]) - 2*sePresent[2],
                                    mean(sjtEval$leReEval[sjtEval$itemStem == "Absent"]) - 2*seAbsent[1], 
                                    mean(sjtEval$leReEval[sjtEval$itemStem == "Present"]) - 2*sePresent[2]),
                    "CIUpper95" = c(mean(sjtEval$meReEval[sjtEval$itemStem == "Absent"]) + 2*seAbsent[1], 
                                    mean(sjtEval$meReEval[sjtEval$itemStem == "Present"]) + 2*sePresent[2],
                                    mean(sjtEval$leReEval[sjtEval$itemStem == "Absent"]) + 2*seAbsent[1], 
                                    mean(sjtEval$leReEval[sjtEval$itemStem == "Present"]) + 2*sePresent[2]))

## Mean bar plots
ggplot(data = allDat, aes(x = rspType, y = meanIter)) +
  geom_col(aes(fill = rspType)) +
  geom_hline(aes(color = rspType, yintercept = meanIter), linetype = 2) +
  geom_text(aes(label = meanIter, y = meanIter, color = rspType, group = rspType), nudge_x = .48, hjust = "right", nudge_y = -.1, vjust = 0, size = 4, show.legend = F, fontface = "bold") +
  #geom_errorbar(aes(ymin = CILower95, ymax = CIUpper95), width = .1) +
  facet_grid(. ~ itemStem) +
  coord_flip() +
  labs(x = "Response Type",
       y = "Iteration number") +
  scale_fill_manual(values = c("ME" = "#18BC9C", "LE" = "#E74C3C")) +
  scale_color_manual(values = c("ME" = "#18BC9C", "LE" = "#E74C3C")) +
  scale_y_continuous(limits = c(0, ceiling(max(allDat$CIUpper95))), breaks = seq(0, ceiling(max(allDat$CIUpper95)), 2)) +
  guides(fill = FALSE, color = FALSE) +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        strip.text = element_text(size = 11, face = "bold"))

# ## Response time distribution plots
# timeDat = gather(sjtEval, key = "rspType", value = "evalIter", meReEval, leReEval)
# timeDat$rspType = factor(timeDat$rspType, levels = c("meReEval", "leReEval"), labels = c("ME", "LE"))
# 
# ggplot(data = timeDat, aes(x = evalIter, fill = rspType, color = itemStem)) +
#   geom_histogram(position = position_dodge(), binwidth = 1, bins = 10)


# Ratio between most commonly selected answer in situation-present and situation-absent conditions
## Overall for ME
meOvrCnt = aggregate(meRspDistDat$count, by = list(meRspDistDat$itemStem, meRspDistDat$sjtItem), function(x)
  max(x))
meOvrCntRatio = data.frame("item" = 1:20,
                           "ratio" = meOvrCnt[seq(2,40,2),"x"]/meOvrCnt[seq(1,40,2),"x"])
summary(meOvrCntRatio$ratio)
## Overall for LE
leOvrCnt = aggregate(leRspDistDat$count, by = list(leRspDistDat$itemStem, leRspDistDat$sjtItem), function(x)
  max(x))
leOvrCntRatio = data.frame("item" = 1:20,
                           "ratio" = leOvrCnt[seq(2,40,2),"x"]/leOvrCnt[seq(1,40,2),"x"])
summary(leOvrCntRatio$ratio)
## Take into account expertise level for ME
meExptCnt = aggregate(meRspDistDatExpt$count, by = list(meRspDistDatExpt$itemStem, meRspDistDatExpt$sjtItem, meRspDistDatExpt$exptLvl), function(x)
  max(x))
meExptCntRatio = data.frame("item" = rep(1:20, 5),
                            "exptLvl" = rep(1:5, each = 20),
                            "ratio" = meExptCnt[seq(2,200,2),"x"]/meExptCnt[seq(1,200,2),"x"])
summary(meExptCntRatio$ratio)
## Take into account expertise level for LE
leExptCnt = aggregate(leRspDistDatExpt$count, by = list(leRspDistDatExpt$itemStem, leRspDistDatExpt$sjtItem, leRspDistDatExpt$exptLvl), function(x)
  max(x))
leExptCntRatio = data.frame("item" = rep(1:20, 5),
                            "exptLvl" = rep(1:5, each = 20),
                            "ratio" = leExptCnt[seq(2,200,2),"x"]/leExptCnt[seq(1,200,2),"x"])
summary(leExptCntRatio$ratio)

## Combine ratio data into single data frame and plot using facets
ratioDat <- data.frame("rspType" = factor(c(rep("ME", 120), rep("LE", 120)), levels = c("ME", "LE")),
                       "exptLvl" = as.factor(c(rep(c(rep("No", 20), rep("Yes", 100)), 2))),
                       "ratio" = c(meOvrCntRatio$ratio, meExptCntRatio$ratio, leOvrCntRatio$ratio, leExptCntRatio$ratio))
ratioOvr <- data.frame("rspType" = factor(c("ME","LE"), levels = c("ME", "LE")),
                       "meanRatio" = c(mean(meOvrCntRatio$ratio), mean(leOvrCntRatio$ratio)))
ratioExpt <- data.frame("rspType" = factor(c("ME","LE"), levels = c("ME", "LE")),
                        "meanRatio" = c(mean(meExptCntRatio$ratio), mean(leExptCntRatio$ratio)))
### Overall data
ggplot(data = ratioDat[ratioDat$exptLvl == "No",], aes(x = ratio)) +
  #geom_histogram(aes(fill = rspType), color = "black", size = 1.05, bins = 12) +
  geom_histogram(color = "black", alpha = .65, size = 1.05, bins = 12) +
  geom_vline(data = ratioOvr, aes(xintercept = meanRatio), size = 1.1, linetype = "dashed")  +
  geom_vline(xintercept = 1, size = 1.1, color = "gray") +
  #scale_fill_manual(values = c("LE" = "#E74C3C", "ME" = "#18BC9C")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 6.25)) +
  facet_grid(. ~ rspType, labeller = labeller(.cols = c('LE' = "Least Effective", 
                                                        'ME' = "Most Effective"))) +
  labs(x = "Ratio",
       y = "Count") +
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        strip.text = element_text(size = 11))
### Expertise data
ggplot(data = ratioDat[ratioDat$exptLvl == "Yes",], aes(x = ratio)) +
  geom_histogram(color = "black", alpha = .65, size = 1.05, bins = 25) +
  geom_vline(data = ratioExpt, aes(xintercept = meanRatio), size = 1.1, linetype = "dashed")  +
  geom_vline(xintercept = 1, size = 1.1, color = "gray") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 34)) +
  facet_grid(. ~ rspType, labeller = labeller(.cols = c('LE' = "Least Effective", 
                                                        'ME' = "Most Effective"))) +
  labs(x = "Ratio",
       y = "Count") +
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        strip.text = element_text(size = 11))


# ~~~~~~~~ #
# Random Analyses #
# ~~~~~~~~ #
# Create frequency tables for ME & LE selections by exptLvl and expStrength
meTabDat <- table(sjtEval$meRspSlct, sjtEval$sjtItem)
leTabDat <- table(finalDat$leRspSlct, finalDat$exptLvl, finalDat$expStrength, finalDat$sjtItem)

# Extract "answer key" for each exptLvl (when expStrength = 8) [SHOULD RUN A NEW SIMULATION TO GENERATE THE ANSWER KEY!]
meKey <- t(sapply(1:5, function(x) {apply(meTabDat[,x,4,], 2, which.max)}))
leKey <- t(sapply(1:5, function(x) {apply(leTabDat[,x,4,], 2, which.max)}))
## Most commonly endorsed response for each item (when exptStrength = 8)
meRspSums <- sapply(1:20, function(x) rowSums(meTabDat[,,4,x]))
leRspSums <- sapply(1:20, function(x) rowSums(leTabDat[,,4,x]))

# Score SJTs using answer keys
## 1 pt for picking keyed ME, 1 pt for picking keyed LE
classicScoring <- function(meRsps, leRsps, meKey, leKey, numItems) {
  meCrct <- meRsps == meKey
  leCrct <- leRsps == leKey
  meScore <- colSums(matrix(meCrct, nrow = numItems))
  leScore <- colSums(matrix(leCrct, nrow = numItems))
  out <- meScore + leScore
  return(out)
}

## Compute scoring
scoreKeys <- sapply(1:5, function(x) classicScoring(finalDat$meRspSlct, finalDat$leRspSlct, meKey[x,], leKey[x,], 20))

## SJT performance based on scoring keys
sjtScore <- data.frame(
  agentID = unname(tapply(finalDat$agentID, finalDat$agentID, unique)),
  exptLvl = unname(tapply(finalDat$exptLvl, finalDat$agentID, unique)),
  expStrength = unname(tapply(finalDat$expStrength, finalDat$agentID, unique)),
  scoreKey1 = scoreKeys[,1],
  scoreKey2 = scoreKeys[,2],
  scoreKey3 = scoreKeys[,3],
  scoreKey4 = scoreKeys[,4],
  scoreKey5 = scoreKeys[,5]
)


# Latent profile analysis
library(tidyr)
meTemp <- spread(finalDat[,c(1,2,18,17,3,4)], key = sjtItem, value = meRspSlct)
leTemp <- spread(finalDat[,c(1,2,18,17,3,5)], key = sjtItem, value = leRspSlct)

library(poLCA)
meRsps <- as.matrix(meTemp[,5:24]) ~ 1
leRsps <- as.matrix(leTemp[,5:24]) ~ 1

meRsps <- as.matrix(meTemp[,5:14]) ~ 1
leRsps <- as.matrix(leTemp[,5:14]) ~ 1

lca <- list()
lca$ME.1 <- poLCA(meRsps, meTemp, nclass = 1, nrep = 50, verbose = F)
lca$ME.2 <- poLCA(meRsps, meTemp, nclass = 2, nrep = 50, verbose = F)
lca$ME.3 <- poLCA(meRsps, meTemp, nclass = 3, nrep = 50, verbose = F)
lca$ME.4 <- poLCA(meRsps, meTemp, nclass = 4, nrep = 50, verbose = F)
lca$ME.5 <- poLCA(meRsps, meTemp, nclass = 5, nrep = 50, verbose = F)
lca$ME.6 <- poLCA(meRsps, meTemp, nclass = 6, nrep = 50, verbose = F)
lca$ME.7 <- poLCA(meRsps, meTemp, nclass = 7, nrep = 50, verbose = F)
lca$ME.8 <- poLCA(meRsps, meTemp, nclass = 8, nrep = 50, verbose = F)
lca$ME.9 <- poLCA(meRsps, meTemp, nclass = 9, nrep = 50, verbose = F)
lca$ME.10 <- poLCA(meRsps, meTemp, nclass = 10, nrep = 50, verbose = F)

fitStats <- array(0, dim=c(5,9))
dimnames(fitStats)[[2]] <- c("NumProfiles","LogLikelihood","AIC","BIC","Gsq","Chisq","Entropy","npar","2LLDiff")

# Unable to compute entropy values because there are too many data points...R can't handle it
getFit <- function(x) {
  cls <- length(x$P)
  ll <- x$llik
  AIC <- x$aic
  BIC <- x$bic
  Gsq <- x$Gsq
  Chisq <- x$Chisq
  #entropy <- poLCA.entropy(x)/log(prod(sapply(x$probs,ncol)))
  npar <- x$npar
  #out <- c(cls, ll, AIC, BIC, Gsq, Chisq, entropy, npar)
  out <- c(cls, ll, AIC, BIC, Gsq, Chisq, npar)
  return(out)
}

llikTest <- function(x,y) {
  lr = 2*(x - y)
  return(lr)
}

fitStats[,1:7] <- t(sapply(lca, getFit))

for (i in 1:(nrow(fitStats)-1)) {
  fitStats[i+1,8] <- llikTest(fitStats[i+1,2], fitStats[i,2])
}