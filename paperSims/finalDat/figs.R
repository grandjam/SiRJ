library(tidyr)
library(reshape2)
library(ggplot2)
library(scales)

load("sirjSJTEval.RData")
load("sirjRspEval.RData")

#############################################
# RQ1-RQ3: RESPONSE SELECTION DISTRIBUTIONS #
#############################################
## ~~~ Transform data for plotting ~~~ ##
### ME responses
#### Grouped by expStrength
meRspDistDatExpStr = dcast(sjtEval, sjtItem + expStrength ~ meRspSlct, fun.aggregate = length)
meRspDistDatExpStr = meRspDistDatExpStr[order(meRspDistDatExpStr$sjtItem, meRspDistDatExpStr$expStrength),]
meRspDistDatExpStr = melt(meRspDistDatExpStr, id.vars = c("sjtItem", "expStrength"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
meRspDistDatExpStr$prop = meRspDistDatExpStr$count/10000
meRspDistDatExpStr$rspOption <- factor(meRspDistDatExpStr$rspOption, levels = 1:4, labels = LETTERS[1:4])

#### Grouped by exptLvl
meRspDistDatExpt = dcast(sjtEval, sjtItem + exptLvl ~ meRspSlct, fun.aggregate = length)
meRspDistDatExpt = meRspDistDatExpt[order(meRspDistDatExpt$sjtItem, meRspDistDatExpt$exptLvl),]
meRspDistDatExpt = melt(meRspDistDatExpt, id.vars = c("sjtItem", "exptLvl"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
meRspDistDatExpt$prop = meRspDistDatExpt$count/8000
meRspDistDatExpt$rspOption <- factor(meRspDistDatExpt$rspOption, levels = 1:4, labels = LETTERS[1:4])

#### Grouped by compare
meRspDistDatCompare = dcast(sjtEval, sjtItem + compare ~ meRspSlct, fun.aggregate = length)
meRspRanksCompare = t(apply(meRspDistDatCompare[,3:6], 1, rank))
meRspRankCorCompare = data.frame(x = 4, # Create separate data frame with correlations for plotting
                                 y = 1,
                                 rankCor = sapply(seq(1,nrow(meRspRanksCompare),2), function(x) { # Correlation between rsp-rspCxt
                                   cor(meRspRanksCompare[x,], meRspRanksCompare[x+1,], method = "kendall")
                                 }),
                                 maxAgree = sapply(seq(1,nrow(meRspRanksCompare),2), function(x) { # Is most selected response the same in rsp & rspCxt
                                   which.max(meRspRanksCompare[x,]) == which.max(meRspRanksCompare[x+1,])
                                 }),
                                 sjtItem = 1:max(meRspDistDatCompare$sjtItem)
)
meRspDistDatCompare$rankCorAgree = rep(meRspRankCorCompare$rankCor > 0, each = 2)
meRspDistDatCompare$maxAgree = rep(meRspRankCorCompare$maxAgree, each = 2)
meRspDistDatCompare = melt(meRspDistDatCompare, id.vars = c("sjtItem", "compare", "rankCorAgree", "maxAgree"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
meRspDistDatCompare$prop = meRspDistDatCompare$count/20000
meRspDistDatCompare$rspOption <- factor(meRspDistDatCompare$rspOption, levels = 1:4, labels = LETTERS[1:4])

#### Grouped by itemStem
meRspDistDatItemStem = dcast(sjtEval, sjtItem + itemStem ~ meRspSlct, fun.aggregate = length)
meRspRanksItemStem = t(apply(meRspDistDatItemStem[,3:6], 1, rank))
meRspRankCorItemStem = data.frame(x = 4, # Create separate data frame with correlations for plotting
                                  y = 1,
                                  rankCor = sapply(seq(1,nrow(meRspRanksItemStem),2), function(x) { # Correlation between absent-present
                                    cor(meRspRanksItemStem[x,], meRspRanksItemStem[x+1,], method = "kendall")
                                  }),
                                  maxAgree = sapply(seq(1,nrow(meRspRanksItemStem),2), function(x) { # Is most selected response the same in absent & present
                                    which.max(meRspRanksItemStem[x,]) == which.max(meRspRanksItemStem[x+1,])
                                  }),
                                  sjtItem = 1:max(meRspDistDatItemStem$sjtItem)
)
meRspDistDatItemStem$rankCorAgree = rep(meRspRankCorItemStem$rankCor > 0, each = 2)
meRspDistDatItemStem$maxAgree = rep(meRspRankCorItemStem$maxAgree, each = 2)
meRspDistDatItemStem = melt(meRspDistDatItemStem, id.vars = c("sjtItem", "itemStem", "rankCorAgree", "maxAgree"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
meRspDistDatItemStem$prop = meRspDistDatItemStem$count/20000
meRspDistDatItemStem$rspOption <- factor(meRspDistDatItemStem$rspOption, levels = 1:4, labels = LETTERS[1:4])

#### Grouped by exptLvl & itemStem
meRspDistDatExptLvlItemStem = dcast(sjtEval, sjtItem + exptLvl + itemStem ~ meRspSlct, fun.aggregate = length)
meRspDistDatExptLvlItemStem = meRspDistDatExptLvlItemStem[order(meRspDistDatExptLvlItemStem$exptLvl, meRspDistDatExptLvlItemStem$sjtItem),]
meRspRanksExptLvlItemStem = t(apply(meRspDistDatExptLvlItemStem[,4:7], 1, rank))
meRspRankCorExptLvlItemStem = data.frame(x = 4, # Create separate data frame with correlations for plotting
                                         y = 1,
                                         rankCor = sapply(seq(1,nrow(meRspRanksExptLvlItemStem),2), function(x) { # Correlation between absent-present within each expertise level
                                           cor(meRspRanksExptLvlItemStem[x,], meRspRanksExptLvlItemStem[x+1,], method = "kendall")
                                         }),
                                         maxAgree = sapply(seq(1,nrow(meRspRanksExptLvlItemStem),2), function(x) { # Is most selected response the same in absent & present
                                           which.max(meRspRanksExptLvlItemStem[x,]) == which.max(meRspRanksExptLvlItemStem[x+1,])
                                         }),
                                         sjtItem = 1:max(meRspDistDatExptLvlItemStem$sjtItem),
                                         exptLvl = rep(1:max(meRspDistDatExptLvlItemStem$exptLvl), each = max(meRspDistDatExptLvlItemStem$sjtItem))
                                         
)
meRspDistDatExptLvlItemStem$rankCorAgree = rep(meRspRankCorExptLvlItemStem$rankCor > 0, each = 2)
meRspDistDatExptLvlItemStem$maxAgree = rep(meRspRankCorExptLvlItemStem$maxAgree, each = 2)
meRspDistDatExptLvlItemStem = melt(meRspDistDatExptLvlItemStem, id.vars = c("sjtItem", "exptLvl", "itemStem", "rankCorAgree", "maxAgree"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
meRspDistDatExptLvlItemStem$prop = meRspDistDatExptLvlItemStem$count/4000
meRspDistDatExptLvlItemStem$rspOption <- factor(meRspDistDatExptLvlItemStem$rspOption, levels = 1:4, labels = LETTERS[1:4])

#### Grouped by exptLvl & compare
meRspDistDatExptLvlCompare = dcast(sjtEval, sjtItem + exptLvl + compare ~ meRspSlct, fun.aggregate = length)
meRspDistDatExptLvlCompare = meRspDistDatExptLvlCompare[order(meRspDistDatExptLvlCompare$exptLvl, meRspDistDatExptLvlCompare$sjtItem),]
meRspRanksExptLvlCompare = t(apply(meRspDistDatExptLvlCompare[,4:7], 1, rank))
meRspRankCorExptLvlCompare = data.frame(x = 4, # Create separate data frame with correlations for plotting
                                         y = 1,
                                         rankCor = sapply(seq(1,nrow(meRspRanksExptLvlCompare),2), function(x) { # Correlation between absent-present within each expertise level
                                           cor(meRspRanksExptLvlCompare[x,], meRspRanksExptLvlCompare[x+1,], method = "kendall")
                                         }),
                                         maxAgree = sapply(seq(1,nrow(meRspRanksExptLvlCompare),2), function(x) { # Is most selected response the same in absent & present
                                           which.max(meRspRanksExptLvlCompare[x,]) == which.max(meRspRanksExptLvlCompare[x+1,])
                                         }),
                                         sjtItem = 1:max(meRspDistDatExptLvlCompare$sjtItem),
                                         exptLvl = rep(1:max(meRspDistDatExptLvlCompare$exptLvl), each = max(meRspDistDatExptLvlCompare$sjtItem))
                                         
)
meRspDistDatExptLvlCompare$rankCorAgree = rep(meRspRankCorExptLvlCompare$rankCor > 0, each = 2)
meRspDistDatExptLvlCompare$maxAgree = rep(meRspRankCorExptLvlCompare$maxAgree, each = 2)
meRspDistDatExptLvlCompare = melt(meRspDistDatExptLvlCompare, id.vars = c("sjtItem", "exptLvl", "compare", "rankCorAgree", "maxAgree"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
meRspDistDatExptLvlCompare$prop = meRspDistDatExptLvlCompare$count/4000
meRspDistDatExptLvlCompare$rspOption <- factor(meRspDistDatExptLvlCompare$rspOption, levels = 1:4, labels = LETTERS[1:4])

#### Grouped by itemStem & compare
meRspDistDatCompareItemStem = dcast(sjtEval, sjtItem + compare + itemStem ~ meRspSlct, fun.aggregate = length)
meRspRanksCompareItemStem = t(apply(meRspDistDatCompareItemStem[,4:7], 1, rank))
meRspRankCorCompareItemStem = data.frame(x = 4, # Create separate data frame with correlations for plotting
                                         y = 1,
                                         rankCor = sapply(seq(1,nrow(meRspRanksCompareItemStem),2), function(x) { # Correlation between absent-present within each compare
                                           cor(meRspRanksCompareItemStem[x,], meRspRanksCompareItemStem[x+1,], method = "kendall")
                                         }),
                                         maxAgree = sapply(seq(1,nrow(meRspRanksCompareItemStem),2), function(x) { # Is most selected response the same in absent & present
                                           which.max(meRspRanksCompareItemStem[x,]) == which.max(meRspRanksCompareItemStem[x+1,])
                                         }),
                                         sjtItem = 1:max(meRspDistDatCompareItemStem$sjtItem),
                                         compare = rep(levels(meRspDistDatCompareItemStem$compare), each = max(meRspDistDatCompareItemStem$sjtItem))
)
meRspDistDatCompareItemStem$rankCorAgree = rep(meRspRankCorCompareItemStem$rankCor > 0, each = 2)
meRspDistDatCompareItemStem$maxAgree = rep(meRspRankCorCompareItemStem$maxAgree, each = 2)
meRspDistDatCompareItemStem = melt(meRspDistDatCompareItemStem, id.vars = c("sjtItem", "compare", "itemStem", "rankCorAgree", "maxAgree"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
meRspDistDatCompareItemStem$prop = meRspDistDatCompareItemStem$count/10000
meRspDistDatCompareItemStem$rspOption <- factor(meRspDistDatCompareItemStem$rspOption, levels = 1:4, labels = LETTERS[1:4])

### LE responses
#### Grouped by expStrength
leRspDistDatExpStr = dcast(sjtEval, sjtItem + itemStem + compare + expStrength ~ leRspSlct, fun.aggregate = length)
leRspDistDatExpStr = leRspDistDatExpStr[order(leRspDistDatExpStr$sjtItem, leRspDistDatExpStr$expStrength),]
leRspDistDatExpStr = melt(leRspDistDatExpStr, id.vars = c("sjtItem", "expStrength"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
leRspDistDatExpStr$prop = leRspDistDatExpStr$count/10000
leRspDistDatExpStr$rspOption <- factor(leRspDistDatExpStr$rspOption, levels = 1:4, labels = LETTERS[1:4])

#### Grouped by exptLvl
leRspDistDatExpt = dcast(sjtEval, sjtItem + itemStem + compare + exptLvl ~ leRspSlct, fun.aggregate = length)
leRspDistDatExpt = leRspDistDatExpt[order(leRspDistDatExpt$sjtItem, leRspDistDatExpt$exptLvl),]
leRspDistDatExpt = melt(leRspDistDatExpt, id.vars = c("sjtItem", "exptLvl"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
leRspDistDatExpt$prop = leRspDistDatExpt$count/8000
leRspDistDatExpt$rspOption <- factor(leRspDistDatExpt$rspOption, levels = 1:4, labels = LETTERS[1:4])

#### Grouped by compare
leRspDistDatCompare = dcast(sjtEval, sjtItem + compare ~ leRspSlct, fun.aggregate = length)
leRspRanksCompare = t(apply(leRspDistDatCompare[,3:6], 1, rank))
leRspRankCorCompare = data.frame(x = 4, # Create separate data frame with correlations for plotting
                                 y = 1, 
                                 rankCor = sapply(seq(1,nrow(leRspRanksCompare),2), function(x) { # Correlation between rsp-rspCxt
                                   cor(leRspRanksCompare[x,], leRspRanksCompare[x+1,], method = "kendall")
                                 }),
                                 maxAgree = sapply(seq(1,nrow(leRspRanksCompare),2), function(x) { # Is most selected response the same in rsp & rspCxt
                                   which.max(leRspRanksCompare[x,]) == which.max(leRspRanksCompare[x+1,])
                                 })
                                 ,
                                 sjtItem = 1:max(leRspDistDatCompare$sjtItem)
)
leRspDistDatCompare$rankCorAgree = rep(leRspRankCorCompare$rankCor > 0, each = 2)
leRspDistDatCompare$maxAgree = rep(leRspRankCorCompare$maxAgree, each = 2)
leRspDistDatCompare = melt(leRspDistDatCompare, id.vars = c("sjtItem", "compare", "rankCorAgree", "maxAgree"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
leRspDistDatCompare$prop = leRspDistDatCompare$count/20000
leRspDistDatCompare$rspOption <- factor(leRspDistDatCompare$rspOption, levels = 1:4, labels = LETTERS[1:4])

#### Grouped by itemStem
leRspDistDatItemStem = dcast(sjtEval, sjtItem + itemStem ~ leRspSlct, fun.aggregate = length)
leRspRanksItemStem = t(apply(leRspDistDatItemStem[,3:6], 1, rank))
leRspRankCorItemStem = data.frame(x = 4, # Create separate data frame with correlations for plotting
                                  y = 1, 
                                  rankCor = sapply(seq(1,nrow(leRspRanksItemStem),2), function(x) { # Correlation between absent-present
                                    cor(leRspRanksItemStem[x,], leRspRanksItemStem[x+1,], method = "kendall")
                                  }),
                                  maxAgree = sapply(seq(1,nrow(leRspRanksItemStem),2), function(x) { # Is most selected response the same in absent & present
                                    which.max(leRspRanksItemStem[x,]) == which.max(leRspRanksItemStem[x+1,])
                                  })
                                  ,
                                  sjtItem = 1:max(leRspDistDatItemStem$sjtItem)
)
leRspDistDatItemStem$rankCorAgree = rep(leRspRankCorItemStem$rankCor > 0, each = 2)
leRspDistDatItemStem$maxAgree = rep(leRspRankCorItemStem$maxAgree, each = 2)
leRspDistDatItemStem = melt(leRspDistDatItemStem, id.vars = c("sjtItem", "itemStem", "rankCorAgree", "maxAgree"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
leRspDistDatItemStem$prop = leRspDistDatItemStem$count/20000
leRspDistDatItemStem$rspOption <- factor(leRspDistDatItemStem$rspOption, levels = 1:4, labels = LETTERS[1:4])

#### Grouped by exptLvl & itemStem
leRspDistDatExptLvlItemStem = dcast(sjtEval, sjtItem + exptLvl + itemStem ~ leRspSlct, fun.aggregate = length)
leRspDistDatExptLvlItemStem = leRspDistDatExptLvlItemStem[order(leRspDistDatExptLvlItemStem$exptLvl, leRspDistDatExptLvlItemStem$sjtItem),]
leRspRanksExptLvlItemStem = t(apply(leRspDistDatExptLvlItemStem[,4:7], 1, rank))
leRspRankCorExptLvlItemStem = data.frame(x = 4, # Create separate data frame with correlations for plotting
                                         y = 1,
                                         rankCor = sapply(seq(1,nrow(leRspRanksExptLvlItemStem),2), function(x) { # Correlation between absent-present within each expertise level
                                           cor(leRspRanksExptLvlItemStem[x,], leRspRanksExptLvlItemStem[x+1,], method = "kendall")
                                         }),
                                         maxAgree = sapply(seq(1,nrow(leRspRanksExptLvlItemStem),2), function(x) { # Is most selected response the same in absent & present
                                           which.max(leRspRanksExptLvlItemStem[x,]) == which.max(leRspRanksExptLvlItemStem[x+1,])
                                         }),
                                         sjtItem = 1:max(leRspDistDatExptLvlItemStem$sjtItem),
                                         exptLvl = rep(1:max(leRspDistDatExptLvlItemStem$exptLvl), each = max(leRspDistDatExptLvlItemStem$sjtItem))
)
leRspDistDatExptLvlItemStem$rankCorAgree = rep(leRspRankCorExptLvlItemStem$rankCor > 0, each = 2)
leRspDistDatExptLvlItemStem$maxAgree = rep(leRspRankCorExptLvlItemStem$maxAgree, each = 2)
leRspDistDatExptLvlItemStem = melt(leRspDistDatExptLvlItemStem, id.vars = c("sjtItem", "exptLvl", "itemStem", "rankCorAgree", "maxAgree"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
leRspDistDatExptLvlItemStem$prop = leRspDistDatExptLvlItemStem$count/4000
leRspDistDatExptLvlItemStem$rspOption <- factor(leRspDistDatExptLvlItemStem$rspOption, levels = 1:4, labels = LETTERS[1:4])

#### Grouped by exptLvl & compare
leRspDistDatExptLvlCompare = dcast(sjtEval, sjtItem + exptLvl + compare ~ leRspSlct, fun.aggregate = length)
leRspDistDatExptLvlCompare = leRspDistDatExptLvlCompare[order(leRspDistDatExptLvlCompare$exptLvl, leRspDistDatExptLvlCompare$sjtItem),]
leRspRanksExptLvlCompare = t(apply(leRspDistDatExptLvlCompare[,4:7], 1, rank))
leRspRankCorExptLvlCompare = data.frame(x = 4, # Create separate data frame with correlations for plotting
                                        y = 1,
                                        rankCor = sapply(seq(1,nrow(leRspRanksExptLvlCompare),2), function(x) { # Correlation between absent-present within each expertise level
                                          cor(leRspRanksExptLvlCompare[x,], leRspRanksExptLvlCompare[x+1,], method = "kendall")
                                        }),
                                        maxAgree = sapply(seq(1,nrow(leRspRanksExptLvlCompare),2), function(x) { # Is most selected response the same in absent & present
                                          which.max(leRspRanksExptLvlCompare[x,]) == which.max(leRspRanksExptLvlCompare[x+1,])
                                        }),
                                        sjtItem = 1:max(leRspDistDatExptLvlCompare$sjtItem),
                                        exptLvl = rep(1:max(leRspDistDatExptLvlCompare$exptLvl), each = max(leRspDistDatExptLvlCompare$sjtItem))
                                        
)
leRspDistDatExptLvlCompare$rankCorAgree = rep(leRspRankCorExptLvlCompare$rankCor > 0, each = 2)
leRspDistDatExptLvlCompare$maxAgree = rep(leRspRankCorExptLvlCompare$maxAgree, each = 2)
leRspDistDatExptLvlCompare = melt(leRspDistDatExptLvlCompare, id.vars = c("sjtItem", "exptLvl", "compare", "rankCorAgree", "maxAgree"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
leRspDistDatExptLvlCompare$prop = leRspDistDatExptLvlCompare$count/4000
leRspDistDatExptLvlCompare$rspOption <- factor(leRspDistDatExptLvlCompare$rspOption, levels = 1:4, labels = LETTERS[1:4])

#### Grouped by itemStem & compare
leRspDistDatCompareItemStem = dcast(sjtEval, sjtItem + compare + itemStem ~ leRspSlct, fun.aggregate = length)
leRspRanksCompareItemStem = t(apply(leRspDistDatCompareItemStem[,4:7], 1, rank))
leRspRankCorCompareItemStem = data.frame(x = 4, # Create separate data frame with correlations for plotting
                                         y = 1,
                                         rankCor = sapply(seq(1,nrow(leRspRanksCompareItemStem),2), function(x) { # Correlation between absent-present within each compare
                                           cor(leRspRanksCompareItemStem[x,], leRspRanksCompareItemStem[x+1,], method = "kendall")
                                         }),
                                         maxAgree = sapply(seq(1,nrow(leRspRanksCompareItemStem),2), function(x) { # Is most selected response the same in absent & present
                                           which.max(leRspRanksCompareItemStem[x,]) == which.max(leRspRanksCompareItemStem[x+1,])
                                         }),
                                         sjtItem = 1:max(leRspDistDatCompareItemStem$sjtItem),
                                         compare = rep(levels(leRspDistDatCompareItemStem$compare), each = max(leRspDistDatCompareItemStem$sjtItem))
)
leRspDistDatCompareItemStem$rankCorAgree = rep(leRspRankCorCompareItemStem$rankCor > 0, each = 2)
leRspDistDatCompareItemStem$maxAgree = rep(leRspRankCorCompareItemStem$maxAgree, each = 2)
leRspDistDatCompareItemStem = melt(leRspDistDatCompareItemStem, id.vars = c("sjtItem", "compare", "itemStem", "rankCorAgree", "maxAgree"), measure.vars = c("1","2","3","4"), variable.name = "rspOption", value.name = "count")
leRspDistDatCompareItemStem$prop = leRspDistDatCompareItemStem$count/10000
leRspDistDatCompareItemStem$rspOption <- factor(leRspDistDatCompareItemStem$rspOption, levels = 1:4, labels = LETTERS[1:4])


## ~~~ PLOTS ~~~ ## 
## ME rsp plots
### By expStrength
#### Only items 1-10
ggplot(data = meRspDistDatExpStr[meRspDistDatExpStr$sjtItem %in% 1:10,]) +
  geom_col(aes(x = rspOption, y = prop, size = 1.5)) +
  guides(size = F) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.1), labels = percent) +
  labs(y = "Proportion choosing response option",
       x = "Response option") +
  facet_grid(expStrength ~ sjtItem, labeller = labeller(.cols = c('1' = "Item 1", '2' = "Item 2", '3' = "Item 3", '4' = "Item 4", '5' = "Item 5",
                                                                  '6' = "Item 6", '7' = "Item 7", '8' = "Item 8", '9' = "Item 9", '10' = "Item 10"),
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

#### All items
ggplot(data = meRspDistDatExpStr) +
  geom_col(aes(x = rspOption, y = prop, size = 1.5)) +
  guides(size = F) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.1), labels = percent) +
  labs(y = "Proportion choosing response option",
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
#### Only items 1-10
ggplot(data = meRspDistDatExpt[meRspDistDatExpt$sjtItem %in% 1:10,]) +
  geom_col(aes(x = rspOption, y = prop, size = 1.5)) +
  guides(size = F) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.1), labels = percent) +
  labs(y = "Proportion choosing response option",
       x = "Response option") +
  facet_grid(exptLvl ~ sjtItem, labeller = labeller(.cols = c('1' = "Item 1", '2' = "Item 2", '3' = "Item 3", '4' = "Item 4", '5' = "Item 5",
                                                              '6' = "Item 6", '7' = "Item 7", '8' = "Item 8", '9' = "Item 9", '10' = "Item 10"),
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

#### All items
ggplot(data = meRspDistDatExpt) +
  geom_col(aes(x = rspOption, y = prop, size = 1.5)) +
  guides(size = F) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.1), labels = percent) +
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

### By compare only
ggplot(data = meRspDistDatCompare) +
  geom_rect(data = meRspDistDatCompare[meRspDistDatCompare$maxAgree == T & meRspDistDatCompare$compare == "rspCxt" & meRspDistDatCompare$rspOption == "A",], aes(fill = maxAgree), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .5, show.legend = F) +
  geom_col(aes(x = rspOption, y = prop, fill = compare, size = 1.5), position = position_dodge()) +
  geom_text(data = meRspRankCorCompare, aes(x = x, y = y, label = paste("tau == ", round(rankCor,2), sep = ""), hjust = 1), size = 3.5, parse = T) +
  scale_fill_manual(breaks = c("rspCxt", "rsp"), labels = c("Consequences", "Behavior"), values = c("rspCxt" = "#E74C3C", "rsp" = "#18BC9C", "TRUE" = "#F39C12")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.05), labels = percent) +
  guides(size = F) +
  labs(y = "Proportion choosing response option",
       x = "Response option",
       fill = "Evaluation Criteria") +
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

### By itemStem only
ggplot(data = meRspDistDatItemStem) +
  geom_rect(data = meRspDistDatItemStem[meRspDistDatItemStem$maxAgree == T & meRspDistDatItemStem$itemStem == "Absent" & meRspDistDatItemStem$rspOption == "A",], aes(fill = maxAgree), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .5, show.legend = F) +
  geom_col(aes(x = rspOption, y = prop, fill = itemStem, size = 1.5), position = position_dodge()) +
  geom_text(data = meRspRankCorItemStem, aes(x = x, y = y, label = paste("tau == ", round(rankCor,2), sep = ""), hjust = 1), size = 3.5, parse = T) +
  scale_fill_manual(breaks = c("Absent", "Present"), values = c("Absent" = "#E74C3C", "Present" = "#18BC9C", "TRUE" = "#F39C12")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.1), labels = percent) +
  guides(size = F) +
  labs(y = "Proportion choosing response option",
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
#### All items
ggplot(data = meRspDistDatExptLvlItemStem) +
  geom_rect(data = meRspDistDatExptLvlItemStem[meRspDistDatExptLvlItemStem$maxAgree == T & meRspDistDatExptLvlItemStem$itemStem == "Absent" & meRspDistDatExptLvlItemStem$rspOption == "A",], aes(fill = maxAgree), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .5, show.legend = F) +
  geom_col(aes(x = rspOption, y = prop, fill = itemStem, size = 1.5), position = position_dodge()) +
  geom_text(data = meRspRankCorExptLvlItemStem, aes(x = x, y = y, label = paste("tau == ", round(rankCor,2), sep = ""), hjust = 1), size = 3, parse = T) +
  scale_fill_manual(breaks = c("Absent", "Present"), values = c("Absent" = "#E74C3C", "Present" = "#18BC9C", "TRUE" = "#F39C12")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.1), labels = percent) +
  guides(size = F) +
  labs(y = "Proportion choosing response option",
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

### By exptLvl and itemStem
#### Only items 1-10
ggplot(data = meRspDistDatExptLvlItemStem[meRspDistDatExptLvlItemStem$sjtItem %in% 1:10,]) +
  geom_rect(data = meRspDistDatExptLvlItemStem[meRspDistDatExptLvlItemStem$maxAgree == T & meRspDistDatExptLvlItemStem$itemStem == "Absent" & meRspDistDatExptLvlItemStem$rspOption == "A" & meRspDistDatExptLvlItemStem$sjtItem %in% 1:10,], aes(fill = maxAgree), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .5, show.legend = F) +
  geom_col(aes(x = rspOption, y = prop, fill = itemStem, size = 1.5), position = position_dodge()) +
  geom_text(data = meRspRankCorExptLvlItemStem[meRspRankCorExptLvlItemStem$sjtItem %in% 1:10,], aes(x = x, y = y, label = paste("tau == ", round(rankCor,2), sep = ""), hjust = 1), size = 3, parse = T) +
  scale_fill_manual(breaks = c("Absent", "Present"), values = c("Absent" = "#E74C3C", "Present" = "#18BC9C", "TRUE" = "#F39C12")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.1), labels = percent) +
  guides(size = F) +
  labs(y = "Proportion choosing response option",
       x = "Response option",
       fill = "SJT Item Stem") +
  facet_grid(exptLvl ~ sjtItem, labeller = labeller(.cols = c('1' = "Item 1", '2' = "Item 2", '3' = "Item 3", '4' = "Item 4", '5' = "Item 5",
                                                              '6' = "Item 6", '7' = "Item 7", '8' = "Item 8", '9' = "Item 9", '10' = "Item 10"),
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

### By exptLvl and compare
ggplot(data = meRspDistDatExptLvlCompare) +
  geom_rect(data = meRspDistDatExptLvlCompare[meRspDistDatExptLvlCompare$maxAgree == T & meRspDistDatExptLvlCompare$compare == "rspCxt" & meRspDistDatExptLvlCompare$rspOption == "A",], aes(fill = maxAgree), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .5, show.legend = F) +
  geom_col(aes(x = rspOption, y = prop, fill = compare, size = 1.5), position = position_dodge()) +
  geom_text(data = meRspRankCorExptLvlCompare, aes(x = x, y = y, label = paste("tau == ", round(rankCor,2), sep = ""), hjust = 1), size = 3, parse = T) +
  scale_fill_manual(breaks = c("rspCxt", "rsp"), labels = c("Consequences", "Behavior"), values = c("rspCxt" = "#E74C3C", "rsp" = "#18BC9C", "TRUE" = "#F39C12")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.1), labels = percent) +
  guides(size = F) +
  labs(y = "Proportion choosing response option",
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

### By compare and itemStem
ggplot(data = meRspDistDatCompareItemStem) +
  geom_rect(data = meRspDistDatCompareItemStem[meRspDistDatCompareItemStem$maxAgree == T & meRspDistDatCompareItemStem$itemStem == "Absent" & meRspDistDatCompareItemStem$rspOption == "A",], aes(fill = maxAgree), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .5, show.legend = F) +
  geom_col(aes(x = rspOption, y = prop, fill = itemStem, size = 1.5), position = position_dodge()) +
  geom_text(data = meRspRankCorCompareItemStem, aes(x = x, y = y, label = paste("tau == ", round(rankCor,2), sep = ""), hjust = 1), size = 3, parse = T) +
  scale_fill_manual(breaks = c("Absent", "Present"), values = c("Absent" = "#E74C3C", "Present" = "#18BC9C", "TRUE" = "#F39C12")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.1), labels = percent) +
  guides(size = F) +
  labs(y = "Number choosing response option",
       x = "Response option",
       fill = "SJT Item Stem") +
  facet_grid(compare ~ sjtItem, labeller = labeller(.cols = c('1' = "Item 1", '2' = "Item 2", '3' = "Item 3", '4' = "Item 4", '5' = "Item 5",
                                                              '6' = "Item 6", '7' = "Item 7", '8' = "Item 8", '9' = "Item 9", '10' = "Item 10",
                                                              '11' = "Item 11", '12' = "Item 12", '13' = "Item 13", '14' = "Item 14", '15' = "Item 15",
                                                              '16' = "Item 16", '17' = "Item 17", '18' = "Item 18", '19' = "Item 19", '20' = "Item 20"),
                                                    .rows = c('rspCxt' = "Consequences",
                                                              'rsp' = "Behavior"))) +
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
### By expStrength only
#### Only items 1:10
ggplot(data = leRspDistDatExpStr[leRspDistDatExpStr$sjtItem %in% 1:10,]) +
  geom_col(aes(x = rspOption, y = prop, size = 1.5)) +
  guides(size = F) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.1), labels = percent) +
  labs(y = "Proportion choosing response option",
       x = "Response option") +
  facet_grid(expStrength ~ sjtItem, labeller = labeller(.cols = c('1' = "Item 1", '2' = "Item 2", '3' = "Item 3", '4' = "Item 4", '5' = "Item 5",
                                                                  '6' = "Item 6", '7' = "Item 7", '8' = "Item 8", '9' = "Item 9", '10' = "Item 10"),
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

#### All items
ggplot(data = leRspDistDatExpStr) +
  geom_col(aes(x = rspOption, y = prop, size = 1.5)) +
  guides(size = F) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.1), labels = percent) +
  labs(y = "Proportion choosing response option",
       x = "Response option") +
  facet_grid(expStrength ~ sjtItem, labeller = labeller(.cols = c('1' = "Item 1", '2' = "Item 2", '3' = "Item 3", '4' = "Item 4", '5' = "Item 5",
                                                                  '6' = "Item 6", '7' = "Item 7", '8' = "Item 8", '9' = "Item 9", '10' = "Item 10"),
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
#### Only items 1:10
ggplot(data = leRspDistDatExpt[leRspDistDatExpt$sjtItem %in% 1:10,]) +
  geom_col(aes(x = rspOption, y = prop, size = 1.5)) +
  guides(size = F) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.1), labels = percent) +
  labs(y = "Proportion choosing response option",
       x = "Response option") +
  facet_grid(exptLvl ~ sjtItem, labeller = labeller(.cols = c('1' = "Item 1", '2' = "Item 2", '3' = "Item 3", '4' = "Item 4", '5' = "Item 5",
                                                              '6' = "Item 6", '7' = "Item 7", '8' = "Item 8", '9' = "Item 9", '10' = "Item 10"),
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

#### All items
ggplot(data = leRspDistDatExpt) +
  geom_col(aes(x = rspOption, y = prop, size = 1.5)) +
  guides(size = F) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.1), labels = percent) +
  labs(y = "Proportion choosing response option",
       x = "Response option") +
  facet_grid(exptLvl ~ sjtItem, labeller = labeller(.cols = c('1' = "Item 1", '2' = "Item 2", '3' = "Item 3", '4' = "Item 4", '5' = "Item 5",
                                                              '6' = "Item 6", '7' = "Item 7", '8' = "Item 8", '9' = "Item 9", '10' = "Item 10"),
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

### By compare only
ggplot(data = leRspDistDatCompare) +
  geom_rect(data = leRspDistDatCompare[leRspDistDatCompare$maxAgree == T & leRspDistDatCompare$compare == "rspCxt" & leRspDistDatCompare$rspOption == "A",], aes(fill = maxAgree), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .5, show.legend = F) +
  geom_col(aes(x = rspOption, y = prop, fill = compare, size = 1.5), position = position_dodge()) +
  geom_text(data = leRspRankCorCompare, aes(x = x, y = y, label = paste("tau == ", round(rankCor,2), sep = ""), hjust = 1), size = 3.5, parse = T) +
  scale_fill_manual(breaks = c("rspCxt", "rsp"), labels = c("Consequences", "Behavior"), values = c("rspCxt" = "#E74C3C", "rsp" = "#18BC9C", "TRUE" = "#F39C12")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.05), labels = percent) +
  guides(size = F) +
  labs(y = "Proportion choosing response option",
       x = "Response option",
       fill = "Evaluation Criteria") +
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

### By itemStem only
ggplot(data = leRspDistDatItemStem) +
  geom_rect(data = leRspDistDatItemStem[leRspDistDatItemStem$maxAgree == T & leRspDistDatItemStem$itemStem == "Absent" & leRspDistDatItemStem$rspOption == "A",], aes(fill = maxAgree), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .5, show.legend = F) +
  geom_col(aes(x = rspOption, y = prop, fill = itemStem, size = 1.5), position = position_dodge()) +
  geom_text(data = leRspRankCorItemStem, aes(x = x, y = y, label = paste("tau == ", round(rankCor,2), sep = ""), hjust = 1), size = 3.5, parse = T) +
  scale_fill_manual(breaks = c("Absent", "Present"), values = c("Absent" = "#E74C3C", "Present" = "#18BC9C", "TRUE" = "#F39C12")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.1), labels = percent) +
  guides(size = F) +
  labs(y = "Proportion choosing response option",
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

## ~~~ RESPONSE SELECTION RATIOS ~~~ ##
# Ratio between most commonly selected answer across evaluation criterion and item stem presence conditions
## ME RESPONSES ##
## Compare for ME
meCompareCnt = aggregate(meRspDistDatCompare$count, by = list(meRspDistDatCompare$compare, meRspDistDatCompare$sjtItem), function(x)
  max(x))
meCompareCntRatio = data.frame("item" = 1:20,
                               "ratio" = meCompareCnt[seq(2,40,2),"x"]/meCompareCnt[seq(1,40,2),"x"])
summary(meCompareCntRatio$ratio)
## Compare by expertise level for ME
meExptCompareCnt = aggregate(meRspDistDatExptLvlCompare$count, by = list(meRspDistDatExptLvlCompare$compare, meRspDistDatExptLvlCompare$sjtItem, meRspDistDatExptLvlCompare$exptLvl), function(x)
  max(x))
meExptCompareCntRatio = data.frame("item" = rep(1:20, 5),
                                   "exptLvl" = rep(1:5, each = 20),
                                   "ratio" = meExptCompareCnt[seq(2,200,2),"x"]/meExptCompareCnt[seq(1,200,2),"x"])
summary(meExptCompareCntRatio$ratio)
## Item Stem for ME
meItemStemCnt = aggregate(meRspDistDatItemStem$count, by = list(meRspDistDatItemStem$itemStem, meRspDistDatItemStem$sjtItem), function(x)
  max(x))
meItemStemCntRatio = data.frame("item" = 1:20,
                                "ratio" = meItemStemCnt[seq(2,40,2),"x"]/meItemStemCnt[seq(1,40,2),"x"])
summary(meItemStemCntRatio$ratio)
## Item Stem by expertise level for ME
meExptItemStemCnt = aggregate(meRspDistDatExptLvlItemStem$count, by = list(meRspDistDatExptLvlItemStem$itemStem, meRspDistDatExptLvlItemStem$sjtItem, meRspDistDatExptLvlItemStem$exptLvl), function(x)
  max(x))
meExptItemStemCntRatio = data.frame("item" = rep(1:20, 5),
                                    "exptLvl" = rep(1:5, each = 20),
                                    "ratio" = meExptItemStemCnt[seq(2,200,2),"x"]/meExptItemStemCnt[seq(1,200,2),"x"])
summary(meExptItemStemCntRatio$ratio)

## LE RESPONSES ##
## Compare for LE
leCompareCnt = aggregate(leRspDistDatCompare$count, by = list(leRspDistDatCompare$compare, leRspDistDatCompare$sjtItem), function(x)
  max(x))
leCompareCntRatio = data.frame("item" = 1:20,
                               "ratio" = leCompareCnt[seq(2,40,2),"x"]/leCompareCnt[seq(1,40,2),"x"])
summary(leCompareCntRatio$ratio)
## Compare by expertise level for ME
leExptCompareCnt = aggregate(leRspDistDatExptLvlCompare$count, by = list(leRspDistDatExptLvlCompare$compare, leRspDistDatExptLvlCompare$sjtItem, leRspDistDatExptLvlCompare$exptLvl), function(x)
  max(x))
leExptCompareCntRatio = data.frame("item" = rep(1:20, 5),
                                   "exptLvl" = rep(1:5, each = 20),
                                   "ratio" = leExptCompareCnt[seq(2,200,2),"x"]/leExptCompareCnt[seq(1,200,2),"x"])
summary(leExptCompareCntRatio$ratio)
## Item Stem for LE
leItemStemCnt = aggregate(leRspDistDatItemStem$count, by = list(leRspDistDatItemStem$itemStem, leRspDistDatItemStem$sjtItem), function(x)
  max(x))
leItemStemCntRatio = data.frame("item" = 1:20,
                                "ratio" = leItemStemCnt[seq(2,40,2),"x"]/leItemStemCnt[seq(1,40,2),"x"])
summary(leItemStemCntRatio$ratio)
## Item Stem by expertise level for LE
leExptItemStemCnt = aggregate(leRspDistDatExptLvlItemStem$count, by = list(leRspDistDatExptLvlItemStem$itemStem, leRspDistDatExptLvlItemStem$sjtItem, leRspDistDatExptLvlItemStem$exptLvl), function(x)
  max(x))
leExptItemStemCntRatio = data.frame("item" = rep(1:20, 5),
                                    "exptLvl" = rep(1:5, each = 20),
                                    "ratio" = leExptItemStemCnt[seq(2,200,2),"x"]/leExptItemStemCnt[seq(1,200,2),"x"])
summary(leExptItemStemCntRatio$ratio)

## ~~~ PLOTS ~~~ ##
### Combine ratio data into single data frame and plot using facets
ratioAll <- data.frame("rspType" = factor(c(rep("ME", 120), rep("LE", 120)), levels = c("ME", "LE")),
                       "exptLvl" = as.factor(c(rep(c(rep("No", 20), rep("Yes", 100)), 2))),
                       "ratio" = c(meItemStemCntRatio$ratio, meExptItemStemCntRatio$ratio, leItemStemCntRatio$ratio, leExptItemStemCntRatio$ratio))
ratioItemStem <- data.frame("rspType" = factor(c("ME","LE"), levels = c("ME", "LE")),
                            "meanRatio" = c(mean(meItemStemCntRatio$ratio), mean(leItemStemCntRatio$ratio)))
ratioExptItemStem <- data.frame("rspType" = factor(c("ME","LE"), levels = c("ME", "LE")),
                                "meanRatio" = c(mean(meExptItemStemCntRatio$ratio), mean(leExptItemStemCntRatio$ratio)))
### Overall data
ggplot(data = ratioAll[ratioAll$exptLvl == "No",], aes(x = ratio)) +
  #geom_histogram(aes(fill = rspType), color = "black", size = 1.05, bins = 12) +
  geom_histogram(color = "black", alpha = .65, size = 1.05, bins = 12) +
  geom_vline(data = ratioItemStem, aes(xintercept = meanRatio), size = 1.1, linetype = "dashed")  +
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
ggplot(data = ratioAll[ratioAll$exptLvl == "Yes",], aes(x = ratio)) +
  geom_histogram(color = "black", alpha = .65, size = 1.05, bins = 25) +
  geom_vline(data = ratioExptItemStem, aes(xintercept = meanRatio), size = 1.1, linetype = "dashed")  +
  geom_vline(xintercept = 1, size = 1.1, color = "gray") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 34)) +
  facet_grid(. ~ rspType, labeller = labeller(.cols = c('LE' = "Least Effective", 
                                                        'ME' = "Most Effective"))) +
  labs(x = "Ratio",
       y = "Count") +
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        strip.text = element_text(size = 11))

### Data transformation for computing Fleiss' kappa for response selection [Not run/tested]
meRspSlctExptItemStem = as.data.frame(matrix(NA, nrow = 200, ncol = 4000))
for (i in levels(sjtEval$itemStem)) {
  if (i == "Absent") {ndx = 0} else {ndx = 1}
  for (j in 1:max(sjtEval$exptLvl)) {
    tmp = sjtEval[sjtEval$itemStem == i & sjtEval$exptLvl == j, 1:3]
    tmp = spread(tmp, key = sjtItem, value = meRspSlct)
    meRspSlctExptItemStem[((20*(j-1)+1)+(ndx*100)):((20*j)+(ndx*100)),] <- t(tmp[,2:21])
    rm(tmp)
  }
}
meRspSlctExptItemStem$exptLvl <- rep(rep(1:5, each = 20), 2)
meRspSlctExptItemStem$itemStem <- as.factor(rep(c("Absent", "Present"), each = 100))
library(irr)
kappam.fleiss(meRspSlctExptItemStem[meRspSlctExptItemStem$exptLvl == 5,1:4000])


######################
# RQ4: RESPONSE TIME #
######################
# ~~~ Transform data for plotting ~~~ ##
## Create data frames for plotting survival rates
survDatAll = data.frame("rspType" = rep(factor(c("ME", "LE"), levels = c("LE", "ME")), each = 11),
                        "iterNum" = rep(0:10, 2),
                        "propChoose" = c(as.numeric(c(0,c(cumsum(table(sjtEval$meReEval))[as.character(1:9)], cumsum(table(sjtEval$meReEval))["9"])/800000)),
                                         as.numeric(c(0,0,c(cumsum(table(sjtEval$leReEval))[as.character(2:9)], cumsum(table(sjtEval$leReEval))["9"])/800000))
                        ))

survDatItemStem = data.frame("rspType" = rep(factor(c("ME", "LE"), levels = c("LE", "ME")), each = 22),
                             "itemStem" = rep(rep(factor(c("Absent", "Present"), levels = c("Absent", "Present")), each = 11), 2),
                             "iterNum" = rep(0:10, 4),
                             "propChoose" = c(as.numeric(c(0,c(cumsum(table(sjtEval$meReEval[sjtEval$itemStem == "Absent"]))[as.character(1:9)], cumsum(table(sjtEval$meReEval[sjtEval$itemStem == "Absent"]))["9"])/sum(table(sjtEval$meReEval[sjtEval$itemStem == "Absent"])))), 
                                              as.numeric(c(0,c(cumsum(table(sjtEval$meReEval[sjtEval$itemStem == "Present"]))[as.character(1:9)],cumsum(table(sjtEval$meReEval[sjtEval$itemStem == "Present"]))["9"])/sum(table(sjtEval$meReEval[sjtEval$itemStem == "Present"])))),
                                              as.numeric(c(0,0,c(cumsum(table(sjtEval$leReEval[sjtEval$itemStem == "Absent"]))[as.character(2:9)],cumsum(table(sjtEval$leReEval[sjtEval$itemStem == "Absent"]))["9"])/sum(table(sjtEval$leReEval[sjtEval$itemStem == "Absent"])))), 
                                              as.numeric(c(0,0,c(cumsum(table(sjtEval$leReEval[sjtEval$itemStem == "Present"]))[as.character(2:9)],cumsum(table(sjtEval$leReEval[sjtEval$itemStem == "Present"]))["9"])/sum(table(sjtEval$leReEval[sjtEval$itemStem == "Present"]))))
                             ))

survDatCompare = data.frame("rspType" = rep(factor(c("ME", "LE"), levels = c("LE", "ME")), each = 22),
                            "compare" = rep(rep(factor(c("RspCxt", "Rsp"), levels = c("RspCxt", "Rsp")), each = 11), 2),
                            "iterNum" = rep(0:10, 4),
                            "propChoose" = c(as.numeric(c(0,c(cumsum(table(sjtEval$meReEval[sjtEval$compare == "rspCxt"]))[as.character(1:9)], cumsum(table(sjtEval$meReEval[sjtEval$compare == "rspCxt"]))["9"])/sum(table(sjtEval$meReEval[sjtEval$compare == "rspCxt"])))), 
                                             as.numeric(c(0,c(cumsum(table(sjtEval$meReEval[sjtEval$compare == "rsp"]))[as.character(1:9)],cumsum(table(sjtEval$meReEval[sjtEval$compare == "rsp"]))["9"])/sum(table(sjtEval$meReEval[sjtEval$compare == "rsp"])))),
                                             as.numeric(c(0,0,c(cumsum(table(sjtEval$leReEval[sjtEval$compare == "rspCxt"]))[as.character(2:9)],cumsum(table(sjtEval$leReEval[sjtEval$compare == "rspCxt"]))["9"])/sum(table(sjtEval$leReEval[sjtEval$compare == "rspCxt"])))), 
                                             as.numeric(c(0,0,c(cumsum(table(sjtEval$leReEval[sjtEval$compare == "rsp"]))[as.character(2:9)],cumsum(table(sjtEval$leReEval[sjtEval$compare == "rsp"]))["9"])/sum(table(sjtEval$leReEval[sjtEval$compare == "rsp"]))))
                            ))

survDatItemStemCompare = data.frame("rspType" = rep(factor(c("ME", "LE"), levels = c("LE", "ME")), each = 44),
                                    "itemStem" = rep(rep(factor(c("Absent", "Present"), levels = c("Absent", "Present")), each = 22), 2),
                                    "compare" = rep(rep(factor(c("RspCxt", "Rsp"), levels = c("RspCxt", "Rsp")), each = 11), 4),
                                    "iterNum" = rep(0:10, 8),
                                    "propChoose" = c(as.numeric(c(0,c(cumsum(table(sjtEval$meReEval[sjtEval$compare == "rspCxt" & sjtEval$itemStem == "Absent"]))[as.character(1:9)], cumsum(table(sjtEval$meReEval[sjtEval$compare == "rspCxt" & sjtEval$itemStem == "Absent"]))["9"])/sum(table(sjtEval$meReEval[sjtEval$compare == "rspCxt" & sjtEval$itemStem == "Absent"])))), 
                                                     as.numeric(c(0,c(cumsum(table(sjtEval$meReEval[sjtEval$compare == "rsp" & sjtEval$itemStem == "Absent"]))[as.character(1:9)], cumsum(table(sjtEval$meReEval[sjtEval$compare == "rsp" & sjtEval$itemStem == "Absent"]))["9"])/sum(table(sjtEval$meReEval[sjtEval$compare == "rsp" & sjtEval$itemStem == "Absent"])))),
                                                     as.numeric(c(0,c(cumsum(table(sjtEval$meReEval[sjtEval$compare == "rspCxt" & sjtEval$itemStem == "Present"]))[as.character(1:9)], cumsum(table(sjtEval$meReEval[sjtEval$compare == "rspCxt" & sjtEval$itemStem == "Present"]))["9"])/sum(table(sjtEval$meReEval[sjtEval$compare == "rspCxt" & sjtEval$itemStem == "Present"])))), 
                                                     as.numeric(c(0,c(cumsum(table(sjtEval$meReEval[sjtEval$compare == "rsp" & sjtEval$itemStem == "Present"]))[as.character(1:9)], cumsum(table(sjtEval$meReEval[sjtEval$compare == "rsp" & sjtEval$itemStem == "Present"]))["9"])/sum(table(sjtEval$meReEval[sjtEval$compare == "rsp" & sjtEval$itemStem == "Present"])))),
                                                     as.numeric(c(0,0,c(cumsum(table(sjtEval$leReEval[sjtEval$compare == "rspCxt" & sjtEval$itemStem == "Absent"]))[as.character(2:9)], cumsum(table(sjtEval$leReEval[sjtEval$compare == "rspCxt" & sjtEval$itemStem == "Absent"]))["9"])/sum(table(sjtEval$leReEval[sjtEval$compare == "rspCxt" & sjtEval$itemStem == "Absent"])))), 
                                                     as.numeric(c(0,0,c(cumsum(table(sjtEval$leReEval[sjtEval$compare == "rsp" & sjtEval$itemStem == "Absent"]))[as.character(2:9)], cumsum(table(sjtEval$leReEval[sjtEval$compare == "rsp" & sjtEval$itemStem == "Absent"]))["9"])/sum(table(sjtEval$leReEval[sjtEval$compare == "rsp" & sjtEval$itemStem == "Absent"])))),
                                                     as.numeric(c(0,0,c(cumsum(table(sjtEval$leReEval[sjtEval$compare == "rspCxt" & sjtEval$itemStem == "Present"]))[as.character(2:9)], cumsum(table(sjtEval$leReEval[sjtEval$compare == "rspCxt" & sjtEval$itemStem == "Present"]))["9"])/sum(table(sjtEval$leReEval[sjtEval$compare == "rspCxt" & sjtEval$itemStem == "Present"])))), 
                                                     as.numeric(c(0,0,c(cumsum(table(sjtEval$leReEval[sjtEval$compare == "rsp" & sjtEval$itemStem == "Present"]))[as.character(2:9)], cumsum(table(sjtEval$leReEval[sjtEval$compare == "rsp" & sjtEval$itemStem == "Present"]))["9"])/sum(table(sjtEval$leReEval[sjtEval$compare == "rsp" & sjtEval$itemStem == "Present"]))))
                                    ))

## Survival plots
### Overall
ggplot(data = survDatAll, aes(x = iterNum, y = 1-propChoose)) +
  geom_step(aes(color = rspType), size = 2) +
  scale_color_manual(values = c("LE" = "#E74C3C", "ME" = "#18BC9C")) +
  scale_y_continuous(breaks = seq(0,1, by = .1), limits = c(0,1), labels = scales::percent) +
  scale_x_continuous(breaks = 0:10) +
  labs(y = "Survival Probability",
       x = "Iteration Number",
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
  geom_step(aes(color = rspType, linetype = itemStem), size = 2) +
  scale_color_manual(values = c("LE" = "#E74C3C", "ME" = "#18BC9C")) +
  scale_linetype_manual(values = c("Present" = "solid", "Absent" = "11")) +
  scale_y_continuous(breaks = seq(0,1, by = .1), limits = c(0,1), labels = scales::percent) +
  scale_x_continuous(breaks = 0:10) +
  labs(y = "Survival Probability",
       x = "Iteration Number",
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

### By compare
ggplot(data = survDatCompare, aes(x = iterNum, y = 1-propChoose)) +
  geom_step(aes(color = rspType, linetype = compare), size = 2) +
  scale_color_manual(values = c("LE" = "#E74C3C", "ME" = "#18BC9C")) +
  scale_linetype_manual(values = c("Rsp" = "solid", "RspCxt" = "11"), labels = c("Consequences", "Behavior")) +
  scale_y_continuous(breaks = seq(0,1, by = .1), limits = c(0,1), labels = scales::percent) +
  scale_x_continuous(breaks = 0:10) +
  labs(y = "Survival Probability",
       x = "Iteration Number",
       color = "Response Type",
       linetype = "Evaluation Criterion") +
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

# ADDITIONAL CODE NOT RUN:
# ## Response time distribution plots
# timeDat = gather(sjtEval, key = "rspType", value = "evalIter", meReEval, leReEval)
# timeDat$rspType = factor(timeDat$rspType, levels = c("meReEval", "leReEval"), labels = c("ME", "LE"))
# 
# ggplot(data = timeDat, aes(x = evalIter, fill = rspType, color = itemStem)) +
#   geom_histogram(position = position_dodge(), binwidth = 1, bins = 10)
# propMEExpt = apply(table(sjtEval$meReEval, sjtEval$exptLvl), 2, function(x) cumsum(x)/80000)
# propLEExpt = apply(table(sjtEval$leReEval, sjtEval$exptLvl), 2, function(x) cumsum(x)/80000)
# propMEExpt <- rbind(0,propMEExpt)
# propLEExpt <- rbind(0,0,propLEExpt)
# 
# survDatExpt = data.frame("rspType" = rep(factor(c("ME", "LE"), levels = c("LE", "ME")), each = 55),
#                          "exptLvl" = rep(rep(factor(1:5), each = 11), 2),
#                          "iterNum" = rep(0:10, 10),
#                          "propChoose" = as.numeric(c(propMEExpt, propLEExpt)))
# 
# propMEExpStrength = apply(table(sjtEval$meReEval, sjtEval$expStrength), 2, function(x) cumsum(x)/100000)
# propLEExpStrength = apply(table(sjtEval$leReEval, sjtEval$expStrength), 2, function(x) cumsum(x)/100000)
# propMEExpStrength <- rbind(0,propMEExpStrength)
# propLEExpStrength <- rbind(0,0,propLEExpStrength)
# 
# survDatExpStrength = data.frame("rspType" = rep(factor(c("ME", "LE"), levels = c("LE", "ME")), each = 44),
#                                 "expStrength" = rep(rep(factor(1:4), each = 11), 2),
#                                 "iterNum" = rep(0:10, 8),
#                                 "propChoose" = as.numeric(c(propMEExpStrength, propLEExpStrength)))
# ### By exptLvl
# ggplot(data = survDatExpt, aes(x = iterNum, y = 1-propChoose)) +
#   geom_line(aes(color = exptLvl, linetype = rspType), size = 2) +
#   #scale_color_manual(values = c("LE" = "#E74C3C", "ME" = "#18BC9C")) +
#   #scale_linetype_manual(values = c("Present" = "solid", "Absent" = "11")) +
#   scale_y_continuous(breaks = seq(0,1, by = .1), labels = scales::percent) +
#   scale_x_continuous(breaks = 0:10) +
#   labs(y = "Survival",
#        x = "Iteration number",
#        color = "Expertise Lvl",
#        linetype = "Response Type") +
#   theme_bw() +
#   theme(legend.position = "top",
#         legend.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
#         legend.background = element_rect(fill = "white", color = "black", size = 0.5, linetype = "solid"),
#         legend.justification = "right",
#         legend.title = element_text(face = "bold", size = 10),
#         legend.text = element_text(size = 10),
#         axis.text = element_text(size = 10),
#         axis.title.x = element_text(face = "bold", size = 10),
#         axis.title.y = element_text(face = "bold", size = 10),
#         strip.text.x = element_text(size = 9),
#         strip.text.y = element_text(size = 10))
# 
# ### By expStrength
# ggplot(data = survDatExpStrength, aes(x = iterNum, y = 1-propChoose)) +
#   geom_line(aes(color = expStrength, linetype = rspType), size = 2) +
#   #scale_color_manual(values = c("LE" = "#E74C3C", "ME" = "#18BC9C")) +
#   #scale_linetype_manual(values = c("Present" = "solid", "Absent" = "11")) +
#   scale_y_continuous(breaks = seq(0,1, by = .1), labels = scales::percent) +
#   scale_x_continuous(breaks = 0:10) +
#   labs(y = "Survival",
#        x = "Iteration number",
#        color = "Experience Strength",
#        linetype = "Response Type") +
#   theme_bw() +
#   theme(legend.position = "top",
#         legend.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
#         legend.background = element_rect(fill = "white", color = "black", size = 0.5, linetype = "solid"),
#         legend.justification = "right",
#         legend.title = element_text(face = "bold", size = 10),
#         legend.text = element_text(size = 10),
#         axis.text = element_text(size = 10),
#         axis.title.x = element_text(face = "bold", size = 10),
#         axis.title.y = element_text(face = "bold", size = 10),
#         strip.text.x = element_text(size = 9),
#         strip.text.y = element_text(size = 10))
## Mean bar plots
# sePresent = c(sd(sjtEval$meReEval[sjtEval$itemStem == "Present"])/sqrt(length(sjtEval$meReEval[sjtEval$itemStem == "Present"])), sd(sjtEval$leReEval[sjtEval$itemStem == "Present"])/sqrt(length(sjtEval$leReEval[sjtEval$itemStem == "Present"])))
# seAbsent = c(sd(sjtEval$meReEval[sjtEval$itemStem == "Absent"])/sqrt(length(sjtEval$meReEval[sjtEval$itemStem == "Absent"])), sd(sjtEval$leReEval[sjtEval$itemStem == "Absent"])/sqrt(length(sjtEval$leReEval[sjtEval$itemStem == "Absent"])))
# allDat = data.frame("rspType" = rep(factor(c("ME", "LE"), levels = c("LE", "ME")), each = 2),
#                     "itemStem" = rep(factor(c("Absent", "Present"), levels = c("Absent", "Present")), 2),
#                     "meanIter" = c(round(mean(sjtEval$meReEval[sjtEval$itemStem == "Absent"]),2),
#                                    round(mean(sjtEval$meReEval[sjtEval$itemStem == "Present"]),2), 
#                                    round(mean(sjtEval$leReEval[sjtEval$itemStem == "Absent"]),2), 
#                                    round(mean(sjtEval$leReEval[sjtEval$itemStem == "Present"]),2)), 
#                     "medianIter" = c(median(sjtEval$meReEval[sjtEval$itemStem == "Absent"]),
#                                      median(sjtEval$meReEval[sjtEval$itemStem == "Present"]), 
#                                      median(sjtEval$leReEval[sjtEval$itemStem == "Absent"]), 
#                                      median(sjtEval$leReEval[sjtEval$itemStem == "Present"])), 
#                     "sdIter" = c(sd(sjtEval$meReEval[sjtEval$itemStem == "Absent"]),
#                                  sd(sjtEval$meReEval[sjtEval$itemStem == "Present"]), 
#                                  sd(sjtEval$leReEval[sjtEval$itemStem == "Absent"]), 
#                                  sd(sjtEval$leReEval[sjtEval$itemStem == "Present"])), 
#                     "CILower95" = c(mean(sjtEval$meReEval[sjtEval$itemStem == "Absent"]) - 2*seAbsent[1], 
#                                     mean(sjtEval$meReEval[sjtEval$itemStem == "Present"]) - 2*sePresent[2],
#                                     mean(sjtEval$leReEval[sjtEval$itemStem == "Absent"]) - 2*seAbsent[1], 
#                                     mean(sjtEval$leReEval[sjtEval$itemStem == "Present"]) - 2*sePresent[2]),
#                     "CIUpper95" = c(mean(sjtEval$meReEval[sjtEval$itemStem == "Absent"]) + 2*seAbsent[1], 
#                                     mean(sjtEval$meReEval[sjtEval$itemStem == "Present"]) + 2*sePresent[2],
#                                     mean(sjtEval$leReEval[sjtEval$itemStem == "Absent"]) + 2*seAbsent[1], 
#                                     mean(sjtEval$leReEval[sjtEval$itemStem == "Present"]) + 2*sePresent[2]))
# ggplot(data = allDat, aes(x = rspType, y = meanIter)) +
#   geom_col(aes(fill = rspType)) +
#   geom_hline(aes(color = rspType, yintercept = meanIter), linetype = 2) +
#   geom_text(aes(label = meanIter, y = meanIter, color = rspType, group = rspType), nudge_x = .48, hjust = "right", nudge_y = -.1, vjust = 0, size = 4, show.legend = F, fontface = "bold") +
#   #geom_errorbar(aes(ymin = CILower95, ymax = CIUpper95), width = .1) +
#   facet_grid(. ~ itemStem) +
#   coord_flip() +
#   labs(x = "Response Type",
#        y = "Iteration number") +
#   scale_fill_manual(values = c("ME" = "#18BC9C", "LE" = "#E74C3C")) +
#   scale_color_manual(values = c("ME" = "#18BC9C", "LE" = "#E74C3C")) +
#   scale_y_continuous(limits = c(0, ceiling(max(allDat$CIUpper95))), breaks = seq(0, ceiling(max(allDat$CIUpper95)), 2)) +
#   guides(fill = FALSE, color = FALSE) +
#   theme_bw() +
#   theme(axis.text = element_text(size = 10),
#         strip.text = element_text(size = 11, face = "bold"))
## Create data frame for mean response times
