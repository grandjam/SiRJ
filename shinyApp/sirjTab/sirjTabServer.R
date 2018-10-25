###################
# DATA GENERATION #
################### 
# Source function for generating UI controls for customizing people
source(file.path("sirjTab", "sirjCustomUIFuncs.R"), local = T)
## Source function for generating Figure 3
source(file.path("sirjTab", "Fig3.R"), local = T)
## Source function for generating Figure 4
source(file.path("sirjTab", "Fig4.R"), local = T)
## Source function for generating Figure 5
source(file.path("sirjTab", "Fig5.R"), local = T)

#~CREATE ENVIRONMENT~#
# Create environment parameters for SiRJ example
## Initialize probability distributions for situations
envSitProb = reactiveValues(sit1 = defaultSit1,
                            sit2 = defaultSit2,
                            sit3 = defaultSit3)
## Initialize environment data
env <- reactiveValues(out = defaultEnv)

## Store environment parameters for easier plotting
envSitDistPlotParams <- reactiveValues(sit1 = .8,
                                       sit2 = .6,
                                       sit3 = .5,
                                       brSitVar = 20)

# Update probability distributions for situations in environment
## Update EACH situation distribution independently when corresponding slider is changed
observeEvent(input$sitProb1, {
  envSitProb$sit1 = sitProbDist(input$sitProb1, input$brSitVar, 5)
})
observeEvent(input$sitProb2, {
  envSitProb$sit2 = sitProbDist(input$sitProb2, input$brSitVar, 5)
})
observeEvent(input$sitProb3, {
  envSitProb$sit3 = sitProbDist(input$sitProb3, input$brSitVar, 5)
})
## Update ALL situation distributionns when variance in distribution slider is changed
observeEvent(input$brSitVar, {
  envSitProb$sit1 = sitProbDist(input$sitProb1, input$brSitVar, 5)
  envSitProb$sit2 = sitProbDist(input$sitProb2, input$brSitVar, 5)
  envSitProb$sit3 = sitProbDist(input$sitProb3, input$brSitVar, 5)
})

## Set environment structure parameters to default when default values button pressed
observeEvent(input$defaultEnv, {
  env$out = defaultEnv
  envSitDistPlotParams$sit1 = .8
  envSitDistPlotParams$sit2 = .6
  envSitDistPlotParams$sit3 = .5
  envSitDistPlotParams$brSitVar = 20
  updateSliderInput(session, "sitProb1",
                    value = envSitDistPlotParams$sit1)
  updateSliderInput(session, "sitProb2",
                    value = envSitDistPlotParams$sit2)
  updateSliderInput(session, "sitProb3",
                    value = envSitDistPlotParams$sit3)
  updateSliderInput(session, "brSitVar",
                    value = envSitDistPlotParams$brSitVar)
})

## Update environment structure with new parameters when create environment button is pressed
observeEvent(input$createEnv, {
  env$out = envStr(envSitProb$sit1, envSitProb$sit2, envSitProb$sit3, input$brSitVar)
  envSitDistPlotParams$sit1 = input$sitProb1
  envSitDistPlotParams$sit2 = input$sitProb2
  envSitDistPlotParams$sit3 = input$sitProb3
  envSitDistPlotParams$brSitVar = input$brSitVar
})


#~CREATE SJT~#
# Create SJT parameters for SiRJ example
## Initialize SJT data
sjtAtt <- reactiveValues(attMat = defaultSJTAtt)

## Basic SJT settings
### Set SJT parameters to default when default values button pressed
observeEvent(input$defaultSJT, {
  sjtAtt$attMat = defaultSJTAtt
})

### Set SJT parameters to random values when random values button pressed
observeEvent(input$randomSJT, {
  sjtAtt$attMat$sitType = as.integer(sample(1:3, 5, replace = T))
  sjtAtt$attMat$sitSpec = as.integer(sample(1:5, 5, replace = T))
  sjtAtt$attMat$rspA = as.integer(sample(1:5, 5, replace = T))
  sjtAtt$attMat$rspB = as.integer(sample(1:5, 5, replace = T))
  sjtAtt$attMat$rspC = as.integer(sample(1:5, 5, replace = T))
  sjtAtt$attMat$rspD = as.integer(sample(1:5, 5, replace = T))
})

## Advanced SJT settings
### Render controls for setting values for individual SJT items when selected from drop-down menu
observeEvent(input$sjtItem, {
  output$sirjCreateSJTUI <- renderUI({
    tagList(
      sirjCreatSJTUIFunc(),
      actionButton(inputId = "createItem",
                   label = paste("Create SJT Item", input$sjtItem, sep = " "),
                   icon = icon("check", class = "fa-fw"),
                   style = "background-color: #18BC9C; border: #18BC9C; margin-bottom: 15px")
    )
  })
})

### Update parameters for a specific SJT item when create item button pressed
observeEvent(input$createItem, {
  sjtAtt$attMat$sitType[as.numeric(input$sjtItem)] = as.integer(input$sitTypeItem)
  sjtAtt$attMat$sitSpec[as.numeric(input$sjtItem)] = as.integer(input$sitSpecItem)
  sjtAtt$attMat$rspA[as.numeric(input$sjtItem)] = as.integer(input$rspAItem)
  sjtAtt$attMat$rspB[as.numeric(input$sjtItem)] = as.integer(input$rspBItem)
  sjtAtt$attMat$rspC[as.numeric(input$sjtItem)] = as.integer(input$rspCItem)
  sjtAtt$attMat$rspD[as.numeric(input$sjtItem)] = as.integer(input$rspDItem)
})

# Create SJT items and response options based on parameters
## Initialize data for storing SJT items and respons options
sjt = reactiveValues(all = defaultSJT,
                     sjtSit = defaultSJT$sjtSit,
                     sjtRsp = defaultSJT$sjtRsp)

## Update all SJT items when buttons to create new environments, create default SJT OR create random SJT are pressed
observeEvent(c(input$defaultEnv, input$createEnv, input$defaultSJT, input$randomSJT), {
  sjt$all = createSJT(sjtSitNdx = sjtAtt$attMat[,2:3],
                      sjtRspLvls = sjtAtt$attMat[,4:7],
                      sitSpecProto = env$out$sitSpecProto,
                      rspExptLvls = env$out$rspExptLvls)
  sjt$sjtSit = sjt$all$sjtSit
  sjt$sjtRsp = sjt$all$sjtRsp
})

## Update specific SJT items when create SJT item button pressed
observeEvent(input$createItem, {
  sjt$all = createSJT(sjtSitNdx = sjtAtt$attMat[,2:3],
                      sjtRspLvls = sjtAtt$attMat[,4:7],
                      sitSpecProto = env$out$sitSpecProto,
                      rspExptLvls = env$out$rspExptLvls)
  sjt$sjtSit[as.numeric(input$sjtItem),] = sjt$all$sjtSit[as.numeric(input$sjtItem),]
  sjt$sjtRsp[[as.numeric(input$sjtItem)]] = sjt$all$sjtRsp[[as.numeric(input$sjtItem)]]
})


#~CREATE PEOPLE~#
# Create person parameters for SiRJ example
## Initialize person data
pplAtt <- reactiveValues(attMat = defaultPplAtt)

## Basic person settings
### Set person parameters to default when default values button pressed
observeEvent(input$defaultPpl, {
  pplAtt$attMat = defaultPplAtt
})

### Set person parameters to have same specified values when same values button pressed
observeEvent(input$sameValPpl, {
  pplAtt$attMat$expt = as.integer(input$exptAll)
  pplAtt$attMat$expLvl = as.integer(input$expLvlAll)
  pplAtt$attMat$METhr = as.numeric(input$METhrAll)
  pplAtt$attMat$LEThr = as.numeric(input$LEThrAll)
  pplAtt$attMat$Ac = as.numeric(input$AcAll)
  pplAtt$attMat$Tmax = as.integer(input$TmaxAll)
})

### Set person parameters to random values when random values button pressed
observeEvent(input$randomPpl, {
  pplAtt$attMat$expt = as.integer(sample(1:5, 5, replace = T))
  pplAtt$attMat$expLvl = as.integer(sample(1:4, 5, replace = T))
  pplAtt$attMat$METhr = round(runif(5, .2, 1), 2)
  pplAtt$attMat$LEThr = round(runif(5, -1, -.2), 2)
  pplAtt$attMat$Ac = round(runif(5, .01, 1), 2)
  pplAtt$attMat$Tmax = as.integer(sample(3:10, 5, replace = T))
})

## Advanced person settings
### Render controls for setting values for individual persons when selected from drop-down menu
observeEvent(input$person, {
  output$sirjCreatePplUI <- renderUI({
    tagList(
      sirjCreatPplUIFunc(),
      actionButton(inputId = "createPerson",
                   label = paste("Create Person", input$person, sep = " "),
                   icon = icon("check", class = "fa-fw"),
                   style = "background-color: #18BC9C; border: #18BC9C; margin-bottom: 15px")
    )
  })
})

### Update parameters for a specific person when create person button pressed
observeEvent(input$createPerson, {
  pplAtt$attMat$expt[as.numeric(input$person)] = as.integer(input$exptPerson)
  pplAtt$attMat$expLvl[as.numeric(input$person)] = as.integer(input$expLvlPerson)
  pplAtt$attMat$METhr[as.numeric(input$person)] = as.numeric(input$METhrPerson)
  pplAtt$attMat$LEThr[as.numeric(input$person)] = as.numeric(input$LEThrPerson)
  pplAtt$attMat$Ac[as.numeric(input$person)] = as.numeric(input$AcPerson)
  pplAtt$attMat$Tmax[as.numeric(input$person)] = as.integer(input$TmaxPerson)
})

# Create memory structure data for SiRJ persons based on parameters
## Initialize data for storing memory structures and distributions
pplMem = reactiveValues(all = defaultPplMem,
                        sitCnt = defaultSitCnt,
                        sitCxtLvlCnt = defaultSitCxtLvlCnt,
                        rspLvlCnt = defaultRspLvlCnt,
                        rspCxtLvlCnt = defaultRspCxtLvlCnt)

## Update memory structures for all people when buttons to create new environments, create default ppl values, create random ppl, OR create same values buttons is pressed
observeEvent(c(input$defaultEnv, input$createEnv, input$defaultPpl, input$randomPpl, input$sameValPpl), {
  pplMem$all = lapply(1:nrow(pplAtt$attMat), function(x) {
    createPersonMem(exptLvl = pplAtt$attMat$expt[x], 
                    expLvl = pplAtt$attMat$expLvl[x],
                    mvLength = env$out$mvLength, 
                    numSitMv = env$out$numSitMv, 
                    numRspMv = env$out$numRspMv, 
                    numSitCxtMv = env$out$numSitCxtMv, 
                    numRspCxtMv = env$out$numRspCxtMv, 
                    numSitType = env$out$numSitType, 
                    numSpecSit = env$out$numSpecSit, 
                    sitSpecProb = env$out$sitSpecProb, 
                    tLen = env$out$tLen, 
                    numExptLvls = env$out$numExptLvls, 
                    sitSpecProto = env$out$sitSpecProto, 
                    sitCxtExptLvls = env$out$sitCxtExptLvls, 
                    rspExptLvls = env$out$rspExptLvls, 
                    rspCxtExptLvls = env$out$rspCxtExptLvls, 
                    numSjtItems = sjt$all$numSjtItems, 
                    sjtSitNdx = sjtAtt$attMat[,c("sitType", "sitSpec")])
  })
  pplMem$sitCnt$Freq = unlist(lapply(lapply(pplMem$all, "[[", "epMemCntVec"), as.vector))
  pplMem$sitCxtLvlCnt[,1:2] = do.call(rbind, lapply(pplMem$all, "[[", "sitCxtLvlCnt"))
  pplMem$rspLvlCnt[,1:2] = do.call(rbind, lapply(pplMem$all, "[[", "rspLvlCnt"))
  pplMem$rspCxtLvlCnt[,1:2] = do.call(rbind, lapply(pplMem$all, "[[", "rspCxtLvlCnt"))
  pplAtt$attMat$epMemSize = as.integer(sapply(pplMem$all, "[[", "epMemSize"))
})

## Update memory structures for a specific person when create person button is pressed
observeEvent(input$createPerson, {
  pplMem$all[[as.numeric(input$person)]] = createPersonMem(exptLvl = pplAtt$attMat$expt[as.numeric(input$person)], 
                                                           expLvl = pplAtt$attMat$expLvl[as.numeric(input$person)],
                                                           mvLength = env$out$mvLength, 
                                                           numSitMv = env$out$numSitMv, 
                                                           numRspMv = env$out$numRspMv, 
                                                           numSitCxtMv = env$out$numSitCxtMv, 
                                                           numRspCxtMv = env$out$numRspCxtMv, 
                                                           numSitType = env$out$numSitType, 
                                                           numSpecSit = env$out$numSpecSit, 
                                                           sitSpecProb = env$out$sitSpecProb, 
                                                           tLen = env$out$tLen, 
                                                           numExptLvls = env$out$numExptLvls, 
                                                           sitSpecProto = env$out$sitSpecProto, 
                                                           sitCxtExptLvls = env$out$sitCxtExptLvls, 
                                                           rspExptLvls = env$out$rspExptLvls, 
                                                           rspCxtExptLvls = env$out$rspCxtExptLvls,
                                                           numSjtItems = sjt$all$numSjtItems, 
                                                           sjtSitNdx = sjtAtt$attMat[,c("sitType", "sitSpec")])
  pplMem$sitCnt$Freq[pplMem$sitCnt$ID == as.numeric(input$person)] = as.vector(pplMem$all[[as.numeric(input$person)]]$epMemCntVec)
  pplMem$sitCxtLvlCnt$Freq[pplMem$sitCxtLvlCnt$ID == as.numeric(input$person)] = pplMem$all[[as.numeric(input$person)]]$sitCxtLvlCnt$Freq
  pplMem$rspLvlCnt$Freq[pplMem$rspLvlCnt$ID == as.numeric(input$person)] = pplMem$all[[as.numeric(input$person)]]$rspLvlCnt$Freq
  pplMem$rspCxtLvlCnt$Freq[pplMem$rspCxtLvlCnt$ID == as.numeric(input$person)] = pplMem$all[[as.numeric(input$person)]]$rspCxtLvlCnt$Freq
  pplAtt$attMat$epMemSize[pplAtt$attMat$ID == as.numeric(input$person)] = as.integer(pplMem$all[[as.numeric(input$person)]]$epMemSize)
})

#~Run SiRJ~#
# Create data using SiRJ
## Initialize data for storing SiRJ results
sirjRslt <- reactiveValues()

# Run SiRJ when button pressed
observeEvent(input$runSirj, {
  # Progress bar for tracking people
  progressPpl = Progress$new() # Initialize progress bar
  progressPpl$set(message = "Taking SJT...", value = 0) # Set progress bar to start at 0 and display default message
  on.exit(progressPpl$close()) # Close progress bar when complete
  
  # Function for updating progress bar
  updateProgressPpl <- function(value = NULL, detail = NULL, ndx, total) {
    if (is.null(value)) {
      value = ndx/total
    }
    progressPpl$set(value = value, detail = detail)
  }
  ## Take SJT for each person and store results
  sirjRslt$all = lapply(1:nrow(pplAtt$attMat), function(x) {
    if (is.function(updateProgressPpl)) {
      updateProgressPpl(detail = paste("Person", x, sep = " "), ndx = x, total = nrow(pplAtt$attMat))
    }
    takeSJT(exptLvl = pplAtt$attMat$expt[x], 
            expStrength = pplAtt$attMat$expLvl[x],
            mvLength = env$out$mvLength,
            numSitMv = env$out$numSitMv,
            numSitCxtMv = env$out$numSitCxtMv,
            numRspMv = env$out$numRspMv,
            numRspCxtMv = env$out$numRspCxtMv,
            tLen = env$out$tLen,
            numSjtItems = sjt$all$numSjtItems,
            numSjtRsp = sjt$all$numSjtRsp,
            sjtSit = sjt$sjtSit,
            sjtRsp = sjt$sjtRsp,
            epMemory = pplMem$all[[x]]$epMemory,
            semMemory = pplMem$all[[x]]$semMemory,
            MESim = pplAtt$attMat$METhr[x],
            LESim = pplAtt$attMat$LEThr[x],
            Ac = pplAtt$attMat$Ac[x],
            Tmax = pplAtt$attMat$Tmax[x],
            sjtEval = pplMem$all[[x]]$sjtEval)
  })
  
  # Process sjtEval data to create data tables for displaying
  sirjRslt$sjtEval = sirjSJTEval(dat = sirjRslt$all, pplAtt = pplAtt$attMat)
  
  # Process rspEval data to create data suitable for plotting
  sirjRslt$rspEval <- sirjRspEval(dat = sirjRslt$all, Tmax = pplAtt$attMat$Tmax, sjtEval = sirjRslt$sjtEval)
  
  # Create rspEval plotting objects
  sirjRslt$rspEvalPlots <- sapply(1:5, function(x) {
    sapply(1:5, function(y) {
      rspEvalPlot(dat = sirjRslt$rspEval[sirjRslt$rspEval$person == x & sirjRslt$rspEval$item == y,], 
                  personNdx = personNdx[x,y], 
                  itemNdx = y, 
                  METhr = pplAtt$attMat$METhr[x],
                  LEThr = pplAtt$attMat$LEThr[x])
    }, simplify = F)
  }, simplify = F)
  
  ## Create legend for rspEval ggplot objects
  sirjRslt$leg = rspEvalLegend(sirjRslt$rspEval[sirjRslt$rspEval$person == 1 & sirjRslt$rspEval$item == 1,])
  
  ## Create combined plots
  sirjRslt$combinedPlots <- lapply(1:5, function(x) {
    grid_arrange_shared_legend(plotLeg = sirjRslt$leg, plots = sirjRslt$rspEvalPlots[[x]], position = "right") # Creates plots for person 1
  })
})

# Render UI for SiRJ results tabs
output$sirjResultsUI <- renderUI({
  if (is.null(sirjRslt$sjtEval)) {
    p(icon("check-circle", class = "fa-fw"),
      "Press \"Run SiRJ\" button at right to generate data.",
      style = "font-size: 20px; font-weight: bold; text-align:left; color:white; background-color:#F39C12; padding: 10px 15px; border-radius: 5px"
    )
    #htmlOutput("sirjWarn", style = "font-size: 20px; color: #E74C3C; font-weight: bold")
  } else {
    tabsetPanel(id = "sirjResults",
      tabPanel(title = "Summary Table",
        br(),
        p("Table 3: Summary of response selections", style = "font-weight: bold; font-style: italic"),
        dataTableOutput("sirjSummary"),
        br(),
        p(strong("Table 3"), "provides a summary of the response selections made by each simulated individual to the specified SJT items from the current
          simulation run, as well as person and item characteristics of interest. Columns can be sorted by clicking the arrows next to each column heading 
          or filtered using the search boxes provided at the top of a column.")
      ),
      tabPanel(title = "Response selection distribution",
        br(),
        tagList(
          plotOutput("sirjRspDistPlot"),
          radioButtons(inputId = "barDisplay",
                       label = "Group data by:",
                       choices = c("Expertise level" = 1,
                                   "Experience strength" = 2,
                                   "Person" = 3),
                       selected = 1,
                       inline = T),
          p(strong("Figure 3"), "shows a breakdown of the number of simulated people who selected each response option as the most effective (ME) and least 
            effective (LE) response for each SJT item. The coloring of the stacked bars can be changed using the radio buttons below the figure to 
            help visualize how the expertise level and experience strength of individuals influences the distribution of response option selections.")
        ),
        wellPanel(
          div(
            h4("What to note:", style = "font-weight: bold"),
            tags$ul(#style = "font-size: 14px",
              tags$li("Simulated individuals with", strong("similar expertise levels are generally more likely to make the same ME and LE selections"), "in SiRJ. 
                      For example, running a simulation in which all five indivduals have the same expertise level typically results in a higher degree of agreement
                      regarding ME and LE option selections than when all five individuals have different expertise levels. In this regard, SiRJ
                      predicts that individuals with similar experiential-/knowledge-bases should tend to \"think similarly\" about the material presented on 
                      SJT items."),
              tags$li("The", strong("experience strength (and thus the episodic memory size) of individuals tends to have little effect"), "regarding which 
                      options are selected. In conjunction with the observation above, SiRJ thus predicts that the \"amount\" of history/experience an individual 
                      has is not likely to be a driving factor with respect to how SJT items are processed and responses selected.")
            )
          ), style = "padding:2px 15px; color:#E74C3C"
        )
      ),
      tabPanel(title = "Response time",
        br(),
        plotOutput("sirjTimePlot"),
        p(strong("Figure 4"), "plots the average number of times individuals in the current simulation run iterated through the item evaluation process in SiRJ
          before identifying their most effective (ME) and least effective (LE) response selections on each SJT item. Although SiRJ does not produce true response
          times in terms of the number of seconds required to make response selections, more iterations through the item evaluation process can be assumed to correspond
          with a simulated individual spending a longer amount of time deciding which response selections to make. The green and red bars in the figure reflect the 
          amount of time elapsed before the ME and LE responses were chosen, respectively; the error bars correspond to the 95% confidence interval associated with 
          these times. The data shown in the chart labeled \"Overall\" is the time required to make final response selections averaged across all people and all items, 
          whereas the data shown in the charts labeled \"Person 1\" through \"Person 5\" is the time each person requred to respond averaged across all items."
        ),
        wellPanel(
          div(
            h4("What to note:", style = "font-weight: bold"),
            tags$ul(#style = "font-size: 14px",
              tags$li("In general, the simulation results tend to predict that", strong("identifying the ME response to an SJT item takes less time/cognitive processing effort
                      than identifying the LE response."), "Note that the small number of individuals and test items included in the current application often results
                      in overlapping confidence intervals; when the number of individuals and test items simulated are increased, the intervals are typically much
                      smaller."),
              tags$li("The above pattern is attributable to the nature of the memory structures and response comparison processes operationalized in SiRJ. 
                      Consistent with HyGene, episodic memory is assumed to be a record of people's experiences, interpretations, and actual responses to events faced 
                      in the environment, whereas semantic memory is conceptualized as an aggregate (i.e., prototype) of these encoded situation-response-interpretation 
                      traces in SiRJ. Furthermore, the item evaluation process in SiRJ proposes that people make judgments of similarity to determine the extent to 
                      which SJT response options are likely to be effective (i.e., \"similar to what I've done in the past\") versus ineffective (\"dissimilar to 
                      what I've done in the past\"). As a result, unless a response option is available for an SJT item that is a direct \"opposite\" reaction to 
                      how a test taker has responded in the past, it will tend to take longer for the individual to determine what response is least effective."),
              tags$li("It is important to note that the above pattern occurs", em("on average."), strong("It is not uncommon to observe a single simulated test taker 
                      identifying the LE option as or more quickly than the ME option for a particular item."), "Furthermore, there are certain circumstances under 
                      which this pattern of responding can fail to emerge even in the aggregate. For example:",
                tags$ul(
                  tags$li(strong("If the threshold for identifying the LE response is set much lower than that of the ME response,"), "it is possible to observe 
                          the opposite pattern of results (e.g., LE response identified more quickly than ME response). This occurs because simulated 
                          individuals require less evidence to conclude which response option is ineffective and thus have the potential to reach a 
                          conclusion after less delibration."),
                  tags$li("ME and LE responding tend to require about the same amount of time to select", strong("if the expertise level of the simulated test takers 
                          is not well represented in the response options provided on the SJT"), "(e.g., test takers are all expertise level 1, response options are 
                          all consistent with expertise level 5). This occurs because test takers tend to have an equally difficult time identifying the single-most
                          ME or LE option given that none of the options represent their encoded experiences."),
                  tags$li(strong("In cases where multiple response options appear equally effective to a respondent,"), "identifying the single best ME response can be 
                          more time consuming. This scenario is difficult to recreate in the current simulations given the parameterization options available; however, 
                          it is most likely to occur when SJT items possess more than one response option that is consistent with the test taker's expertise level and 
                          only one response option that is highly dissimilar.")
                )
              )
            )
          ), style = "padding:2px 15px; color:#E74C3C"
        )
      ),
      tabPanel(title = "Response option evaluations",
        br(),
        tagList(
          plotOutput("rspEvalPlot"),
          radioButtons(inputId = "pplRspEvalSlct",
                       label = "Show person:",
                       choices = c("Person 1" = 1,
                                   "Person 2" = 2,
                                   "Person 3" = 3,
                                   "Person 4" = 4,
                                   "Person 5" = 5),
                       selected = 1,
                       inline = T),
          p(strong("Figure 5"), "presents a visualization of how the evidence accumulation process unfolded over time for simulated test takers while making their
            most effective (ME) and least effective (LE) response decisions for each SJT item in the current simulation run. The y-axis of each graph displays the 
            cumulative similarity judgment of response options and the x-axis displays the number of iterations through SiRJ's item evaluation procedure (i.e., time). 
            Each line presents the trajectory of the cumulative similarity judgment for response options over time (note that in instances where different response 
            options are evaluated identically, the lines on the figure will overlap and thus can be difficult to distinguish). The response options are 
            differentiated by shape, with the option ultimately selected as ME highlighted and labeled in green and the option ultimately selected as LE 
            highlighted and labeled in red. The solid horizontal lines shown in each plot signify the location of the person's threshold for identifying the ME 
            (upper line) and LE (lower line) responses. Lastly, the radio buttons provided below the figure can be used to select which simulated individual's data 
            is displayed in the figure."
          ),
          wellPanel(
            div(
              h4("What to note:", style = "font-weight: bold"),
              tags$ul(#style = "font-size: 14px",
                tags$li("Note that the evidence accumulation for some responses appears more \"linear\" whereas for other response options it is more 
                        \"jagged\"/fluctuating.", strong("The nature of this accumulation process is attributable to the nature of the memories stored by an individual 
                        and the response comparison process in SiRJ."), "Recall that SiRJ proposes individuals formulate and retreive interpretations about an SJT 
                        item by first generating and then selecting from a plausible set of contenders derived from semantic memory (see 
                        Step 4 on \"HyGene\" tab). In cases where an individual possesses only one (or only one very strong) interpretation of an SJT situation or
                        its response options, they will (nearly) always select and use this interpretation when evaluating the effectiveness of the available
                        response options. Using this same point of reference results in the more ballistic-/linear-appearing accumulation of similarity judgments.
                        However, in cases where the simulated individual produces multiple plausible interpretations for the situation and options presented for an
                        SJT item, they may not always select the same interpretation and thus may evaluate the available responses to an SJT item using a different
                        experiential point of reference. Using different interpretations as the point of comparison for evaluating the effectiveness of an SJT option
                        produces the observed nonlinear growth patterns as the relative effectveness of the response is judged differentially."),
                tags$li("Although it does not occur on every simulation run,", strong("it is possible to observe that the response option which surpasses a response 
                        threshold first is not ultimately selected as the ME or LE response."), "This is explained in the text above under the section 
                        labeled\"Item evaluation and 'changing your mind'.\"")
              )
            ), style = "padding:2px 15px; color:#E74C3C"
          )
        )
      )
    )
  }
})

#################
# GRAPHS/OUTPUT #
#################
# Table 1: SJT parameters
output$sjtAttMat <- renderTable(
  data.frame("Item Number" = sjtAtt$attMat$item,
             "Broad Situation Class" = sjtAtt$attMat$sitType,
             "Specific Situation Type" = sjtAtt$attMat$sitSpec,
             "Situation ID" = interaction(sjtAtt$attMat$sitType, sjtAtt$attMat$sitSpec),
             "Response A Expertise Level" = sjtAtt$attMat$rspA,
             "Response B Expertise Level" = sjtAtt$attMat$rspB,
             "Response C Expertise Level" = sjtAtt$attMat$rspC,
             "Response D Expertise Level" = sjtAtt$attMat$rspD,
             check.names = F),
  caption.placement = "top",
  align = "c",
  striped = T,
  hover = T,
  bordered = T
)

# Table 2: Person parameters
output$pplAttMat <- renderTable(
  data.frame("Person" = pplAtt$attMat$ID,
             "Expertise Level" = pplAtt$attMat$expt,
             "Experience Strength" = pplAtt$attMat$expLvl,
             "ME Threshold" = pplAtt$attMat$METhr,
             "LE Threshold" = pplAtt$attMat$LEThr,
             "Activation level threshold" = pplAtt$attMat$Ac,
             "Motivation" = pplAtt$attMat$Tmax,
             "Episodic Memory Size" = pplAtt$attMat$epMemSize,
             check.names = F),
  align = "c",
  striped = T,
  hover = T,
  bordered = T
)

# Table 3: SiRJ summary table
observeEvent(input$runSirj, {
  output$sirjSummary = renderDT(
    formatStyle(table = datatable(data = data.frame("Person" = sirjRslt$sjtEval$person,
                                                    "Person Expertise Level" = rep(pplAtt$attMat$expt, each = sjt$all$numSjtItems),
                                                    "SJT Item" = sirjRslt$sjtEval$sjtItem,
                                                    "SJT Situation ID" = sirjRslt$sjtEval$sitID,
                                                    "ME Response" = as.character(sirjRslt$sjtEval$meRspSlct),
                                                    "LE Response" = as.character(sirjRslt$sjtEval$leRspSlct), 
                                                    check.names = F),
                                  rownames = F,
                                  filter = "top",
                                  options = list(dom = '<"top">lrt<"bottom">ip', 
                                                 columnDefs = list(list(className = "dt-center", targets = "_all")))
                                  ),
    columns = 1:6, "text-align" = 'center'
    )
  )
})

# Figure 1: Probability distribtion of situations in environment
source(file.path("sirjTab", "Fig1.R"), local = T)
observeEvent(c(input$defaultEnv, input$createEnv), {
  output$envSitProb = renderPlot({
    envSitProb = envSitDistPlot(sitProb = c(envSitDistPlotParams$sit1, envSitDistPlotParams$sit2, envSitDistPlotParams$sit3), brSitVar = envSitDistPlotParams$brSitVar)
    print(envSitProb)
  })
})

# Figure 2a-2d: Distribution of memory trace types for people across expertise level
## Source function for generating Figure 2
source(file.path("sirjTab", "Fig2.R"), local = T)
## Select which person's data to display for each of the memory distribution plots
sitSlct <- reactive({
  personSelect(dat = pplMem$sitCnt, slct = input$sitDistSlct)
})
sitCxtSlct <- reactive({
  personSelect(dat = pplMem$sitCxtLvlCnt, slct = input$sitCxtDistSlct)
})
rspSlct <- reactive({
  personSelect(dat = pplMem$rspLvlCnt, slct = input$rspDistSlct)
})
rspCxtSlct <- reactive({
  personSelect(dat = pplMem$rspCxtLvlCnt, slct = input$rspCxtDistSlct)
})
## Plot distribution of situations
output$sitDist = renderPlot({
  sitDist = memLvlPlotA(dat = sitSlct(), titleText = "situation")
  print(sitDist)
})
## Plot distribution of situation contexts
output$sitCxtLvlDist = renderPlot({
  sitCxtLvl = memLvlPlotB(dat = sitCxtSlct(), titleText = "situation context")
  print(sitCxtLvl)
})
## Plot distribution of responses
output$rspLvlDist = renderPlot({
  rspLvl = memLvlPlotB(dat = rspSlct(), titleText = "response")
  print(rspLvl)
})
## Plot distribution of response context
output$rspCxtLvlDist = renderPlot({
  rspCxtLvl = memLvlPlotB(dat = rspCxtSlct(), titleText = "response context")
  print(rspCxtLvl)
})

# Figure 3: Distribution of response options selected
output$sirjRspDistPlot <- renderPlot({
  sirjRspDistPlot = sirjRspDist(sirjRslt$sjtEval, barDisplay = input$barDisplay)
  print(sirjRspDistPlot)
})

# Figure 4: Comparison of iterations required
output$sirjTimePlot <- renderPlot({
  sirjTimePlot = sirjRspTimeDist(sirjRslt$sjtEval)
  grid.newpage()
  grid.draw(sirjTimePlot)
})

# Figure(s) 5: Response evaluation plots
observeEvent(input$pplRspEvalSlct, {
  output$rspEvalPlot <- renderPlot({
    grid.newpage()
    grid.draw(sirjRslt$combinedPlots[[as.numeric(input$pplRspEvalSlct)]])
  })
})
