###################
# DATA GENERATION #
###################
# Create data for memExample plot based on inputs (Fig 2)
mem <- reactive({
  createMem(nSits = input$nSits, nMems = input$nMems, nFeats = input$nFeats, encFid = input$encFidFig2)
})

# Calculate width and height of memExample plot dynamically
memExDims <- reactive({
  width = (input$nFeats*4)*38
  if (width < 400) {
    width = 400
  }
  height = ((input$nSits*input$nMems)+(input$nSits+1))*30
  return(list("width" = width,
              "height" = height))
})

# Creates episodic memory for HyGene example (Fig 3,4)
memHyG <- eventReactive(c(input$newEpMemFig3, input$newEpMemFig4, input$newEpMemFig5), {
  createMem(nSits = 3, nMems = 5, nFeats = 7, encFid = memSettings$encFid, MVs = c("Situation", "Situation Context"))
}, ignoreNULL = F)

# Creates probe for HyGene example (Fig 3,4)
prbHyG <- eventReactive(c(input$newProbeFig3, input$newProbeFig4, input$newProbeFig5), {
  createProbe(randVec = memHyG()$sitRandVec, epMemory = memHyG()$epMemoryPlot, probeSit = memSettings$probeSit, probeSim = memSettings$probeSim)
}, ignoreNULL = F)

# Compute activation level of epMemory traces for HyGene example and create plotting object (Fig 3,4)
epMemHyGAct <- reactive({
  compAct(memory = memHyG()$epMemory, nSits = memHyG()$nSits, nMems = memHyG()$nMems, nFeats = memHyG()$nFeats, MVs = memHyG()$MVs, probe = prbHyG()$value, Ac = input$AcFig4, ep = T)
})

# Compute activation level of semMemory traces for HyGene example and create plotting object (Fig 5)
semMemHyGAct <- reactive({
  compAct(memory = memHyG()$semMemory, nSits = memHyG()$nSits, nMems = memHyG()$nMems, nFeats = memHyG()$nFeats, MVs = memHyG()$MVs, probe = epMemHyGAct()$unspPrbPlot$value[epMemHyGAct()$unspPrbPlot$rowID == 1], Ac = input$AcFig4, ep = F)
})

# Create SOC for HyGene example (Fig 6)
## Initialize reactive object for SOC
SOC <- reactiveValues(soc = NA,
                      socPlot = NA,
                      socRectNdx = NA,
                      AMin = NA,
                      state = NA,
                      result = NA,
                      samp = NA)
## Initialize counter for retrieval attempts
SOCStepNum <- reactiveValues()
SOCdims <- reactiveValues()

## Create blank WM when "Create new WM" button is pressed
observeEvent(input$newSOC, {
  SOC$soc = matrix(NA, nrow = input$WM, ncol = ((memHyG()$nFeats*length(memHyG()$MVs))+2))
  SOC$socPlot = data.frame("rowID" = rep(1:input$WM, each = (memHyG()$nFeats*length(memHyG()$MVs))),
                           "colID" = rep(1:(memHyG()$nFeats*length(memHyG()$MVs)), input$WM),
                           "value" = NA,
                           "adj" = NA,
                           "sitID" = NA,
                           "trace" = NA)
  SOC$socRectNdx = data.frame("sits" = NA,
                              "xStrt" = NA,
                              "xEnd" = NA,
                              "yStrt" = NA,
                              "yEnd" = NA)
  SOC$AMin = 0
  SOC$state = 0 # SOC$state = 0 -> SOC is blank
  SOCStepNum$k = 0
  SOC$result = NA
  SOC$samp = NA
  SOCdims$height = switch(input$WM,
                          "1" = 240,
                          "2" = 247.5,
                          "3" = 292.5)
  SOCdims$width = switch(input$WM,
                         "1" = 755,
                         "2" = 751,
                         "3" = 770)
}, ignoreNULL = F)

## Update SOC when "Retrieval step" button is pressed
observeEvent(input$SOCstep, {
  if (SOCStepNum$k < input$TMax-1) {
    socOut = updateSOC(soc = SOC$soc, socPlot = SOC$socPlot, socRectNdx = SOC$socRectNdx, semMemory = cbind(1:memHyG()$nSits, semMemHyGAct()$actLvlPlot$value, memHyG()$semMemory), semProbLvl = semMemHyGAct()$actLvlPlot$prob, AMin = SOC$AMin, nSits = memHyG()$nSits, nFeats = memHyG()$nFeats, MVs = memHyG()$MVs)
    SOC$soc = socOut$soc
    SOC$socPlot = socOut$socPlot
    SOC$socRectNdx = socOut$socRectNdx
    SOC$AMin = socOut$AMin
    SOC$state = 1 # SOC$state = 1 -> SOC is populating
    SOC$result = socOut$fail
    SOC$samp = socOut$samp
    if (socOut$fail == T) {
      SOCStepNum$k = SOCStepNum$k + 1
    }
  } else {
    SOC$state = 2 # SOC$state = 2 -> SOC creation complete
    SOCStepNum$k = SOCStepNum$k + 1
  }
})

# Compute echo intensity, mean echo intensity, and probability of SOC traces and create table/plotting objects (Fig 6)
SOCechoInt <- reactive({
  echoInt(noActTr = unique(epMemHyGAct()$trNoActPlot$rowID), socObj = SOC, memory = memHyG()$epMemory, nSits = memHyG()$nSits, nMems = memHyG()$nMems, nFeats = memHyG()$nFeats, MVs = memHyG()$MVs)
})

####################
# UI FUNCTIONALITY #
####################
# Dynamically update input settings for control panels in Figs 3,4,5
## Initialize values for input settings
memSettings <- reactiveValues(Ac = .2,
                              encFid = .85,
                              probeSit = 1,
                              probeSim = .75)
## Update encoding fidelity sliders when new epMem traces are created in Figs 3,4, or 5
observeEvent(input$newEpMemFig3, {
  memSettings$encFid <- input$encFidFig3
  updateSliderInput(session, inputId = "encFidFig4", value = memSettings$encFid)
  updateSliderInput(session, inputId = "encFidFig5", value = memSettings$encFid)
})
observeEvent(input$newEpMemFig4, {
  memSettings$encFid <- input$encFidFig4
  updateSliderInput(session, inputId = "encFidFig3", value = memSettings$encFid)
  updateSliderInput(session, inputId = "encFidFig5", value = memSettings$encFid)
})
observeEvent(input$newEpMemFig5, {
  memSettings$encFid <- input$encFidFig5
  updateSliderInput(session, inputId = "encFidFig3", value = memSettings$encFid)
  updateSliderInput(session, inputId = "encFidFig4", value = memSettings$encFid)
})
## Update probe radio and sliders when new probe is created in Figs 3,4, or 5
observeEvent(input$newProbeFig3, {
  memSettings$probeSit <- input$probeSitFig3
  memSettings$probeSim <- input$probeSimFig3
  updateRadioButtons(session, inputId = "probeSitFig4", selected = memSettings$probeSit)
  updateRadioButtons(session, inputId = "probeSitFig5", selected = memSettings$probeSit)
  updateSliderInput(session, inputId = "probeSimFig4", value = memSettings$probeSim)
  updateSliderInput(session, inputId = "probeSimFig5", value = memSettings$probeSim)
})
observeEvent(input$newProbeFig4, {
  memSettings$probeSit <- input$probeSitFig4
  memSettings$probeSim <- input$probeSimFig4
  updateRadioButtons(session, inputId = "probeSitFig3", selected = memSettings$probeSit)
  updateRadioButtons(session, inputId = "probeSitFig5", selected = memSettings$probeSit)
  updateSliderInput(session, inputId = "probeSimFig3", value = memSettings$probeSim)
  updateSliderInput(session, inputId = "probeSimFig5", value = memSettings$probeSim)
})
observeEvent(input$newProbeFig5, {
  memSettings$probeSit <- input$probeSitFig5
  memSettings$probeSim <- input$probeSimFig5
  updateRadioButtons(session, inputId = "probeSitFig3", selected = memSettings$probeSit)
  updateRadioButtons(session, inputId = "probeSitFig4", selected = memSettings$probeSit)
  updateSliderInput(session, inputId = "probeSimFig3", value = memSettings$probeSim)
  updateSliderInput(session, inputId = "probeSimFig4", value = memSettings$probeSim)
})
## Update activation level thresholod sliders when slider is changed in Figs 4 or 5
observeEvent(input$AcFig4, {
  updateSliderInput(session, inputId = "AcFig5", value = input$AcFig4)
})
observeEvent(input$AcFig5, {
  updateSliderInput(session, inputId = "AcFig4", value = input$AcFig5)
})

# Control panels, warning, visual controls, and summary text for Step 4/Fig 6
## Disable TMax slider for Fig 6 once "Retrieval step" button is pressed
observe({
  if (SOC$state == 1) {disable("TMax")} else {enable("TMax")}
})
## Disable retrieval step button if unspecified probe returns NA or SOC retrieval is completed
observe({
  if (is.factor(epMemHyGAct()$unspPrbPlot$value)) { # if unspPrb is all NA
    disable("SOCstep")
    # output$SOCWarn <- renderText({
    #   "At least one episodic memory trace must exceed the activation level threshold to generate a set of contenders. Return to
    #   Step 2/Figure 4 and try to decrease the activation level threshold, create a new probe, or start over with a new set of memories 
    #   to proceed."
    # })
    } else {
      if (SOC$state == 2) {disable("SOCstep")} else {enable("SOCstep")} # if SOC retrieval is complete
  }
})
## Variable for helping to display SOC before any retrieval attempts have occurred (e.g., all(is.na(SOC$socPlot$value)) == T)
socRowAdj <- reactiveValues()
observe({ 
  if (all(is.na(SOC$socPlot$value))) {
    socRowAdj$a = nrow(SOC$soc)
  } else {
    socRowAdj$a = sum(!is.na(SOC$soc[,1]))
  }
})
## Text for which semMemory trace was sampled in the SOC
output$semSample <- renderText({
  if (!is.na(SOC$result)) {
    colorNdx = switch((memHyG()$nSits+1)-SOC$samp,
                      "1" = "#18BC9C",
                      "2" = "#F39C12",
                      "3" = "#E74C3C")
    paste("The trace in semantic memory for <span style = \"color:", colorNdx,"; font-weight: bold; text-decoration: underline\"> Situation ", (memHyG()$nSits+1)-SOC$samp, "</span> was retrieved.", sep = "")
  }
})
## Text for whether a new trace was added to the SOC on the retrieval attempt
output$SOCResult <- renderText({
  if (is.na(SOC$result)) {
    "Press the \"Retrieval step\" button to begin populating the SOC."
  } else {
    paste(
      "This retrieval attempt was a ",
      if(SOC$result == F) {
        "<span style = \"color: #3498DB; font-weight: bold; text-decoration: underline\">success</span>. A trace was added to the SOC."
      } else {
        "<span style = \"color: #3498DB; font-weight: bold; text-decoration: underline\">failure</span>. A new trace could not be added to the SOC."
      }, sep = "")
  }
})
## Text for number of retrieval attempts taken
output$SOCStep <- renderText({
  if (SOC$state != 2) {
    paste("A total of <span style = \"color: #3498DB; font-weight: bold; text-decoration: underline\">", SOCStepNum$k, "</span> retrieval failures have occurred.", sep = "")
  } else {
    paste("The maximum of <span style = \"color: #3498DB; font-weight: bold; text-decoration: underline\">", SOCStepNum$k, "</span> retrieval failures has been reached.
          To run this process again with a blank SOC, press the \"Create SOC\" button.", sep = "")
  }
})

## Updates which echoIntTab is shown based on which row in the postProb data table is selected
observeEvent(input$postProb_rows_selected, {
  updateTabsetPanel(session, "echoIntTab",
                    selected = c("SOC Trace 1",
                                 "SOC Trace 2",
                                 "SOC Trace 3")[input$postProb_rows_selected])
})
## Text summarizing which trace was selected from sOC
observe({
  if (SOC$state == 2) {
    output$SOCSlct <- renderText({
      paste("In this run, the interpretation (i.e., <span style = \"color: #4682B4; font-weight: bold\"> situation context </span>)
            associated with <span style = \"color:", c("#18BC9C;", "#F39C12;", "#E74C3C;")[as.numeric(levels(SOC$socRectNdx$sits))[SOC$socRectNdx$sits][which.max(SOCechoInt()$postProb)]], 
            "font-weight: bold; text-decoration: underline\"> Situation ", as.numeric(levels(SOC$socRectNdx$sits))[SOC$socRectNdx$sits][which.max(SOCechoInt()$postProb)], 
            "</span> had the highest estimated posterior probability and would be selected as the most plausible interpretation for the
            observed <span style = \"color: #9370DB; font-weight: bold\"> trace probe. </span>", sep = "")
    })
  }
})

# RENDERUI CALLS #
## Step 4/Figure 6: Renders UI and summary text if an unspecified probe was extracted; if not, warning text is displayed
output$SOCUI <- renderUI({
  if (is.factor(epMemHyGAct()$unspPrbPlot$value)) {
    p(icon("exclamation-circle", class = "fa-3x fa-pull-left"),
      "At least one episodic memory trace must exceed the activation level threshold to generate a SOC. Return to
      Step 2 or Step 3 and decrease the activation level threshold, create a new probe, or start over with a new set of memories 
      to proceed.",
      style = "font-size: 20px; font-weight: bold; text-align:left; color:white; background-color:#E74C3C; padding: 10px 15px 10px; border-radius: 5px"
    )
  } else {
    tagList(
      plotOutput("hygStep4", width = paste(SOCdims$width, "px", sep=""), height = paste(SOCdims$height, "px", sep="")),
      htmlOutput("semSample", style = "font-size: 20px"),
      htmlOutput("SOCResult", style = "font-size: 20px"),
      htmlOutput("SOCStep", style = "font-size: 20px"),
      br()
    )
  }
})

## Step 5/Figure 7: Initializes UI with correct number of tabpanels and echo intensity plots based on number of traces in SOC
output$echoIntTabsUI <- renderUI({
  nTabs = nrow(SOC$socRectNdx)
  if (nTabs > 1) { # if more than one trace in SOC, create multiple tabs and corresponding plot objects
    tabList = lapply(1:nTabs, function(x) {
      plotName = paste("memHyGPlot5", x, sep = "_")
      tabPanel(title = (paste("SOC Trace ", x, sep = "")),
               plotOutput(plotName, height = "600px")) # CHANGE DIMENSIONS OF ECHO INTENSITY PLOTS HERE
    })
    do.call(tabsetPanel, c(id = "echoIntTab", tabList))
  } else { # if only one trace in SOC, create single tab and corresponding plot object
    tabsetPanel(tabPanel("SOC Trace1", plotOutput("memHyGPlot5_1", height = "600px")) # CHANGE DIMENSIONS OF ECHO INTENSITY PLOTS HERE
    )
  }
})

## Step 5/Figure 7/Table 1: Renders all results of Step 5 if SOC generation complete; if not, warning text is displayed
output$step5UI <- renderUI({
  if (SOC$state == 2) {
    fluidRow(
      column(width = 5,
        DT::DTOutput("postProb"),
        br(),
        htmlOutput("SOCSlct", style = "font-size: 20px; color:#2C3E50; background-color:#ECF0F1; border: 2px solid #2C3E50; padding: 10px 15px 10px; border-radius: 5px")
      ),
      column(width = 5,
        uiOutput("echoIntTabsUI")
      )
    )
  } else {
    fluidRow(
      column(width = 11, offset = 0.5,
        p(icon("exclamation-circle", class = "fa-3x fa-pull-left"), 
          "The SOC creation must be complete before computing echo intensity. Continue pressing the \"Retrieval step\" button in Step 4
          until the SOC population process is finished.",
          style = "font-size: 20px; font-weight: bold; text-align:left; color:white; background-color:#E74C3C; padding: 10px 15px 10px; border-radius: 5px"
        )
      )
    )
  }
})

#################
# GRAPHS/TABLES #
#################
# Figure 1: Create visual of single example memory trace
output$memTrace <- renderPlot({
  source(file.path("hygeneTab", "Fig1.R"), local = T)
  print(exMemTracePlot)
})

# Figure 2: Create visual of example dynamic episodic & semantic memory
output$memExample <- renderPlot({
  source(file.path("hygeneTab", "Fig2.R"), local = T)
  print(epMemPlot)
})
## Render Figure 2 with dynamically changing plot width/height
output$memExampleUI <- renderUI({
  plotOutput("memExample", width = paste(memExDims()$width,"px",sep=""), height = paste(memExDims()$height,"px",sep=""))
})

# Figure 3: Create visual for showing epMemory trace activation in Step 1 of HyGene
output$hygStep1 <- renderPlot({
  source(file.path("hygeneTab", "Fig3.R"), local = T)
  print(hygStep1)
})

# Figure 4: Create visual for showing epMemory trace activation in Step 2 of HyGene
output$hygStep2 <- renderPlot({
  source(file.path("hygeneTab", "Fig4.R"), local = T)
  print(hygStep2)
})

# Figure 5: Create visual for showing semantic trace activation in Step 3 of HyGene
output$hygStep3 <- renderPlot({
  source(file.path("hygeneTab", "Fig5.R"), local = T)
  print(hygStep3)
})

# Figure 6: Create visual for showing SOC creation in Step 4 of HyGene
observe({
  ### Draws blank SOC (i.e., nothing in the SOC)
  if (all(is.na(SOC$socPlot$value))) { 
    output$hygStep4 <- renderPlot({
      source(file.path("hygeneTab", "Fig6a.R"), local = T)
      print(hygStep4)
    })
    ### Draws SOC once it contains traces
  } else { 
    output$hygStep4 <- renderPlot({
      source(file.path("hygeneTab", "Fig6b.R"), local = T)
      print(hygStep4)
    })
  }
})

# Figure 7: Create visual for echo intensity in Step 5 of HyGene
observe({
  if (SOC$state == 2) { # only create graphs if SOC is completed
    ## Create an output$memHyg5_x for each trace in the SOC
    for (i in 1:nrow(SOC$socRectNdx)) {
      local({ # Local tag ensure that each memHyg5Plot_x object has a unique identifier
        my_i <- i
        plotName = paste("memHyGPlot5", my_i, sep = "_")
        output[[plotName]] <- renderPlot({
          source(file.path("hygeneTab", "Fig7.R"), local = T)
          print(hygStep5)
        })
      })
    }
  }
})

## Table 1: Summary of conditional echo intensities and posterior probabilities in SOC
observe({
  if (SOC$state == 2) {
    output$postProb <- renderDT(
      datatable(data = data.frame("SOC Trace" = 1:nrow(SOC$socRectNdx),
                                  "Conditional Echo Intensity" = SOCechoInt()$meanEchoInt,
                                  "Posterior Probability" = SOCechoInt()$postProb,
                                  check.names = F),
                caption = tags$caption(style = "color: #2C3E50; font-weight: bold; font-style: italic",
                                       "Table 1: Conditional echo intensity and posterior probability of SOC traces"),
                rownames = F,
                selection = list(mode = "single", target = "row", selected = which(c("SOC Trace 1", "SOC Trace 2", "SOC Trace 3") == input$echoIntTab)),
                options = list(searching = F,
                               paging = F, 
                               info = F)
      )
    )
  }
})