library(shiny)
library(shinythemes)
library(shinyjs)
library(ggplot2)
library(grid)
library(gridExtra)
library(DT)
library(tidyr)
library(V8)

###########
# GLOBALS #
###########
# Source functions used in model
## Functions for stepping through & visualizing HyGene
source(file.path("hygeneTab", "hygeneFuncs.R"))
## Functions for running SiRJ
source(file.path("sirjTab", "modelFuncs.R"))
source(file.path("sirjTab", "environInits.R"))
source(file.path("sirjTab", "personInits.R"))
source(file.path("sirjTab", "SJTInits.R"))
source(file.path("sirjTab", "answerSJT.R"))
## Functions for visualizing SiRJ
source(file.path("sirjTab", "sirjFuncs.R"))
# Helper function for scrolling to top of page
jumpTop <- "shinyjs.pageTop = function() {window.scrollTo(0,0);};"

# Source content for each tab
source(file.path("homeTab", "homeTabUI.R"))
source(file.path("hygeneTab", "hygeneTabUI.R"))
source(file.path("sirjTab", "sirjTabUI.R"))

# Create one-time/non-reactive data 
## Example memory trace for HyGene Figure 1
memTrace <- createMem(nSits = 2, nMems = 1, nFeats = 5, encFid = .85)

## Default attributes for environment ecology in SiRJ
defaultSit1 = sitProbDist(.8, 20, 5)
defaultSit2 = sitProbDist(.6, 20, 5)
defaultSit3 = sitProbDist(.5, 20, 5)
defaultEnv = envStr(defaultSit1, defaultSit2, defaultSit3, 20)

## Default attributes for SJT in SiRJ
defaultSJTAtt = data.frame("item" = 1:5,
                           "sitType" = as.integer(c(1,2,3,2,1)),
                           "sitSpec" = as.integer(c(1,1,1,5,3)),
                           "rspA" = as.integer(c(1,2,3,4,5)),
                           "rspB" = as.integer(c(5,4,3,2,1)),
                           "rspC" = as.integer(c(1,1,2,2,3)),
                           "rspD" = as.integer(c(3,3,4,5,5)))
defaultSJT = createSJT(sjtSitNdx = defaultSJTAtt[,2:3], sjtRspLvls = defaultSJTAtt[,4:7], sitSpecProto = defaultEnv$sitSpecProto, rspExptLvls = defaultEnv$rspExptLvls)

## Default attributes for ppl used in SiRJ
### Create table with person parameters
defaultPplAtt = data.frame("ID" = 1:5,
                           "expt" = as.integer(1:5),
                           "expLvl" = as.integer(2),
                           "METhr" = .85,
                           "LEThr" = -.85,
                           "Ac" = .22,
                           "Tmax" = as.integer(8),
                           "epMemSize" = NA)
## Default memory structure for ppl used in SiRJ
### Create memory structures for all people
defaultPplMem = lapply(1:nrow(defaultPplAtt), function(x) {
  createPersonMem(exptLvl = defaultPplAtt$expt[x], 
                  expLvl = defaultPplAtt$expLvl[x], 
                  mvLength = defaultEnv$mvLength, 
                  numSitMv = defaultEnv$numSitMv, 
                  numRspMv = defaultEnv$numRspMv, 
                  numSitCxtMv = defaultEnv$numSitCxtMv, 
                  numRspCxtMv = defaultEnv$numRspCxtMv, 
                  numSitType = defaultEnv$numSitType, 
                  numSpecSit = defaultEnv$numSpecSit, 
                  sitSpecProb = defaultEnv$sitSpecProb, 
                  tLen = defaultEnv$tLen, 
                  numExptLvls = defaultEnv$numExptLvls, 
                  sitSpecProto = defaultEnv$sitSpecProto, 
                  sitCxtExptLvls = defaultEnv$sitCxtExptLvls, 
                  rspExptLvls = defaultEnv$rspExptLvls, 
                  rspCxtExptLvls = defaultEnv$rspCxtExptLvls, 
                  numSjtItems = defaultSJT$numSjtItems, 
                  sjtSitNdx = defaultSJTAtt[,c("sitType", "sitSpec")])
})
### Wrangle distributions of sits, sitCxtLvl, rspLvl, & rspCxtLvl levels for plotting
defaultSitCnt = data.frame("Var1" = as.numeric(rep(paste(rep(1:3, each = 5), ".", rep(1:5, times = 3), sep = ""), 5)),
                           "Freq" = unlist(lapply(lapply(defaultPplMem, "[[", "epMemCntVec"), as.vector)),
                           "ID" = rep(1:nrow(defaultPplAtt), each = 15))
defaultSitCxtLvlCnt = do.call(rbind, lapply(defaultPplMem, "[[", "sitCxtLvlCnt"))
defaultRspLvlCnt = do.call(rbind, lapply(defaultPplMem, "[[", "rspLvlCnt"))
defaultRspCxtLvlCnt = do.call(rbind, lapply(defaultPplMem, "[[", "rspCxtLvlCnt"))
defaultSitCxtLvlCnt$ID = defaultRspLvlCnt$ID = defaultRspCxtLvlCnt$ID = rep(1:nrow(defaultPplAtt), each = 5)

### Add size of epMemory to person table
defaultPplAtt$epMemSize = as.integer(sapply(defaultPplMem, "[[", "epMemSize"))

## Helper index for plotting rspEval
personNdx = matrix(c(1:5, rep(NA, 20)), nrow = 5)

###########
# RUN APP #
###########
shinyApp(
  # UI CODE #
  ui = tagList(
    tags$head(
      tags$script(type = "text/x-mathjax-config",
        HTML(
          "MathJax.Hub.Config({
          TeX: { equationNumbers: { autoNumber: \"AMS\" } }
          });"
        )
      ),
      tags$link(rel = "stylesheet", type = "text/css", href = "footer.css")
    ),
    withMathJax(),
    useShinyjs(), 
    navbarPage(title = "Situated Reasoning & Judgment", id = "mainNavBar", theme = shinytheme("flatly"),
      homeTab,
      hyGeneTab,
      sirjTab
      # Link to OSF page is written at top of SiRJUI file
    ),
    tags$footer(class = "footer",
      extendShinyjs(text = jumpTop),
      div(class = "alignedTable",
        div(class = "row",
          div(class = "cell",
            a(class = "footer", id = "homeFooterLink", "Home"),
            a(class = "footer", id = "hygeneFooterLink", "HyGene"),
            a(class = "footer", id = "sirjFooterLink", "SiRJ"),
            a(class = "footer", id = "osfFooterLink", href = "https://bit.ly/2mFQfc5", target = "_blank", "OSF Page")
          ),
          div(class = "cell alignRight",
            h5(icon("copyright"), "2018, AUTHOR NAME [EMAIL]")
          )
        )
      )
    )
  ),
  
  # SERVER CODE #
  server = function(input, output, session) {
    source(file.path("homeTab", "homeTabServer.R"), local = T)
    source(file.path("hygeneTab", "hygeneTabServer.R"), local = T)
    source(file.path("sirjTab", "sirjTabServer.R"), local = T)
    onclick("homeFooterLink", function() {
      updateNavbarPage(session, "mainNavBar", "Home")
      js$pageTop()
      })
    onclick("hygeneFooterLink", function() {
      updateNavbarPage(session, "mainNavBar", "HyGene")
      js$pageTop()
    })
    onclick("sirjFooterLink", function() {
      updateNavbarPage(session, "mainNavBar", "SiRJ")
      js$pageTop()
    })
  } # Close server function
) # Close shinyapp
