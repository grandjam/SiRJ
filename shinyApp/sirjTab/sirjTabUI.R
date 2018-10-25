sirjTab <-
  tabPanel(title = "SiRJ",
    fluidRow(
      column(width = 11, offset = 0.5,
        h3(icon("angle-double-right"), "Description", style = "font-weight: bold"),
        p("SiRJ is a formal theory and computational model proposing the cognitive processes used by individuals to generate, evaluate, and
          select potential responses to an observed situational stimulus. For purposes of this paper, this general model is applied specifically 
          to describe the psychological processes involved in answering situational judgment tests (SJTs). Specifically, it applies the 
          fundamental judgment and conditional reasoning processes depicted in ",
          a(href = "http://www.damlab.umd.edu/PDF%20articles/2008%20Thomas%2CDougherty%2CSprenger%2CHarbison_PsycReview.pdf", 
            target = "_blank", 
            span("Thomas et al.'s (2008)")
          ),
          "HyGene framework to posit how individuals identify the \"most effective\" and \"least effective\" responses presented
          to them on an SJT item. It is highly recommended to first read through and interact with the applications found on the \"HyGene\" 
          tab on this web page as it provides important information on the foundation of the SiRJ architecture and how it maps to core 
          psychological systems and processes. The purpose of this page is to provide a tool for exploring key input parameters of SiRJ, 
          run the model under different conditions, and better understand how the model operates and the types of predictions it generates."
        )
      )
    ),
    
    h3(icon("angle-double-right"), "Model of Situated Reasoning and Judgment (SiRJ)", style = "font-weight: bold"),
    fluidRow(
      column(width = 6, offset = 0.5,
        p("The intention of SiRJ in the present application is to provide a formal process-level account for how people evaluate 
          and select answers to SJT items containing multiple choice response options. The figure at right provides a visual depiction of the 
          pseudocode and algorithm that constitute the core process mechanisms in SiRJ. These steps and their supporting evidence are described 
          in greater detail in the paper accompanying this interactive application (CITATION REDACTED FOR BLIND REVIEW). A general 
          overview of the model and its functioning is provided here."
        ),
        p("SJT responding is represented as a series of three phases of activity in the SiRJ framework: (I) interpreting an item 
          stem/situation, (II) interpreting the provided response options, and (III) selecting a response. Spanning these phases is a sequence of
          seven cognitive processing activities proposed to represent the judgment and decision-making processes involved 
          when answering an SJT item. These are represented in the figure at right in the numbered white boxes. Steps 1-4 of this process 
          implement the conditional judgment and reasoning operations described in HyGene and represent how individuals generate interpretations 
          relevant to evaluating the item stem and response options presented for an SJT item. Steps 5-7 describe an evidence accumulation 
          and criterion evaluation process to represent how individuals use these interpretations to make a response option choice."
        )
      ),
      
      column(width = 5,
        em(p("Overview of SiRJ architecture. Figure reproduced from CITATION REDACTED FOR BLIND REVIEW.")),
        img(src = 'sirj.png', width = "720px")
      )
    ),
    
    fluidRow(
      column(width = 11, offset = 0.5,
        h4(icon("adjust"), "Step 1: Interpret situation in the SJT item stem", style = "font-weight:bold"),
        p("SiRJ proposes that individuals begin SJT responding by first constructing an appraisal of the needs, demands, and circumstances 
          surrounding the situation presented in an SJT item stem. The situational features of an SJT item serve as 
          conditional information cues that facilitate probing one's learned knowledge to identify \"What is happening here?\" and 
          extract an interpretation for \"Why is this happening?\" This process is carried out computationally by completing an iteration of HyGene
          in which the", strong(span("situation", style = "color:darkslategray")), "described in the SJT item stem serves as the observed",
          strong(span("trace probe", style = "color:mediumPurple")), "used to generate the most plausible interpretation of the", 
          strong(span("situation context", style = "color:steelblue")), "based on the individual's experiences."
        ),
        
        h4(icon("adjust"), "Step 2: Generate a response to situation", style = "font-weight:bold"),
        p("Having identified the perceived situational circumstances and needs, SiRJ proposes that individuals elicit a response from 
          semantic memory that reflects what they have done to address situations with similar demands in the past. This process is represented
          as an iteration of HyGene in which the", strong(span("situation context", style = "color:steelblue")), "identified in the previous
          step serves as the observed", strong(span("trace probe", style = "color:mediumPurple")), "used to generate an individual's 
          preferred", strong(span("response.", style = "color:#CD5555")), "Note that SiRJ proposes decision-makers generate a 
          response", em("even if"), "there are response options from which the test taker will ultimately choose."
        ),
        
        h4(icon("adjust"), "Step 3: Interpret the generated response", style = "font-weight:bold"),
        p("In Step 3 of SiRJ, individuals formulate an interpretation of the consequences and perceived goals accomplished by the generated
          response based on their knowledge and past experiences. Though it is discussed as a separate step in SiRJ for clarity of presentation, 
          this process is proposed to occur simulatenously with Step 2. In other words, respondents are assumed to generate inferences about both 
          \"What action to do?\" and \"What will happen as a result of this action?\" concurrently when evaluating an SJT item stem. To do so,
          the", strong(span("situation context", style = "color:steelblue")), "elicited in Step 1 serves as the observed", 
          strong(span("trace probe", style = "color:mediumPurple")), "used to elicit both a", strong(span("response", style = "color:#CD5555")), 
          "and an interpretation of the associated", strong(span("response context", style = "color:forestgreen")), "during the same iteration of HyGene."
        ),
        
        h4(icon("adjust"), "Step 4: Interpret the available response options", style = "font-weight:bold"),
        p("At this stage of processing, SiRJ proposes that a test taker has generated an interpretation of what is occurring in the situation, 
          what he/she has done in the past to resolve such circumstances, and the results of and reasons for performing that action. 
          The next stages of processing describe how respondents use this information to evaluate and ultimately select which of the potential 
          response options given to them on a multiple choice SJT they believe is the most and least effective response. In Step 4, test takers
          are proposed to generate an interpretation of the consequences and perceived goals accomplished by the given response options for an 
          SJT item based on the action described in the response option. This step thus proposes that individuals evaluate \"What would be the 
          likely outcome if the proposed action was taken?\" for each response option. This is represented in SiRJ by performing separate 
          iterations of HyGene in which decision makers use the", strong(span("response", style = "color:#CD5555")), 
          "presented in a given reponse option as the observed", strong(span("trace probe", style = "color:mediumPurple")), "to determine its", 
          strong(span("response context.", style = "color:forestgreen"))," Thus, for an SJT item with four potential response 
          options, the HyGene algorithm is implemented four times (one for each option) during this step to generate response interpretations."
        ),
        
        h4(icon("adjust"), "Step 5: Compare generated response to available response options", style = "font-weight:bold"),
        p("The next cognitive processing activity represented in SiRJ proposes that individuals compare their interpretation of the outcomes
          achieved by their self-generated response from Step 3 to the predicted outcomes of the available response options produced in Step 4.
          Step 5 thus posits that individuals examine each response option and evaluate \"Is the expected outcome of this response option 
          similar to the expected outcome of the action I would do?\" In this sense, SiRJ assumes that individuals do not directly compare the 
          behaviors/actions represented in the self-generated and provided", strong(span("responses,", style = "color:#CD5555")), "but rather their",
          strong(span("response contexts.", style = "color:forestgreen")), "Computationally, this is operationalized by computing the cosine similarity between
          the", strong(span("response context", style = "color:forestgreen")), "minivectors of the generated response produced in Step 3 and given
          response options produced in Step 4. It is assumed that a given response option whose perceived outcome more closely resembles that of the 
          individual's generated response for a given", strong(span("situation context", style = "color:steelblue")),
          "will be perceived as both more similar and more preferable than a response option whose perceived outcomes are believed to differ."
        ),
        
        h4(icon("adjust"), "Step 6: Evaluate response option choices", style = "font-weight:bold"),
        p("In Step 6, the similarity judgments between the", strong(span("response contexts", style = "color:forestgreen")), "for the generated and given
          responses are compared against an internal threshold representing the strength of evidence required by a respondent to conclude that a potential
          response option should be selected. Because the cosine similarity is used to compute the similarity between generated and given", 
          strong(span("response contexts", style = "color:forestgreen")), "in Step 5, a positive value for a given response option indicates that it is
          more similar to the types of responses used by the decision maker in the past while a negative value indicates it is less similar (see description of 
          Equation 1 in \"HyGene\" tab for more info on interpreting cosine similarity). Consequently, a positively valenced threshold (e.g., .85) is used to 
          represent the evidence criterion for selecting the most effective response while a negatively valenced threshold (e.g., -.85) is used as the criterion 
          for selecting the least effective response. Increasing (decreasing) the magnitude of the threshold represents increasing (decreasing) how much
          evidence the decision maker requires in order to endorse a particular", strong(span("response.", style = "color:#CD5555"))
        ),
        
        h4(icon("adjust"), "Step 7: Continue evaluating or select response option", style = "font-weight:bold"),
        p("The final step in SiRJ concerns how test takers decide whether they have identifed the response option they wish to endorse. There are four 
          possible results of the evaluation process carried out in the previous Step 6:",
          tags$ol(type = "A",
            tags$li("Only one response option exceeds a threshold"),
            tags$li("Multiple response options exceed a threshold, but the perceived effectiveness/ineffectiveness of one exceeds the others"),
            tags$li("Multiple response options exceed a threshold, and they have the same perceived effectiveness/ineffectiveness"),
            tags$li("No response options exceed a threshold")
          ),
          "These criteria are applied independently in determining if both a most effective and least effective response has been identified.
          If outcome (A) is observed, then the test taker chooses the single response option that exceeds threshold. If outcome (B) is observed, SiRJ proposes
          that tht test taker will choose the response option with the stronger evidence (i.e., highest similarity in the case of the most effective response, 
          lowest in the case of the least effective response). If either outcomes (C) or (D) are observed, SiRJ proposes that individuals will re-evaluate the 
          SJT item stem and response options and generate further evidence of what option to select. If the individual engages in further evaluation, he/she is 
          assumed to return to the start of the SiRJ process and repeat Steps 1-6. That is, the decision maker will re-evaluate the SJT item stem to potentially 
          generate new/different interpretations of the situation and accompanying responses from experience, re-evaluate the provided response options to 
          potentially generate new interpretations of the provided actions, and finally compare the generated and given response contexts again. It is 
          assumed that the decision maker does not ignore or dismiss the outcomes of their previous evaluations; rather evidence ", em("accumulates"), 
          "in the form of cumulative similarity perceptions as the evaluation process is iterated. Specifically, the cosine similarity values computed in 
          Step 5 of SiRJ are added together at each iteration and compared against the threshold during Step 6. Notably, this mechanism allows a decision maker 
          to potentially reach a selection even if all the response options are perceived as poor matches if given enough time to evaluate."
        ),
        p("Lastly, SiRJ includes a \"motivation\" parameter that controls the number of times an individual is willing/able to engage in the item evaluation process.
          If the motivation limit is reached and either outcomes (C) or (D) are still true, the test taker is assumed to randomly select one of the deadlocked 
          responses (in the case of outcome (C)) or pick the response option that came closest to threshold (in the case of outcome (D)). The evaluation process 
          for a single item concludes when both a most and least effective response are identified, whether a result of one clear winner emerging for both 
          responses or the motivation limit forcing an individual to choose a response is reached."
        ),
        
        h3(icon("angle-double-right"), "Important Model Notes", style = "font-weight: bold"),
        h4(icon("adjust"), "Item evaluation and \"changing your mind\"", style = "font-weight:bold"),
        p("It is possible that an individual may reach the evidence threshold for identifying the most and least effective responses at different times in SiRJ. 
          For example, the accumulated evidence for response option \"A\" may exceed the most effective response threshold for a decision maker after only 
          two iterations of SiRJ though no other options have yet surpassed the least effective response threshold. In such cases (and consistent with Step 
          7 above), the test taker will continue to evaluate the item stem and accumulate evidence about their interpretation of the response options. 
          A potential consequence of this is that a decision maker may \"change their mind\" about a response option they initially thought was the most or least 
          effective as a result of the additional time spent in consideration. Returning to the previous example, although response option \"A\" would have 
          been chosen as the most effective response had the decision maker stopped evaluating the item after two iterations of SiRJ, because the person had 
          not yet identified the least effective response and thus continued evaluating the item, the accumulated evidence for the perceived effectiveness of 
          a different response option (e.g., response option \"B\") may surpass that of response option \"A\" and result in the person perceiving that 
          response option as the most effective."
        ),
        h4(icon("adjust"), "Expertise versus experience", style = "font-weight:bold"),
        p("A key proposition of SiRJ is that differences in SJT response selection result from differences in the experiences and knowledge 
          stored in test takers' memory which, through the processes highlighted above, result in different interpretations of the situations and 
          responses presented for an SJT item. The SiRJ framework uses two parameters to initialize between-person differences in the knowledge and 
          experiences stored in memory. The first parameter,", strong(em("experience strength,")), "is a scaling factor used to influence the size of 
          a simulated test taker's episodic memory. A higher value for experience strength is intended to represent an individual with more history 
          in a domain and thus more traces stored in episodic memory. In contrast, the second parameter,", strong(em("expertise level,")), "is used to represent 
          qualitative differences in the interpretations and responses stored in an individual's memory. Expertise level is operationalized as a nominal 
          variable in SiRJ in which different values represent categorically different \"types\" of expertise with respect to a job domain. For example, 
          a complete novice in a domain might be assigned one expertise level (i.e., expertise = 1) while someone with some exposure or training 
          relevant to the domain might be assigned a different expertise level (i.e., expertise = 3). Functionally, expertise levels correspond to 
          different", HTML(paste(strong(span("situation context", style = "color:steelblue")), ",", sep = "")), 
          HTML(paste(strong(span("response", style = "color:#CD5555")), ",", sep = "")), "and", strong(span("response context", style = "color:forestgreen")), 
          "minivectors in SiRJ. In other words, the features comprising these minivectors are different for different expertise levels. Currently, the SiRJ 
          framework is agnostic to the meaning and nature of expertise levels; no attempt is made to define or specify what distinguishes one 
          expertise level from another. However, the SiRJ framework offers the flexibility to specify more nuanced methods for differentiating
          qualitatively different memory and knowledge representations should it be desirable."
        )
      )
    ),
    
    fluidRow(
      column(width = 11, offset = 0.5,
        h4("Try it out!", style = "color: white; background-color:#18BC9C; padding:4px; border-radius:5px; font-weight:bold"),
        p("The interactive application below allows you to run a small-scale simulation of SiRJ model with five test takers 
          answering five SJT items. The first three tabs presented at the top of the application labeled \"Create Environment,\" \"Create SJT,\" 
          and \"Create People\" provide controls for manipulating the input parameters associated with each of these domains.The final tab labeled 
          \"Run SiRJ\" can be used to run SiRJ using the selected input parameters; once a simulation is run, a summary of the results 
          generated by the model is also displayed. Lastly, descriptions of the information summarized on each tab, key computations, and the 
          input parameters that can be manipulated are provided in each of the tabs."
        ),
       
        tabsetPanel(id = "SiRJ",
          tabPanel(title = "Create Environment",
            br(),
            sidebarLayout(
              sidebarPanel(id = "envPanel", width = 4, # sidebarpanel for creating environment
                br(),
                actionButton(inputId = "defaultEnv",
                             label = "Create default environment",
                             icon = icon("refresh", class = "fa-fw"),
                             style = "background-color: #18BC9C; border: #18BC9C; margin-bottom: 15px"),
                hr(style = "border-top: 1px solid #2C3E50; margin-top: 2px; margin-bottom: 10px"),
                lapply(1:3, function(x) {
                  sliderInput(inputId = paste("sitProb", x, sep = ""), 
                              label = paste("Probability of broad situation class", x, "in environment", sep = " "),
                              min = 0, max = 1, value = c(.8, .6, .5)[x], step = .01)
               }),
               sliderInput(inputId = "brSitVar",
                           label = "Scaling factor for variability of situation probability distribution",
                           min = 10, max = 100, value = 20, step = 1),
               actionButton(inputId = "createEnv",
                            label = "Create custom environment",
                            icon = icon("refresh", class = "fa-fw"),
                            style = "background-color: #18BC9C; border: #18BC9C; margin-bottom: 15px")
              ),
              mainPanel( # main panel for creating environment
                plotOutput("envSitProb"),
                p("The SiRJ simulations allow one to represent how rarely or commonly situations occur in the environment and, therefore, the
                  likelihood that any individual person will experience and contain memories of these events. As described on the
                  \"HyGene\" tab, the SiRJ simulations presented here and run for the paper operationalized", strong("three broad situations"), 
                  "each with", strong("five classes of specific situations"), "for a total of 15 possible situations in the simulated environment.
                  The sliders at left provide controls for manipulating the probability sampling distributions for specific situations in the 
                  environment as well as a single slider for influencing the variability of the distributions."
                ),
                p("To initialize the environment, probability distributions are generated for each broad situation class that define how likely
                  specific situations within that broad class of situations are to be present in the environment. These distribution are specified as \\(beta(p_{b}*varScl, 1-p_{b}*varScl)\\) 
                  distributions, where \\(p_{b}\\) is the mean probability of broad situation class \\(b\\) and \\(varScl\\) is a scaling factor for 
                  influencing the variability of the distribution (note that a larger \\(varScl\\) value results in more restricted variances). These values can be changed using the respective sliders at left. 
                  The beta distributions created for each broad situation class are subsequently used as sampling distributions for determining the prevalence 
                  of related specific situations in the environment and the frequency with which these events have been experienced and encoded in a person's
                  episodic memory (more detail on this process can be found by navigating to the \"Create People\" tab and selecting \"Situations\" from the 
                  \"Episodic memory distribution\" drop-down menu). For example, the probabilities of the five specific situations associated with 
                  broad situation class 1 in the environment are determined by sampling five values from the beta distribution specified for 
                  broad situation class 1 currently shown in Figure 1. Though the present example is restricted to three broad situations each 
                  containing five specific situations, this operationalization provides a convenient way for generating environmental ecologies for 
                  as many situations one would like to have represented in SiRJ."
                ),
                p(strong("Figure 1"), "provides a visual depiction summarizing the probability sampling distributions for situations based 
                on the input parameters selected. Pressing the \"Create default environment\" button will set the input paramters to the default 
                settings used in the SiRJ simulations in the paper, while pressing the \"Create custom environment\" button will set the input 
                parameters to those currently selected with the sliders."
                )
              )
            )
          ),
          tabPanel(title = "Create SJT",
            br(),
            sidebarLayout(
              sidebarPanel(width = 4, # sidebarpanel for creating SJT
                tabsetPanel(id = "SJTParams",
                  tabPanel(title = "Basic",
                    br(),
                    actionButton(inputId = "defaultSJT",
                                 label = "Create default SJT items",
                                 icon = icon("refresh", class = "fa-fw"),
                                 style = "background-color: #18BC9C; border: #18BC9C; margin-bottom: 15px"),
                    br(),
                    actionButton(inputId = "randomSJT",
                                 label = "Create random SJT items",
                                 icon = icon("refresh", class = "fa-fw"),
                                 style = "background-color: #18BC9C; border: #18BC9C; margin-bottom: 15px")
                  ),
                  tabPanel(title = "Advanced",
                    br(),
                    selectInput(inputId = "sjtItem",
                                label = "Select SJT item to create:",
                                choices = c("Item 1" = 1,
                                            "Item 2" = 2,
                                            "Item 3" = 3,
                                            "Item 4" = 4,
                                            "Item 5" = 5)),
                    uiOutput("sirjCreateSJTUI")
                  )
                )
              ),
              mainPanel( # main panel for creating SJT
                br(),
                p("Table 1: Summary of SJT attributes", style = "font-weight: bold; font-style: italic"),
                tableOutput("sjtAttMat"),
                p("SJT items are specified in SiRJ by defining the type of situation presented in an item stem and the corresponding response options. 
                  Item stem situations are operationalized as a", strong(span("situation", style = "color:darkslategray")), "minivector like those 
                  represented in an individual's memory (see Memory Trace section on the \"HyGene\" tab", HTML(paste(").", span("*", style = "color:orangered"), sep = "")),
                  "The type of situation described in an item stem is determined by specifying a broad and specific situation for each item. Similarly, the response options for an item are 
                  operationalized as", strong(span("response", style = "color:#CD5555")), "minivectors like those represented in an individual's memory. 
                  The type of response options available for an item are determined by specifying the expertise level most closely
                  associated with that response for the given situation. For example, specifying expertise level = 1 for Response Option A on SJT item 1 
                  means that the first response option for the first SJT item will contain features similar to the types of responses an individual with 
                  expertise level = 1 would likely perform in the situation presented for that item."
                ),
                p(strong("Table 1"), "provides a summary of the SJT items currently initialized. Each row corresponds to an SJT item, as labeled in the 
                  first column. The second column displays the broad situation class (e.g., \"conflict\") and the third column the specific 
                  situation type from that broad situation class (e.g., \"conflict with a co-worker\") reflected in the SJT item. The fourth column 
                  labeled \"Situation ID\" provides a shorthand identifier for summarizing the broad and specific situation classification (e.g., 
                  Situation ID 3.2 = specific situation type 2 from broad situation class 3; Situation ID 1.5 = broad situation class 1, specific situation 5).
                  The remaining columns display the expertise level associated with each of the four available response options for an item. Note that 
                  the same expertise level can be associated with different response options."
                ),
                p("The controls at left provide two ways to generate SJT items for the present application. Clicking the \"Basic\" tab allows one to set the attributes
                  of SJT items to a default setting or to randomly create a new set of items. Clicking the \"Advanced\" tab provides controls for individually
                  customizing the situations and reponse options represented in each SJT item."
                ),
                helpText(span("*", style = "color:orangered"),"There is one additional parameter in SiRJ that cannot be manipulated in the present applcation 
                  that determines the degree of similarity between the specific situation presented in an SJT item and the prototype of that specific situation
                  in the environment. In the current application, this value is set to .9, indicating that the situations presented in the simulated SJT
                  items stems are highly similar to their prototypic representation in the environment.", style = "font-size: 12px"
                )
              )
            )
          ),
          tabPanel(title = "Create People",
            br(),
            sidebarLayout(
              sidebarPanel(width = 4, # sidebarpanel for creating people
                tabsetPanel(id = "PersonParams",
                  tabPanel(title = "Basic",
                    br(),
                    actionButton(inputId = "defaultPpl",
                                 label = "Create all using default values",
                                 icon = icon("refresh", class = "fa-fw"),
                                 style = "background-color: #18BC9C; border: #18BC9C; margin-bottom: 15px"),
                    br(),
                    actionButton(inputId = "randomPpl",
                                 label = "Create random people",
                                 icon = icon("refresh", class = "fa-fw"),
                                 style = "background-color: #18BC9C; border: #18BC9C; margin-bottom: 15px"),
                    hr(style = "border-top: 1px solid #2C3E50; margin-top: 2px; margin-bottom: 10px"),
                    radioButtons(inputId = "exptAll",
                                 label = "Expertise level",
                                 choices = c("1" = 1,
                                             "2" = 2,
                                             "3" = 3,
                                             "4" = 4,
                                             "5" = 5),
                                 inline = T),
                    sliderInput(inputId = "expLvlAll",
                                label = "Experience strength",
                                min = 1, max = 4, value = 2, step = 1),
                    sliderInput(inputId = "METhrAll",
                                label = "Threshold for ME response",
                                min = .2, max = 1, value = .85, step = .01),
                    sliderInput(inputId = "LEThrAll",
                                label = "Threshold for LE response",
                                min = -1, max = -.2, value = -.85, step = .01),
                    sliderInput(inputId = "AcAll",
                                label = "Activation level threshold",
                                min = .01, max = 1, value = .22, step = .01),
                    sliderInput(inputId = "TmaxAll",
                                label = "Motivation level",
                                min = 3, max = 10, value = 8, step = 1),
                    actionButton(inputId = "sameValPpl",
                                 label = "Create all using same values",
                                 icon = icon("refresh", class = "fa-fw"),
                                 style = "background-color: #18BC9C; border: #18BC9C; margin-bottom: 15px")
                  ),
                  tabPanel(title = "Advanced",
                    br(),
                    selectInput(inputId = "person",
                                label = "Select person to create:",
                                choices = c("Person 1" = 1,
                                            "Person 2" = 2,
                                            "Person 3" = 3,
                                            "Person 4" = 4,
                                            "Person 5" = 5)),
                    uiOutput("sirjCreatePplUI")
                  )
                )
              ),
              mainPanel( # main panel for creating people
                tabsetPanel(id = "sirjPersonDat",
                  tabPanel(title = "Person attributes",
                    br(),
                    p("Table 2: Summary of person attributes", style = "font-weight: bold; font-style: italic"),
                    tableOutput("pplAttMat"),
                    p("Test takers in SiRJ are defined by a variety of attributes;", strong("Table 2"), "above provides a summary of the person attributes
                      that can be manipulated and their currently selected values for the simulations in this", HTML(paste("application.", span("*", style = "color:orangered"), sep = "")),
                      "Each row corresponds to a different simulated person, as labeled in the first column. The second and third columns 
                      summarize the expertise level and experience strength, respectively, of each test taker. The fourth and fifth columns indicate
                      the cumulative similarity threshold that an SJT response option must exceed to be selected as the most effective (ME) or least
                      effective (LE) response by the individual. The sixth column labeled \"Activation level threshold\" represents the activation level that
                      traces in episodic memory must exceed to be considered similar/relevant to the trace probe (see Step 2 on \"HyGene\" tab). The seventh 
                      column labeled \"Motivation\" indicates the number of times a test taker will iterate through the item evaluation process described in SiRJ 
                      (see Step 7 above", HTML(paste(").", span("**", style = "color:orangered"), sep = "")), "Finally, the column labeled \"Episodic Memory Size\" 
                      summarizes the total number of traces stored in a person's episodic memory. This value is not controlled directly and is instead influenced 
                      by an individual's experience strength and the probability of situations in the environment (see description provided in the \"Situations\"
                      section available from the \"Episodic memory distribution\" drop-down menu above)."
                    ),
                    p("The controls at left provide a number of options for generating simulated test takers in the current application. The two buttons at 
                      the top of the \"Basic\" tab allow one to set the attributes for all people to either the default values or to create persons with
                      random", HTML(paste("attributes.", span("***", style = "color:orangered"), sep = "")), "The remaining controls on this tab allow one 
                      to create individuals using the exact same attributes. Note this does not necessarily mean all individuals will have identical
                      episodic and semantic memory traces; it simply means that the memory structures for individuals were constructed using the same
                      parameter values. Finally, clicking the \"Advanced\" tab provides controls for specifying the attributes of each person separately."),
                    helpText(span("*", style = "color:orangered"),"There are three other person-level attributes specified in SiRJ. The first is encoding 
                      fidelity, which determines the degree to which an individual accurately encodes experiences from the environment (see Step 1 on \"HyGene\" 
                      tab). In the current application, this value is set to .85 for all test takers. The second is a strength of association parameter that 
                      determines the likelihood a person will exhibit a particular response to a particular situation interpretation. This value is set to .9 
                      for all test takers in the current application. The final is the size of working memory, which determines the number of traces
                      one can simultaneously entertain when generating the SOC (see Step 4 on \"HyGene\" tab). This value is set to 4 for all individuals
                      in the present simulations.", style = "font-size: 12px"
                    ),                    
                    helpText(span("**", style = "color:orangered"),"To simplify the present simulations and those conducted for the paper, the value selected 
                      for \"Motivation\" also sets the TMAX parameter used in HyGene to control the maximum number of retrieval falures an individual is 
                      willing to undertake when popluating the SOC (see Step 4 on \"HyGene\" tab). Both the motivation and TMAX parameters represent an 
                      individual's willingness/capabaility to persist in cognitive processing and thus are conceptually similar; however, these values are not
                      required to be identical and can be manipulated independently in SiRJ.", style = "font-size: 12px"
                    ),
                    helpText(span("***", style = "color:orangered"),"Caution is advised when running the current application using completely randomized
                      individuals as it can be difficult to make comparisons given that only five individuals are simulated.", style = "font-size: 12px"
                    )
                  ),
                  navbarMenu(title = "Episodic memory distribution",
                    tabPanel(title = "Situations",
                      br(),
                      plotOutput("sitDist"),
                      checkboxGroupInput(inputId = "sitDistSlct",
                                         label = "Show person:",
                                         choices = c("Person 1" = 1,
                                                     "Person 2" = 2,
                                                     "Person 3" = 3,
                                                     "Person 4" = 4,
                                                     "Person 5" = 5),
                                         selected = c(1:5),
                                         inline = T),
                      p(strong("Figure 2a"), "shows the distribution of", strong(span("situations", style = "color:darkslategray")), "in episodic memory 
                        for each person across all specific situations. Each bar indicates the number of memory traces encoded in episodic memory for each specific 
                        situation (identified by its Situation ID on the x-axis) and person (identified by different color bars). The sum of 
                        all the traces shown in Figure 2a for a particular person is equal to the Episodic Memory Size shown in Table 2 on the 
                        \"Person attributes\" tab. Lastly, the checkboxes below the figure control which persons' memory distributions are displayed."
                      ),
                      p("The number of specific situations stored in episodic memory for an individual is determined by two inputs: the probability 
                        of broad situation classes in the environment and the experience strength of an individual. In the present application, the probability 
                        of situations is set in the \"Create Environment\" tab and is applied to all specific situations within a given broad situation 
                        class for all people. This operationalization thus assumes that all individuals in the simulation come from the same environmental 
                        ecology; for example, decreasing the probability of broad situation class 1 will decrease the number of stored episodic memories for 
                        specific situations 1.1 to 1.5 for ALL individuals as these situations are assumed to be less likely to occur in the environment. 
                        The experience strength parameter of individuals is controlled using the sliders on the left. Using these input values, the  
                        number of specific situation traces stored in the episodic memories of simulated individuals was determined as follows:"
                      ),
                      tags$ol(
                        tags$li("A value for the probability of each specific situation in the environment (\\(p_{s}\\)) is selected by sampling from 
                                the probability distribution created for its corresponding broad situation class. In the current application, the 
                                probability distributions of broad situation classes can be controlled and are visualized on the \"Create Environment\" 
                                tab. Note that a unique value is sampled for EACH specific situation from its corresponding probability distribution. 
                                Consequently, it is possible for specific situations within the same broad situation class (e.g., specific situations 
                                1.1 to 1.5) to be differentially represented in episodic memory if the variance of its broad situation class probability
                                distribution is sufficiently large."),
                        tags$li("The total number of traces in episodic memory for a particular specific situation and person is given as:",
                                p("$$\\begin{equation}
                                  TotSit_{si} = \\sum_{1}^{2^{(expStr_{i}+4)}}R {,}
                                  \\end{equation}$$"
                                ), 
                                "where \\(TotSit_{si}\\) is the total number of traces for specific situation \\(s\\) stored in episodic memory for 
                                person \\(i\\), \\(expStr_{i}\\) is the experience strength value for the person, and \\(R\\) is a randomly sampled integer
                                given by the probability function \\(P(R = 1) = p_{s}\\) and \\(P(R = 0) = 1-p_{s}\\). The expected value for the total
                                number of specific situations traces stored in memory for a given situation and person is thus \\(E(TotSit_{si}) = p_{s}*2^{(expStr_{i}+4)}\\). 
                                Note that the decision to make experience strength an exponent and add \"4\" to its value is not intended to represent a substantive claim 
                                about the relationship between situation prevalance and episodic memory size; it is simply a convenient scaling factor 
                                for creating sufficient between-person differences in episodic memory size across different levels of experience strength 
                                and ensuring that indivdiuals possess a reasonably large overall number of episodic memories for purposes of the simulation."),
                        tags$li("Repeat Step 2 for each specific situation."),
                        tags$li("Repeat Steps 1-3 for each person.")
                      ),
                      p("Once the total number of traces for each specific situation to be included in simulated individuals' episodic memory is determined, the corresponding", 
                        strong(span("situation", style = "color:darkslategray")), "minivectors are generated according to the individual's encoding fidelity (see Step 1 on
                        \"HyGene\" tab)."
                      )
                    ),
                    tabPanel(title = "Situation contexts",
                      br(),
                      plotOutput("sitCxtLvlDist"),
                      checkboxGroupInput(inputId = "sitCxtDistSlct",
                                         label = "Show person:",
                                         choices = c("Person 1" = 1,
                                                     "Person 2" = 2,
                                                     "Person 3" = 3,
                                                     "Person 4" = 4,
                                                     "Person 5" = 5),
                                         selected = c(1:5),
                                         inline = T),
                      p(strong("Figure 2b"), "shows how the simulated individuals' interpretations of specific situations (i.e.,", strong(span("situation context", style = "color:steelblue")), "minivectors) 
                        are distributed across expertise level in their episodic memory. Recall that for every situation stored in episodic memory, SiRJ assumes that individuals
                        also encode an interpretation of the circumstances, needs, and demands surrounding the situation. The nature of this interpretation is informed by the 
                        individual's expertise, such that people with different expertise levels are assumed to interpret situations differently. However, rather than 
                        specify that an individual with a given expertise level only and exclusively interprets ALL situations at that expertise level, it is instead posited that a 
                        person's expertise level identifies the", em("most likely"), "type of interpretation they would generate for a situation. Consequently, SiRJ allows the 
                        possibility for individuals to possess interpretations for any given experience of a situation that are more consistent with a different expertise level 
                        than what is attributed to that person (e.g., a person with expertise level = 3 can have situation interpretations more consistent with expertise levels 2, 3, and 4)."
                      ),
                      p("There are numerous ways in which the distribution of situation interpretations in an individual's episodic memory could be specified. In the current
                        application as well as the simulations run in the paper, a simplifying assumption was made that the probability distribution of situation interpretations
                        across expertise would be approximately normal with a small standard deviation and a mean centered at the individual's assigned expertise. 
                        Functionally, this operationalization results in a large proportion of a simulated person's encoded situation interpretations being consistent with 
                        their specified expertise level and a smaller number associated with immediately \"adjacent\" expertise levels."
                      )
                    ),
                    tabPanel(title = "Responses",
                      br(),
                      plotOutput("rspLvlDist"),
                      checkboxGroupInput(inputId = "rspDistSlct",
                                         label = "Show person:",
                                         choices = c("Person 1" = 1,
                                                     "Person 2" = 2,
                                                     "Person 3" = 3,
                                                     "Person 4" = 4,
                                                     "Person 5" = 5),
                                         selected = c(1:5),
                                         inline = T),
                      p(strong("Figure 2c"), "shows how simulated individuals' responses to the specific situations stored in episodic memory (i.e.,", strong(span("response", style = "color:#CD5555")), "minivectors)
                        are distributed across expertise level. In SiRJ, it is posited that individuals respond to situations based on how they interpret its contextual 
                        demands and that individuals who reach similar interpretations about a situation should be more likely to respond similarly. Given that the
                        interpretations of the", strong(span("situation context", style = "color:steelblue")), "associated with a situation are informed by an individual's 
                        expertise level, the responses stored in episodic memory by an individual for a given situation are also assumed to be consistent with the expertise level of the 
                        situation interpretation attached to that situation (e.g., a situation interpretation consistent with expertise level 1 should be associated with an encoded response
                        consistent with expertise level 1). However, rather than specify that an individual with a given expertise level only and exclusively generated and encoded a response
                        to ALL situations consistent with the expertise level of their situation interpretation, the SiRJ simulations allow the possibility for 
                        individuals to have produced and encoded responses that are more consistent with an expertise level different than that attributed to the situation interpretation
                        (e.g., a memory trace for a situation with an interpretation consistent with expertise level 3 can have an encoded response more consistent with expertise level 4)."
                      ),
                      p("To represent the potential for individuals to generate responses with an expertise level inconsistent with the expertise level of the associated situation
                        interpretation, a person-level strength-of-association parameter \\(S\\) was implemented. This parameter determines the probability that the expertise level between the encoded",
                        strong(span("response", style = "color:#CD5555")), "and", strong(span("situation context", style = "color:steelblue")), "minivectors is the same. In the
                        current application, this parameter cannot be manipulated (nor was it manipulated in the simulations conducted in the paper) and is set to .9 for all individuals. This
                        parameterization means that, on average, the expertise level of the", strong(span("response", style = "color:#CD5555")), "and", 
                        strong(span("situation context", style = "color:steelblue")), "minivectors are the same for 90% of the traces stored in a person's episodic memory. In those 
                        cases where the expertise level of the response differs from the situation interpretation, the expertise level of the encoded response is randomly selected."
                      )
                    ),
                    tabPanel(title = "Response contexts",
                      br(),
                      plotOutput("rspCxtLvlDist"),
                      checkboxGroupInput(inputId = "rspCxtDistSlct",
                                         label = "Show person:",
                                         choices = c("Person 1" = 1,
                                                     "Person 2" = 2,
                                                     "Person 3" = 3,
                                                     "Person 4" = 4,
                                                     "Person 5" = 5),
                                         selected = c(1:5),
                                         inline = T),
                      p(strong("Figure 2d"), "shows how the simulated individuals' interpretations of responses (i.e.,", strong(span("response context", style = "color:forestgreen")), "minivectors) 
                        are distributed across expertise level in their episodic memory. Recall that for every response stored in episodic memory, SiRJ assumes that individuals
                        also encode an interpretation of the goals accomplished by and consequences of the response. The nature of this interpretation is informed by the 
                        individual's expertise, such that people with different expertise levels are assumed to interpret responses differently. In the current application and the
                        simulations conducted for the paper, the expertise level associated with response interpretations are assumed to be the same as those associated with
                        responses (e.g., a person with expertise level = 3 will always have responses and response interpretations consistent with expertise level 3). As such, 
                        this operationalization assumes that people who have encoded the same response have also encoded the same interpretation of the response."
                      )
                    )
                  )
                )
              )
            )
          ),
          tabPanel(title = "Run SiRJ",
            br(),
            sidebarLayout(
              sidebarPanel(width = 4, # sidebarpanel for running SiRJ
                actionButton(inputId = "runSirj",
                             label = "Run SiRJ",
                             icon = icon("play", class = "fa-fw"),
                             style = "background-color: #18BC9C; border: #18BC9C; margin-bottom: 15px"),
                helpText("Once the \"Run SiRJ\" button is pressed, progress bars will appear in the lower-right corner of the screen indicating the
                         status of the simulation. While the simulation is running, it is recommended to not change the input parameters on the
                         other tabs until the simulation is completed.", style = "font-size: 12px")
              ),
              mainPanel( # main panel for running SiRJ
                uiOutput("sirjResultsUI")
              )
            )
          )
        ) # end of tabsetPanel for specifying/running SiRJ simulation
      )
    ),
    br()
  )
