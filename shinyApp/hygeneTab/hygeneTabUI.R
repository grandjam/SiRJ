hyGeneTab <-
  tabPanel(title = "HyGene",  
    fluidRow(
     column(width = 11, offset = 0.5,
        h3(icon("angle-double-right"), "Description", style = "font-weight: bold"),
        p("HyGene is a general theory of human judgment and decision-making describing 
          how people generate diagnostic hypotheses from memory. The theory and computational 
          model of HyGene is described in", 
          a(href = "http://www.damlab.umd.edu/PDF%20articles/2008%20Thomas%2CDougherty%2CSprenger%2CHarbison_PsycReview.pdf", 
          target = "_blank", 
          span("Thomas, R.P., Doughtery, M.R., Sprenger, A.M., & Harbison, J.I. (2008). Diagnostic hypothesis generation and human judgment.", 
          em("Psychological Review, 115,"), "155-185.")), 
          " This page provides an interactive application for helping understand how HyGene operates and 
          its integration as a core mechanism in SiRJ."
        )
      )
    ),
           
    fluidRow(
      column(width = 11, offset = 0.5,
        h3(icon("angle-double-right"), "Memory Systems", style = "font-weight: bold"),
        p("The representation of human memory is a central component of the HyGene model. HyGene incorporates three memory 
          systems:", em(" episodic memory, "), em("semantic memory, "), "and", 
          em("working memory.")," The working memory system does not store information, but instead determines how much 
          information can be held in active awareness at any given time. The episodic and semantic memory systems store", em("memory traces,"),
          "conceptualized as the set of features associated with an encoded event. 
          In HyGene, information about encoded events that occur together is assumed to be 
          stored in a single memory trace."
        ),
                    
        h4(icon("adjust"), "Memory Traces", style = "font-weight: bold"),
        p("In translating HyGene into the SiRJ framework, information about four types of characteristics
          were represented in a memory trace: 
          (1) chracteristics of a situation", strong(span("[situation]", style = "color:darkslategray")), ", 
          (2) what and why that situation was happening", strong(span("[situation context]", style = "color:steelblue")), ", 
          (3) the response made in that situation", strong(span("[response]", style = "color:#CD5555")), ", and 
          (4) the goals accomplished by/consequences of that response", strong(span("[response context].", style = "color:forestgreen")), "
          A single memory trace is operationalized as a concatenated set of minivectors representing
          these four types of information. A minivector contains", em("N"), "cells that represent features or attributes which describe 
          that information. For example, features within a", strong(span("situation", style = "color:darkslategray")), "minivector could be whether
          the experience occurred at work, involved performing a task, or involved another person. Though the number of features represented in a 
          minivector is specified in the model, it is not necessary to define the exact nature or meaning of features in a memory trace. 
          Instead, features are simply recorded as present [1], not present [-1], or unknown/forgotten [0] within a given memory trace. 
          Figure 1 provides a visualization of how a single memory trace is represented in the model using this formulation. In this figure, the", 
          strong(span("situation", style = "color:darkslategray")), ", ",
          strong(span("situation context", style = "color:steelblue")), ", ",
          strong(span("response", style = "color:#CD5555")), ", and ",
          strong(span("response context", style = "color:forestgreen")),
          "minvectors are differentiated by color. Each minivector contain 5 features (labeled F1-F5) and the numbers/shading 
          within each cell correspond to the presence of a given feature in the memory trace."
        ),
                    
        plotOutput("memTrace", height = "175px"),
                    
        wellPanel(
          div(
            h4("SiRJ simulations:", style = "font-weight: bold"),
            p("In all simulations conducted for the paper, a total of", strong("18 features"), "were used to represent the", 
              strong(span("situation,", style = "color:darkslategray")), strong(span("situation context,", style = "color:steelblue")), 
              strong(span("response,", style = "color:#CD5555")), "and", strong(span("response context", style = "color:forestgreen")), "minivectors. This
              operationalization was chosen to allow a sufficiently large number of unique feature combinations to be created per minivector (i.e.,  
              18^3 = 5832 possible minivectors). In total then, a single memory trace in the simulation was defined by 72 features: 
              9x2 = 18", strong(span("situation", style = "color:darkslategray")), "features, 
              9x2 = 18", strong(span("situation context", style = "color:steelblue")), " features, 
              9x2 = 18", strong(span("response", style = "color:#CD5555")), " features, 
              and 9x2 = 18", strong(span("response context", style = "color:forestgreen")), " features."
            )
          ), style = "padding:2px 15px; color:#E74C3C"
        )
      )
    ),
           
    fluidRow(
      column(width = 11, offset= 0.5,
        h4(icon("adjust"), "Episodic Memory", style = "font-weight: bold"),
        p("The episodic memory system represents a person's database of past experiences. In this sense, episodic memory maintains 
          the", em("base rates"), "of an individual's experiences and interpretations. The episodic memory system in HyGene stores 
          (imperfect) memory traces of experienced events. For example, if an indvidual experienced a particularly hostile interaction with
          a co-worker, characteristics of the event (e.g., name of the co-worker, what the coworker said, etc.) as well as associated 
          interpretations, responses, and outcomes of the events (e.g., why the co-worker was hostile, how the employee reacted, what 
          happened when the employee reacted) would be stored as a single trace in episodic memory."
        ),
        p("For the SiRJ framework, this representation was used to generate an episodic memory system for a simulated person in the following manner.
          First, minivectors representing different classes of \"broad\" situations (e.g., conflicts, working in teams, etc.) were created. The degree of similarity
          among features representing each broad situation class was controlled with a ", em("broad-situation similarity "),"parameter. This parameter 
          determined the percentage of overlapping situation features across broad situation classes (on average). Each broad situation trace was then
          slightly altered to create a number of \"specific\" situations falling within each broader type (e.g., interpersonal conflict with co-worker, conflict over
          decisions, conflict over responsibilities, etc.). The degree of similarity among the specific situations within each broad situation class 
          was controlled by a ", em("specific-situation similarity "),"parameter. Lastly, the number of times a simulated person had experienced each 
          specific situation was contolled by an ", em("experience strength"), "parameter that allowed simulated people to vary in the overall size
          of their episodic memory. The extent to which these experiential traces were accurately recorded in episodic memory was controlled by an ",
          em("encoding fidelity"), "parameter that allowed memories of experienced specific situations to be degraded."
        ),
                    
        wellPanel(
          div(
            h4("SiRJ simulations:", style = "font-weight: bold"),
            p("In all simulations conducted for the paper, ", strong("three broad situation classes "), "each with ", strong("five types of specific situations "), 
              "were generated (i.e., a total of 15 specific situations). The", strong("broad-situation similarity parameter = .2"), "and the", 
              strong("specific-situation similarity parameter = .75."), "These values were selected so as to make the the three broad situation classes relatively distinct
              from one another (i.e., expected to share 20% of their situational features) while making the five specific situations within a broad situation class 
              more similar (i.e., expected to share 75% of their situational features. The ", strong("experience strength "), "parameter was experimentally manipulated 
              and is discussed in greater detail in the \"SiRJ\" tab. Lastly, the parameter value for ", strong("encoding fidelity = .85."), "This meant that specific 
              experiences of any given specific situations were largely encoded accurately."
            )
          ), style = "padding:2px 15px; color:#E74C3C"
        )
      )
    ),
           
    fluidRow(
      column(width = 11, offset = 0.5,
        h4(icon("adjust"), "Semantic Memory", style = "font-weight: bold"),
        p("The representation of semantic memory in HyGene is a storage system for abstractions or prototypes of concepts 
          derived from either direct experience or learned (e.g., \"book knowledge\"). Unlike the episodic memory system, 
          which may store multiple instances related to the same concept (e.g., multiple conflict experiences), the 
          semantic memory system stores only a single generalized representation of a concept. For example, a person can 
          have a prototypical interpretation of \"conflict\" in semantic memory. This representation could be informed by 
          the person's previous conflict experiences (e.g., episodic memory traces) as well as knowledge/information 
          learned about conflicts through other means (e.g., conflicts seen in movies, conflict scenarios discussed in 
          training, etc.)"
        ),
                    
        wellPanel(
          div(
            h4("SiRJ simulations:", style = "font-weight: bold"),
            p("In all simulations conducted for the paper, a single semantic memory trace was computed for each of the 15 specific 
              situations initialized in episodic memory. This was accomplished by averaging together all episodic memory traces
              for each specific situation into a single representative semantic memory trace. This operationalization meant that 
              semantic memory in the simulations was", em("only"), "a function of the direct experiences stored in episodic memory for 
              a simulated person."
            )
          ), style = "padding:2px 15px; color:#E74C3C"
        )
      )
    ),
           
    fluidRow(
      column(width = 11, offset = 0.5,
          h4("Try it out!", style = "color: white; background-color:#18BC9C; padding:4px; border-radius:5px; font-weight:bold"),
          p("The interactive Figure 2 below provides a simplified demonstration for visualizing how the operationalizations of 
            episodic and semantic memory in HyGene were implemented in SiRJ. The block of memory traces at the top of the figure 
            labeled with \"EpT\" make up episodic memory while the traces at the bottom labeled with \"SmT\" are the semantic 
            memory system. The sliders on the left can be used to generate a simulated memory system under different parameters. 
            The first slider controls the number of unique situations to represent in episodic memory (similar to the number of broad 
            situation classes modeled in", HTML(paste("SiRJ).", span("*", style = "color:orangered"), sep = "")), "The second slider controls the 
            number of memory traces per situation (similar to the experience strength parameter in SiRJ). The third 
            slider controls the number of features included in each minivector. Lastly, the fourth slider controls the 
            degree to which experiences of the same situation are encoded accurately/similarly in memory (similar to the 
            encoding fidelity parameter in SiRJ)."
          ),
        
          helpText(span("*", style = "color:orangered"),"For ease of presentation, this example does not differentiate between broad and specific situations. In SiRJ however, 
            each memory trace encoded in episodic memory represents a unique experience of a specific situation which also 
            belongs to a higher-order broad situation categorization as well (e.g., broad = \"conflicts\", specific = \"conflicts with
            co-workers\"). In the example shown here, each memory trace can simply be thought of as a unique experience of a situation.", style = "font-size: 12px"
          ),
                    
        sidebarLayout(
          sidebarPanel(width = 4,
            sliderInput(inputId = "nSits",
                        label = "Number of situations in memory",
                        min = 2, max = 4, value = 3, step = 1),
                                   
            sliderInput(inputId = "nMems",
                        label = "Number of memory traces per situation",
                        min = 2, max = 5, value = 4, step = 1),
                                   
            sliderInput(inputId = "nFeats",
                        label = "Number of features per information type",
                        min = 2, max = 5, value = 4, step = 1),
                                   
            sliderInput(inputId = "encFidFig2",
                        label = "Encoding fidelity",
                        min = .5, max = 1, value = .85, step = .01)
          ),
                      
          mainPanel(
            uiOutput("memExampleUI")
          )
        ),
        
        wellPanel(
          div(
            h4("What to note:", style = "font-weight: bold"),
            tags$ul(
              tags$li("Increasing (decreasing) the number of situations represented in memory increases (decreases) 
                      the overall size of both episodic memory and semantic memory. However, there is always only one semantic memory 
                      trace for each corresponding situation represented in memory."),
              tags$li("Increasing (decreasing) the number of traces per situation increases (decreases) the overall size of episodic
                      memory, but NOT the size of semantic memory."),
              tags$li("Increasing (decreasing) the number of features increases (decreases) the size of each minivector 
                      as well as the size of the overall memory trace."),
              tags$li("Regardless of the number of situations, memory traces, or features represented in episodic memory, the feature 
                      values encoded in semantic memory for each situation are always equal to the average of the corresponding 
                      features in episodic memory. That is, the value for Feature 1 in the the semantic memory trace for Situation 1 equals
                      the arithmetic mean of the values for Feature 1 in the episodic memory traces for Situation 1."),
              tags$li("Increasing (decreasing) the encoding fidelity tends to increase (decrease) the number of features 
                      that memory traces within each situation share; pushing this slider all the way to 1 makes all 
                      traces within each situation identical. For purposes of this example, a higher encoding fidelity can be 
                      interpreted as an individual having remembered ", em("each experience "), "of a given situation more similarly.")
            )
          ), style = "padding:2px 15px; color:#E74C3C"
        )
      )
    ),
           
    h3(icon("angle-double-right"), "Judgment & Reasoning in HyGene", style = "font-weight: bold"),
    fluidRow(
      column(width = 7, offset = 0.5, 
        p("HyGene was proposed as a general model of human judgment;",
          a(href = "http://www.damlab.umd.edu/PDF%20articles/2008%20Thomas%2CDougherty%2CSprenger%2CHarbison_PsycReview.pdf", 
            target = "_blank", 
            span("Thomas et al. (2008)")
          ), 
          "describe the model as \"neither normative nor entirely semantic based, but rather a compromise between a Bayesian-like inference 
          process and a semantic memory system (p.179).\" The intention of HyGene is to provide a formal process-level account of 
          how people develop an answer to the following:", em("\"Given what I've just observed, what is the most likely 
          explanation?\""), "The HyGene architecture proposes that this appraisal entails a conditional reasoning process
          in which experiences in episodic memory are probed, potential explanations are derived from semantic memory, and 
          the most likely explanatory \"contenders\" are evaluated in working memory to derive probability judgments about each explanation's validity."
        ),
        p("The figure at right provides a visual of the pseudocode/algorithm for the HyGene model. These stages and their 
          operationalization are elaborated in the sections below. To make this description more concrete and relevant to SiRJ, 
          these steps will be presented in the context of a person attempting to answer an SJT item. For example, imagine a test taker
          is presented with the following item on an SJT",
          a(href = "http://psycnet.apa.org/buy/2010-04488-008", 
            target = "_blank", 
            HTML(paste("(", span("from Motowidlo & Beier, 2010"), "):", sep = ""))
          )
        ), 
        
        div(
          strong(
            p("You and someone from another department are jointly responsible for coordinating a project involving both departments.
              The other person is not carrying out his share of the responsibilities. Indicate which of the actions below would be most 
              effective and which would be least effective."),
            tags$ol(type = "A",
              tags$li("Discuss the situation with your manager and ask him to take it up with the other person's manager."),
              tags$li("Remind him that you need his help and that the project won't be completed effectively without a full team effort
                      from both of you."),
              tags$li("Try to find out why he is not doing his share and explain to him that this creates more work for you and 
                      makes it harder to finish the project."),
              tags$li("Get someone else from his department to help with the project.")
            )
          )
        ),
        
        p("The SiRJ framework proposes that the first step in deciding how individuals answer this item involves reading the scenario and determining
          the perceived cause, needs, and demands of that situation (see \"SiRJ\" tab for more details). Tranlsating this goal into the context of the memory 
          systems detailed above and represented in SiRJ, the individual thus first attempts to determine the ", strong(span("situation context ", style = "color:steelblue")), 
          "given the presented ", strong(span("situation.", style = "color:darkslategray")), "HyGene provides a useful architecture for representing
          this cogntive process."
        )
      ),  
      
      column(width = 4, 
        p(
          em("Overview of HyGene architecture. Figure reproduced from ",
            a(href = "http://www.damlab.umd.edu/PDF%20articles/2008%20Thomas%2CDougherty%2CSprenger%2CHarbison_PsycReview.pdf", 
              target = "_blank", 
              span("Thomas et al. (2008)")
            )
          )
        ),
        img(src = 'hygene.jpg', height = "500px")
      )
    ),
           
    fluidRow(
      column(width = 11, offset = 0.5,
        h4(icon("adjust"), "Step 1: Activate traces in episodic memory", style = "font-weight: bold"),
        p("In the first step of HyGene, a simulated individual perceives a stimulus that prompts a judgment and reasoning process. 
          The stimulus is operationalized as a", strong(span("trace probe,", style = "color:mediumPurple")), "which is a minivector 
          containing the same set/number of features as those stored in memory for similar stimuli. For instance, the stimulus in 
          our SJT example is the ", strong(span("situation", style = "color:darkslategray")), "presented to an individual in an SJT item. 
          A situation", strong(span("trace probe", style = "color:mediumPurple"))," for this stimulus is represented by a minivector 
          containing the same number of features as the ", strong(span("situation", style = "color:darkslategray")), 
          "traces initialized in the simulated individual's episodic memory."
        ),
        p("Once formed, the trace probe is compared against all the traces stored in episodic memory. This process is labeled ", 
          em("trace activation"), "in HyGene and is used to determine which traces stored in episodic memory are similar to the trace probe
          (i.e., likely belong to the same category in memory). In SiRJ, the activation level of each memory trace is computed as the 
          cubed cosine", HTML(paste("similarity", span("*", style = "color:orangered"), sep = "")), "between the feature vector of the trace probe and the 
          feature vector of the memory trace", 
          a(href = "http://blog.christianperone.com/2013/09/machine-learning-cosine-similarity-for-vector-space-models-part-iii/", 
            target = "_blank", 
            HTML(paste("(", span("see here for a non-technical description of cosine similarity"), "):", sep = ""))
          )
        ),
        p("$$\\begin{equation}
          A_{i} = \\left ( \\frac{\\sum_{j=1}^{n}P_{j}T_{ij}}{\\sqrt{\\sum_{j=1}^{n}P_{j}^{2}}\\sqrt{\\sum_{j=1}^{n}T_{ij}^{2}}} \\right )^{3}{,}
            \\end{equation}$$"
        ),
        p("where \\(A_{i}\\) is the activation level of episodic memory trace \\(i\\), \\(P_{j}\\) is the \\(jth\\) feature of the probe,
          \\(T_{ij}\\) is the \\(jth\\) feature of episodic memory trace \\(i\\), and \\(n\\) is the number of features. 
          Cosine similarity (and by extenstion, the activation level for a trace in SiRJ), is a measure of the correlation between two 
          vectors. Consequently, an activation level for an episodic memory trace of -1 means the probe and that trace share complete 
          opposite features, a value of +1 means the trace and probe share the exact same features, and a value of 0 means the probe 
          and trace are orthogonal to one another (i.e., have just as many identical features as opposite features). Functionally, this 
          process can be interpreted as the derivation of base rates from episodic memory for a stimulus probe."
        ),
        
        helpText(style = "font-size: 12px",
          span("*", style = "color:orangered"),"The original HyGene model described in ", 
          a(href = "http://www.damlab.umd.edu/PDF%20articles/2008%20Thomas%2CDougherty%2CSprenger%2CHarbison_PsycReview.pdf", 
            target = "_blank", 
            span("Thomas et al. (2008)")
          ),
          " used a modified version of the ",
          a(href = "https://en.wikipedia.org/wiki/Jaccard_index", 
            target = "_blank", 
            span("Jaccard index")
          ),
          "to calculate trace activation. The cosine similarity index is used to calculate trace activation in SiRJ because the 
          trace probes are not always composed of features with integer values as is required for the Jaccard index. In general, the use of
          cosine similarity results in the same rank ordering of activation levels as the Jaccard index, though the magnitude of the
          activation tends to be smaller. Consequently, the same interpretations can be reached for the operationalization 
          used in SiRJ if one accounts for this difference when setting the activation criteria threshold (see Step 2 below)."
        )
      )    
    ),
    
    fluidRow(
      column(width = 11, offset = 0.5,
        h4("Try it out!", style = "color: white; background-color:#18BC9C; padding:4px; border-radius:5px; font-weight: bold"),
        p("The interactive Figure 3 below demonstrates how the episodic memory trace activation process in HyGene operates. A situation", 
          strong(span("trace probe", style = "color:mediumPurple")), " is shown in purple above 15 ", 
          strong(span("situation", style = "color:darkslategray")), "traces stored in episodic memory (5 traces per 3 situatons).
          The ", strong(span("activation level", style = "color:red")), "computed for each episodic memory trace is displayed in the
          red column to the left of the figure. The controls on the left can be used to explore how the activation level
          of episodic memory traces changes in response to different trace probes and episodic memories. The first slider controls the degree to 
          which experiences of the same situation are encoded accurately/similarly in memory. The \"Create memory traces\"
          button generates a new set of episodic memory traces based on the selected encoding fidelity. The radio buttons 
          allow one to select whether the trace probe should be more likely to resemble the first, second, or third situation stored in
          episodic memory, or whether it should be a random probe (i.e., not likely to be represented in memory). The final slider 
          controls the degree to which the probe is likely to be perceived as similar to the selected situation. Lastly, the \"Create probe\"
          button generates a new trace probe based on the selected options."
        ),
        
        sidebarLayout(
          sidebarPanel(width = 4,
                       sliderInput(inputId = "encFidFig3",
                                   label = "Encoding fidelity",
                                   min = .5, max = 1, value = .85, step = .01),
                       actionButton(inputId = "newEpMemFig3",
                                    label = "Create new episodic memory traces",
                                    icon = icon("refresh", class = "fa-fw"),
                                    style = "background-color: #18BC9C; border: #18BC9C; margin-bottom: 15px"),
                       hr(style = "border-top: 1px solid #2C3E50; margin-top: 2px; margin-bottom: 10px"),
                       radioButtons(inputId = "probeSitFig3",
                                    label = "Select situation for probe to most likely resemble",
                                    choices = list("1" = "3", 
                                                   "2" = "2", 
                                                   "3" = "1",
                                                   "Random" = "4"),
                                    inline = T),
                       sliderInput(inputId = "probeSimFig3",
                                   label = "Degree of similarity between probe and situation trace",
                                   min = .3, max = 1, value = .75, step = .01),
                       actionButton(inputId = "newProbeFig3",
                                    label = "Create new trace probe",
                                    icon = icon("refresh", class = "fa-fw"),
                                    style = "background-color: #18BC9C; border: #18BC9C")
          ),
        
          mainPanel(
            plotOutput("hygStep1", width = paste(450, "px", sep = ""), height = paste(600, "px", sep = ""))
          )
        ),
        
        wellPanel(
          div(
            h4("What to note:", style = "font-weight: bold"),
            tags$ul(#style = "font-size: 14px",
              tags$li("Creating new memory traces without creating a new probe OR creating a random probe is akin to 
                      examining how an individual that does not contain previous experience with a situation would 
                      perceive the stimulus. Note that the activation levels in both of these cases tend to be very low."),
              tags$li("Creating a new probe for situation 1, 2, or 3 tends to result in higher activation levels for 
                      the episodic memory traces associated with that situation."),
              tags$li("Decreasing the degree of similarity between a probe and the situations in memory tends to result in
                      lower activation levels for the memory traces associated with that situation (though see final note below). 
                      This is akin to a simulated individual only weakly recognizing the situation trace probe. Conversely, 
                      increasing the degree of similarity tends to result in higher activation levels."),
              tags$li("It is possible for memory traces from ", em("different"), "situations (or for memory traces from a 
                      situation different than the probe) to have similar activation levels. For example, when the encoding 
                      fidelity and degree of similarity between the probe and any given situation are both low, the probe may 
                      end up containing features shared by traces from different situations. This can also occur if the 
                      unique situations stored in memory tend to be highly similar (e.g., Situations 1 and 2 share many
                      of the same features)."),
              tags$li("For purposes of the examples presented on this page, it was desirable to make the three situations 
                      represented in episodic memory in Figure 3 fairly similar to one another (i.e., traces share a 
                      relatively high proportion of features on average). As a result it is possible--and relatively easy--
                      to generate a probe that results in either strong activation or weak activation across all the traces 
                      in episodic memory. When situations are made more distinct from one another (as they were in the actual
                      SiRJ simulations), the activation step tends to be more discriminating.")
            )
          ), style = "padding:2px 15px; color:#E74C3C"
        )  
      )
    ),
           
    fluidRow(
      column(width = 11, offset = 0.5,
        h4(icon("adjust"), "Step 2: Extract unspecified probe", style = "font-weight: bold"),
        p("The next step in HyGene proposes that the patterns of episodic memory activation are transformed into 
          an overall impression or \"gist.\" To accomplish this task, the ", strong(span("activation level", style = "color:red")), 
          "of each episodic memory trace is first compared against an ", em("activation level threshold."), "The threshold determines 
          how similar an experience stored in episodic memory must be in relation to the trace probe for it to be considered relevant to the 
          judgment being made. The activation threshold is thus conceptually similar to those discussed in signal detection and 
          connectionist models that represent how strong an evaluation must be in order for an individual to act upon it in a particular
          way. In HyGene, the threshold value is treated as a stable individual difference parameter. Returning to the SJT example, let's 
          assume that the situation reflected in the SJT item and formulated into the", strong(span("trace probe", style = "color:mediumPurple")), 
          "possessed features involving working with others, social loafing, and project management. During this stage, previously
          experienced", strong(span("situations", style = "color:darkslategray")), "that involved similar characteristics would likely be", 
          strong(span("activated", style = "color:red")), "at a level above threshold and thus considered relevant to the judgment, while
          situations lacking those characteristics would not."
        ),
        p("The activated subset of traces are next translated into what HyGene refers to as an", em("unspecified probe."), 
          "The unspecified probe represents the individual's overall \"gist\" or sense of the experience 
          and explanation most commonly and strongly associated with the stimulus", strong(span("trace probe.", style = "color:mediumPurple")),
          "Generating the unspecified probe is referred to as", em("conditional echo content with resolution"), "in HyGene. 
          The conditional echo content vector is calculated by multiplying each feature value of the traces in the subset of activated traces by
          the activation level of its corresponding trace, and then summing the resulting values across traces.  
          This computation is carried out for both the experience and explanation components of the activated traces (e.g., the ", 
          strong(span("situation", style = "color:darkslategray"))," and the ", strong(span("situation context ", style = "color:steelblue")),
          "minvectors in the present example). The equation for calculating conditional echo content is given as:"
        ),
        p("$$\\begin{equation}
          C_{j} = \\sum_{i=1}^{K}A_{i}T_{ij}{,}
          \\end{equation}$$"
        ),
        p("where \\(C_{j}\\) is the conditional echo content for the \\(jth\\) feature, \\(K\\) is the the number of traces that exceed 
          the activation threshold, and \\(A_{i}\\) and \\(T_{ij}\\) are defined as in Equation 1. Once computed, these values are concatenated
          into a single overall vector and normalized by dividing by the absolute value of the largest value in the conditional 
          echo content vector. This \"resolution\" process ensures that the final feature values fall within the allowable range 
          of -1 and 1 for features while still preserving the sign of the original content values. The normalized conditional 
          echo content vector represents the final unspecified probe extracted from episodic memory. In the context of the running SJT
          example, the final result of this step is an overall impression of the", strong(span("situation", style = "color:darkslategray")), 
          "reflected in the SJT item and the", strong(span("situation context ", style = "color:steelblue")), 
          "(i.e., the circumstances, needs, and demands surrounding the situation) informed by the individual's previous experience."
        ),
      
        wellPanel(
          div(
            h4("SiRJ simulations:", style = "font-weight: bold"),
            p("In all simulations conducted for the paper, the ", strong("activation level threshold"),"= .216. This value corresponds
              to a cosine similarity = .6, indicating that the correlation between a probe and a memory trace needed to be fairly 
              strong and positive for the trace to be activated. It is possible to observe scenarios in which no traces exceed threshold and thus 
              no traces would be activated and no unspecified probe created. For purposes of the SiRJ simulations, if no trace exceeded 
              the activation level threshold, the trace with the highest activation level was selected as the unspecified probe. This scenario
              happened infrequently given the parameter settings used in the SiRJ simulations. However, the rationale for this decision 
              was to allow simulated individuals in SiRJ to attempt to identify a rationale for an observed stimulus even when it was
              largely unrecognized. As described later in Step 5 though, this most often resulted in a simulated individual simply being unable
              to generate a conclusion from their memory search."
            )
          ), style = "padding:2px 15px; color:#E74C3C"
        )        
      )
    ),

    fluidRow(
      column(width = 11, offset = 0.5,
        h4("Try it out!", style = "color: white; background-color:#18BC9C; padding:4px; border-radius:5px; font-weight: bold"),
        p("The interactive Figure 4 below demonstrates how an unspecified probe is extracted from episodic memory in HyGene. The figure 
          displays the same episodic memory traces, situation", strong(span("trace probe,", style = "color:mediumPurple")), "and ", 
          strong(span("activation levels", style = "color:red")), " currently shown in Figure 3, though in this case both the 
          ", strong(span("situation", style = "color:darkslategray")), "and associated", 
          strong(span("situation context ", style = "color:steelblue")), "minvectors are displayed. The extracted conditional echo content vector
          and unspecified probe are displayed at the bottom of the figure (labeled \"EchoCnt\" and \"UnspPrb,\" respectively). 
          The controls on the left can be used to explore how the activation level threshold influences which episodic memory traces 
          are activated and the formation of the unspecified probe. The \"Activation level threshold\" slider controls the activation level 
          that must be surpassed for an episodic memory trace to be considered relevant to the situation trace probe. The remaining 
          controls are identical to those provided in the previous figure and can again be used to create a different set of episodic 
          memories or situation", strong(span("trace probe.", style = "color:mediumPurple"))
        ),
        
        sidebarLayout(
          sidebarPanel(width = 4,
                       sliderInput(inputId = "AcFig4",
                                   label = "Activation level threshold",
                                   min = .01, max = 1, value = .2, step = .01),
                       hr(style = "border-top: 1px solid #2C3E50; margin-top: 2px; margin-bottom: 10px"),
                       sliderInput(inputId = "encFidFig4",
                                   label = "Encoding fidelity",
                                   min = .5, max = 1, value = .85, step = .01),
                       actionButton(inputId = "newEpMemFig4",
                                    label = "Create new episodic memory traces",
                                    icon = icon(name = "refresh", class = "fa-fw"),
                                    style = "background-color: #18BC9C; border: #18BC9C; margin-bottom: 15px"),
                       hr(style = "border-top: 1px solid #2C3E50; margin-top: 2px; margin-bottom: 10px"),
                       radioButtons(inputId = "probeSitFig4",
                                    label = "Select situation for probe to most likely resemble",
                                    choices = list("1" = "3", 
                                                   "2" = "2", 
                                                   "3" = "1",
                                                   "Random" = "4"),
                                    inline = T),
                       sliderInput(inputId = "probeSimFig4",
                                   label = "Degree of similarity between probe and situation trace",
                                   min = .3, max = 1, value = .75, step = .01),
                       actionButton(inputId = "newProbeFig4",
                                    label = "Create new trace probe",
                                    icon = icon(name = "refresh", class = "fa-fw"),
                                    style = "background-color: #18BC9C; border: #18BC9C"),
                       helpText(style = "font-size: 12px",
                        "The episodic memory traces, situation trace probe, and activation levels shown at right are
                        the same as those currently displayed in Figure 3. Pressing the \"Create new episodic memory traces\" or 
                        \"Create new trace probe\" buttons in this control panel will generate a new set of memory traces or
                        situation trace probe, respectively, for both figures.")
          ),
          
          mainPanel(
            plotOutput("hygStep2", width = paste(680, "px", sep = ""), height = paste(650, "px", sep = ""))
          )
        ),
        
        wellPanel(
          div(
            h4("What to note:", style = "font-weight: bold"),
            tags$ul(#style = "font-size: 14px",
              tags$li("Increasing (decreasing) the activation threshold tends to decrease (increase) the number of traces
                      that are considered activated for computing the unspecified probe."),
              tags$li("The ", strong(span("situation", style = "color:darkslategray")), "component of the unspecified probe
                      tends to closely resemble the situation", strong(span("trace probe", style = "color:mediumPurple")), "that
                      produced it. Similarly, the ", strong(span("situation context", style = "color:steelblue")), "component
                      of the unspecified probe tends to resemble the ", strong(span("situation context", style = "color:steelblue")), 
                      "minivectors most often and most strongly associated with the traces in memory whose activation level exceed 
                      threshold"),
              tags$li("When no traces exceed threshold, neither a conditional echo content vector or unspecified probe can be extracted. This
                      will be reflected in the figure as a string of \"NAs\" for both the echo content and unspecified probe in Figure 4. 
                      This scenario is most likely to occur when the activation level threshold is high, the situation probe is not well 
                      represented in episodic memory (i.e., degree of similarity between probe and situation trace is low or a random 
                      probe is created), and/or the encoding fidelity of the memory traces is low (i.e., many of the features stored 
                      in episodic memory have a value of 0).")
            )
          ), style = "padding:2px 15px; color:#E74C3C"
        )
      )  
    ),
       
    fluidRow(
      column(width = 11, offset = 0.5,
        h4(icon("adjust"), "Step 3: Compare against semantic memory", style = "font-weight: bold"),
        p("In the third step of HyGene, the \"gist\" reflected in the newly extracted unspecified probe is contrasted against semantic memory. 
          Functionally, this stage represents a quasi-categorization procedure in which the general impression created during episodic memory
          is evaluated to determine to what extent it matches any prototypes/abstractions stored in semantic memory. Operationally, the 
          calculations carried out in this stage of HyGene are the same as those described in Step 1; the cubed cosine similarity between 
          the unspecified probe and each trace in semantic memory is computed to derive its", strong(span("activation level.", style = "color:red")),
          "Only traces in semantic memory that acheive an", strong(span("activation level", style = "color:red")), "greater than zero are 
          considered to be relevant candidate interpretations of the unspecified probe. The", strong(span("activation level", style = "color:red")), 
          "for each semantic memory trace is subsequently transformed by dividing it by the sum of all the semantic memory activation levels 
          which exceed zero. The resultant", strong(span("normed activation level", style = "color:orange")), "for a semantic memory trace 
          reflects its relative activation strength compared to all other traces in semantic memory."
        ),
        p("Returning to the SJT example, the unspecified probe from the previous step represents a loosely formed impression that could have
          drawn information from diverse and disparate experiences. For example, the individual may have had experiences working with a 
          family member on a chore, organizing a charity fun run, and writing a group paper in school that all possessed certain characteristics 
          similar to the situation reflected in the SJT item. These situations and their diverse features would be captured in the unspecified probe, 
          but they may not provide a coherent picture. To generate a general interpretation and understanding of the circumstances that tend 
          to underlie such situations (i.e., the", HTML(paste(span("situation context", style = "color:steelblue; font-weight:bold"), "),", sep = "")),
          "this loose impression (i.e., unspecified probe) is compared against learned knowledge and prototypes in semantic memory to 
          assign it meaning. As before, the impression may appear to fit in with multiple prototypes/abstractions in semantic memory and thus 
          its relative strength of identification with any particular semantic trace is formulated (e.g.,", 
          HTML(paste(span("normed activation level", style = "color:orange; font-weight:bold"), ").", sep =""))
        )
      )
    ),
    
    fluidRow(
      column(width = 11, offset = 0.5,
        h4("Try it out!", style = "color: white; background-color:#18BC9C; padding:4px; border-radius:5px; font-weight: bold"),
        p("The interactive Figure 5 below demonstrates how the semantic memory trace activation process in HyGene operates. The 
          unspecified probe extracted from the previous Step 2/Figure 4 (labeled \"UnspPrb\") is shown above the three semantic memory 
          traces. Note that there are only three semantic memory traces as there are only three broad situations in this
          example. The feature values for the", strong(span("situation", style = "color:darkslategray")), "and associated", 
          strong(span("situation context ", style = "color:steelblue")), "minivectors for each semantic memory trace are based on
          the corresponding minivectors from the episodic memory structure shown in Figures 3 and 4. The",
          strong(span("activation level", style = "color:red")), "and", strong(span("normed activation level", style = "color:orange")), 
          "(labeled \"ActLvl\" and \"NormAct,\") for each semantic memory trace are displayed in the red and orange columns, 
          respectively, to the left of the figure. The controls on the left perform the same functions as those provided previously 
          for Figures 3 and 4 and can be used to explore how the activation level of semantic memory traces change in response to 
          activation level thresholds, different trace probes, and different episodic memories."
        ),
        
        sidebarLayout(
          sidebarPanel(width = 4,
                       sliderInput(inputId = "AcFig5",
                                   label = "Activation level threshold",
                                   min = .01, max = 1, value = .2, step = .01),
                       hr(style = "border-top: 1px solid #2C3E50; margin-top: 2px; margin-bottom: 10px"),
                       sliderInput(inputId = "encFidFig5",
                                   label = "Encoding fidelity",
                                   min = .5, max = 1, value = .85, step = .01),
                       actionButton(inputId = "newEpMemFig5",
                                    icon = icon(name = "refresh", class = "fa-fw"),
                                    label = "Create new episodic memory traces",
                                    style = "background-color: #18BC9C; border: #18BC9C; margin-bottom: 15px"),
                       hr(style = "border-top: 1px solid #2C3E50; margin-top: 2px; margin-bottom: 10px"),
                       radioButtons(inputId = "probeSitFig5",
                                    label = "Select situation for probe to most likely resemble",
                                    choices = list("1" = "3", 
                                                   "2" = "2", 
                                                   "3" = "1",
                                                   "Random" = "4"),
                                    inline = T),
                       sliderInput(inputId = "probeSimFig5",
                                   label = "Degree of similarity between probe and situation trace",
                                   min = .3, max = 1, value = .75, step = .01),
                       actionButton(inputId = "newProbeFig5",
                                    icon = icon(name = "refresh", class = "fa-fw"),
                                    label = "Create new trace probe",
                                    style = "background-color: #18BC9C; border: #18BC9C"),
                       helpText(style = "font-size: 12px",
                        "The semantic memory traces shown in this figure are derived from the episodic memory traces shown in 
                        Figures 3 and 4. Pressing the \"Create memory traces\" or \"Create probe\" buttons in this control panel will 
                        change the", em("episodic memory traces"), "or situation trace probe (respectively) shown in those figures.")
               ),
          
          mainPanel(
            #plotOutput("hygStep3", width = paste(736, "px", sep = ""), height = paste(205, "px", sep = ""))
            plotOutput("hygStep3", width = paste(850, "px", sep = ""), height = paste(237, "px", sep = ""))
          )
        ),
        
        wellPanel(
          div(
            h4("What to note:", style = "font-weight: bold"),
            tags$ul(
              tags$li("Pressing the \"Create new episodic memory traces\" button to generate a new episodic memory stucture will 
                      change the semantic memory traces as well as result in a different unspecified probe being extracted 
                      (see Step 2/Figure 4). This is because both the composition of semantic memory and the unspecified probe
                      are based on the traces present in episodic memory."),
              tags$li("Pressing the \"Create new trace probe\" button to generate a new trace probe will NOT result in changes to 
                      the semantic memory traces, but will result in a different unspecified probe (see Step 2/Figure 4). This is
                      because generating a new probe has no impact on the composition of episodic memory traces, but is likely to 
                      change which episodic memory traces exceed activation threshold and therefore enter into the unspecified probe."),
              tags$li("In general, the magnitude of the", strong(span("activation level", style = "color:red")), "for any given semantic 
                      memory trace is affected by (a) how many of its corresponding episodic memory traces exceed the activation 
                      threshold and (b) the magnitude of the activation levels for those activated episodic memory traces. Consequently,
                      the effect of the activation threshold--and thus what traces are activated in episodic memory--will affect the 
                      patterns of activation observed in semantic memory."),
              tags$li("Decreasing the activation level threshold will typically result in a (relatively) more uniform pattern of", 
                      strong(span("activation levels", style = "color:red")), "and", strong(span("normed activation levels", style = "color:orange")), 
                      "among semantic memory traces; in contrast, increasing the activation level threshold will typically 
                      result in one semantic memory trace emerging with the highest activations. Note that if the activation threshold 
                      is set too high, then no traces in episodic memory will be activated and neither the unspecified probe nor semantic 
                      memory activations can be calculated.")
            )
          ), style = "padding:2px 15px; color:#E74C3C"
        )
      )
    ),
           
    fluidRow(
      column(width = 11, offset = 0.5,
        h4(icon("adjust"), "Step 4: Create set of contenders", style = "font-weight: bold"),
        p("The cognitive processes reflected in HyGene up to this stage can largely be considered \"automatic.\" That is, 
          one does not need to assume that an individual engages in effortful processing to determine activation levels, formulate the 
          unspecified probe, or determine to what extent the unspecified probe aligns with existing prototypes. In Step 4 of HyGene, 
          the first explicitly controlled processing occurs in which potential interpretations for the unspecified probe 
          are generated from semantic memory."
        ),
        p("The HyGene architecture posits that when individuals are faced with a stimulus that prompts judgment, they 
          generate a", em("set of leading contenders"), "(SOC) containing the decision-maker's best-guess interpretations
          for what gave rise to the stimulus. In this sense, the SOC can be thought of as a dynamic \"cognitive bulletin board\" where 
          potential interpretations can be \"pinned up,\" \"taken down,\" and actively evaluated in order to determine its plausibility as 
          an explanation. The size of the SOC represents the number of interpretations that an individual is able 
          to actively entertain/\"pin to the bulletin board\" and is constrained by the person's available working memory. People with 
          higher working memory capacity are thus capable of simultaneously considering more possible explanations, though that capacity
          can be further reduced by other demands that tax working memory."
        ),
        p("Populating the SOC is proposed to occur in an iterative fashion:"
        ),
        tags$ol(type = "A", 
          tags$li("A trace is sampled from semantic memory for consideration at a rate equal to its", 
                  strong(span("normed activation level.", style = "color:orange")), "Thus, a trace in semantic memory with a", 
                  strong(span("normed activation level.", style = "color:orange")), "= .65 means there is a 65% chance that trace will be sampled
                  at this stage."),
          tags$li("The", strong(span("activation level", style = "color:red")), "of the sampled semantic memory trace is compared against
                  the current threshold for generation, referred to as \\(Act_{Min}\\) in HyGene. \\(Act_{Min}\\) is initially set to zero. 
                  However, it is dynamically updated as the SOC is populated such that \\(Act_{Min}\\) = the lowest", strong(span("activation level", style = "color:red")), 
                  "among all traces residing in the SOC."),
          tags$li("If the", strong(span("activation level", style = "color:red")), "of the sampled semantic memory trace exceeds \\(Act_{Min}\\)
                  and it is not already present in the SOC, it is added to the SOC and considered a retrieval success. If a sampled trace does 
                  not exceed \\(Act_{Min}\\) or already resides in the SOC, it is not added to the SOC and is considered a retrieval failure"),
          tags$li("Steps A-C are repeated until a maximum number of retrieval failures is reached, referred to as TMAX in HyGene. TMAX 
                  can be thought of as an individual difference reflecting a decision-maker's motivation and/or capability to continue attempting 
                  to generate potential interpretations for the observed stimulus.")
        ),
        p("At the conclusion of this process, the SOC will be comprised of one or more semantic memory traces. Only those traces that reside in
          the SOC at the end are considered potential interpretations for the oberved stimulus.")
      )
    ),
    
    fluidRow(
      column(width = 11, offset = 0.5,
        h4("Try it out!", style = "color: white; background-color:#18BC9C; padding:4px; border-radius:5px; font-weight:bold"),
        p("The interactive Figure 6 below demonstrates how the generation of the SOC in HyGene operates. The semantic memory traces and 
          accompanying", strong(span("activation", style = "color:red")), "and", strong(span("normed activation", style = "color:orange")),
          "levels from the previous Step 3/Figure 5 are shown at the top of the figure. The SOC is displayed at the bottom of the figure. 
          When a new SOC is initialized, the figure will display a number of empty rows equal to the size of working memory as a visual
          indicator for the maximum size of the SOC (labeled \"SOC#\"). The controls on the left can be used to step through the SOC population 
          process. The \"Size of working memory\" slider determines the maximum number of traces that could potentially be included in the SOC, 
          while the \"Max number of retrieval failures\" slider determines the total number of times one can fail to generate a new entry 
          into the SOC before the process terminates. Pressing the \"Create new SOC\" button will generate a new blank SOC using the 
          selected parameters. Pressing the \"Retrieval step\" button will attempt to retrieve a trace from semantic memory and enter it 
          into the SOC. Each time this button is pressed, the results of the retreival attempt will be printed below the figure and the 
          visual for the SOC updated accordingly."
        ),
             
        sidebarLayout(
          sidebarPanel(width = 4,
                       sliderInput(inputId = "WM",
                                   label = "Size of working memory",
                                   min = 1, max = 3, value = 2, step = 1),
                       actionButton(inputId = "newSOC",
                                    label = "Create new SOC",
                                    icon = icon(name = "refresh", class = "fa-fw"),
                                    style = "background-color: #18BC9C; border: #18BC9C; margin-bottom: 15px"),
                       br(),
                       sliderInput(inputId = "TMax",
                                   label = "Max number of retrieval failures",
                                   min = 3, max = 8, value = 5, step = 1),
                       actionButton(inputId = "SOCstep",
                                    label = "Retrieval step",
                                    icon = icon(name = "step-forward", class = "fa-fw"),
                                    style = "background-color: #18BC9C; border: #18BC9C")
          ),
               
          mainPanel(
            uiOutput("SOCUI")
          )
        ),
        
        wellPanel(
          div(
            h4("What to note:", style = "font-weight: bold"),
            tags$ul(
              tags$li("The SOC can end up with fewer traces than what it could possibly hold given the size of working
                      memory (e.g., SOC can hold up to three traces, but only one trace is generated into the SOC). This will occur when the
                      first trace sampled into the SOC has the highest activation level in semantic memory and thus it is impossible
                      for any other trace to exceed", HTML(paste("Act", tags$sub("Min"), ".", sep = "")), "This reflects a circumstance in which
                      the first interpretation an individual considers cannot be plausibly \"beaten\" by any other alternatives they are 
                      able to generate."),
              tags$li("A simulated individual can generate more interpretations than can be held in the SOC. This can occur when the
                      SOC is capable of holding fewer traces than there are semantic memories and a trace with a weak", 
                      strong(span("activation level", style = "color:red")), "happens to be selected for inclusion. In this case, 
                      the trace with the weaker activation can be replaced by a trace with a stronger activation. This reflects a circumstance
                      in which an individual generates an interpretation, recognizes it cannot compete with any of the alternatives they are
                      able to simultaneously entertain, and thus dismisses it from consideration."),
              tags$li("The most strongly activated semantic memory trace may not always end up in the final SOC. The chances of this 
                      occuring are increased when the maximum number of retrieval failures is small. This reflects a circumstance
                      in which an individual may have been able to generate a better fitting interpretation had they spent more time
                      considering potential alternatives."),
              tags$li("Changing the size of working memory does not influence the likelihood of populating the SOC with the most strongly
                      activated traces from semantic memory. However, it can allow more potential interpretations to be generated into the SOC,
                      which influences the magnitude of the probability estimates assigned to each interpretation (see Step 5/Table 1).")
            )
          ), style = "padding:2px 15px; color:#E74C3C"
        )
      )
    ),
           
    fluidRow(
      column(width = 11, offset = 0.5,
        h4(icon("adjust"), "Step 5: Determine probability judgments and select interpretation", style = "font-weight: bold"),
        p("At this point in the judgment and reasoning process, the decision-maker has populated one or more potential interpretations for 
          an observered stimulus in the SOC. The final step of HyGene describes a method by which the posterior probability for each trace in the 
          SOC is derived. The", em("posterior probability judgment"), "represents the likelihood that a particular interpretation generated by
          the decision-maker offers an adequate explanation for the observed stimulus given the decision-maker's experiences."
        ),
        p("The first step of this calculation involves computing the", em("conditional echo intensity"), "for each trace residing in the", HTML(paste("SOC.", span("*", style = "color:orangered"), sep = "")), 
          "This value provides an indication of the \"memory strength\" for a given interpretation. Operationally, the conditional 
          echo intensity is a measure of the similarity between an interpretation from the SOC and the interpretations stored in episodic 
          memory that are perceived as relevant to previous experiences with the observed stimulus. The formula for computing the conditional
          echo intensity is given by:"
        ),
        p("$$\\begin{equation}
          I_{C_{s}} = \\frac{\\sum I_{s[A_{i} \\geq A_{C}]}}{K}{,}
          \\end{equation}$$"
        ),
        p("where \\(I_{C_{s}}\\) is the conditional echo intensity of the \\(sth\\) trace in the SOC, \\(I_{s[A_{i} \\geq A_{C}]}\\)
          is the activation level between the \\(sth\\) SOC trace and the subset of traces in episodic memory that exceeded the 
          activation level threshold, and \\(K\\) is the number of traces in episodic memory that exceeded the activation level threshold.
          HyGene posits that a decision-maker's posterior probability judgment for a potential interpretation is based on its conditional
          echo intensity relative to the conditional echo intensities of all the traces residing in the SOC. Specifically, the judged posterior 
          probability of each trace in the SOC is computed according to the following:"
        ),
        p("$$\\begin{equation}
          P(H_{s}|D_{obs}) = \\frac{I_{C_{s}}}{\\sum_{s=1}^{w}I_{C_{s}}}{,}
          \\end{equation}$$"
        ),
        p("where \\(P(H_{s}|D_{obs})\\) is the probability of the \\(sth\\) SOC trace conditional on the observed trace probe,
          \\(I_{C_{s}}\\) is the conditional echo intensity of the \\(sth\\) SOC trace, and \\(w\\) is the total number of traces in the SOC.
          Returning to the running example used on this page, the conditional echo intensity would be the average
          cubed cosine similarity computed between the", strong(span("situation context ", style = "color:steelblue")), "minivector of a given trace 
          in the SOC with all the", strong(span("situation context ", style = "color:steelblue")), "minivectors in episodic 
          memory whose", strong(span("activation level", style = "color:red")), "exceeded the activation level threshold. The probability
          judgment that the decision-maker would place on each interpretation in the SOC for offering a viable explanation of the
          observed situation", strong(span("trace probe", style = "color:mediumPurple")), "would then be computed according to Equation 4."
        ),
        
        helpText(style = "font-size: 12px",
          span("*", style = "color:orangered"), "Though not shown in the example on this page, a", em("consistency check"), "procedure is 
          conducted prior to computing the conditional echo intensity in both HyGene and SiRJ. The consistency check allows the removal of 
          any interpretations from the SOC if the stimulus associated with the generated interpretation is inconsistent with the observed 
          stimulus. In the running example used on this page, this equates to computing the cosine similarity between the", 
          strong(span("situation", style = "color:darkslategray")), "minivector of each trace in the SOC and the observed situation", strong(span("trace probe.", style = "color:mediumPurple")),
          "A cosine similarity less than zero indicates that a particular interpretation in the SOC was not associated with any stimulus similar
          to the observed stimulus and should be removed from consideration."
        ),
        
        wellPanel(
          div(
            h4("SiRJ simulations:", style = "font-weight: bold"),
            p("HyGene does not specify how an individual selects an interpretation generated in the SOC once its posterior
              probability is calculated. In the SiRJ framework, it is assumed that an individual chooses the interpretation with 
              the highest judged posterior probability. This operationalization reflects that decision-makers choose to endorse the
              interpretation they see as having the highest likelihood of explaining an observed stimulus relative to the other
              potential interpretations they were able to generate."
            )
          ), style = "padding:2px 15px; color:#E74C3C"
        )
      )
    ),
    
    fluidRow(
      column(width = 11, offset = 0.5,
        h4("Try it out!", style = "color: white; background-color:#18BC9C; padding:4px; border-radius:5px; font-weight:bold"),
        p("Table 1 and Figure 7 below demonstrate how the conditional echo intensity and posterior probability for traces generated
          in the SOC are computed in HyGene. Figure 7 is divided into separate tabs for each trace populated in the SOC during Step 4/Figure 6. 
          Each tab provides a visual representation of how the conditional echo intensity is computed. The", strong(span("situation context ", style = "color:steelblue")), 
          "minivector from the corresponding SOC trace is displayed at the top of the figure above the", strong(span("situation context ", style = "color:steelblue")),
          "minvectors from episodic memory. The episodic memory traces which did not exceed the activation level threshold in Step 2/Figure 4
          are grayed out in the display. The", strong(span("activation level", style = "color:red")), "for each episodic memory trace relative
          to the SOC trace is displayed in the red column to the left of the figure. These represent the \\(I_{s[A_{i} \\geq A_{C}]}\\) 
          values used to calculate the conditional echo intensity as shown in Equation 3. Lastly, Table 1 provides a summary of the final 
          condtional echo intensity and posterior probability judgments computed for each trace generated into the SOC. When multiple
          SOC traces are generated, the blue-shaded row in Table 1 will correspond with the visualization currently displayed in Figure 7.
          Lastly, the text-box below Table 1 will print the outcome of this final step and report which interpretation from the SOC
          would be selected."
        ), 
        
        uiOutput("step5UI"),
        
        wellPanel(
          div(
            h4("What to note:", style = "font-weight: bold"),
            tags$ul(#style = "font-size: 14px",
              tags$li("The posterior probability judgments for a given SOC trace will change depending on the number of other interpretations
                      generated in the SOC. Specifically, as more alternative interpretations are retrieved into the SOC, the judged 
                      probability for any given trace will decrease. For example, imagine a decision-maker only generates a single 
                      interpretation for an observed stimulus. As shown in Equation 4, the posterior probability estimate for that 
                      interpretation will equal 1. However, if the decision-maker generates a second interpretation for the observed 
                      stimulus, the probability estimate for the first interpretation will be decreased as the decision-maker now considers 
                      another plausible explantion for the observation."),
              tags$li("The posterior probability judgments for a given SOC trace will also change depending on the strength (i.e., 
                      conditional echo intensity) of the traces generated in the SOC. Specifically, as stronger interpretations are 
                      retrieved into the SOC, the judged probability for any given trace will decrease. This effect is described 
                      in the probability judgment literature as the", em("alternative outcomes effect,"), "in which individuals tend to
                      provide higher probability estimates for a given prediction/explanation when the other alternatives are relatively
                      weak versus when there are one or a few very strong predictions/explanations."),
              tags$li("The SOC trace with the strongest conditional echo intensity will always be judged to have the highest posterior 
                      probability. However, this does not mean that the most strongly activated interpretation in", em("semantic memory"), 
                      "is always selected as the most plausible interpretation for the observed stimulus. If the decision-maker fails
                      to retrieve the most strongly activated semantic memory trace into the SOC (e.g., because they ran out of time or
                      motivation to continue probing semantic memory), a poorer-fitting interpretation than could have been concluded
                      based on the person's experiences may be selected.")
            )
          ), style = "padding:2px 15px; color:#E74C3C"
        )
      )
    ),
    br()
  ) # Close HygeneTab panel
