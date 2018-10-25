homeTab <-
  tabPanel(title = "Home",
    fluidRow(
      column(width = 11, offset = 0.5,
        h3(icon("angle-double-right"), "Welcome!", style = "font-weight: bold", id = "topHome"),
        p("This web application is intended to provide an interactive supplement to the paper (CITATION REDACTED FOR BLIND REVIEW). The paper
          presents a general theory, computational model, and evidence from both computer simulations and an empirical study of the cognitive
          processes used by individuals when prompted to respond to items presented on situational judgment tests (SJTs) commonly used in 
          employment assessment and selection. The purpose of this web application is to provide further details on the computational theory
          and how it functions by allowing one to actively engage and experiment with the core model features in real-time. It is our hope 
          that reading the material on this website and (most importantly!) interacting with the dynamic figures will facilitate understanding of 
          the theory as well as stimulate further development and exploration of its implications for advancing research and practice relevant to 
          low fidelity simulation assessments."
        ),
        
        h3(icon("angle-double-right"), "Overview", style = "font-weight: bold"),
        p("The web application is organized into sections that can be accessed by clicking the navigation links at the top of 
          the page. Additional instructions on how to operate the interactive applications as well as highlights of what to look for are 
          presented on each page as well.",
          tags$ul(
            tags$li("The tab entitled", actionLink("hygeneLink", "HyGene"), "(short for \"Hypothesis Generation\") presents an overview of a 
                    previously developed computational model by the same name. On this page, you will be able to read about the core features 
                    of HyGene, learn how it was adapted to fit the context modeled in SiRJ, and interactively step through its mechanisms to 
                    understand how it operates. It is highly recommended to read and interact with the applications found in this section first 
                    as the HyGene framework is the foundation of the theory and model advanced by (CITATION REDACTED FOR BLIND REVIEW) and how 
                    it maps to important psychological systems and processes."),
            tags$li("The tab entitled", actionLink("sirjLink", "SiRJ"), "(short for \"Situated Reasoning and Judgment\") presents an overview 
                    of the computational model presented in (CITATION REDACTED FOR BLIND REVIEW). On this page, you will be able to explore 
                    how the various input parameters of that model operate as well as run and examine results from a small-scale simulation 
                    similar to the one presented in the paper."),
            tags$li("Lastly, the tab entitled OSF Page provides a direct link to the research project page for this paper housed on the Open Science 
                    Framework. All the code for SiRJ as well as the code used to run the simulations presented in the paper are available for download
                    from that page; additionally, the data from the simulations and empirical study as well as all analyses conducted for the paper
                    are provided.")
          )
        )
      )
    ),
    br()
  )