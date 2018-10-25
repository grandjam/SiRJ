####################
# UI FUNCTIONALITY #
####################
observeEvent(input$hygeneLink, {
  updateNavbarPage(session, "mainNavBar", "HyGene")
})
observeEvent(input$sirjLink, {
  updateNavbarPage(session, "mainNavBar", "SiRJ")
})