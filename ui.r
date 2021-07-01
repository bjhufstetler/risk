function(request) {
  header <- dashboardHeader(title = "533 TRS RISK")
  
  sidebar <- dashboardSidebar(
    shinyalert::useShinyalert(),
    shinyjs::useShinyjs(),
    hidden(textOutput(outputId = "serverName")),
    actionButton(inputId = "buttonCreate", label = "Create"),
    actionButton(inputId = "buttonJoin", label = "Join"),
    hidden(actionButton(inputId = "buttonAttack", label = "Attack")),
    hidden(actionButton(inputId = "buttonMission", label = "Mission"))
  )
  
  body <- dashboardBody(
    shinyalert::useShinyalert(),
    shinyjs::useShinyjs(),
    tabBox(width = NULL,
           tabPanel(
             h5("Overview"),
             hidden(plotOutput("plotMap"))
           ),
           tabPanel(
             h5("Play History")
           ),
           tabPanel(
             h5("Stats")
           )
    )
  )
  
  dashboardPage(
    header,
    sidebar,
    body
  )
}
