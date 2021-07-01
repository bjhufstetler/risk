server <- function(input, output, session){
   autoInvalidate <- reactiveTimer(3000, session)
   
   # Display Server Name
   serverName <- reactiveVal(value = "NA")
   teamName <- reactiveVal(value = "NA")
   teamMission <- reactiveVal(value = NA)
   output$serverName <- renderText({
      paste0("SERVER: ", serverName(), " || TEAM: ", teamName())
   })
   
   # Server Data
   serverData <- reactiveValues(data = NULL)
   mapData <- reactiveValues(data = NULL)
   observe({
      autoInvalidate()
      if(serverName() != "NA"){
         serverData$data <- Pull(serverName())
         mapData$data <- MapData(serverData$data)
         }
      })
   
   # Create New Server (Game) Modal
   observeEvent(input$buttonCreate,{
      showModal(modalDialog(
         shiny::textInput(inputId = "textServerName", label = "Server Name", value = GenerateID()),
         shiny::actionButton(inputId = "buttonCreateServer", label = "Create New Server")
      ))
   })
   
   observeEvent(input$buttonCreateServer,{
      # Set the server name, download server template, initialize game data, push server data, get mission data
      serverData$data <- Pull("DX_server")
      serverData$data <- InitializeGame(serverName(), serverData$data)
      Push(server = serverData$data, serverName = input$textServerName)
      serverName(input$textServerName)
      AddMissions(serverName())
      removeModal()
   })
   
   # Join Game Modal
   observeEvent(input$buttonJoin,{
       showModal(modalDialog(
               shiny::selectInput(inputId = "selectServerName", label = "Server Name", choices = GetServerNames()),
               shiny::selectInput(inputId = "selectTeam", label = "Team", choices = global$teams),
               shiny::actionButton(inputId = "buttonJoinServer", label = "Join")
           ))
   })
   
   # Join Game button clicked
   observeEvent(input$buttonJoinServer,{
       teamName(input$selectTeam)
       serverName(input$selectServerName)
       teamMission(GetMission(teamName(), serverName()))
       removeModal()
       show("serverName")
       show("buttonAttack")
       show("buttonMission")
       show("plotMap")
       hide("buttonCreate")
       hide("buttonJoin")
   })
   
   observeEvent(input$buttonAttack,{
      showModal(modalDialog(
         title = "Attack",
         selectInput(inputId = "selectAttacker", choices = )
         actionButton(inputId = "buttonAttackSubmit", label = "Submit")
      ))
   })
   
   observeEvent(input$buttonMission,{
      showModal(modalDialog(
         title = "Mission",
         teamMission(),
         easyClose = T
      ))
   })
   
    output$plotMap <- renderPlot({
       PlotMap(serverData$data)
 }) 
}