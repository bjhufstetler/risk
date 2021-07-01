InitializeGame <- function(serverName, serverData){
  # serverName <- "DXDX"
  # teamName <- "green"
  # serverData <- googlesheets4::read_sheet(ss = global$link, sheet = serverName)
  
  # Assign countries to teams randomly
  init <- tibble::tibble(team = (global$teams %>% rep(11))[1:42],
                         region = sample(1:42))
  init <- init[,1] %>%
    cbind(global$countries[init$region,4])
  
  serverData %>%
    rbind(tibble::tibble(round = 1,
                         action = "init",
                         team = init$team,
                         region = init$region,
                         armies = 1,
                         d1 = NA,
                         d2 = NA,
                         d3 = NA,
                         team2 = NA,
                         region2 = NA,
                         d4 = NA,
                         d5 = NA)
    )
}

AddMissions <- function(serverName){
  # Select 4 random mission cards
  missions <- sample(rep(1:4, c(3,6,1,1)))[1:4]
  
  # Create a Hamiltonian path with <=1 clique for mission 1 cards.
  mission1_teams <- sample(global$teams)
  mission1 <- tibble::tibble(server = serverName,
                             team = mission1_teams,
                             mission = paste0("Eliminate the ", mission1_teams[c(2:4, 1)], " team."),
                             created = Sys.time())
  
  # Create a description for all mission 2 cards
  mission2 <- paste0("Conquer the continents of ", 
                     c("ASIA and SOUTH AMERICA.",
                       "ASIA and AFRICA.",
                       "EUROPE, AUSTRALIA, and one other continent of your choice.",
                       "EUROPE, SOUTH AMERICA, and one other continent of your choice.",
                       "NORTH AMERICA and AFRICA.",
                       "NORTH AMERICA and AUSTRALIA.")) %>%
    sample()
  
  # Assign selected mission cards to each team.
  missionList <- mission1 %>%
    mutate(missionNumber = missions,
           row = row_number(),
           mission = case_when(missionNumber == 2 ~ mission2[row],
                               missionNumber == 3 ~ "Occupy 24 territories with at least 1 army in each territory.",
                               missionNumber == 4 ~ "Occupy 18 territories with at least 2 armies in each territory.",
                               T ~ mission)) %>%
    select(server, team, mission, created)
  googlesheets4::read_sheet(ss = global$link, sheet = "DX_missions") %>%
    mutate(server = server %>% as.character,
           team = team %>% as.character,
           mission = mission %>% as.character,
           created = created %>% as.POSIXct()) %>%
    add_row(missionList) %>%
    googlesheets4::write_sheet(ss = global$link, sheet = "DX_missions")
  
}

GetMission <- function(teamName, serverName){
  (googlesheets4::read_sheet(ss = global$link, sheet = "DX_missions") %>%
    filter(server == serverName,
           team == teamName))$mission
}

Push <- function(serverName, server){
  server %>%
    googlesheets4::write_sheet(ss = global$link, sheet = serverName)
}

Pull <- function(serverName){
  googlesheets4::read_sheet(ss = global$link, sheet = serverName)
}

GetServerNames <- function(){
  serverNames <- googlesheets4::sheet_names(ss = global$link)
  serverNames[!(serverNames %in% c("DX_regions", "DX_cards", "DX_server", "DX_missions"))]
}

GenerateID <- function(n = 5000) {
  a <- do.call(paste0, replicate(2, sample(LETTERS, 1, TRUE), FALSE))
  paste0(a, sprintf("%02d", sample(99, 1, TRUE))) 
}

PlotMap <- function(serverData){
  # serverName <- "DXDX"
  # teamName <- "green"
  # serverData <- googlesheets4::read_sheet(ss = global$link, sheet = serverName)
  
  # mapData needs region, x, y, team, armies
  # take serverData and manipulate as time series
  # TODO:
  # 1) determine army delta from dice rolls
  # 2) get resulting army counts
  # 3) determine team in control of region

  
    

    
  
  serverData %>%
    left_join(global$countries[,c(1:2,4)], by = "region") %>%
    ggplot() +
    aes(x = x,
        y = y,
        label = armies,
        fill = team) +
    annotation_custom(rasterGrob(readJPEG("riskmap.jpeg"), 
                                 width = unit(1,"npc"), 
                                 height = unit(1,"npc")), 
                      -Inf, Inf, -Inf, Inf) +
    geom_label() +
    xlim(35,765) +
    ylim(22,513) +
    coord_fixed() + 
    theme_void() +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("red" = "red",
                                 "green" = "green",
                                 "yellow" = "yellow",
                                 "blue" = "blue"))
}

MapData <- function(serverData){
  serverData %>%
    rbind(tibble::tibble(round = 2,
                         action = "attack",
                         team = "blue",
                         region = "Eastern Australia",
                         armies = NA,
                         d1 = c(6, 6, 3, 6, 5, 6),
                         d2 = c(1, 2, 3, NA, NA, NA),
                         d3 = c(1, 1, NA, NA, NA, NA),
                         team2 = NA,
                         region2 = "Quebec",
                         d4 = c(3, 3, 4, 5, 1, 1),
                         d5 = c(NA, 2, 3, 4, NA, NA))) %>%
    rowwise() %>%
    mutate(dtot = min(length(na.omit(c(d1,d2,d3))), length(na.omit(c(d4, d5)))),
           loser1 = case_when(dtot < 1 ~ NA %>% as.character(),
                              sort(na.omit(c(d1, d2, d3)), decreasing = T)[1] <= sort(na.omit(c(d4, d5)), decreasing = T)[1] ~ region,
                              T ~ region2),
           loser2 = case_when(dtot < 2 ~ NA %>% as.character(),
                              sort(na.omit(c(d1, d2, d3)), decreasing = T)[2] <= sort(na.omit(c(d4, d5)), decreasing = T)[2] ~ region,
                              T ~ region2)) %>% View()
}
