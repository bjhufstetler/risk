library(googlesheets4)
library(tibble)
library(dplyr)
library(ggplot2)
library(tidyr)
library(grid)
library(jpeg)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyalert)

source("functions/risk.R")

global <- NULL

# Admin password
global$key = "533risk"

# Team names
global$teams <- c("blue", "green", "red", "yellow")

# Link to google sheet (regions, cards, created server)
global$link <- "https://docs.google.com/spreadsheets/d/1xrerTPaHp5cXZVkhtbPGHfi_st1_Obo0QAKOLZq4a8s/edit?usp=sharing"
# Download country and card data  
global$countries <- googlesheets4::read_sheet(ss = global$link, sheet = "DX_regions")
global$cards <- googlesheets4::read_sheet(ss = global$link, sheet = "DX_cards")

global$relationships <- tibble::tribble(~attacker, ~n1, ~n2, ~n3, ~n4, ~n5, ~n6,
                                        "Alaska", "Kamchatka", "North West Territory", "Alberta", NA, NA, NA,
                                        "North West Territory", "Alaska", "Alberta", "Ontario", "Greenland", NA, NA,
                                        "Alberta", "Alaska", "North West Territory", "Ontario", "Western United States", "North West Territory", NA,
                                        "Ontario", "Alberta", "North West Territory", "Western United States", "Eastern United States", "Quebec", "Greenland",
                                        "Quebec", "Ontario", "Greenland", "Eastern United States", NA, NA, NA,
                                        "Greenland", "North West Territory", "Ontario", "Quebec", "Iceland", NA, NA,
                                        "Western United States", "Alberta", "Ontario", "Eastern United States", "Central America", NA, NA,
                                        "Eastern United States", "Central America", "Western United States", "Ontario", "Quebec", NA, NA,
                                        "Central America", "Western United States", "Eastern United States", "Venezuela", NA, NA, NA,
                                        "Venezuela", "Central America", "Peru", "Brazil", NA, NA, NA,
                                        "Peru", "Venezuela", "Brazil", "Argentina", NA, NA, NA,
                                        "Brazil", "Venezuela", "Peru", "Argentina", "North Africa", NA, NA,
                                        "Argentina", "Peru", "Brazil", NA, NA, NA, NA,
                                        "North Africa", "Brazil", "Western Europe", "Southern Europe", "Egypt", "East Africa", "Congo",
                                        "Egypt", "North Africa", "Southern Europe", "Middle East", "East Africa", NA, NA,
                                        "Congo", "North Africa", "East Africa", "South Africa", NA, NA, NA,
                                        "East Africa", "Egypt", "Middle East", "Madagascar", "South Africa", "Congo", "North Africa",
                                        "South Africa", "Congo", "East Africa", "Madagascar", NA, NA, NA,
                                        "Madagascar", "South Africa", "East Africa", NA, NA, NA, NA,
                                        "Iceland", "Greenland", "Great Britain", "Scandinavia", NA, NA, NA,
                                        "Great Britain", "Iceland", "Scandinavia", "Northern Europe", "Western Europe", NA, NA,
                                        "Western Europe", "Great Britain", "Northern Europe", "Southern Europe", "North Africa", NA, NA,
                                        "Scandinavia", "Iceland", "Great Britain", "Northern Europe", "Ukraine", NA, NA,
                                        "Northern Europe", "Great Britain", "Scandinavia", "Ukraine", "Southern Europe", "Western Europe", NA,
                                        "Southern Europe", "Western Europe", "Northern Europe", "Ukraine", "Middle East", "North Africa", "Egypt",
                                        "Ukraine", "Scandinavia", "Northern Europe", "Southern Europe", "Ural", "Afghanistan", "Middle East",
                                        "Indonesia", "Siam", "New Guinea", "Western Australia", NA, NA, NA,
                                        "New Guinea", "Indonesia", "Western Australia", "Eastern Australia", NA, NA, NA,
                                        "Western Australia", "Indonesia", "New Guinea", "Eastern Australia", NA, NA, NA,
                                        "Eastern Australia", "New Guinea", "Western Australia", NA, NA, NA, NA,
                                        "Ural", "Ukraine", "Afghanistan", "China", "Siberia", NA, NA,
                                        "Afghanistan", "Ukraine", "Ural", "China", "India", "Middle East", NA,
                                        "Middle East", "Southern Europe", "Egypt", "East Africa", "India", "Afghanistan", "Ukraine",
                                        "Siberia", "Ural", "China", "Mongolia", "Irkutsk", "Yakutsk", NA,
                                        "Yakutsk", "Siberia", "Irkutsk", "Kamchatka", NA, NA, NA,
                                        "Irkutsk", "Siberia", "Yakutsk", "Kamchatka", "Mongolia", NA, NA,
                                        "Mongolia", "Siberia", "Irkutsk", "Kamchatka", "Japan", "China", NA,
                                        "China", "Afghanistan", "Ural", "Mongolia", "Siam", "India", NA,
                                        "India", "Middle East", "Afghanistan", "China", "Siam", NA, NA,
                                        "Siam", "India", "China", "Indonesia", NA, NA, NA,
                                        "Japan", "Kamchatka", "Mongolia", NA, NA, NA, NA,
                                        "Kamchatka", "Yakutsk", "Irkutsk", "Mongolia", "Alaska", "Japan", NA) %>%
  pivot_longer(cols = paste0("n", 1:6), names_to = "neighbor", values_to = "defender") %>%
  select(attacker, defender) %>%
  filter(!is.na(defender))

# actions:
# initialize, bolster, attack, overthrow, fortify