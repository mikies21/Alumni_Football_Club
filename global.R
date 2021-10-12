#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
library(shiny.router)
library(shinyWidgets)
library(shinydashboard)
# library(shinydashboardPlus)
library(tidyverse)
library(plotly)
library(xlsx)
library(RColorBrewer)
# Define UI for application that draws a histogram



### Get Data
player_stat <- readxl::read_xlsx("data/Player_List.xlsx", sheet = "Sheet1")

season <- str_split(list.files(path = "data/", pattern = "*season.xlsx"), "_") %>%
  lapply(function(x) {
    x[1]
  })
myfiles <- lapply(list.files(path = "data/", pattern = "*season.xlsx"), function(x) {
  list_df <- read.xlsx(paste("data/", x, sep = ""), sheetName = "Sheet1") %>%
    left_join(player_stat %>% select(ID, Picture), by = "ID")
})
names(myfiles) <- season

all_seasons_data <- myfiles %>%
  bind_rows(.id = "season")

player_all_statistics <- all_seasons_data %>%
  group_by(ID) %>%
  mutate(across(where(is.numeric), sum)) %>%
  slice_max(order_by = ID, n = 1, with_ties = F) %>%
  tidyr::unite(col = "NameSurname", sep = " ", remove = F, Name, Surname) %>%
  select(-season)



seasons_total <- all_seasons_data %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(across(where(is.double), sum))

# Total history stats
goals <- seasons_total$G %>% sum()

goalsagainst <- seasons_total$A %>% sum()


Yellow <- seasons_total$Y %>% sum()


Redcards <- seasons_total$R %>% sum()


SinBins <- seasons_total$SB %>% sum(na.rm = T)




season_stats_plot <- seasons_total %>%
  dplyr::select(season, G, A, Y, R, SB) %>%
  mutate(SB = ifelse(is.na(SB), 0, SB)) %>%
  plot_ly(x = ~season, y = ~G, name = "Goals", type = "scatter", mode = "lines", line = list(color = "purple")) %>%
  add_trace(y = ~A, name = "Goals Against", mode = "lines", line = list(color = "orange")) %>%
  add_trace(y = ~Y, name = "Yellow Card", mode = "lines", line = list(color = "#ada502")) %>%
  add_trace(y = ~R, name = "Red Card", mode = "lines", line = list(color = "red")) %>%
  add_trace(y = ~SB, name = "Sin Bin", mode = "lines", line = list(color = "green")) %>%
  layout(hovermode = "x unified", legend = list(
    orientation = "v",
    y = 1, x = -0.1
  ))



history_top_plyers <- all_seasons_data %>%
  group_by(ID) %>%
  mutate(across(where(is.numeric), sum)) %>%
  ungroup() %>%
  dplyr::select(-season) %>%
  dplyr::distinct() %>%
  tidyr::unite(col = "NameSurname", sep = " ", Name, Surname)

HistoryTopGoalscorer <- history_top_plyers %>% dplyr::slice_max(G, n = 1, with_ties = TRUE)

HistoryTopAppearence <- history_top_plyers %>% dplyr::slice_max(Ap, n = 1, with_ties = TRUE)

HistorySupersub <- history_top_plyers %>% dplyr::slice_max(AS, n = 1, with_ties = TRUE)

HistoryTopAssist <- history_top_plyers %>% dplyr::slice_max(A, n = 1, with_ties = TRUE)

HistoryTopRed <- history_top_plyers %>% dplyr::slice_max(R, n = 1, with_ties = TRUE)

HistoryTopYellow <- history_top_plyers %>% dplyr::slice_max(Y, n = 1, with_ties = TRUE)


source("server.R")
source("ui.R")


# Run the application
shinyApp(ui, server)
