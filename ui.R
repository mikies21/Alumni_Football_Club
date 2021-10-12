

ui <- tags$div(
  # shinyWidgets::setBackgroundImage(src = "football_background.jpg"),
  shinyWidgets::useShinydashboard(),
  navbarPage(
    title = "Alumni Football Club",
    id = "SelectedTab",
    position = "static-top",
    collapsible = T,
    selected = NULL,
    fluid = T,

    # tags$head(
    #   tags$link(href = "mystyle.css", rel = "stylesheet", type = "text/css")
    #  ),
    tabPanel(
      title = "Home", value = "HomeTab",
      box(
        title = "homepage", width = 6,
        "Lads, this is a little project I have that I will use as a to learn a few thigs and as a portfolio for my prospectiv recruiters", br(),
        'just straight to the next page "season stats" and play around with that page. I am looking at the old data and will be adding alot more features soon', br(),
        "please let me know what things you would love to see. I can costumise it as much as i want a create beutiful graphs that would put sky sports to shame.", br(),
        "Thank you", background = "blue",
      )
    ),
    tabPanel(
      title = "Season Stats", value = "SeasonStatTab",
      fluidPage(
        fluidRow(
          column(
            width = 12,
            valueBox(value = goals, subtitle = "Total Goals Scored", icon = icon("futbol"), color = "purple", width = 3),
            valueBox(
              value = goalsagainst, subtitle = "Total Goals Against", icon = icon("futbol"),
              color = "orange", width = 3
            ),
            valueBox(
              value = Yellow, subtitle = "Yellow Cards", icon = icon("futbol"),
              color = "yellow", width = 2
            ),
            valueBox(
              value = Redcards, subtitle = "Red cards", icon = icon("futbol"),
              color = "red", width = 2
            ),
            valueBox(
              value = SinBins, subtitle = "sin bin", icon = icon("futbol"),
              color = "green", width = 2
            )
          )
        ),
        fluidRow(
          h1("Alumni Football Club across the seasons"),
          column(
            width = 5,
            plotlyOutput("season_stat_plot")
          ),
          column(
            width = 3,
            plotlyOutput("SeasonGoalBoxplot")
          ),
          column(
            width = 4,
            plotlyOutput("SeasonbookingsBoxplot")
          )
        ),
        fluidRow(
          box(
            width = 3,
            uiOutput("SeasonPick_UI"),
            background = 'blue', collapsible = F,collapsed = F,
            textOutput(outputId = 'seasonStats')
          ),
          column(
            width = 3,
            uiOutput("SeasonPlayerPick_UI")
          )
        ),
        fluidRow(
          column(
            width = 4,
            DT::dataTableOutput("SeasonPlayerTable")
          ),
          column(
            width = 8,
            plotlyOutput("SeasonPlayerPlot")
          )
        )
      )
    ),
    tabPanel(
      title = "Player Stats", value = "PlayerStatsTab",
      fluidPage(
        fluidRow(
          column(3, shinydashboardPlus::userBox(
            id = "TopGoalscorer",
            title = shinydashboardPlus::userDescription(
              title = HistoryTopGoalscorer[1, "NameSurname"],
              subtitle = paste0("Top Goalscorer: ", HistoryTopGoalscorer[1, "G"]),
              type = 2,
              image = "MicheleFresneda.jpg", imageElevation = 1
            ), background = "purple", collapsible = F, collapsed = F, width = 12
          )),
          column(3, shinydashboardPlus::userBox(
            id = "TopAppearence",
            title = shinydashboardPlus::userDescription(
              title = HistoryTopAppearence[1, "NameSurname"],
              subtitle = paste0("Most Appearences with Alumni: ", HistoryTopGoalscorer[1, "Ap"]),
              type = 2,
              image = "MicheleFresneda.jpg", imageElevation = 1
            ), background = "green", collapsible = F, collapsed = F, width = 12
          )),
          column(3, shinydashboardPlus::userBox(
            id = "TopAssistman",
            title = shinydashboardPlus::userDescription(
              title = HistoryTopAssist[1, "NameSurname"],
              subtitle = paste0("Most assist:  ", HistoryTopAssist[1, "A"]),
              type = 2,
              image = "MicheleFresneda.jpg", imageElevation = 1
            ), background = "yellow", collapsible = F, collapsed = F, width = 12
          )),
          column(3, shinydashboardPlus::userBox(
            id = "Supersub",
            title = shinydashboardPlus::userDescription(
              title = HistorySupersub[1, "NameSurname"],
              subtitle = paste0("SuperSub ", HistorySupersub[1, "AS"]),
              type = 2,
              image = "MicheleFresneda.jpg", imageElevation = 1
            ), background = "yellow", collapsible = F, collapsed = F, width = 12
          ))
        ),
        fluidRow(
          column(3, shinydashboardPlus::userBox(
            id = "Reds",
            title = shinydashboardPlus::userDescription(
              title = HistoryTopRed[1, "NameSurname"],
              subtitle = paste0("Most Red Cards ", HistoryTopRed[1, "R"]),
              type = 2,
              image = "MicheleFresneda.jpg", imageElevation = 1
            ), background = "yellow", collapsible = F, collapsed = F, width = 12
          )),
          column(3, shinydashboardPlus::userBox(
            id = "Reds",
            title = shinydashboardPlus::userDescription(
              title = HistoryTopYellow[1, "NameSurname"],
              subtitle = paste0("Most Yellow Cards ", HistoryTopYellow[1, "Y"]),
              type = 2,
              image = "MicheleFresneda.jpg", imageElevation = 1
            ), background = "yellow", collapsible = F, collapsed = F, width = 12
          ))
        ),
        fluidRow(
          column(
            width = 3,
            pickerInput(
              inputId = "PlayerStatPick",
              label = "Pick a player",
              choices = setNames(object = as.list(player_all_statistics$ID), nm = player_all_statistics$NameSurname),
              inline = T,
              multiple = F,
              options = list(
                `live-search` = TRUE,
                `selected-text-format` = "count > 2",
                `actions-box` = TRUE
              )
            )
          ),
          column(
            width = 9,
            plotlyOutput("PlayersStatPlot")
          )
        ),
        fluidRow(column(width = 12, DT::dataTableOutput("overall_stats")))
      )
    )
  )
)
