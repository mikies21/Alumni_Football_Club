



server <- function(input, output, session) {
  # SeasonPick
  output$SeasonPick_UI <- renderUI(
    pickerInput(
      inputId = "SeasonPick",
      label = "Pick Season",
      choices = names(myfiles),
      selected = names(myfiles)[length(names(myfiles))],
      inline = T,
      multiple = F,
      width = "100%",
      options = list(
        `live-search` = TRUE
      )
    )
  )
  
  # get all plauers data for a specifiv picked season

  player_season_data <- reactive({
    myfiles %>%
      bind_rows(.id = "season") %>%
      dplyr::filter(season == input$SeasonPick)
  })

  
  # render the line plot with every Alumni season

  output$season_stat_plot <- renderPlotly(
    season_stats_plot
  )


  # get overall Alumni stats for a picked season
  filter_season <- reactive({
    seasons_total %>%
      dplyr::filter(season == input$SeasonPick)
  })

  
  # Pick a plaiyer during a specific season

  output$SeasonPlayerPick_UI <- renderUI({
    players_in_season <- player_season_data() %>%
      tidyr::unite(col = "Name_Surname", Name, Surname, sep = " ") %>%
      dplyr::pull(Name_Surname)

    list_IDs <- as.list(player_season_data()$ID)

    names(list_IDs) <- players_in_season

    pickerInput(
      inputId = "SeasonPlayerPick",
      label = paste0("Pick multiple to compare player stats during the ", input$SeasonPick, " season"),
      choices = list_IDs,
      inline = T,
      multiple = T,
      width = "100%",
      options = list(
        `live-search` = TRUE,
        `selected-text-format` = "count > 2",
        `actions-box` = TRUE
      )
    )
  })

  output$SeasonPlayerTable <- DT::renderDataTable(
    {
      if (is.null(input$SeasonPlayerPick)) {
        player_season_data() %>%
          dplyr::select(Picture, Name, Surname, Ap, AS, G, A, Y, R, MOM, SB)
      } else {
        player_season_data() %>%
          dplyr::filter(ID %in% input$SeasonPlayerPick) %>%
          dplyr::select(Picture, Name, Surname, Ap, AS, G, A, Y, R, MOM, SB)
      }
    },
    escape = FALSE,
    rownames = FALSE,
    options = list(autoWidth = TRUE, scrollX = T)
  )
  
  
  output$SeasonGoalBoxplot <- renderPlotly({
    dat <- seasons_total %>%
      select(season, G, A) # %>%
    
    plot_ly(dat,
      type = "box", text = ~season, y = ~G, name = "Goals", boxpoints = "all", pointpos = 0,
      marker = list(color = "black"),
      line = list(color = "black"),
      fillcolor = "rgb(121, 32, 189)"
    ) %>%
      add_boxplot(
        y = ~A, text = ~season, name = "Goals Against", boxpoints = "all", pointpos = 0,
        marker = list(color = "black"),
        line = list(color = "black"),
        fillcolor = "rgb(230, 168, 76)"
      ) %>%
      layout(showlegend = FALSE)
  })

  output$SeasonbookingsBoxplot <- renderPlotly({
    dat <- seasons_total %>%
      select(season, Y, R, SB) 

    plot_ly(dat,
      type = "box", text = ~season, y = ~Y, name = "Yellow Cards", boxpoints = "all", pointpos = 0,
      marker = list(color = "black"),
      line = list(color = "black"),
      fillcolor = "rgb(222, 224, 63)"
    ) %>%
      add_boxplot(
        y = ~R, text = ~season, name = "Red Cards", boxpoints = "all", pointpos = 0,
        marker = list(color = "black"),
        line = list(color = "black"),
        fillcolor = "rgb(240, 70, 70)"
      ) %>%
      add_boxplot(
        y = ~SB, text = ~season, name = "Sin Bins", boxpoints = "all", pointpos = 0,
        marker = list(color = "black"),
        line = list(color = "black"),
        fillcolor = "rgb(43, 153, 41)"
      ) %>%
      layout(showlegend = FALSE)
  })
  
  output$seasonStats <- renderText({
    players_in_season <- player_season_data() %>%
      tidyr::unite(col = "Name_Surname", Name, Surname, sep = " ")
    
    topG <- players_in_season %>% slice_max(order_by = G, n = 1, with_ties = T)
    if (nrow(topG)>1) {
      topG <- paste0('multip G')
    } else {
      topG <- paste0(topG[1, 'Name_Surname'], ' with ', topG[1, 'G'], ' goals')
    }
    
    paste0('During the season add trophies...',
           'the player with most Goals was ', topG)
  })
  
  

  output$SeasonPlayerPlot <- renderPlotly({
    if (is.null(input$SeasonPlayerPick)) {
      return(NULL)
    } else {
      dat <- player_season_data() %>%
        dplyr::select(ID, Name, Surname, Ap, AS, G, A, Y, R, MOM, SB) %>%
        dplyr::filter(ID %in% input$SeasonPlayerPick) %>%
        mutate(SB = ifelse(is.na(SB), 0, SB)) %>%
        unite(col = "NameSurname", Name, Surname, sep = " ", remove = F) %>%
        pivot_longer(cols = c(Ap, AS, G, A, Y, R, MOM, SB), names_to = "attribute", values_to = "value") %>%
        mutate(
          attribute = str_replace_all(attribute, pattern = " ", replacement = ""),
          attribute = case_when(
            str_detect(attribute, "\\bAp\\b") ~ "Appearences",
            str_detect(attribute, "\\bAS\\b") ~ "Ap as Sub",
            str_detect(attribute, "\\bMOM\\b") ~ "MOM",
            str_detect(attribute, "\\bA\\b") ~ "Assists",
            str_detect(attribute, "\\bG\\b") ~ "Goals",
            str_detect(attribute, "\\bY\\b") ~ "Yellow Cards",
            str_detect(attribute, "\\bR\\b") ~ "Red Cards",
            str_detect(attribute, "\\bSB\\b") ~ "Sin Bins"
          ),
          attribute = factor(attribute, levels = c("Appearences", "Ap as Sub", "Assists", "Goals", "MOM", "Yellow Cards", "Red Cards", "Sin Bins"))
        )
      # mutate(attribute = fct_relevel(attribute, c('Ap',)))

      p <- dat %>%
        ggplot(aes(x = attribute, y = value)) +
        geom_bar(aes(fill = attribute), colour = "black", stat = "identity", position = position_dodge(width = 0)) +
        facet_grid(~NameSurname) +
        theme_bw(base_size = 10) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1.1),
          legend.title = element_blank(),
          legend.position = "left"
        ) +
        ylim(0, NA)
      scale_fill_brewer(palette = "Dark2")
      ggplotly(p)


      ## calculate highest goalscorer in selected season
    }
  })


  output$PlayersStatPlot <- renderPlotly({
    player <- all_seasons_data %>% filter(ID == input$PlayerStatPick)

    plot_ly(player, x = ~season, y = ~Ap, type = "bar", name = "Apperarences") %>%
      add_trace(y = ~G, name = "Goals") %>%
      add_trace(y = ~A, name = "Assists") %>%
      add_trace(y = ~AS, name = "Apps as Sub") %>%
      add_trace(y = ~Y, name = "Yellow Cards") %>%
      add_trace(y = ~R, name = "Red Cards") %>%
      add_trace(y = ~SB, name = "Sin Bins") %>%
      layout(yaxis = list(title = "Count"), barmode = "group")
  })





  output$overall_stats <- DT::renderDataTable(history_top_plyers)
}
