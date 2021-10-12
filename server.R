



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

  player_season_data <- reactive({
    myfiles %>%
      bind_rows(.id = "season") %>%
      dplyr::filter(season == input$SeasonPick)
  })


  output$season_stat_plot <- renderPlotly(
    season_stats_plot
  )


  filter_season <- reactive({
    seasons_total %>%
      dplyr::filter(season == input$SeasonPick)
  })

  # ValueBoxes cards





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


  output$Apps <- renderPlotly({
    dat <- player_season_data() %>% filter(Surname %in% c("Fresneda", "Ferguson"))
    p <- ggplot(dat, aes(x = Surname, y = Ap)) +
      geom_bar(stat = "identity")

    fig <- ggplotly(p)
  })

  output$SeasonGoalBoxplot <- renderPlotly({
    dat <- seasons_total %>%
      select(season, G, A) # %>%
    # dplyr::rename(Goals = G, `Goals Against` = A) %>%
    # pivot_longer(!season, names_to = 'attribute', values_to = 'value')

    # p <- ggplot(dat, aes(x = attribute, y = value))+
    #  geom_boxplot(aes(fill = attribute),show.legend = F)+
    #  geom_jitter(width = 0.1,show.legend = F)+
    #  theme_bw(base_size = 10)+
    #  theme(axis.title.y = element_blank(),
    #        axis.title.x = element_blank())+
    #  facet_grid(~attribute, scales = c('free'))+
    #  scale_fill_manual(values = c('Goals' = 'purple',
    #                                'Goals Against' = 'orange'))
    #  fig <- ggplotly(p)%>% layout(showlegend = FALSE, hooverinfo = 'y')
    #
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
      select(season, Y, R, SB) # %>%
    # dplyr::rename(`Yellow cards`=Y,`Red cards` = R, `Sin bins` = SB) %>%
    # pivot_longer(!season, names_to = 'attribute', values_to = 'value')

    # p <- ggplot(dat, aes(x = attribute, y = value))+
    #  geom_boxplot(aes(fill = attribute),show.legend = F)+
    #  geom_jitter(width = 0.1, show.legend = F)+
    #  theme_bw(base_size = 10)+
    #  theme(axis.title.y = element_blank(),
    #        axis.title.x = element_blank())+
    #  facet_grid(~attribute, scales = c('free'))+
    #  scale_fill_manual(values = c('Yellow cards' = '#ada502',
    #                                'Red cards' = 'red',
    #                                'Sin bins' = 'green'))
    # fig <- ggplotly(p) %>% layout(showlegend = FALSE)

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
