mod_plot_timeseries <- function(df, plot_dict = NULL, region = 'tanganyika', vars = c('cases'),
                                hover_decimals = 0) {
  
  # WRANGE -----------------------------------------------------------------------------------------
  tmp <- df %>%
          summarize.(cases = .sum(cases),
                     pop = sum(pop),
                     .by = c(reg, date)) %>%
          mutate.(inc = cases / pop * 1e4,
                  reg_display = tinker::str_to_display(reg)) %>%
          filter.(reg == region)
           

  # STYLE ------------------------------------------------------------------------------------------
  xaxis <- list(title = "",
                showgrid = FALSE,
                linewidth = 2,
                ticks = 'outside',
                #linecolor = 'rgb(204, 204, 204)',
                #tickcolor = 'rgb(204, 204, 204)',
                tickwidth = 2,
                ticklen = 5,
                tickfont = list(family = 'Arial',
                                size = 12),
                rangeslider = list(thickness = 0.03),
                type = 'date')

  hovertemplate <- paste0('<span style="font-size: 150%"><b> %{y:,.', hover_decimals, '0f}</b>',
                          '</span><br>',
                          '<b>%{text}</b><br>',
                          '<span style="font-size: 85%">%{x}</span><br><extra></extra>')
  margins <- list(l = 80,
                  r = 200,
                  b = 20,
                  t = 20,
                  pad = 20)

  # PLOT -------------------------------------------------------------------------------------------
  p <- list()

  for (var in vars) {
    if (is.null(plot_dict)) {
      color <- '#315799'
      title <- var
    } else {
      color <- plot_dict[[var]]$color
      title <- plot_dict[[var]]$title
    }

    p[[var]] <- 

      plotly::plot_ly(tmp) %>%
                  plotly::add_lines(x = ~date,
                                    y = as.formula(paste0('~', var)),
                                    split = ~reg,
                                    text = ~reg_display,
                                    hovertemplate = hovertemplate,
                                    showlegend = FALSE,
                                    color = I(color)) %>%
                  plotly::layout(annotations = list(xref = 'paper',
                                                    x = 1,
                                                    y = last(tmp[[var]]),
                                                    xanchor = 'left',
                                                    yanchor = 'middle',
                                                    text = tinker::str_to_display(region))) %>%
                  plotly::layout(yaxis = list(title = title,
                                              show_grid = FALSE,
                                              autorange = TRUE,
                                              fixedrange = FALSE,
                                              rangemode = 'tozero',
                                              zerolinecolor = 'rgb(82, 82, 82)',
                                              zerolinewidth = 2),
                                 xaxis = xaxis,
                                 margin = margins)

  }

  out <- plotly::subplot(p,
                         nrows = length(vars),
                         shareX = TRUE,
                         titleY = TRUE) %>%
         plotly::layout(xaxis = xaxis,
                        hoverlabel = list(bgcolor = 'white',
                                          borderwidth = 2,
                                          bordercolor = 'rgb(204, 204, 204)',
                                          font = list(color = 'black'))) %>%
         plotly::config(locale = 'fr',
                        displayModeBar = FALSE) %>% 
         htmlwidgets::onRender(paste0("function(el, x) {",
                                        "Plotly.d3",
                                              ".select('.cursor-crosshair')",
                                              ".style('cursor', 'default')}"))

  return(out)
  #df <- highlight_key(df)
  #p <- list()

  #for (v in vars) {
    #p[[v]] <- plot_ly(df) %>%
                #add_lines(x = ~date,
                          #y = as.formula(paste0('~', v)),
                          #split = ~name,
                          #text = ~name,
                          #hovertemplate = hovertemplate,
                          #showlegend = FALSE,
                          #color = I(plot_dict[[v]]$color)) %>%
                #layout(yaxis = list(title = plot_dict[[v]]$title,
                                    #autorange = TRUE,
                                    #fixedrange = FALSE,
                                    #rangemode = 'tozero',
                                    #zerolinecolor = 'rgb(82, 82, 82)',
                                    #zerolinewidth = 2))
}
