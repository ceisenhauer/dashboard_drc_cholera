mod_plot_bar <- function(df, level = 'reg') {
  if (level == 'zone') {
    df <- df %>%
            mutate.(reg_display = tinker::str_to_display(zone),
            cases = cases_4w)
  }

  tmp <- df %>%
           tidytable::select.(reg_display, trend, cases) %>%
           tidytable::pivot_wider.(names_from = trend,
                                   values_from = cases)

  if ('Baisse' %notin% names(tmp)) {
    tmp$Baisse <- NA
  }
  
  if ('Hausse' %notin% names(tmp)) {
    tmp$Hausse <- NA
  }

  if ('Stable' %notin% names(tmp)) {
    tmp$Stable <- NA
  }

  if ('Inconnu' %notin% names(tmp)) {
    tmp[['Inconnu']] <- NA
  }

  tmp <- tmp %>%
           mutate_rowwise.(total = sum(Baisse, Hausse, Stable, Inconnu, na.rm = TRUE)) %>%
           arrange(total)

  tmp <- tmp %>%
    left_join(trends)

  e <- echarts4r::e_charts(tmp,
                           reg_display,
                           renderer = 'svg') %>%
         e_bar(Hausse,
               color = '#AA8439',
               stack = 'grp',
               universalTransition = TRUE,
               animationDurationUpdate = 1000L) %>%
         e_bar(Stable,
               color = '#7887AB',
               stack = 'grp',
               universalTransition = TRUE,
               animationDurationUpdate = 1000L) %>%
         e_bar(Baisse,
               color = '#2E4172',
               stack = 'grp',
               universalTransition = TRUE,
               animationDurationUpdate = 1000L) %>%
         e_bar(Inconnu,
               color = '#8f8f8f',
               stack = 'grp',
               universalTransition = TRUE,
               animationDurationUpdate = 1000L) %>%
         echarts4r::e_tooltip(formatter = '{b} : {d}',
                              trigger = 'axis') %>%
         echarts4r::e_toolbox(orient = 'vertical') %>%
         echarts4r::e_toolbox_feature(feature = c('dataZoom', 'saveAsImage'),
                                      type = 'png') %>%
         echarts4r::e_tooltip(formatter = htmlwidgets::JS("
                               function(params){
                                 return('<small>' + params.value[1] + '</small><br>' +
                                        'Cas Suspectés: <strong>' + params.value[0] + '</strong>')
                               }")) %>%
         e_x_axis(type = 'category',
                  axisLabel = list(interval = 0,
                                   rotate = 0)) %>%
        e_flip_coords() %>%
        e_legend(bottom = 0,
                 left = 'center',
                 orient = 'horizontal') %>%
        e_grid(containLabel = TRUE,
               top = 5,
               bottom = 25)

  return(e)
}
