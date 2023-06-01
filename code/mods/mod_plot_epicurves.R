#' Plot epicurve latice
#'
#' @description Plots echarts latice of epicurves at the health zone (or regional) level. Curves
#' will be colored according to their trend in the last 6 weeks (stable, increasing, decreasing,
#' unknown). **Warning**: this function is opinionated and expects specific column names.
#'
#' @param df `dataframe` Data to be plotted.
#' @param region `chr` Name of region to be plotted.
mod_plot_epicurves <- function(df, region, acts = NULL, add_acts = FALSE,
                               nrow = 6, ncol = 6, links = NULL) {
  colors <- list(Hausse = '#AA8439',
                 Stable = '#7887AB',
                 Baisse = '#2E4172',
                 Inconnu = '#8f8f8f')

  tmp <- df %>%
           filter(if_all(c(reg, zone, trend),
                         ~ !is.na(.)),
                  reg == region) %>%
           distinct()

  zones <- tmp %>%
             tidytable::select.(trend, zone) %>%
             unique() %>%
             mutate(zone_display = str_to_display(zone)) %>%
             #filter(if_all(everything(), ~ !is.na(.))) %>%
             arrange(zone)

  trends <- zones$trend
  names(trends) <- zones$zone

  zones_display <- zones$zone_display
  zones <- zones$zone
  
  #links <- paste0('<a href="https://www.w3resource.com/">', zones, '</a>')

  tmp <- tmp %>%
           tidytable::select.(zone, date, cases) %>%
           tidytable::pivot_wider.(names_from = zone,
                                   values_from = cases)

  e <- echarts4r::e_charts(tmp,
                           date,
                           renderer = 'svg')

  for (z in zones) {
    e <- e %>%
           echarts4r::e_bar_(z,
                             color = colors[[trends[[z]]]],
                             barCategoryGap = 0,
                             name = trends[[z]])

    if (add_acts) {
      tmp_act <- acts %>%
                   tidytable::filter.(zone == z)

      for (d in tmp_act$date) {
        e <- e %>%
               e_mark_line(data = list(xAxis = as.character(d),
                                       label = list(formatter = 'MSF')),
                           emphasis = list(disabled = TRUE),
                           symbol = 'circle')
      }
    }
  }
           
  e <- e %>%
    echarts4r::e_tooltip(formatter = htmlwidgets::JS("
                          function(params){
                            return('<small>' + params.value[0] + '</small><br>' +
                                   'Cas Suspectés: <strong>' + params.value[1] + '</strong>')
                          }")) %>%
    echarts4r::e_toolbox() %>%
    echarts4r::e_toolbox_feature(feature = c('dataZoom', 'saveAsImage'),
                                 type = 'png') %>%
    echarts4r::e_facet(legend_space = 5,
                       rows = nrow,
                       cols = ncol) %>%
    epiboards::e_grid_titles(titles = zones_display,
                             font_size = '1.25rem',
                             top_space = 4,
                             links = links) %>%
    echarts4r::e_grid(containLabel = TRUE)

  for (i in 1:length(e$x$opts$xAxis)) {
    e$x$opts$xAxis[[i]]$axisLabel$hideOverlap <- TRUE
  }

  for (i in 1:length(e$x$opts$yAxis)) {
    e$x$opts$yAxis[[i]]$axisLabel$hideOverlap <- TRUE
  }

  return(e)
}
