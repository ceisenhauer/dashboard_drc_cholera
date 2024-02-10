mod_render_table <- function(tbl, link_list) {
  colors <- list(alert = '#E6B8B3',
                 cases = '#D6D8B4',
                 deaths = '#A7B9D2')

  badge_colors <- list(alert = list(Alerte = '#e6b8b3'),
                       endemic = list(Endemique = '#e6b8b3'),
                       cases = list(Hausse = '#d6d8b4',
                                    Stable = '#f0eeeb',
                                    Baisse = '#A7B9D2'))

  tooltips <- list(
	  oc = 'section msf responsable dans la zone',
    alert = paste('une alerte suspectée est déclenchée lorsqu\'une zone compte un cas suspect;',
                  'une zone restera en alerte, même en l\'absence de nouveaux cas, jusqu\'à 4',
                  'semaines, après quoi l\'alerte sera levée'),
    endemic = 'indicateur de l\'endemicité du choléra dans la zone',
    neighbors = 'nombre de zones de santé voisines en alerte',
    cases_spk_long = paste('courbe des cas suspects hebdomadaires pendant l\'année en cours',
                           ' [source : IDS]'),
    cases_4w = 'cas suspects au cours des quatre dernières semaines [source : IDS]',
    cases_trend = paste('tendance actuelle des cas suspects (calculée par rapport à une tendance',
                         'exponentielle sur six semaines) [source : IDS]'),
    deaths_4w = 'nombre de décès au cours des quatre dernières semaines [source : IDS]',
    cfr_4w = 'taux de létalité au cours des quatre dernières semaines [source : IDS]'
  )

  columns <- list(zone = col_base(name = 'Zone de Santé',
                                  sticky = 'left',
                                  min_width = 100),
                  reg = col_base(name = 'Province',
                                 cell = function(value) {
                                   value_display <- str_to_display(value)

                                   if (value %in% link_list) {
                                     value <- gsub('_', '-', value)
                                     out <- htmltools::tags$a(href = paste0('#', value),
                                                              #target = 'self',
                                                              value_display)
                                   } else {
                                     out <- value_display
                                   }

                                   return(out)
                                 },
                                 sticky = 'left',
                                 border_right = TRUE,
                                 min_width = 100),
                  oc = col_base(name = 'OC',
                                tooltip = tooltips$oc,
                                maxWidth = 60),
                  alert = col_badge(name = 'Alerte',
                                    tooltip = tooltips$alert,
                                    colors = badge_colors$alert,
                                    min_width = 60,
                                    maxWidth = 70),
                  neighbors = col_bar(tbl, 
                                      col = 'neighbors',
                                      name = 'Alertes Voisinales',
                                      tooltip = tooltips$neighbors,
                                      min_width = 80,
                                      color = colors$alert),
                  endemic = col_badge(name = 'Endemique',
                                      tooltip = tooltips$endemic,
                                      colors = badge_colors$endemic),
                  trend = col_badge(name = 'Tendance',
                                    tooltip = tooltips$cases_trend,
                                    color = badge_colors$cases),
                  cases_spk_long = col_sparkline(data = tbl$cases_spk_long,
                                                 tooltip = tooltips$cases_spk_long,
                                                 minWidth = 150,
                                                 maxWidth = 200,
                                                 color = colors$cases),
                  cases_4w = col_bar(tbl,
                                     col = 'cases_4w',
                                     name = '4 Sem.',
                                     tooltip = tooltips$cases_4w,
                                     color = colors$cases),
                  deaths_4w = col_bar(tbl,
                                      col = 'deaths_4w',
                                      name = '4 Sem.',
                                      tooltip = tooltips$deaths_4w,
                                      color = colors$deaths),
                  cfr_4w = col_bar(tbl,
                                   col = 'cfr_4w',
                                   name = 'Letalité',
                                   tooltip = tooltips$cfr_4w,
                                   percent = TRUE,
                                   border_right = TRUE,
                                   color = colors$deaths))

  column_groups <- list(`Indicateurs MSF` = c('alert', 'neighbors', 'endemic', 'oc'),
                        `Cas Suspects` = c('cases_spk_long', 'cases_4w'),
                        `Décès` = c('deaths_4w', 'cfr_4w'))


  
  out <- tbl %>%
           select(zone, reg, oc, alert, neighbors, endemic, cases_spk_long, 
                  cases_4w, trend, deaths_4w, cfr_4w) %>%
           render_reactable(columns = columns,
                            column_groups = column_groups,
                            page_size = 20,
                            sort_by = 'cases_4w')

  return(out)
}


