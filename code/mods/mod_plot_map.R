mod_plot_map <- function(df, map, x, type = 'continuous', ...) {
  if (type == 'categorical') {
    e <- mod_plot_map_categorical(df, map, x, ...)
  } else {
    e <- mod_plot_map_continuous(df, map, x, ...)
  }

  return(e)
}

mod_plot_map_continuous <- function(df, map, x = 'cases',
                                    colors = c("#E4EDF3", "#2E4473", '#020A1B'), map_name = 'map',
                                    name_property = 'reg_display') {
  e <- df %>%
    e_charts_(name_property,
              renderer = 'svg') %>%
    e_map_register(map_name,
                   map) %>%
    e_map_(x,
           map = map_name,
           aspectScale = 1,
           nameProperty = name_property,
           roam = TRUE,
           emphasis = list(disabled = TRUE),
           scaleLimit = list(min = 0)) %>%
    echarts4r::e_tooltip() %>%
    echarts4r::e_toolbox() %>%
    echarts4r::e_toolbox_feature(feature = c('saveAsImage')) %>%
    e_visual_map(Cases,
                 itemHeight = 200,
                 inRange = list(color = colors),
                 right = 'center',
                 align = 'top',
                 orient = 'horizontal')

  return(e)
}

mod_plot_map_categorical <- function(df, map, x, categories, colors,
                                     map_name = 'map', name_property = 'reg_display') {
  e <- ec.init(df,
    preset = FALSE,
    renderer = 'svg',
    series = list(list(type = 'map',
                       map = map_name,
                       roam = TRUE,
                       emphasis = list(disabled = TRUE),
                       tooltip = list(formatter = '<small>{b}</small><br>Cas Suspects <b>{c}</b>'),
                       nameProperty = name_property,
                       aspectScale = 1)),
        tooltip = list(show = TRUE),
        toolbox = list(show = TRUE,
                       feature = list(saveAsImage = list(show = TRUE))),
        visualMap = list(type = 'piecewise',
                          bottom = 0,
                          left = 'center',
                          orient = 'horizontal',
                          dimension = x,
                          categories = categories,
                          inRange = list(color = colors)
         ))

  e$x$registerMap <- list(list(mapName = map_name,
                               geoJSON = map))

  return(e)
}

