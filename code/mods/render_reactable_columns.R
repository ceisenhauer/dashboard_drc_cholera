col_bar <- function(df, col, name = col, tooltip = NULL, color = '#c5c5c5',
                    digits = 0, percent = FALSE, ...,
                    sticky = NULL, min_width = 75, border_right = NULL) {
  out <- reactable::colDef(
	  header = render_reactable_header(name, tooltip),
    sticky = sticky,
    format = reactable::colFormat(digits = digits,
                                  percent = percent,
                                  separators = TRUE),
    style = function(value) {
      if (is.na(value)) {
      render_reactable_bar(width = 0 / max(df[[col]],
                                     na.rm = TRUE),
                           fill = color,
                           border_right = border_right)
      } else {
      render_reactable_bar(width = value / max(df[[col]],
                                     na.rm = TRUE),
                           fill = color,
                           border_right = border_right)
      }
    },
    minWidth = min_width,
    filterable = TRUE,
    filterMethod = htmlwidgets::JS('filterMinValue'),
    filterInput = htmlwidgets::JS('rangeFilter'),
    ...)

  return(out)
}


col_base <- function(name, tooltip = NULL, color = '#000000', digits = 0, percent = FALSE, ...,
                     min_width = 75, sticky = NULL, border_right = NULL) {

  out <- reactable::colDef(header = render_reactable_header(name, tooltip),
                           sticky = sticky,
                           format = reactable::colFormat(digits = digits,
                                                         percent = percent,
                                                         separators = TRUE),
                           minWidth = min_width,
                           style = list(color = color,
                                        borderRight = render_reactable_border(border_right)),
													 filterable = TRUE,
                           ...)

  return(out)
}

# TODO: check if inline css has meaningful spead tradeoff if so, make two versions of this function,
# one self contained and one pointing to external css
col_badge <- function(name, tooltip = NULL, colors, digits = 0, ..., 
                      min_width = 75, border_right = NULL, sticky = NULL) {
  out <- reactable::colDef(header = render_reactable_header(name, tooltip),
                           sticky = sticky,
                           align = 'center',
                           style = list(color = 'black',
                                        borderRight = render_reactable_border(border_right),
                                        fontWeight = 'bold'),
                           cell = function(value) {
                             if (is.na(value)) {
                               color <- 'rgba(0, 0, 0, 0)'
                             } else {
                               color <- colors[[value]]
                             }

                             css <- paste('display: inline-block;',
                                          'padding: 0.3rem 0.75rem;',
                                          'border-radius: 15px;',
                                          'font-weight: 600;',
                                          'font-size: 0.99rem;',
                                          'background:', color, ';')

                             htmltools::div(style = css, value)
                           },
													 filterable = TRUE,
                           ...)

  return(out)
}

  
col_sparkline <- function(data, name = 'Spark', tooltip = NULL, color = '#c5c5c5', ...,
                          border_right = NULL, maxWidth = 60, sticky = NULL) {
  colDef(header = render_reactable_header(name, tooltip),
         filterable = FALSE,
         sticky = sticky,
         align = 'center',
         style = list(borderRight = render_reactable_border(border_right)),
         maxWidth = maxWidth,
         cell = function(value, index) {sparkline::sparkline(data[[index]],
                                 type = 'bar',
                                 barColor = color)})
}
