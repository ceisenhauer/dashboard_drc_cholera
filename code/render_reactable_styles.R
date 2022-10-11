render_reactable_bar <- function(width, fill = "#e6e6e6", color = '#000000', font_weight = 'bold',
                                 border_right = NULL) {
  width <- paste0(width * 100, "%")
  bar <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, width)

  out <- list(backgroundImage = bar,
              backgroundSize = paste('100%', '75%'),
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center',
              color = color,
              fontWeight = font_weight,
              borderRight = render_reactable_border(border_right))

  return(out)
}

render_reactable_border <- function(border = NULL) {
  out <- if (is.null(border)) {
           return(NULL)
         } else if (border) {
           return("1px solid rgba(0, 0, 0, 0.1)")
         } else if (is(border, 'character')) {
           return(border)
         } else {
           warning('border must be TRUE or a valid style string')
           return(NULL)
         }
}

render_reactable_header <- function(name, tooltip = NULL) {
  if (is.null(tooltip)) {
    out <- name
  } else {
    out <- htmltools::div(style = paste('text-decoration: underline;',
                                        'text-decoration-style: dotted;',
                                        'cursor: help'),
                          tippy::tippy(name,
                                       paste0('<span style="font-size:16px;">',
                                              tooltip,
                                              '</span>'),
                                       allowHTML = TRUE))
  }
  
  return(out)
}
