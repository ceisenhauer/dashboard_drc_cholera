infocon <- function(value, title, subtitle, img_path, img_width = '25%', tooltip = NULL,
									  tooltip_id = NULL) {
  value <- si_format(value)

  title <- shiny::h3(shiny::tags$b(paste(value, title)))

  shiny::div(class = 'info-box-card',
						 title = tooltip,
             shiny::div(class = 'info-box',
                        shiny::div(class = 'icon-lrg',
                                   style = 'width: 30%',
                                   shiny::img(src = img_path)),
                        shiny::div(class = 'inner',
                                   align = 'left',
                                   style = 'width: 70%',
                                   title,
                                   shiny::h6(subtitle))
             )
  )
}
