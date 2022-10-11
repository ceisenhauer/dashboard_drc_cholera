render_reactable <- function(df, columns, column_groups = NULL, digits = 0,
                             element_id = 'my-select',
                             filterable = TRUE, searchable = TRUE, page_size = 25,
                             sort_by = NULL, sort_order = 'desc') {

  # header groups
  colGroups <- list()
  for (i in 1:length(column_groups)) {
    group_name <- names(column_groups)[[i]]
    colGroups[[i]] <- colGroup(name = group_name,
                               columns = column_groups[[group_name]])
  }

  # sorting
  sorting <- list()
  if (!is.null(sort_by)) {sorting[[sort_by]] = sort_order}

  out <- reactable(data = df,
                   style = list(fontFamily = 'Arial',
                                fontSize = '12px'),
                   resizable = TRUE,
                   highlight = TRUE,
									 #filterable = filterable,
                   searchable = searchable,
                   #fullWidth = FALSE,
                   defaultPageSize = page_size,
                   defaultSorted = sorting,
                   defaultColDef = colDef(format = colFormat(digits = digits,
                                                             separators = TRUE),
                                          sortNALast = TRUE,
                                          align = 'left'),
                   columns = columns,
                   columnGroups = colGroups,
                   elementId = element_id)
  
  return(out)
}

