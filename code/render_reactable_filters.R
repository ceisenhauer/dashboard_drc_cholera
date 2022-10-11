render_reactable_dropdown <- function(values, name) {
  tags$select(
    # Set to undefined to clear the filter
    onchange = sprintf("Reactable.setFilter(tbl-select, '%s', event.target.value || undefined)", 
                       name),
    # "All" has an empty value to clear the filter, and is the default option
    tags$option(value = "", "All"),
    lapply(unique(values), tags$option),
    "aria-label" = sprintf("Filter %s", name),
    #multiple = TRUE,
    style = "width: 100%; height: 28px;"
  )
}
