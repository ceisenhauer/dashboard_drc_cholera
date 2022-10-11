library(htmltools)

data <- MASS::Cars93[, c("Manufacturer", "Model", "Type", "Price")]

reactable(
  data,
  filterable = TRUE,
  columns = list(
    Manufacturer = colDef(
      filterInput = function(values, name) {
        tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('cars-select', '%s', event.target.value || undefined)", name),
          # "All" has an empty value to clear the filter, and is the default option
          tags$option(value = "", "All"),
          lapply(unique(values), tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
    )
  ),
  defaultPageSize = 5,
  elementId = "cars-select"
)



mtcars2 <- mtcars |> 
  head() |> 
  tibble::rownames_to_column("model")

e1 <- mtcars2 |> 
  e_charts(model) |> 
  e_bar(
    carb, 
    universalTransition = TRUE,
    animationDurationUpdate = 1000L
  )

e2 <- mtcars2 |> 
  e_charts(model) |> 
  e_pie(
    carb, 
    universalTransition = TRUE,
    animationDurationUpdate = 1000L
  )

cb <- "() => {
  let x = 0;
  setInterval(() => {
    x++
    chart.setOption(opts[x % 2], true);
  }, 3000);
}"

e_morph(e1, e2, callback = cb)

library(echarts4r)
library(htmltools)

mtcars2 <- mtcars |> 
  head() |> 
  tibble::rownames_to_column("model")

e1 <- mtcars2 |> 
  e_charts(model) |> 
  e_bar(
    carb, 
    universalTransition = TRUE,
    animationDurationUpdate = 1000L
  )

e2 <- mtcars2 |> 
  e_charts(model) |> 
  e_pie(
    carb, 
    universalTransition = TRUE,
    animationDurationUpdate = 1000L
  )

cb <- "() => {
  let x = 0;
  let elements = document.getElementsByClassName('echarts-input');
  Array.from(elements).forEach(function(element) {
    element.addEventListener('change', (e) => {
      chart.setOption(opts[e.target.value -1 ], true);
    });
  }); 
}"

e_morph(e1, e2, callback = cb) %>% 
  htmlwidgets::prependContent(
    tags$div(
      class = "form-check",
      tags$input(
        class = "form-check-input echarts-input",
        type = "radio",
        name = "echarts",
        checked = NA,
        value = "1"
      ),
      tags$label(
        class = "form-check-label",
        "First chart"
      )
    ),
    tags$div(
      class = "form-check",
      tags$input(
        class = "form-check-input echarts-input",
        type = "radio",
        name = "echarts",
        value = "2"
      ),
      tags$label(
        class = "form-check-label",
        "Second chart"
      )
    )
  )


v <- LETTERS[1:10]
matrix <- data.frame(
  x = sample(v, 300, replace = TRUE),
  y = sample(v, 300, replace = TRUE),
  z = rnorm(300, 10, 1),
  color = rnorm(300, 10, 1),
  size = rnorm(300, 10, 1),
  stringsAsFactors = FALSE
) |>
  dplyr::group_by(x, y) |>
  dplyr::summarise(
    z = sum(z),
    color = sum(color),
    size = sum(size)
  ) |>
	mutate(y = factor(y)) %>%
  dplyr::ungroup()
#> `summarise()` has grouped output by 'x'. You can override using the `.groups`
#> argument.

matrix |>
  e_charts(x) |>
  e_scatter_3d(y, z, color, size) |>
	e_visual_map(
		y,
		type = 'piecewise',
		dimension = 1,
		categories = unique(matrix$y)
) #|>

  e_visual_map(
    z,
    # scale to z
    inRange = list(symbolSize = c(1, 30)),
    # scale size
    dimension = 3 # third dimension 0 = x, y = 1, z = 2, size = 3
  ) |>
  e_visual_map(
    #z,
		min = 0,
		max = 75,
    # scale to z
    inRange = list(color = c("#bf444c", "#d88273", "#f6efa6")),
    # scale colors
    dimension = 4,
    # third dimension 0 = x, y = 1, z = 2, size = 3, color = 4
    bottom = 300 # padding to avoid visual maps overlap
  )
