auto_handler <- function(use_plotly_instead_of_ggplot = TRUE) {
  gg_handler <- if (use_plotly_instead_of_ggplot) plotly_handler else ggplot_handler
  map <- list(
    list(name = "ggplot2", accept = is.ggplot2, handler = gg_handler()),
    list(name = "plotly", accept = is.plotly, handler = plotly_handler()),
    list(name = "data.frame", accept = is.data.frame, handler = dataframe_handler()),
    list(name = "data.table", accept = is.datatable, handler = datatable_handler())
  )
  return(function (obj, description, params) {
    for (h in map) {
      if (h$accept(obj)) return(h$handler(obj, description, params))
    }
    stop("Can't find suitable handler")
  })
}

is.plotly <- function (x) {
  return(inherits(x, "plotly"))
}

is.ggplot2 <- function(x) {
  return(inherits(x, "ggplot"))
}

is.datatable <- function(x) {
  inherits(x, "data.table")
}
