#' Handle Object Types Automatically
#'
#' @param use_plotly_instead_of_ggplot Use plot.ly to render ggplot2 charts. It is \code{TRUE} by default.
auto_handler <- function(use_plotly_instead_of_ggplot = TRUE) {
  gg_handler <- if (use_plotly_instead_of_ggplot) plotly_handler else ggplot_handler
  map <- list(
    list(name = "ggplot2", accept = is.ggplot2, handler = gg_handler()),
    list(name = "plotly", accept = is.plotly, handler = plotly_handler()),
    list(name = "data.table", accept = is.datatable, handler = datatable_handler()),
    list(name = "data.frame", accept = is.data.frame, handler = dataframe_handler())
  )
  return(function (obj, description, params) {
    for (h in map) {
      if (h$accept(obj)) return(h$handler(obj, description, params))
    }
    stop("Can't find suitable handler")
  })
}

#' Check That Specified Object is 'plot.ly' Chart
#'
#' @param x An object to check.
is.plotly <- function (x) {
  return(inherits(x, "plotly"))
}

#' Check That Specified Object is 'ggplot2' Chart
#'
#' @param x An object to check.
is.ggplot2 <- function(x) {
  return(inherits(x, "ggplot"))
}

#' Check That Specified Object is 'data.table' Object
#'
#' @param x An object to check.
is.datatable <- function(x) {
  return(inherits(x, "data.table"))
}
