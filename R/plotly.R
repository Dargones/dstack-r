library(base64enc)

#' Handle 'plot.ly' Charts
plotly_handler <- function() {
  return(function (plot, description, params) {
    buf <- charToRaw(plotly::plotly_json(plot, FALSE))
    return(list(data = base64encode(buf), type = "plotly", description = description, params = params))
  })
}
