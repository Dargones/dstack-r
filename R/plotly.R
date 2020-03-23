library(base64enc)

plotly_handler <- function() {
  to_frame_data <- function (plot, description, params) {
    buf <- charToRaw(plotly::plotly_json(plot, FALSE))
    return(list(data = base64encode(buf), description = description, params = params))
  }

  return(list(to_frame_data = to_frame_data, type = "plotly"))
}
