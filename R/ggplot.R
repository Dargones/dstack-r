library(base64enc)

#' Handles 'ggplot2' Objects
#'
#' Returns a function that converts 'ggplot2' plots to appropriate format and format itself. PNG and SVG are supported.
#' @param dpi DPI, default is 300.
#' @param width Image width.
#' @param height Image height.
#' @param format Image format to use, can be "png" or "svg", by default PNG will be used.
#' @return A list which contains conversion function and format inself.
ggplot_handler <- function(dpi = 300, width = NA, height = NA, format = "png") {
  return(function (plot, description, params) {
    filename <- paste0(tempdir(), "/test.", format)
    ggplot2::ggsave(file = filename, plot = plot, width = width, height = height, dpi = dpi)
    f <- file(filename, "rb")
    size <- file.size(filename)
    buf <- readBin(f, raw(), n = size)
    close(f)
    file.remove(filename)
    return(list(data = base64encode(buf), type = paste0("image/", format), description = description, params = params))
  })
}
