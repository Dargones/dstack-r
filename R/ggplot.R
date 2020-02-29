library(base64enc)

#' Handles ggplot2 objects.
#'
#' @param dpi DPI, default is 300.
#' @param width Image width.
#' @param height Image height.
#' @param format Image format to use, can be "png" or "svg", by default PNG will be used.
ggplot_handler <- function(dpi = 300, width = NA, height = NA, format = "png") {
  as_frame <- function (plot, description, params) {
    filename <- paste0(tempdir(), "/test.", format)
    ggplot2::ggsave(file = filename, plot = plot, width = width, height = height, dpi = dpi)
    f <- file(filename, "rb")
    size <- file.size(filename)
    buf <- readBin(f, raw(), n = size)
    close(f)
    file.remove(filename)
    return(list(data = base64encode(buf), description = description, params = params))
  }

  return(list(as_frame = as_frame, type = paste0("image/", format)))
}
