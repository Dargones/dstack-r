library(base64enc)

ggplot_handler <- function(dpi = 300, width = NA, height = NA, type = "png") {
  as_frame <- function (plot, description, params) {
    filename <- paste0(tempdir(), "/test.", type)
    ggsave(file = filename, plot = plot, width = width, height = height, dpi = dpi)
    f <- file(filename, "rb")
    size <- file.size(filename)
    buf <- readBin(f, raw(), n = size)
    close(f)
    file.remove(filename)
    return(list(data = base64encode(buf), description = description, params = params))
  }

  return(list(as_frame = as_frame, type = paste0("image/", type)))
}
