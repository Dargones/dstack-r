#' Handle 'data.table' Objects
#'
#' @param col_names Save column names. \code{TRUE} by default.
#' @param row_names Save row names. \code{FALSE} by default.
datatable_handler <- function(col_names = TRUE, row_names = FALSE) {
  return(function (df, description, params) {
    df[, ] <- lapply(df[, ], as.character)
    filename <- tempfile()
    data.table::fwrite(df, filename, row.names = row_names, col.names = col_names, quote = TRUE, sep = ",")
    size <- file.size(filename)
    buf <- readBin(filename, raw(), n = size)
    return(list(data = base64enc::base64encode(buf), type = "text/csv", description = description, params = params))
  })
}