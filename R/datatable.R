datatable_handler <- function(col_names = TRUE, row_names = FALSE) {
  return(function (df, description, params) {
    df[, ] <- lapply(df[, ], as.character)
    content <- capture.output(fwrite(df, stdout(), row.names = row_names, col.names = col_names, quote = TRUE, sep = ","))
    buf <- charToRaw(paste(content, collapse="\n"))
    return(list(data = base64enc::base64encode(buf), type = "text/csv", description = description, params = params))
  })
}