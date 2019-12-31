library(testthat)
library(ggplot2)

.result <- NULL

.setup_protocol <- function () {
  return(function(endpoint, data) {
    .result <<- list(endpoint = endpoint, data = data)
    #cat(toJSON(data, indent=2))
  })
}

.setup_frame <- function(stack, protocol = .setup_protocol()) {
  return(create_frame(stack    = stack,
                      token    = "my_token",
                      handler  = ggplot_handler(),
                      protocol = .setup_protocol()))
}

test_that("create_frame() must call send_access()", {
  f <- .setup_frame("myplots/test_plot")
  expect_equal(.result$endpoint, "/stacks/access")
})

test_that("test commit and push", {
  f <- .setup_frame("myplots/test_plot")
  image <- qplot(clarity, data = diamonds, fill = cut, geom = "bar")
  f <- commit(f, image)
  f <- push(f)
  expect_equal(.result$endpoint, "/stacks/push")
  expect_equal(.result$data$type, "image/png") # default media type
  expect_equal(.result$data$token, "my_token")
  expect_equal(length(.result$data$attachments), 1)
  expect_equal(is.null(.result$data$id), FALSE)
  expect_null(.result$data$attacments[[1]]$params)
  expect_null(.result$data$attacments[[1]]$description)
  expect_equal(is.null(.result$data$attachments[[1]]$data), FALSE)
})

