library(testthat)
library(ggplot2)

.result <- NULL
.valid_token <- "my_token"

.SUCCESS <- list(status = 0, message = NULL)
.BAD_CREDENTIALS <- list(status = 4, message = "bad credentials")

.setup_protocol <- function () {
  return(function(endpoint, data) {
    .result <<- list(endpoint = endpoint, data = data)
    if (data$token != .valid_token) return(.BAD_CREDENTIALS)
    return(.SUCCESS)
  })
}

.setup_frame <- function(stack, protocol = .setup_protocol(), token = .valid_token) {
  return(create_frame(stack    = stack,
                      token    = token,
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
  push(f)
  expect_equal(.result$endpoint, "/stacks/push")
  expect_equal(.result$data$type, "image/png") # default media type
  expect_equal(.result$data$token, "my_token")
  expect_equal(length(.result$data$attachments), 1)
  expect_equal(is.null(.result$data$id), FALSE)
  expect_null(.result$data$attacments[[1]]$params)
  expect_null(.result$data$attacments[[1]]$description)
  expect_equal(is.null(.result$data$attachments[[1]]$data), FALSE)
})

test_that("test access denied", {
  tryCatch({
  f <- .setup_frame("myplots/test_plot", token = "invalid token")
  fail()
  },
  error = function (cond) {
    expect_equal(geterrmessage(), "bad credentials")
  })
})

# test_that("use real server", {
#   f <- create_frame(stack    = "cheptsov/test",
#                     token    = "f71e42ae-5209-4d21-933c-883d75722cf6",
#                     handler  = ggplot_handler(),
#                     protocol = json_protocol())
#
#   image <- qplot(clarity, data = diamonds, fill = cut, geom = "bar")
#   f <- commit(f, image, "My first plot")
#   push(f)
# })

