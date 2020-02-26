library(testthat)
library(ggplot2)

.result <- NULL
.valid_token <- "my_token"

.test_config <- function(token) {
  return(function (profile) {
    p <- list(token=token, user="test_user", server="https://api.dstack.ai")
    return(p)
  })
}

.setup_protocol <- function () {
  return(function(endpoint, data) {
    .result <<- list(endpoint = endpoint, data = data)
    if (data$token != .valid_token) stop("Forbidden")
  })
}

.setup_frame <- function(stack, protocol = .setup_protocol(), token = .valid_token) {
  return(create_frame(stack    = stack,
                      config   = .test_config(token),
                      handler  = ggplot_handler(),
                      protocol = .setup_protocol()))
}

test_that("create_frame() must call send_access() by default", {
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
    expect_equal(geterrmessage(), "Forbidden")
  })
})

test_that("test relative stack path", {
  f <- .setup_frame("myplots/test_plot")
  image <- qplot(clarity, data = diamonds, fill = cut, geom = "bar")
  f <- commit(f, image)
  push(f)
  expect_equal(.result$data$stack, "test_user/myplots/test_plot")
})

test_that("test absolute stack path", {
  f <- .setup_frame("/user/myplots/test_plot")
  image <- qplot(clarity, data = diamonds, fill = cut, geom = "bar")
  f <- commit(f, image)
  push(f)
    expect_equal(.result$data$stack, "user/myplots/test_plot")
})

