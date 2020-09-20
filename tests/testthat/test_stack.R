library(testthat)
library(ggplot2)

.setup_test_protocol <- function() {
  configure("default", "user", "my_token", persist = "in_place")
  use_config(in_place_config)
  test_protocol <- TestProtocol$new()
  .dstack_env$protocol_factory <- TestProtocolFactory$new(test_protocol)
  return(test_protocol)
}

test_that("single plot", {
  test_protocol <- .setup_test_protocol()

  frame <- create_frame(stack = "plots/my_plot")

  fig <- ggplot(mpg, aes(cty)) +
    geom_density(aes(fill = factor(cyl)), alpha = 0.8) +
    labs(title = "Density plot",
         subtitle = "City Mileage Grouped by Number of cylinders",
         caption = "Source: mpg",
         x = "City Mileage",
         fill = "# Cylinders")

  my_desc <- "my desc"
  frame <- commit(frame, fig, my_desc)
  push(frame)

  expect_equal(test_protocol$data$stack, "user/plots/my_plot")
  data <- test_protocol$get_data("plots/my_plot")
  expect_false(is.null(data))
  attachments <- data$attachments
  expect_false(is.null(data$id))
  expect_equal(test_protocol$token, "my_token")
  expect_equal(length(attachments), 1)
  #expect_equal(attachments[1]$content_type, "image/svg+xml")

  expect_null(attachments[[1]]$params)
  expect_equal(attachments[[1]]$description, my_desc)
  #self.assertEqual("image/svg+xml", attachments[0]["content_type"])
  #self.assertEqual("matplotlib", attachments[0]["application"])
  #self.assertEqual(my_desc, attachments[0]["description"])
})

test_that("multiple plots", {
  test_protocol <- .setup_test_protocol()

  line_plot <- function(a) {
    x <- c(0:20)
    y <- sapply(x, function(x) { return(a * x) })
    df <- data.frame(x = x, y = y)
    plot <- ggplot(data = df, aes(x = x, y = y)) +
      geom_line() +
      xlim(0, 20) +
      ylim(0, 20)
    return(plot)
  }

  coeff <- c(0.5, 1.0, 1.5, 2.0)
  frame <- create_frame(stack = "line_plot")

  for (c in coeff) {
    frame <- commit(frame, line_plot(c),
                    paste0("Line plot with the coefficient of ", c),
                    Coefficient = c)
  }

  push(frame)

  expect_equal(test_protocol$data$stack, "user/line_plot")
  data <- test_protocol$get_data("line_plot")
  attachments <- data$attachments
  expect_equal(length(attachments), 4)

  for (idx in seq_along(coeff)) {
    att <- attachments[[idx]]
    c <- coeff[[idx]]
    expect_equal(att$description, paste0("Line plot with the coefficient of ", c))
    expect_equal(length(att$params), 1)
    expect_equal(att$params$Coefficient, c)
  }
})


