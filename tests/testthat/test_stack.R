library(testthat)
library(ggplot2)

setup_protocol <- function () {
  return(function(endpoint, data) {
    print(endpoint)
    cat(toJSON(data, indent=2))
  })
}

test_that("test single plot", {
  f <- create_frame(stack="plots/simple_plot",
                     token="token",
                     handler=ggplot_handler(),
                     protocol=setup_protocol())
  image <- qplot(clarity, data=diamonds, fill=cut, geom="bar")
  f <- commit(f, image)
  f <- push(f)
  expect_equal(1, 1)
})


