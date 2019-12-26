library(testthat)

setup_protocol <- function () {
  return(function(endpoint, data) {
    print(endpoint)
    cat(toJSON(data, indent=2))
  })
}

test_that("test single plot", {
  f <- create_frame(stack="plots/simple_plot",
                     token="token",
                     handler=ggplot_handler,
                     protocol=setup_protocol())
  myplot <- c(1,2)
  f <- commit(f, myplot)
  print("----------")
  f <- push(f)
  expect_equal(1, 1)
})
