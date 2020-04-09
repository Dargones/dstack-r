library(testthat)

test_that("test in-place config", {
  expect_length(.dstack_env$in_place_config_data, 0)
  configure("default", "test", "test-token", persist = "in_place")
  conf <- get_config()
  print(conf)
  expect_length(.dstack_env$in_place_config_data$profiles, 1)
  expect_equal(conf("default")$user, "test")
  expect_equal(conf("default")$token, "test-token")
  expect_equal(conf("default")$server, "https://api.dstack.ai")

  configure("test", "test1", "test-token1", persist = "in_place")
  expect_length(.dstack_env$in_place_config_data$profiles, 2)
})

test_that("test local config", {
  configure("default", "test", "test-token", persist = "local", dstack_dir = ".dstack_test")
  conf <- get_config()
  #print(conf("default"))
  expect_equal(conf("default")$user, "test")
})

