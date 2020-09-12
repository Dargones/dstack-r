library(testthat)
library(rlang)

MyAbstractClass <-
  AbstractClass("MyAbstractClass",
                abstract = list(
                  foo = function(x, y) { },
                  bar = function(x) { }
                )
  )

MyClass <-
  Class("MyClass",
        inherit = MyAbstractClass,
        public = list(
          foo = function(x, y) { return(x + y) },
          bar = function(x) { return(x) }
        )
  )

test_that("test .are_signatures_compatible", {
  foo <- function(x, y) { x + y }
  bar <- function(x, y) { x * y }
  baz <- function(x, z) { x - z }
  expect_true(.are_signatures_compatible(list(foo, bar)))
  expect_false(.are_signatures_compatible(list(foo, bar, baz)))
  expect_true(.are_signatures_compatible(list(foo)))
})

test_that("test .merge_abstract_methods", {
  foo <- function(x, y) { x + y }
  bar <- function(x, y) { x * y }
  baz <- function(x, z) { x - z }

  f <- function() {
    .merge_abstract_methods(list(foo = foo), list(foo = baz))
  }

  expect_error(f())

  l <- .merge_abstract_methods(list(foo = foo), list(foo = bar, baz = baz))
  expect_equal(length(l), 2)
})

test_that("abstract class can't be initialized", {
  f <- function() MyAbstractClass$new()
  expect_error(f())
  obj <- MyClass$new()
})

test_that("is_abstract returns TRUE only for abstract class", {
  expect_true(MyAbstractClass$is_abstract)
  expect_false(MyClass$is_abstract)
})

test_that("methods in interface are all abstract", {
  MyInterface <-
    Interface("MyInterface",
              abstract = list(
                foo = function(x, y) { },
                bar = function(x) { }
              ))

  expect_true(MyInterface$is_abstract)
  expect_true(MyInterface$is_interface)

  f <- function() {
    MyAbstractClass1 <- Class("MyAbstractClass1",
                              implement = MyInterface,
                              public = list(
                                bar = function(x) { return(x) }
                              )
    )

    obj <- MyAbstractClass1$new()
  }

  expect_error(f())

})

test_that("interface methods override", {
  MyInterface <-
    Interface("MyInterface",
              abstract = list(
                foo = function(x, y) { },
                bar = function(x) { }
              ))

  MyImpl <-
    Class("MyImpl",
          implement = MyInterface,
          public = list(
            bar = function(x) { return(x) },
            foo = function(x, y) { return(x + y) }
          )
    )

  expect_false(MyImpl$is_abstract)
  obj <- MyImpl$new()
  expect_equal(obj$bar(10), 10)
  expect_equal(obj$foo(10, 20), 30)
})

test_that("test .check_implemented_methods", {
  foo <- function(x, y) { }
  bar <- function(x, z) { }
  baz <- function() { }

  res <- .check_implemented_methods(list(foo = foo, bar = bar, baz = baz), list(
    foo = function(x, y) { x + y },
    baz = function() { }
  ))

  expect_equal(length(res), 1)
  expect_equal(names(res), "bar")

  f <- function() {
    .check_implemented_methods(list(foo = foo, bar = bar, baz = baz), list(
      foo = function(x, z) { x + z }, # incompatible signature
      baz = function() { }
    ))
  }

  expect_error(f())

})

test_that("abstract class implements something", {
  MyInterface <-
    Interface("MyInterface",
              abstract = list(
                foo = function(x, y) { },
                bar = function(x) { }
              ))

  expect_equal(length(MyInterface$abstract), 2)

  MyAbstractClass <-
    AbstractClass("MyImpl",
                  implement = MyInterface,
                  public = list(
                    foo = function(x, y) { return(x + y) }
                  )
    )

  expect_equal(names(MyAbstractClass$abstract), "bar")
})

test_that("interface multiple interitance", {
  MyInterface <-
    Interface("MyInterface",
              abstract = list(
                foo = function(x, y) { },
                bar = function(x) { }
              ))


  MyBazInterface <-
    Interface("MyBazInterface",
              abstract = list(
                baz = function() { }
              ))

  MyCompInterface <-
    Interface("MyCompInterface",
              inherit = list(MyInterface, MyBazInterface),
              abstract = list(
                comp = function(x, y, z) { }
              ))


  expect_equal(length(MyCompInterface$abstract), 4)
  expect_true(is.element("foo", names(MyCompInterface$abstract)))
  expect_true(is.element("bar", names(MyCompInterface$abstract)))
  expect_true(is.element("baz", names(MyCompInterface$abstract)))
  expect_true(is.element("comp", names(MyCompInterface$abstract)))
})



