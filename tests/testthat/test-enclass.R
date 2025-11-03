test_that("enclass() works", {
  Foo := enclass({
    current <- 0L
    do <- function(x = 1L) {
      x <- as.integer(x)
      self@current <- self@current * x
      self@.record <- c(
        self@.record,
        list(list(time = Sys.time(), value = self@current))
      )
      invisible(self)
    }

    .record <- list()

    .__new__. <- function(start = 0L) {
      self@do(start)
    }
  })

  foo <- Foo()
  expect_identical(foo@current, 0L)
  foo@do(5L)
  expect_identical(foo@current, 0L)
})

test_that("enclass() errors", {
  Foo := enclass({
    a <- 1
    b <- function() 2
  })

  expect_error(Foo@aa)
  expect_error(Foo@bb())
  expect_error(Foo@a())
  expect_type(Foo@b, "closure")

  foo <- Foo()
  expect_error(foo@aa)
  expect_error(Foo@bb())
  expect_error(Foo@a())
  expect_type(foo@b, "closure")
})
