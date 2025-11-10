r"(
ec: enclass()()
R6: R6Class()$new()
RC: setRefClass()$new()

ec is about 5-10 microseconds faster than R6; both of which about 6 times faster
than RC.

processing, ec is about 50 us slower than R6; about 25% slower; both better than
RC (300-350)
)"

library(ec)
library(R6)

ec_queue <- enclass("Queue", {
  .__new__. <- function(...) {
    for (item in list(...)) {
      self@add(item)
    }
  }

  add <- function(x) {
    self@.queue <- c(self@.queue, list(x))
    invisible(self)
  }

  remove <- function() {
    if (length(self@.queue) == 0) return(NULL)
    head <- self@.queue[[1]]
    self@.queue <- self@.queue[-1]
    head
  }

  .queue <- list()
  .length <- function() {
    length(self@.queue)
  }
})

r6_queue <- R6Class(
  "Queue",
  public = list(
    initialize = function(...) {
      for (item in list(...)) {
        self$add(item)
      }
    },
    add = function(x) {
      private$queue <- c(private$queue, list(x))
      invisible(self)
    },
    remove = function() {
      if (private$length() == 0) return(NULL)
      # Can use private$queue for explicit access
      head <- private$queue[[1]]
      private$queue <- private$queue[-1]
      head
    }
  ),
  private = list(
    queue = list(),
    length = function() base::length(private$queue)
  )
)

rc_queue <- setRefClass(
  "Queue",
  fields = list(
    .queue = "list"
  ),
  methods = list(
    initialize = function(...) {
      .self$.queue <- list()
      for (item in list(...)) {
        .self$add(item)
      }
    },
    add = function(x) {
      .self$.queue <- c(.self$.queue, list(x))
      invisible(.self)
    },
    remove = function() {
      if (.length() == 0) return(NULL)
      head <- .self$.queue[[1]]
      .self$.queue <- .self$.queue[-1]
      head
    },
    .length = function() {
      length(.self$.queue)
    }
  )
)

# actually, this looks better

# ec  16 (105)
# r6  26  (32)
# rc 135 (145)

bench::mark(
  ec_queue(),
  r6_queue$new(),
  rc_queue$new(),
  iterations = 9999,
  check = FALSE
) |>
  print() |>
  ggplot2::autoplot()


# ec 245 (290)
# r6 211 (220)
# rc 350

# may the use of @ incurs a little overhead?

local({
  ec <- ec_queue()
  r6 <- r6_queue$new()
  rc <- rc_queue$new()

  bench::mark(
    ec = {
      ec@add(1)
      ec@add(2)
      ec@add(3)
      ec@remove()
    },
    r6 = {
      r6$add(1)
      r6$add(2)
      r6$add(3)
      r6$remove()
    },
    rc = {
      rc$add(1)
      rc$add(2)
      rc$add(3)
      rc$remove()
    },
    iterations = 9999,
    check = FALSE
  ) |>
    print() |>
    ggplot2::autoplot()
})
