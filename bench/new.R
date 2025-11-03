library(ec)
library(R6)

# this is just as bad as RC...

# ec  130
# R6   25
# RC  115

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

# not much worse

# ec  30 (105)
# r6  29  (32)
# rc 145

bench::mark(
  ec_queue(),
  r6_queue$new(),
  rc_queue$new(),
  iterations = 99999,
  check = FALSE
) |>
  print() |>
  ggplot2::autoplot()


# ec 290
# r6 220
# rc 350

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
