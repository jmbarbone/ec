library(ec)
library(R6)

# there may be some easier way to copy/move around environments and stuff
# ... a 5x difference is kind of ehh, but it's still in microseconds

# ec  200
# R6   40
# RC 6000

bench::mark(
  ec = enclass("Queue", {
    .__new__. <- function(...) {
      for (item in list(...)) {
        self$add(item)
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
  }),
  R6 = R6Class(
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
  ),
  ReferenceClass = setRefClass(
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
        if (.length(.self$.queue) == 0) return(NULL)
        head <- .self$.queue[[1]]
        .self$.queue <- .self$.queue[-1]
        head
      },
      .length = function() {
        length(.self$.queue)
      }
    )
  ),
  check = FALSE
) |>
  print() |>
  ggplot2::autoplot()

profvis::profvis({
  enclass("Queue", {
    .__new__. <- function(...) {
      for (item in list(...)) {
        self$add(item)
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
})
