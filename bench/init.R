library(ec)
library(R6)

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
  check = FALSE,
  iterations = 50000
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
