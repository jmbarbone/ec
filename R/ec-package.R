#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' An example class
#' @export
#' @param x A non-negative integer to initialize the counter
#'
#' @section Properties:
#' - `x`: An active binding representing the current value of the counter.
#'  It must be a non-negative integer.
#'
#'  @section Methods:
#'  - `increment(n = 1L)`: Increment the counter by a non-negative integer `n`.
#'  - `reset()`: Reset the counter to zero.
Example <- NULL

delayedAssign(
  "Example",
  enclass("Example", {
    .__new__. <- function(x = 0L) {
      stopifnot(x >= 0)
      self@x <- x
    }

    x <- active(
      default = 0L,
      set = function(value) {
        value <- as.integer(value)
        stopifnot(value >= 0L)
        value
      }
    )

    increment <- function(n = 1L) {
      n <- as.integer(n)
      stopifnot(n >= 0L)
      self@x <- self@x + n
      self$.record()
      invisible(self)
    }

    reset <- function() {
      self@x <- 0L
      self$.record()
      invisible(self)
    }

    .log <- NULL

    .record <- function() {
      self@.log <- c(
        self@.log,
        list(list(
          call = sys.call(-1),
          time = Sys.time(),
          value = self@x
        ))
      )
    }
  })
)
