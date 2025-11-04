#' Create a new class capsule
#'
#' @param expr An expression for assigning variables in new capsule.
#' @export
#' @returns An environment with class `ec_capsule`
#' @examples
#' cap <- new_capsule({
#'   x <- 1
#'   y <- active(
#'     default = 2L,
#'     set = function(value) {
#'       self@x <- value
#'     },
#'     get = function() {
#'       ..value.. <<- ..value.. + 1L
#'       ..value..
#'     }
#'   )
#'
#'   reset <- function() {
#'     self@y <- 0L
#'     invisible(self)
#'   }
#' })
#' cap@y
#' cap@y
new_capsule <- contain(function(expr) {
  # browser()
  capsule <- container$.__create__.()

  if (!missing(expr)) {
    expr <- substitute(expr)
    assign(".__expr__.", expr, capsule)
    eval(expr, capsule$self)
  }

  class(capsule) <- "ec_capsule"
  nms <- names(capsule$self)
  vals <- mget(nms, capsule$self)

  for (i in seq_along(nms)) {
    if (inherits(vals[[i]], "active")) {
      rm(list = nms[i], envir = capsule$self)
      assign("..name..", nms[i], environment(vals[[i]]))
      assign("self", capsule$self, environment(vals[[i]]))
      makeActiveBinding(nms[i], vals[[i]], capsule$self)
    }
  }

  assign(".__init__.", container$.__init__., capsule)
  environment(capsule$.__init__.) <- capsule

  for (restricted in container$.__restricted__.) {
    if (restricted %in% nms) {
      assign(
        restricted,
        get(restricted, capsule$self, inherits = FALSE),
        capsule
      )
      rm(list = restricted, envir = capsule$self)
    }
  }

  lockEnvironment(capsule, bindings = TRUE)
  capsule
})
