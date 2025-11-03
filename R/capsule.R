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
  # TODO include name

  # The .__clone__.() method may not be ideal, and could potentially be slowing
  # down some operations.  Instead, we could just create the container, assign
  # the appropriate fields, then lock it down.

  # FIXME okay, new idea:
  # @ accesses user slots
  # $ accesses restricted slots
  #   - uses get0(name, self, inherits = FALSE, ifnotfound = get(name, container, inherits = FALSE))
  #   - `$<-` defines in self
  #   - `$<-` doesn't allow setting those restricted in capsule
  #   - bindings are created (to restrict editing) then removed
  # [[ accesses anything

  capsule <- container$.__clone__.()
  class(capsule) <- "environment"

  assign("self", capsule, capsule)

  for (i in get(".__locked__.", envir = container)) {
    lock_binding(capsule, i)
  }

  if (!missing(expr)) {
    expr <- substitute(expr)
    assign(".__expr__.", expr, capsule)
    eval(expr, capsule)
  }

  class(capsule) <- "ec_capsule"
  nms <- setdiff_(names(capsule), capsule$.__restricted__.)
  vals <- mget(nms, capsule)

  for (i in seq_along(nms)) {
    if (inherits(vals[[i]], "active")) {
      rm(list = nms[i], envir = capsule)
      assign("..name..", nms[i], environment(vals[[i]]))
      assign("self", capsule, environment(vals[[i]]))
      makeActiveBinding(nms[i], vals[[i]], capsule)
    }
  }

  unlock_binding(capsule, ".__init__.")
  environment(capsule$.__init__.) <- capsule
  lock_binding(capsule, ".__init__.")
  lockEnvironment(capsule)
  capsule
})
