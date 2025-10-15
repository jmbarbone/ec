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
new_capsule <- function(expr) {
  # TODO include name
  self <- list2env(
    as.list(template, all.names = TRUE, sorted = TRUE),
    parent = parent.env(template)
  )

  self$self <- self

  for (i in self$.__locked__.) {
    lockBinding(i, self)
  }

  if (!missing(expr)) {
    self$.__expr__. <- substitute(expr)
    # TODO consider if the syntax should explicitly reflect:
    #
    # ```r
    # new_capsule({
    #   # explicitly require assigning to `self`, otherwise is not accessible
    #   self@public <- 1
    #   self@.private <- 2
    #
    #   self$instance_method <- function(self, x) self@a + x
    #   self$static_method <- function(x) x * 2 # accessible via capsule
    # })
    # ```
    #
    # The class for `self` may need to be temporarily changed to allow `@`, and
    # `$`to be used without signalling an error for undefined variables.
    #
    # I'm not so sure if we need to use `function(self)`, since we don't really
    # care about that.
    #
    # Although, we could differentiate:
    # Counter := enclass({...})
    # Counter$static_method()
    # try(Counter$instance_method()) # fails
    # Counter()$instance_method() # works
    eval(substitute(expr), self)
  }

  for (binding in c(".__init__.", ".__clone__.")) {
    # temporarily locked so that the user doesn't overwrite them directly
    (base::unlockBinding)(binding, self)
    environment(self[[binding]]) <- self
    lockBinding(binding, self)
  }

  accessibles <- setdiff(
    ls(envir = self, all.names = TRUE),
    self$.__restricted__.
  )

  # temporarily locked so that the user doesn't overwrite them directly
  (base::unlockBinding)(".__methods__.", self)
  (base::unlockBinding)(".__properties__.", self)

  actives <- Filter(function(x) inherits(x, "active"), mget(accessibles, self))
  nonactive <- setdiff(accessibles, names(actives))

  self$.__methods__. <- Filter(is.function, mget(nonactive, self))
  self$.__properties__. <- mget(
    setdiff(nonactive, names(self$.__methods__.)),
    self
  )
  self$.__methods__. <- list2env(self$.__methods__., parent = self)
  self$.__properties__. <- list2env(self$.__properties__., parent = self)

  for (name in names(actives)) {
    makeActiveBinding(name, actives[[name]], self$.__properties__.)
  }

  rm(list = accessibles, envir = self)
  lockEnvironment(self$.__methods__.)
  lockEnvironment(self$.__properties__.)

  lockBinding(".__methods__.", self)
  lockBinding(".__properties__.", self)

  properties <- names(self$.__properties__.)
  public <- properties[!startsWith(properties, ".")]
  private <- properties[startsWith(properties, ".")]

  lockEnvironment(self)
  class(self) <- "ec_capsule"
  self
}
