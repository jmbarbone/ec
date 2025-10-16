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
  clone <- container$.__clone__.()
  clone$self <- clone
  class(clone) <- "ec_capsule"

  for (i in clone[.__locked__.]) {
    lockBinding(i, clone)
  }

  clone[.__expr__.] <- substitute(expr)
  if (!missing(expr)) {
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
    expr <- substitute(expr)
    eval(expr, clone)
  }

  for (binding in c(".__init__.", ".__clone__.")) {
    # temporarily locked so that the user doesn't overwrite them directly
    (base::unlockBinding)(binding, clone)
    environment(clone[[binding]]) <- clone
    lockBinding(binding, clone)
  }

  user <- setdiff(ls(envir = clone, all.names = TRUE), clone[.__restricted__.])

  # temporarily locked so that the user doesn't overwrite them directly
  (base::unlockBinding)(".__methods__.", clone)
  (base::unlockBinding)(".__properties__.", clone)

  actives <- Filter(function(x) inherits(x, "active"), mget(user, clone))
  methods <- Filter(is.function, mget(setdiff(user, names(actives)), clone))

  clone[.__methods__.] <- methods
  clone[.__properties__.] <- mget(
    setdiff(user, c(names(methods), names(actives))),
    clone
  )
  clone[.__methods__.] <- list2env(clone[.__methods__.], parent = clone)
  clone[.__properties__.] <- list2env(clone[.__properties__.], parent = clone)

  for (name in names(actives)) {
    assign("..name..", name, environment(actives[[name]]))

    makeActiveBinding(
      name,
      actives[[name]],
      clone[[
        if (is.function(environment(actives[[name]])$..value..)) {
          ".__methods__."
        } else {
          ".__properties__."
        }
      ]]
    )
  }

  rm(list = user, envir = clone)
  lockEnvironment(clone[.__methods__.])
  lockEnvironment(clone[.__properties__.])

  lockBinding(".__methods__.", clone)
  lockBinding(".__properties__.", clone)

  properties <- names(clone[.__properties__.])
  public <- properties[!startsWith(properties, ".")]
  private <- properties[startsWith(properties, ".")]

  lockEnvironment(clone)
  class(clone) <- "ec_capsule"
  clone
})
