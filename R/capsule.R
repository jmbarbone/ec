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
  capsule <- container$.__clone__.()
  capsule$self <- capsule
  class(capsule) <- "ec_capsule"

  for (i in capsule[.__locked__.]) {
    lock_binding(capsule, i)
  }

  capsule[.__expr__.] <- substitute(expr)
  if (!missing(expr)) {
    # TODO consider if the syntax should explicitly reflect:
    #
    # ```r
    # new_capsule({
    #   # explicitly require assigning to `self`, otherwise is not accessible
    #   self@public_a <- 1
    #   self@.private_b <- 2
    #
    #   # this wouldn't be _too_ much effort in ec
    #   self$instance_method <- function(self, x) self@a + x
    #   self$static_method <- function(x) x * 2 # accessible via capsule
    #
    #   # this would be a bit easier
    #   self@active_property <- active()
    #   self$method <- function(self) self@public_a
    # })
    # ```
    #
    # The class for `self` may need to be temporarily changed to allow `@`, and
    # `$`to be used without signalling an error for undefined variables.
    #
    # This may also be faster as we can have `@` and `$` directly modify
    # our properties and methods environments; saving the time of moving these
    # around and recreating environments.
    #
    # I'm not so sure if we need to use `function(self)`, since we don't really
    # care about that.
    #
    # Although, we could differentiate:
    # Counter := enclass({...})
    # Counter$static_method()
    # try(Counter$instance_method()) # fails
    # Counter()$instance_method() # works
    #
    expr <- substitute(expr)
    eval(expr, capsule)
  }

  # temporarily locked so that the user doesn't overwrite them directly
  unlock_binding(capsule, ".__methods__.")
  unlock_binding(capsule, ".__properties__.")
  user <- setdiff_(names(capsule), capsule[.__restricted__.])
  new <- mget(user, capsule)

  is_active <- vapply(new, inherits, NA, "active")
  is_locked_property <- vapply(new, inherits, NA, "locked_property")
  is_locked_method <- vapply(new, inherits, NA, "locked_method")
  is_method <- vapply(new, is.function, NA)

  # assign non-active properties
  capsule[.__methods__.] <- list_as_env(
    new[is_method & !is_locked_method],
    capsule
  )
  capsule[.__properties__.] <- list_as_env(
    new[!(is_method | is_active | is_locked_property)],
    capsule
  )

  # actives have to be reassigned
  for (name in user[is_active | is_locked_property]) {
    assign("..name..", name, environment(new[[name]]))
    makeActiveBinding(
      name,
      new[[name]],
      capsule[.__properties__.]
    )
  }

  for (name in user[is_locked_method]) {
    assign("..name..", name, environment(new[[name]]))
    makeActiveBinding(
      name,
      new[[name]],
      capsule[.__methods__.]
    )
  }

  rm(list = user, envir = capsule)
  class(capsule[.__methods__.]) <- "ec_methods"
  class(capsule[.__properties__.]) <- "ec_properties"
  lockEnvironment(capsule[.__methods__.])
  lockEnvironment(capsule[.__properties__.])
  lock_binding(capsule, ".__methods__.")
  lock_binding(capsule, ".__properties__.")
  unlock_binding(capsule, ".__init__.")
  environment(capsule[.__init__.]) <- capsule
  lock_binding(capsule, ".__init__.")
  lockEnvironment(capsule)
  capsule
})
