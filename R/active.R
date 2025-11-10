..value.. <- structure(list(), class = "pseudo")
..setter.. <- structure(list(), class = "pseudo")
..getter.. <- structure(list(), class = "pseudo")
..name.. <- structure(list(), class = "pseudo")

#' @export
print.pseudo <- function(x, ...) {
  cat("<pseudo>")
  invisible(x)
}

#' active
#'
#' define a new active binding property
#'
#' @details For both `get` and `set` functions, the value returned by the
#'   function will be returned when the symbol is accessed or reassigned,
#'   respectively.  You can use `self` to refer to the object within these
#'   functions.  A special value of `..value..` will be accessible within these
#'   functions; defaulting to the value in `default`.  The default value of
#'   `get` uses this symbol.
#'
#'
#' @param default The default value of the object
#' @param get A function to get the value. It must take no arguments.  The value
#'   returned by this function will be returned when the active binding is
#'   called.
#' @param set A function to set the value. It must take one argument, the new
#'   value.  The value returned will be assigned to the active binding.
#' @return An object with class `active`
#' @export
#' @examples
#' enclass("Example", {
#'   x <- active(
#'     default = 0L,
#'     get = function() {
#'       ..value.. <<- ..value.. + 1L # increment value each time it's accessed
#'     },
#'     set = function(value) { # 'value' must be the only argument
#'       self@y <- value + 1L  # set another property on the object
#'       value                 # return object is assigned to the property
#'     }
#'   )
#'
#'   y <- 0L
#' })
#'
#'
#' capsule
active <- contain(function(
  default = NULL,
  get = function() ..value..,
  set = function(value) value
) {
  stopifnot(
    is.null(formals(get)),
    identical(formals(set), as.pairlist(alist(value = )))
  )

  parent <- parent.frame()
  env <- new.env(parent = parent, hash = TRUE)
  env$parent <- parent
  environment(get) <- env
  environment(set) <- env
  env$..value.. <- default
  env$..getter.. <- get
  env$..setter.. <- set
  env$..name.. <- NULL
  env$self <- NULL

  # TODO consider if all bindings should be locked, then only unlocked through
  # internal functions
  lockBinding("..getter..", env)
  lockBinding("..setter..", env)
  lockEnvironment(env)

  fun <- local(
    envir = env,
    function(value) {
      if (missing(value)) {
        get("..getter..")()
      } else {
        ..value.. <<- get("..setter..")(value)
      }
    }
  )

  structure(fun, class = "active")
})

lock <- function(value) {
  active(
    default = value,
    set = function(value) {
      stop(sprintf("'%s' is read-only", ..name..), call. = FALSE)
    }
  )
}

is_active <- function(x) {
  inherits(x, "active")
}

#' @export
print.active <- function(x, ...) {
  print(unclass(x))
  invisible(x)
}
