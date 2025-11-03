#' @export
`@.ec_capsule` <- function(object, name) {
  # ec_at(object, name)
  name <- substitute(name)
  name <- as.character(name)
  ec_at(object, name)
}

#' @export
`@.ec_generator` <- function(object, name) {
  # ec_at(environment(object), name)
  name <- substitute(name)
  name <- as.character(name)
  ec_at(environment(object), name)
}

ec_at <- function(object, name) {
  if (name %in% get(".__restricted__.", object)) {
    stop(sprintf("Cannot access '%s'", name))
  }
  get(name, envir = object, inherits = FALSE)
}

#' @export
`@<-.ec_capsule` <- function(object, name, value) {
  name <- substitute(name)
  name <- as.character(name)

  if (name %in% object$.__restricted__.) {
    stop(sprintf("Cannot reassign '%s'", name))
  }

  # if (!is_field(object[.__properties__.], name)) {
  #   if (is_field(object[.__methods__.], name)) {
  #     stop(sprintf(
  #       "'%1$s' is a method, not a property, use '$%1$s <- value' instead",
  #       name
  #     ))
  #   }
  #   stop(sprintf("No property named '%s'", name))
  # }
  #
  # assign(name, value, object[.__properties__.])
  assign(name, value, object)
  object
}

#' @export
`$.ec_capsule` <- function(x, name) {
  name <- substitute(name)
  name <- as.character(name)
  # if (!is_field(x[.__methods__.], name)) {
  #   if (is_field(x[.__properties__.], name)) {
  #     stop(sprintf(
  #       "'%1$s' is a property, not a method, use '@%1$s' instead",
  #       name
  #     ))
  #   }
  #   stop(sprintf("No method named '%s'", name))
  # }
  # x[.__methods__.][[name]]
  ec_dollar(x, name)
}

#' @export
`$.ec_generator` <- function(x, name) {
  name <- substitute(name)
  name <- as.character(name)
  ec_dollar(environment(x), name)
}

#' @export
`$<-.ec_capsule` <- function(x, name, value) {
  name <- substitute(name)
  name <- as.character(name)
  # value <- match.fun(value)

  # if (!is_field(x[.__methods__.], name)) {
  #   if (is_field(x[.__properties__.], name)) {
  #     stop(sprintf(
  #       "'%1$s' is a property, not a method, use '@%1$s <- value' instead",
  #       name
  #     ))
  #   }
  #   stop(sprintf("No method named '%s'", name))
  # }

  # assign(name, value, x[.__methods__.])
  assign(name, value, x)
  x
}

ec_dollar <- function(object, name) {
  if (!name %in% get(".__restricted__.", object)) {
    stop(sprintf("Cannot access '%s'", name))
  }
  get(name, envir = object, inherits = FALSE)
}

#' #' @export
#' `[[.ec_capsule` <- function(x, i, ...) {
#'   if (i %in% names(x)) {
#'     return(get(i, x, inherits = FALSE))
#'   }
#'
#'   stop(sprintf("No element named '%s'", i))
#' }
#'
#' #' @export
#' `[[.ec_generator` <- function(x, i, ...) {
#'   i <- substitute(i)
#'   i <- as.character(i)
#'   `[[.ec_capsule`(environment(x), i, ...)
#' }
#'
#' #' @export
#' `[.ec_capsule` <- function(x, i, ...) {
#'   i <- substitute(i)
#'   i <- as.character(i)
#'   `[[.ec_capsule`(x, i, ...)
#' }
#'
#' #' @export
#' `[.ec_generator` <- function(x, i, ...) {
#'   i <- substitute(i)
#'   i <- as.character(i)
#'   get(i, environment(x))
#' }
#'
#' #' @export
#' `[[<-.ec_capsule` <- function(x, i, value) {
#'   if (i %in% names(x)) {
#'     # unlockBinding(i, x)
#'     # on.exit(lockBinding(i, x), add = TRUE)
#'     assign(i, value, x)
#'     return(x)
#'   }
#'
#'   stop(sprintf("No element named '%s'", i))
#' }
#'
#' #' @export
#' `[<-.ec_capsule` <- function(x, i, value) {
#'   i <- substitute(i)
#'   i <- as.character(i)
#'   `[[<-.ec_capsule`(x, i, value)
#' }

#' @exportS3Method utils::.DollarNames
.DollarNames.ec_capsule <- function(x, pattern = "") {
  # dot_names(x[.__methods__.], pattern)
  grep(pattern, x$.__restricted__., value = TRUE, fixed = TRUE)
}

#' @exportS3Method utils::.DollarNames
.DollarNames.ec_generator <- function(x, pattern = "") {
  # dot_names(environment(x)[.__methods__.], pattern)
  grep(pattern, environment(x)$.__restricted__., value = TRUE, fixed = TRUE)
}

#' @exportS3Method utils::.AtNames
.AtNames.ec_capsule <- function(x, pattern = "") {
  # dot_names(x[.__properties__.], pattern)
  at_names(x, pattern)
}

#' @exportS3Method utils::.AtNames
.AtNames.ec_generator <- function(x, pattern = "") {
  # dot_names(environment(x)[.__properties__.], pattern)
  at_names(environment(x), pattern)
}

at_names <- function(x, pattern = "") {
  nms <- names(x)
  nms <- nms[match(nms, x$.__restricted__., 0L) == 0L]
  grep(pattern, nms, value = TRUE, fixed = TRUE)
}

# helpers -----------------------------------------------------------------

dot_names <- contain(function(x, pattern) {
  nms <- names(x)
  grep(pattern, nms[!startsWith(nms, ".")], value = TRUE, fixed = TRUE)
})

is_field <- contain(function(x, name) {
  name %in% names(x)
})
