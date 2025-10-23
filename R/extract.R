#' @export
`@.ec_capsule` <- function(object, name) {
  name <- substitute(name)
  name <- as.character(name)
  ec_at(object, name)
}

#' @export
`@.ec_generator` <- function(object, name) {
  ec_at(environment(object), name)
}

ec_at <- function(object, name) {
  if (name == "self") {
    return(object)
  }

  if (!is_field(object[.__properties__.], name)) {
    if (is_field(object[.__methods__.], name)) {
      stop(sprintf(
        "'%1$s' is a method, not a property, use '$%1$s()` instead",
        name
      ))
    }
    stop(sprintf("No property named '%s'", name))
  }

  object[.__properties__.][[name]]
}

#' @export
`@<-.ec_capsule` <- function(object, name, value) {
  name <- substitute(name)
  name <- as.character(name)

  if (name == "self") {
    stop("Cannot reassign 'self'")
  }

  if (!is_field(object[.__properties__.], name)) {
    if (is_field(object[.__methods__.], name)) {
      stop(sprintf(
        "'%1$s' is a method, not a property, use '$%1$s <- value' instead",
        name
      ))
    }
    stop(sprintf("No property named '%s'", name))
  }

  assign(name, value, object[.__properties__.])
  object
}

#' @export
`$.ec_capsule` <- function(x, name) {
  name <- substitute(name)
  name <- as.character(name)
  if (!is_field(x[.__methods__.], name)) {
    if (is_field(x[.__properties__.], name)) {
      stop(sprintf(
        "'%1$s' is a property, not a method, use '@%1$s' instead",
        name
      ))
    }
    stop(sprintf("No method named '%s'", name))
  }
  x[.__methods__.][[name]]
}

#' @export
`$.ec_generator` <- function(x, name) {
  `$.ec_capsule`(environment(x), name)
}

#' @export
`$<-.ec_capsule` <- function(x, name, value) {
  name <- substitute(name)
  name <- as.character(name)
  value <- match.fun(value)

  if (!is_field(x[.__methods__.], name)) {
    if (is_field(x[.__properties__.], name)) {
      stop(sprintf(
        "'%1$s' is a property, not a method, use '@%1$s <- value' instead",
        name
      ))
    }
    stop(sprintf("No method named '%s'", name))
  }

  assign(name, value, x[.__methods__.])
  x
}

#' @export
`[[.ec_capsule` <- function(x, i, ...) {
  if (i %in% names(x)) {
    return(get(i, x, inherits = FALSE))
  }

  stop(sprintf("No element named '%s'", i))
}

#' @export
`[[.ec_generator` <- function(x, i, ...) {
  i <- substitute(i)
  i <- as.character(i)
  `[[.ec_capsule`(environment(x), i, ...)
}

#' @export
`[.ec_capsule` <- function(x, i, ...) {
  i <- substitute(i)
  i <- as.character(i)
  `[[.ec_capsule`(x, i, ...)
}

#' @export
`[.ec_generator` <- function(x, i, ...) {
  i <- substitute(i)
  i <- as.character(i)
  get(i, environment(x))
}

#' @export
`[[<-.ec_capsule` <- function(x, i, value) {
  if (i %in% names(x)) {
    # unlockBinding(i, x)
    # on.exit(lockBinding(i, x), add = TRUE)
    assign(i, value, x)
    return(x)
  }

  stop(sprintf("No element named '%s'", i))
}

#' @export
`[<-.ec_capsule` <- function(x, i, value) {
  i <- substitute(i)
  i <- as.character(i)
  `[[<-.ec_capsule`(x, i, value)
}

#' @exportS3Method utils::.DollarNames
.DollarNames.ec_capsule <- function(x, pattern = "") {
  dot_names(x[.__methods__.], pattern)
}

#' @exportS3Method utils::.DollarNames
.DollarNames.ec_generator <- function(x, pattern = "") {
  dot_names(environment(x)[.__methods__.], pattern)
}

#' @exportS3Method utils::.AtNames
.AtNames.ec_capsule <- function(x, pattern = "") {
  dot_names(x[.__properties__.], pattern)
}

#' @exportS3Method utils::.AtNames
.AtNames.ec_generator <- function(x, pattern = "") {
  dot_names(environment(x)[.__properties__.], pattern)
}


# helpers -----------------------------------------------------------------

dot_names <- contain(function(x, pattern) {
  nms <- names(x)
  grep(pattern, nms[!startsWith(nms, ".")], value = TRUE, fixed = TRUE)
})

is_field <- contain(function(x, name) {
  name %in% names(x)
})
