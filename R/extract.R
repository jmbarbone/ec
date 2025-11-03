#' @export
`@.ec_capsule` <- function(object, name) {
  name <- substitute(name)
  name <- as.character(name)
  ec_at(object, name)
}

#' @export
`@.ec_generator` <- function(object, name) {
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

  assign(name, value, object)
  object
}

#' @export
`$.ec_capsule` <- function(x, name) {
  name <- substitute(name)
  name <- as.character(name)
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
  assign(name, value, x)
  x
}

ec_dollar <- function(object, name) {
  if (!name %in% get(".__restricted__.", object)) {
    stop(sprintf("Cannot access '%s'", name))
  }
  get(name, envir = object, inherits = FALSE)
}


#' @exportS3Method utils::.DollarNames
.DollarNames.ec_capsule <- function(x, pattern = "") {
  grep(pattern, x$.__restricted__., value = TRUE, fixed = TRUE)
}

#' @exportS3Method utils::.DollarNames
.DollarNames.ec_generator <- function(x, pattern = "") {
  grep(pattern, environment(x)$.__restricted__., value = TRUE, fixed = TRUE)
}

#' @exportS3Method utils::.AtNames
.AtNames.ec_capsule <- function(x, pattern = "") {
  at_names(x, pattern)
}

#' @exportS3Method utils::.AtNames
.AtNames.ec_generator <- function(x, pattern = "") {
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
