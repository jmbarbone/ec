#' @export
`@.ec_capsule` <- function(object, name) {
  name <- substitute(name)
  name <- as.character(name)
  eget(object, name)
}

#' @export
`@.ec_generator` <- function(object, name) {
  name <- substitute(name)
  name <- as.character(name)
  eget(environment(object), name)
}

#' @export
`@<-.ec_capsule` <- function(object, name, value) {
  name <- substitute(name)
  name <- as.character(name)
  assign(name, value, object)
  object
}

#' @export
`$.ec_capsule` <- function(x, name) {
  name <- substitute(name)
  name <- as.character(name)
  eget(x, name)
}

#' @export
`$.ec_generator` <- function(x, name) {
  name <- substitute(name)
  name <- as.character(name)
  eget(get(".__capsule__.", environment(x)), name)
}

#' @export
`$<-.ec_capsule` <- function(x, name, value) {
  name <- substitute(name)
  name <- as.character(name)
  assign(name, value, x)
  x
}

#' @exportS3Method utils::.DollarNames
.DollarNames.ec_capsule <- function(x, pattern = "") {
  dollar_names(x, pattern)
}

#' @exportS3Method utils::.DollarNames
.DollarNames.ec_generator <- function(x, pattern = "") {
  dollar_names(get(".__capsule__.", environment(x)), pattern)
}

#' @exportS3Method utils::.AtNames
.AtNames.ec_capsule <- function(x, pattern = "") {
  at_names(x, pattern)
}

#' @exportS3Method utils::.AtNames
.AtNames.ec_generator <- function(x, pattern = "") {
  at_names(environment(x), pattern)
}

# helpers -----------------------------------------------------------------

dollar_names <- function(x, pattern = "") {
  grep(pattern, ls(x, all.names = TRUE), value = TRUE, fixed = TRUE)
}

at_names <- function(x, pattern = "") {
  grep(pattern, names(x), value = TRUE, fixed = TRUE)
}

eget <- function(env, name) {
  get(name, envir = env, inherits = FALSE)
}
