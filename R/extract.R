#' @export
`@.ec_capsule` <- function(object, name) {
  name <- substitute(name)
  name <- as.character(name)
  eget(eget(object, "self"), name)
}

#' @export
`@.ec_generator` <- function(object, name) {
  name <- substitute(name)
  name <- as.character(name)
  eget(eget(environment(object), "self"), name)
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
  eget(eget(environment(x), ".__capsule__."), name)
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
  dollar_names(eget(environment(x), ".__capsule__."), pattern)
}

#' @exportS3Method utils::.AtNames
.AtNames.ec_capsule <- function(x, pattern = "") {
  at_names(eget(x, "self"), pattern)
}

#' @exportS3Method utils::.AtNames
.AtNames.ec_generator <- function(x, pattern = "") {
  at_names(eget(environment(x), "self"), pattern)
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
