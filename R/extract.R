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
`@.ec_object` <- function(object, name) {
  name <- substitute(name)
  name <- as.character(name)
  got <- eget(eget(object, "self"), name)

  # maybe this is something done when created
  # explicitly require a 'self' parameter?
  if (is.function(got) && identical(names(formals(got)[1L]), "self")) {
    formals(got) <- formals(got)[-1L]
    environment(got) <- object@self
  }

  got
}

#' @export
`@<-.ec_capsule` <- function(object, name, value) {
  name <- substitute(name)
  name <- as.character(name)
  assign(name, value, object)
  object
}

# TODO maybe handle object methods?
# method <- function(self, thing)
# https://github.com/hadley/proto/blob/833b200e0441a5acc91e45ec06667fcdbd055261/R/proto.R#L329-L344
# and throw a check?

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

#' @export
`$.ec_object` <- function(x, name) {
  eget(parent.env(x@self), name)
}

#' @export
`$<-.ec_object` <- function(x, name, value) {
  stop("assignment via $ is not allowed for ec_object instances.")
}


#' @exportS3Method utils::.DollarNames
.DollarNames.ec_capsule <- function(x, pattern = "") {
  dollar_names(x, pattern)
}

#' @exportS3Method utils::.DollarNames
.DollarNames.ec_generator <- function(x, pattern = "") {
  dollar_names(eget(environment(x), ".__capsule__."), pattern)
}

#' @exportS3Method utils::.DollarNames
.DollarNames.ec_object <- function(x, pattern = "") {
  grep(pattern, names(parent.env(x@self)), fixed = TRUE, value = TRUE)
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
  grep(pattern, names(x), value = TRUE, fixed = TRUE)
}

at_names <- function(x, pattern = "") {
  grep(pattern, names(x), value = TRUE, fixed = TRUE)
}

eget <- function(env, name) {
  get(name, envir = env, inherits = FALSE)
}
