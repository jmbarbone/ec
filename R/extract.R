#' @export
`@.ec_capsule` <- function(object, name) {
  name <- substitute(name)

  if (name == "self") {
    return(object)
  }

  if (!name %in% names(object[[".__properties__."]])) {
    if (name %in% names(object[[".__methods__."]])) {
      stop(sprintf(
        "'%1$s' is a method, not a property, use '$%1$s()` instead",
        name
      ))
    }
    stop(sprintf("No property named '%s'", name))
  }

  # if (isTRUE(object[[".__getter_invoked__."]][[name]])) {
  #   return(get(name, object[[".__properties__."]]))
  # }
  #
  # assign(name, TRUE, object[[".__getter_invoked__."]])
  # on.exit(assign(name, FALSE, object[[".__getter_invoked__."]]))
  # get(name, get(".__getters__.", object))()
  get(name, object[[".__properties__."]], inherits = FALSE)
}

#' @export
`@<-.ec_capsule` <- function(object, name, value) {
  name <- substitute(name)
  if (!name %in% names(get(".__properties__.", object))) {
    if (name %in% names(get(".__methods__.", object))) {
      stop(sprintf(
        "'%1$s' is a method, not a property, use '$%1$s <- value' instead",
        name
      ))
    }
    stop(sprintf("No property named '%s'", name))
  }

  # hmm... makeActiveBinding() may be a better solution here...
  # if (isTRUE(object[[".__setter_invoked__."]][[name]])) {
  #   return(assign(name, value, object[[".__properties__."]]))
  # }
  #
  # assign(name, TRUE, object[[".__setter_invoked__."]])
  # on.exit(assign(name, FALSE, object[[".__setter_invoked__."]]))
  # get(name, object[[".__setters__."]], inherits = FALSE)(value)
  # unlockBinding(name, object[[".__properties__."]])
  # on.exit(lockBinding(name, object[[".__properties__."]]), add = TRUE)
  assign(name, value, object[[".__properties__."]])
  invisible(object)
}

#' @export
`$.ec_capsule` <- function(x, name) {
  name <- substitute(name)
  name <- as.character(name)
  get(name, x[[".__methods__."]], mode = "function", inherits = FALSE)
}

#' @export
`$<-.ec_capsule` <- function(x, name, value) {
  name <- substitute(name)
  name <- as.character(name)
  value <- match.fun(value)
  assign(name, value, x[[".__methods__."]])
  invisible(x)
}

#' @export
`[[.ec_capsule` <- function(x, i, ...) {
  if (i %in% names(x)) {
    return(get(i, x, inherits = FALSE))
  }

  stop(sprintf("No element named '%s'", i))
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

#' @exportS3Method utils::.DollarNames
.DollarNames.ec_capsule <- function(x, pattern = "") {
  methods <- names(x[[".__methods__."]])

  if (!startsWith(pattern, ".")) {
    methods <- methods[!startsWith(methods, ".")]
  }

  grep(pattern, methods, fixed = TRUE, value = TRUE)
}

#' @exportS3Method utils::.AtNames
.AtNames.ec_capsule <- function(x, pattern = "") {
  properties <- names(x[[".__properties__."]])

  # does this not work?
  if (!startsWith(pattern, ".")) {
    properties <- properties[!startsWith(properties, ".")]
  }

  grep(pattern, c("self", properties), fixed = TRUE, value = TRUE)
}
