#' @export
print.ec_capsule <- function(x, ...) {
  cat("<ec_capsule>\n")
  cat("  .__name__.  ", x$.__name__., "\n", sep = "")
  invisible(x)
}

#' @export
print.ec_properties <- function(x, ...) {
  stars <- rep("", length(x))
  stars[vapply(names(x), bindingIsActive, NA, env = x)] <- "*"
  cat(
    sprintf(
      "<ec_properties> %s\n",
      paste0("\n  @ ", names(x), stars, collapse = "")
    )
  )
  invisible(x)
}

#' @export
print.ec_methods <- function(x, ...) {
  stars <- rep("", length(x))
  stars[vapply(names(x), bindingIsActive, NA, env = x)] <- "*"
  cat(
    sprintf(
      "<ec_methods> %s\n",
      paste0("\n  $ ", names(x), "()", stars, collapse = "")
    )
  )
  invisible(x)
}
