#' @export
print.ec_capsule <- function(x, ...) {
  cat("<ec_capsule>\n")
  cat("  .__name__.  ", x[.__name__.], "\n", sep = "")
  cat("  .__properties__.\n")
  properties <- names(x[.__properties__.])
  if (!is.null(properties)) {
    properties <- properties[!startsWith(properties, ".")]
  }
  if (length(properties)) {
    for (p in properties) {
      cat(
        "      @ ",
        p,
        if (bindingIsActive(p, x[.__properties__.])) "*",
        "\n",
        sep = ""
      )
    }
  } else {
    cat("    <none>\n")
  }

  cat("  .__methods__.\n")
  methods <- names(x[.__methods__.])
  if (!is.null(methods)) {
    methods <- methods[!startsWith(methods, ".")]
  }

  if (length(methods)) {
    for (m in methods) {
      forms <- formals(x[.__methods__.][[m]])
      if (is.null(forms)) {
        args <- ""
      } else {
        args <- mapply(
          function(arg, value) {
            value <- deparse1(value)
            sprintf(
              "%s%s",
              arg,
              if (isTRUE(nzchar(value))) paste0(" = ", value) else ""
            )
          },
          names(forms),
          forms,
          SIMPLIFY = TRUE,
          USE.NAMES = FALSE
        )
        args <- paste(args, collapse = ", ")
      }
      star <- if (bindingIsActive(m, x[.__methods__.])) "*" else ""
      cat(sprintf("      $ %s(%s)%s\n", m, args, star))
    }
  } else {
    cat("    <none>\n")
  }
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
