#' @export
print.ec_capsule <- function(x, ...) {
  cat("<ec_capsule>\n")
  cat("  .__name__.  ", x[[".__name__."]], "\n", sep = "")
  cat("  .__properties__.\n")
  properties <- names(x[[".__properties__."]])
  properties <- properties[!startsWith(properties, ".")]
  if (length(properties)) {
    for (p in properties) {
      cat("      @ ", p, "\n", sep = "")
    }
  } else {
    cat("    <none>\n")
  }

  cat("  .__methods__.\n")
  methods <- names(x[[".__methods__."]])
  methods <- methods[!startsWith(methods, ".")]
  if (length(methods)) {
    for (m in methods) {
      forms <- formals(x[[".__methods__."]][[m]])
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
      cat(sprintf("      $ %s(%s)\n", m, args))
    }
  } else {
    cat("    <none>\n")
  }
  invisible(x)
}
