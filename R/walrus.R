#' Walrus operator
#'
#' @param sym A variable name
#' @param val A value or expression with which `sym` is passed as a character,
#'   unnamed, as the first argument to a function call, as a
#'
#' @export
`:=` <- function(sym, val) {
  sym <- substitute(sym)
  sym <- as.character(sym) # `"sym" <- val` is fine

  val <- substitute(val)
  val <- as.list(val)
  val <- as.call(c(val[[1L]], sym, val[-1L]))

  eval.parent(call("<-", sym, val))
}
