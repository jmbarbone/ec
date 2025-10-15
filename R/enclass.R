#' Create an encapsulated class
#'
#' @param name An optional name for the class.  If passed an `ec_capsule` object, ignores `expr`.
#' @inheritParams new_capsule
#' @param package An optional package name for the class.
#'
#' @description
#' `ec_object`s are very flexible, and follow a simple set of rules:
#'   - **properties** are accessed with `@`
#'     - _active bindings_ can be issues with `active()` to provide validators for setting and getting values
#'     - new **properties** cannot be (easily) added to objects
#'   - **methods** are accessed with `$`
#'   - `self` is a special **property** that refers to the `ec_object` itself, an can be used inside **methods** and **properties** with _active bindings_
#'   - `environment(ec_object)` is an `ec_capsule` environment
#'   - objects (i.e., **properties** and **methods**) are defined in the `expr` argument
#'     - all **objects** assigned will be assigned to the `ClassObject`
#'     - `self@<prop> <- value` and `self$<method> <- function(...) {}` are equivalent to `<prop> <- value` and `<method> <- function(...) {}`;
#'     - **methods** do not need
#'   - objects defined with `.` names are considered _private_
#'     - _private_ objects are still accessible
#'     - _private_ objects are hidden in tab completions
#'   - objects defined with `.__*__.` names are considered _reserved_
#'     - some reserved objects can be overridden (e.g., `.__new__.()`, `.__name__.`)
#'     - _reserved_ objects are usually only accessed with `ec_object[[reserved]]`
#'     - all _reserved_ objects are directly stored within the `ec_capsule` environment
#'
#' @section Recommendations:
#'   `ec_object`s are meant to prioritize flexibility and ease of use over strict requirements.
#'   The below are some opinions on best practices, but are not enforced:
#'   - avoid modifying **properties** and **methods** of a `ec_object` after it is created
#'   - avoid calling  _private_ objects from classes (especially if you're not the author)
#'   - use `self` and explicit namespace accessing (e.g., `pkg::name`) within **methods** to avoid incorrect scoping
#'   - avoid using `<<-` within **methods**, use `self` instead, e.g., `self@prop <- value`
#'   - if you need temporary variables, wrap your expression in `local()` or be sure to use `rm()` to clean up afterwards
#'   - use _private_ objects for things you don't want/need users to call directly, or are only called within **methods**
#'
#' @return A object with class `ec_object`
#' @aliases ec_object
#' @examples
#' Counter := enclass({
#'   .actions <- list()
#'   .track <- function(action, value) {
#'     self$.actions <- c(
#'       self$.actions,
#'       list(
#'         list(
#'           time = Sys.time(),
#'           action = action,
#'           value = value
#'         )
#'       )
#'     )
#'   }
#'
#'   .show_actions <- function() {
#'     Reduce(rbind, lapply(.actions, as.data.frame))
#'   }
#'
#'   current <- active(
#'     default = 0L,
#'     set = function(value) {
#'       value <- as.integer(value)
#'       stopifnot(value >= 0L)
#'       value
#'     }
#'   )
#'
#'   add <- function(x = 1L) {
#'     x <- as.integer(x)
#'     self$.track("add", x)
#'     self@current <- self@current + x
#'     invisible(self)
#'   }
#'
#'   show <- function() {
#'     cat("Current value:", self@current, "\n")
#'     invisible(self)
#'   }
#'
#'   reset <- function(x = 0L) {
#'     stopifnot(x >= 0L)
#'     x <- as.integer(x)
#'     self$.track("reset", x)
#'     self@current <- x
#'     invisible(self)
#'   }
#' })
#'
#' counter <- Counter()
#' counter@current
#' counter$add()
#' counter$show()
#' counter@current
#' counter$add(2)
#' counter$add(3L)
#' counter$show()
#' counter$reset()
#' counter$show()
#' try(counter$reset(-1L))
#' counter$show()
#' counter$.show_actions()
#' counter@.__name__.
#' counter@.__package__.
enclass <- function(
  name,
  expr = NULL,
  package = .package()
) {
  expr <- substitute(expr)
  if (inherits(name, "ec_capsule") && is.null(expr)) {
    capsule <- name
    name <- NULL
  } else {
    capsule <- eval(substitute(new_capsule(expr)))
  }

  if (!is.null(name)) {
    capsule[[".__name__."]] <- name
  }

  if (!is.null(package)) {
    capsule[[".__package__."]] <- package
  }

  # temporary locked to prevent user from overwriting
  # browser()
  (base::unlockBinding)(".__init__.", capsule)
  formals(capsule[[".__init__."]]) <- formals(capsule[[".__new__."]])
  lockBinding(".__init__.", capsule)
  capsule[[".__init__."]]
}

# fuj:::package
.package <- function(env = parent.frame()) {
  top <- topenv(env)
  if (isNamespace(top)) {
    unname(getNamespaceName(top))
  }
}
