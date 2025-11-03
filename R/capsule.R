#' Create a new class capsule
#'
#' @param expr An expression for assigning variables in new capsule.
#' @export
#' @returns An environment with class `ec_capsule`
#' @examples
#' cap <- new_capsule({
#'   x <- 1
#'   y <- active(
#'     default = 2L,
#'     set = function(value) {
#'       self@x <- value
#'     },
#'     get = function() {
#'       ..value.. <<- ..value.. + 1L
#'       ..value..
#'     }
#'   )
#'
#'   reset <- function() {
#'     self@y <- 0L
#'     invisible(self)
#'   }
#' })
#' cap@y
#' cap@y
new_capsule <- contain(function(expr) {
  # TODO include name

  # The .__clone__.() method may not be ideal, and could potentially be slowing
  # down some operations.  Instead, we could just create the container, assign
  # the appropriate fields, then lock it down.

  # FIXME okay, new idea:
  # @ accesses user slots
  # $ accesses restricted slots
  #   - uses get0(name, self, inherits = FALSE, ifnotfound = get(name, container, inherits = FALSE))
  #   - `$<-` defines in self
  #   - `$<-` doesn't allow setting those restricted in capsule
  #   - bindings are created (to restrict editing) then removed
  # [[ accesses anything

  capsule <- container$.__clone__.()
  class(capsule) <- "environment"
  # capsule <- new.env(parent = container)
  # list2env(.container(), capsule)
  # for (i in ls(container)) {
  #   if (is.function(container[[i]])) {
  #     environment(capsule[[i]]) <- capsule
  #   }
  # }

  # capsule <- new.env(parent = container)
  assign("self", capsule, capsule)
  # class(capsule) <- "ec_capsule" # should this have the ec_capsule class or not?

  for (i in get(".__locked__.", envir = container)) {
    lock_binding(capsule, i)
  }

  # methods <- new.env(parent = capsule)
  # properties <- new.env(parent = capsule)
  if (!missing(expr)) {
    # TODO consider if the syntax should explicitly reflect:
    #
    # ```r
    # new_capsule({
    #   # explicitly require assigning to `self`, otherwise is not accessible
    #   self@public_a <- 1
    #   self@.private_b <- 2
    #
    #   # this wouldn't be _too_ much effort in ec
    #   self$instance_method <- function(self, x) self@a + x
    #   self$static_method <- function(x) x * 2 # accessible via capsule
    #
    #   # this would be a bit easier
    #   self@active_property <- active()
    #   self$method <- function(self) self@public_a
    # })
    # ```
    #
    # The class for `self` may need to be temporarily changed to allow `@`, and
    # `$`to be used without signalling an error for undefined variables.
    #
    # This may also be faster as we can have `@` and `$` directly modify
    # our properties and methods environments; saving the time of moving these
    # around and recreating environments.
    #
    # I'm not so sure if we need to use `function(self)`, since we don't really
    # care about that.
    #
    # Although, we could differentiate:
    # Counter := enclass({...})
    # Counter$static_method()
    # try(Counter$instance_method()) # fails
    # Counter()$instance_method() # works

    # expr <- substitute(expr)
    # expr <- as.list(expr)
    # if (expr[[1L]] == as.symbol("{")) {
    #   expr <- expr[-1L]
    # }
    # # print(expr)
    #
    # for (e in as.list(expr)) {
    #   e <- as.list(e)
    #   ls <- as.list(e[[3L]])
    #   if (identical(ls, list())) {
    #     assign(as.character(e[[2L]]), NULL, properties)
    #     next
    #   }
    #
    #   symbol <- as.character(e[[2L]])
    #   indicator <- as.character(ls)[[1L]]
    #
    #   if (symbol %in% container$.__restricted__.) {
    #     # let the current bindings check
    #     eval(as.call(e), capsule)
    #   } else if (indicator == "`function`") {
    #     eval(as.call(e), methods)
    #   } else if (indicator == "active" || indicator == "lock") {
    #     name <- as.character(e[[2L]])
    #     eval(
    #       makeActiveBinding(name, eval(e[[3L]], capsule), properties),
    #       capsule
    #     )
    #   } else {
    #     eval(as.call(e), properties)
    #   }
    # }

    # or, we just assign these valuezs directly into the environment, and rely
    # on naming conventions for special fields .__name__.; then we could remove
    # the `[`, `[[`, `@`, `$` differences: `@` becomes slots, and `$` becomes
    # methods/properties.  For `@`, get("name", new_capsule, get("name", container))

    # for `.__field__.`:
    # get0("name", envir = self, ifnotfound = get("name", envir = container))in

    # browser()
    expr <- substitute(expr)
    assign(".__expr__.", expr, capsule)
    eval(expr, capsule)
  }

  class(capsule) <- "ec_capsule"
  # temporarily locked so that the user doesn't overwrite them directly
  # unlock_binding(capsule, ".__methods__.")
  # unlock_binding(capsule, ".__properties__.")
  nms <- setdiff_(names(capsule), capsule$.__restricted__.)
  vals <- mget(nms, capsule)

  for (i in seq_along(nms)) {
    if (inherits(vals[[i]], "active")) {
      rm(list = nms[i], envir = capsule)
      assign("..name..", nms[i], environment(vals[[i]]))
      assign("self", capsule, environment(vals[[i]]))
      makeActiveBinding(nms[i], vals[[i]], capsule)
    }
  }

  # new <- mget(user, capsule)

  # capsule[.__methods__.] <- methods
  # capsule[.__properties__.] <- properties

  # is_active <- vapply(new, inherits, NA, "active")
  # is_locked_property <- vapply(new, inherits, NA, "locked_property")
  # is_locked_method <- vapply(new, inherits, NA, "locked_method")
  # is_method <- vapply(new, is.function, NA)

  # assign non-active properties
  # capsule[.__methods__.] <- list_as_env(
  #   new[is_method & !is_locked_method],
  #   capsule
  # )

  # capsule[.__properties__.] <- list_as_env(
  #   new[!(is_method | is_active | is_locked_property)],
  #   capsule
  # )
  #
  # # actives have to be reassigned
  # for (name in user[is_active | is_locked_property]) {
  #   assign("..name..", name, environment(new[[name]]))
  #   makeActiveBinding(
  #     name,
  #     new[[name]],
  #     capsule[.__properties__.]
  #   )
  # }
  #
  # for (name in user[is_locked_method]) {
  #   assign("..name..", name, environment(new[[name]]))
  #   makeActiveBinding(
  #     name,
  #     new[[name]],
  #     capsule[.__methods__.]
  #   )
  # }
  #
  # rm(list = user, envir = capsule)
  # class(capsule$.__methods__.) <- "ec_methods"
  # class(capsule$.__properties__.) <- "ec_properties"
  # lockEnvironment(capsule$.__methods__.)
  # lockEnvironment(capsule$.__properties__.)
  # lock_binding(capsule, ".__methods__.")
  # lock_binding(capsule, ".__properties__.")

  unlock_binding(capsule, ".__init__.")
  environment(capsule$.__init__.) <- capsule
  lock_binding(capsule, ".__init__.")
  lockEnvironment(capsule)
  capsule
})
