#' Class Capsules
#'
#' @section reserved names:
#'  - `self` \cr`[environment]`\cr the environment of the instance (environment)
#'  - `.__name__.` \cr`[character]`\cr the name of the class (string)
#'  - `.__package__.` \cr`[character]`\cr The package where the class is defined (string)
#'  - `.__expr__.` \cr`[language]`\cr the expression used to define the class (language object)
#'  - `.__restricted__.` \cr`[character]`\cr a character vector of names that are restricted from being used as properties or methods
#'  - `.__locked__.` \cr`[character]`\cr a character vector of names that are locked from being modified
#'  - `.__new__.(...)` \cr`[function]`\cr function to create a new instance of the class
#'  - `.__init__.()` \cr`[function]`\cr function to initialize a new instance of the class
#'  - `.__clone__.()` \cr`[function]`\cr function to clone the class definition
#'  - `.__as_list__.()` \cr`[function]`\cr function to convert the capsule to a list
#'
#' @name capsules
#' @keywords internal
NULL

# we don't want this to be in ls(container)
act <- contain(function(name, expr, env = parent.frame()) {
  expr <- substitute(expr)
  makeActiveBinding(
    name,
    eval(substitute(local(expr, env))),
    env
  )
})

contain({
  on.exit({
    # ensure that I don't forget anything
    assign(
      ".__restricted__.",
      c(
        "self",
        ls(
          envir = self,
          pattern = .__restricted_pattern__.,
          all.names = TRUE
        )
      ),
      envir = self
    )
  })

  assign("self", environment())
  act(".__name__.", {
    .value <- NULL
    function(value) {
      if (missing(value)) {
        return(.value)
      }

      if (!isTRUE(nzchar(as.character(value)))) {
        stop("`.__name__.` must be a non-empty string", call. = FALSE)
      }

      .value <<- value
    }
  })

  act(".__package__.", {
    .value <- NULL
    function(value) {
      if (missing(value)) {
        return(.value)
      }

      if (!isTRUE(nzchar(as.character(value)))) {
        stop("`.__package__.` must be a non-empty string", call. = FALSE)
      }

      .value <<- value
    }
  })

  assign(".__expr__.", NULL)
  assign(".__restricted__.", character()) # set on exit
  assign(".__restricted_pattern__.", "^\\.__.*\\__.$")
  assign(
    ".__locked__.",
    c(
      ".__locked__.",
      ".__restricted_pattern__.",
      ".__restricted__.",
      ".__clone__.",
      ".__init__."
    )
  )

  act(".__new__.", {
    .value <- function() NULL
    function(value) {
      if (missing(value)) {
        return(.value)
      }

      if (!is.function(value)) {
        stop("`.__new__.` must be a function", call. = FALSE)
      }

      .value <<- value
    }
  })

  # TODO consider .__create__. vs .__init__.: .__init__. would initialize
  # properties if they aren't preset
  assign(
    ".__init__.",
    function() {
      new <- self$.__clone__.()
      class(new) <- c("ec_object", "ec_capsule")
      args <- as.list(match.call(expand.dots = TRUE))[-1L]
      do.call(new$.__new__., args, envir = new)
      # TODO different print method for ec_object
      class(new) <- c(new$.__name__., "ec_object", "ec_capsule")
      new
    }
  )

  assign(
    ".__init__.",
    function() {
      new <- self$.__clone__.()
      class(new) <- c("ec_object", "ec_capsule")
      args <- as.list(match.call(expand.dots = TRUE))[-1L]
      do.call(new$.__new__., args, envir = new)
      new
    }
  )

  assign(
    ".__clone__.",
    function(deep = FALSE) {
      new <- new.env(parent = parent.env(self))
      if (deep) {
        warning("deep cloning not yet implemented")
      }

      list2env(
        lapply(
          as.list.environment(self, all.names = TRUE),
          function(x) {
            if (is.function(x)) {
              environment(x) <- new
            }
            x
          }
        ),
        new
      )
      new$self <- new
      class(new) <- class(self)
      new
    }
  )

  self
})

#' @export
as.list.ec_capsule <- function(x, ...) {
  as.list.environment(x, all.names = TRUE)
}

self <- structure(list(), class = "pseudo")
