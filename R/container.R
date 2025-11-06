#' Class Capsules
#'
#' @section reserved names:
#'  - `self` \cr`[environment]`\cr the environment of the instance (environment)
#'  - `.__capsule__.` \cr`[environment]`\cr the environment of the class definition (environment)
#'  - `.__name__.` \cr`[character]`\cr the name of the class (string)
#'  - `.__package__.` \cr`[character]`\cr The package where the class is defined (string)
#'  - `.__expr__.` \cr`[language]`\cr the expression used to define the class (language object)
#'  - `.__restricted__.` \cr`[character]`\cr a character vector of names that are restricted from being used as properties or methods
#'  - `.__locked__.` \cr`[character]`\cr a character vector of names that are locked from being modified
#'  - `.__new__.(...)` \cr`[function]`\cr function to create a new instance of the class
#'  - `.__init__.()` \cr`[function]`\cr function to initialize a new instance of the class
#'  - `.__create__.()` \cr`[function]`\cr function to spawn a new class definition from the current one
#'  - `.__setup__.()` \cr`[function]`\cr function to clone the class definition
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
  .__capsule__. <- environment()
  .__restricted_pattern__. <- "^\\.__.*\\__.$"

  on.exit({
    # ensure that I don't forget anything
    assign(
      ".__restricted__.",
      ls(
        envir = .__capsule__.,
        pattern = .__restricted_pattern__.,
        all.names = TRUE
      ),
      envir = .__capsule__.
    )

    local(
      for (binding in ls(.__capsule__., all.names = TRUE)) {
        if (inherits(get(binding, .__capsule__.), "active")) {
          value <- get(binding, .__capsule__.)
          rm(list = binding, envir = .__capsule__.)
          makeActiveBinding(binding, value, .__capsule__.)
        }
        lockBinding(binding, .__capsule__.)
      }
    )
  })

  .__name__. <- active(
    set = function(value) {
      if (!isTRUE(nzchar(as.character(value)))) {
        stop("`.__name__.` must be a non-empty string", call. = FALSE)
      }
      value
    }
  )

  .__package__. <- active(
    set = function(value) {
      if (!isTRUE(nzchar(as.character(value)))) {
        stop("`.__package__.` must be a non-empty string", call. = FALSE)
      }
      value
    }
  )

  .__expr__. <- NULL
  .__restricted__. <- character() # set on exit
  .__locked__. <- c(
    ".__locked__.",
    ".__restricted_pattern__.",
    ".__restricted__.",
    ".__setup__.",
    ".__create__.",
    ".__init__."
  )

  .__new__. <- active(
    function() NULL,
    set = function(value) {
      if (!is.function(value)) {
        stop("`.__new__.` must be a function", call. = FALSE)
      }
      value
    }
  )

  # TODO consider .__create__. vs .__init__.: .__init__. would initialize
  # properties if they aren't preset
  .__init__. <- function() {
    new <- .__setup__.()
    class(new) <- c("ec_object", "ec_capsule")
    args <- as.list(match.call(expand.dots = TRUE))[-1L]
    .__new__. <- get(".__new__.", new)
    environment(.__new__.) <- new
    do.call(.__new__., args, envir = new)

    # TODO different print method for ec_object
    class(new) <- c(.__name__., "ec_object", "ec_capsule")
    new
  }

  # create a new capsule
  .__create__. <- function() {
    capsule <- new.env(parent = container)
    list2env(
      lapply(
        as.list.environment(container, all.names = TRUE),
        function(x) {
          if (is.function(x)) {
            environment(x) <- capsule
          }
          x
        }
      ),
      capsule
    )

    capsule$self <- new.env(parent = capsule)
    class(capsule$self) <- "ec_capsule"
    capsule$.__capsule__. <- capsule
    class(capsule) <- "ec_capsule"
    capsule
  }

  # setup a new new instance
  .__setup__. <- function() {
    new <- new.env(parent = .__capsule__.)

    # list2env(
    #   lapply(
    #     as.list.environment(.__capsule__., all.names = TRUE),
    #     function(x) {
    #       if (is.function(x)) {
    #         environment(x) <- new
    #       }
    #       x
    #     }
    #   ),
    #   new
    # )

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

    class(new) <- c(.__name__., "ec_capsule")
    assign("self", new, new)
    # browser()
    new
  }

  .__capsule__.
})

#' @export
as.list.ec_capsule <- function(x, ...) {
  as.list.environment(x, all.names = TRUE)
}

self <- structure(list(), class = "pseudo")
