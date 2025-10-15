#' template capsule
#' @section reserved names:
#'  - `self` \cr`[environment]`\cr the environment of the instance (environment)
#'  - `.__name__.` \cr`[character]`\cr the name of the class (string)
#'  - `.__package__.` \cr`[character]`\cr The package where the class is defined (string)
#'  - `.__expr__.` \cr`[language]`\cr the expression used to define the class (language object)
#'  - `.__properties__.`\cr`[environment]`\cr an environment containing proporties accessed with `@`
#'  - `.__methods__.`\cr`[environment]`\cr an environment containing methods accessed with `$`
#'  - `.__restricted__.` \cr`[character]`\cr a character vector of names that are restricted from being used as properties or methods
#'  - `.__locked__.` \cr`[character]`\cr a character vector of names that are locked from being modified
#'  - `.__new__.(...)` \cr`[function]`\cr function to create a new instance of the class
#'  - `.__init__.()` \cr`[function]`\cr function to initialize a new instance of the class
#'  - `.__clone__.()` \cr`[function]`\cr function to clone the class definition
#'
#' @export
"template"

act <- function(name, expr, env = parent.frame()) {
  expr <- substitute(expr)
  makeActiveBinding(name, eval(substitute(local(expr, env))), env)
}

template <- local({
  on.exit({
    # ensure that I don't forget anything
    .__restricted__. <- c(
      "self",
      ls(
        envir = self,
        pattern = .__restricted_pattern__.,
        all.names = TRUE
      )
    )
  })

  self <- environment()
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

  .__expr__. <- NULL
  .__restricted__. <- NULL # set on exit
  .__restricted_pattern__. <- "^\\.__.*\\__.$"
  .__properties__. <- list()
  .__methods__. <- list()
  # .__setter_invoked__. <- list() # saved over as env
  # .__getter_invoked__. <- list() # saved over as env
  # populate with defaults
  # TODO look at makeActiveBinding()
  # .__setters__. <- list() # converted to env
  # .__getters__. <- list() # converted to env
  .__restricted__. <- character()
  .__locked__. <- c(
    ".__locked__.",
    ".__restricted_pattern__.",
    ".__properties__.",
    ".__methods__.",
    # ".__setter_invoked__.",
    # ".__getter_invoked__.",
    ".__restricted__.",
    # ".__setters__.",
    # ".__getters__.",
    ".__clone__.",
    ".__init__."
  )

  # .__prop__. <- function(
  #   name,
  #   value = NULL,
  #   getter = function() self@..name..,
  #   setter = function(value) self@..name.. <- value
  # ) {
  #   body(getter) <- do.call(substitute, list(body(getter), list(..name.. = name)))
  #   body(setter) <- do.call(substitute, list(body(setter), list(..name.. = name)))
  #   self$.__setters__.[[name]] <- setter
  #   self$.__getters__.[[name]] <- getter
  #   assign(name, value, parent.frame())
  # }

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
  .__init__. <- function() {
    new <- self[[".__clone__."]]()
    args <- as.list(match.call(expand.dots = TRUE))[-1L]
    do.call(new[[".__new__."]], args, envir = new)
    # TODO different print method for ec_object
    class(new) <- c(new[[".__name__."]], "ec_object", "ec_capsule")
    new
  }

  .__clone__. <- function(env = parent.frame()) {
    e <- new.env(parent = env)
    list2env(as.list.environment(self, all.names = TRUE, sorted = TRUE), e)
  }

  self
})
