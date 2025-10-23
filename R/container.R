#' Class Capsules
#'
#' @section reserved names:
#'  - `self` \cr`[environment]`\cr the environment of the instance (environment)
#'  - `.__name__.` \cr`[character]`\cr the name of the class (string)
#'  - `.__package__.` \cr`[character]`\cr The package where the class is defined (string)
#'  - `.__expr__.` \cr`[language]`\cr the expression used to define the class (language object)
#'  - `.__properties__.`\cr`[environment]`\cr an environment containing properties accessed with `@`
#'  - `.__methods__.`\cr`[environment]`\cr an environment containing methods accessed with `$`
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
  assign(".__properties__.", list())
  assign(
    ".__locked__",
    c(
      ".__locked__.",
      ".__restricted_pattern__.",
      ".__properties__.",
      ".__methods__.",
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
      new <- self[[".__clone__."]]()
      class(new) <- c("ec_object", "ec_capsule")
      args <- as.list(match.call(expand.dots = TRUE))[-1L]
      do.call(new[[".__new__."]], args, envir = new)
      # TODO different print method for ec_object
      class(new) <- c(new[[".__name__."]], "ec_object", "ec_capsule")
      new
    }
  )

  assign(
    ".__clone__.",
    function() {
      # browser()
      clone <- new.env(parent = self, hash = TRUE)
      list2env(self[[".__as_list__."]](), clone)

      # copies environments, except for .__properties__.
      for (field in names(self)) {
        switch(
          field,
          self = {
            # this has to be done anyway
            clone[["self"]] <- clone
          },
          ".__methods__." = {
            assign(".__methods__.", new.env(parent = clone, hash = TRUE), clone)
            if (is.environment(self[[".__methods__."]])) {
              assign(
                ".__methods__.",
                clone_env(self[[".__methods__."]], clone),
                as.list(self[[".__methods__."]], all.names = TRUE),
                clone
              )
            } else {
              list2env(self[[".__methods__."]], clone[[".__methods__."]])
            }
          },
          ".__properties__." = {
            assign(
              ".__properties__.",
              new.env(parent = clone, hash = TRUE),
              clone
            )

            for (binding in names(self[[".__properties__."]])) {
              if (bindingIsActive(binding, self[[".__properties__."]])) {
                makeActiveBinding(
                  binding,
                  activeBindingFunction(binding, self[[".__properties__."]]),
                  clone[[".__properties__."]]
                )
              } else {
                assign(
                  binding,
                  get(binding, self[[".__properties__."]]),
                  clone[[".__properties__."]]
                )
              }
            }
          },
          {
            assign(field, self[[field]], clone)
            if (is.function(self[[field]])) {
              environment(clone[[field]]) <- clone
            }
          }
        )
      }

      # re-establish active bindings in .__properties__.; there are some issues
      # # when we try to make a new active binding?

      clone
    }
  )

  assign(
    ".__as_list__.",
    function(all = FALSE) {
      ls <- as_list_env(self)
      if (all) {
        for (field in names(self)) {
          if (is.environment(self[[field]])) {
            self[field] <- as_list_env(self[[field]])
          }
        }
      }
      ls
    }
  )

  # would this work? Could the assignment operator be masked and move everything
  # into the appropriate environment?
  # assign("<-", function(x, value) UseMethod("<-"))
  # assign("<-.default", function(x, value) base::`<-`(x, value))
  # assign(
  #   "<-.ec_capsule",
  #   function(x, value) {
  #     x <- substitute(x)
  #     x <- as.character(x)
  #     browser()
  #     if (is.function(value)) {
  #       assign(x, value, self[[".__methods__."]])
  #     } else if (is_active(value)) {
  #       makeActiveBinding(x, value, self[[".__properties__."]])
  #     } else {
  #       assign(x, value, self[[".__properties__."]])
  #     }
  #   }
  # )
  self
})

#' @export
as.list.ec_capsule <- function(x, ...) {
  x[[".__as_list__."]]()
}

# saving restricted variables as pseudos
invisible(lapply(
  container$.__restricted__.,
  function(r) assign(r, structure(list(), class = "pseudo"), parent.frame(2L))
))
