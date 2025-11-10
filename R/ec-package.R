#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# | python          | ec              |
# |-----------------|-----------------|
# | class method    | class method    |
# | static method   | simple method   |
# | instance method | object method   |
# | field           | public field    |
# | _field          | private field   |

# Person := enclass({
#   age <- 0
#   name <- "name"
#   .created <- NULL
#   greet <- function() message("Hello, ", self@name)
#   nothing <- function() NULL
#
#   .__new__. <- function(name, age) {
#     self@name <- name
#     self@age <- age
#     self@.created <- Sys.time()
#   }
# })
#
# person <- Person()
# person@age       # public field
# person@greet()   # object method
# person@nothing() # simple method
# person@.created  # private field
