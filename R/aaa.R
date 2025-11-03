# We need a portable environment for when classes are defined in different
# versions of {ec}
container <- new.env(hash = TRUE)
class(container) <- "container"

# for all functions required to generate new classes
contain <- function(expr) {
  expr <- substitute(expr)
  eval(expr, container)
}


r"(
container: All functions required to generate new capsules
new_capsule(): Create a new capsule (object instance of class `ec_capsule`)
enclass(): Create a new class capsule (object instance of class `ec_capsule`)

Do we need to allow for the definition of a new capsule?
)"

.container <- local({
  .. <- NULL
  function() {
    if (is.null(..)) {
      .. <<- as_list_env(container)
    }
    ..
  }
})
