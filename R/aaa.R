# We need a portable environment for when classes are defined in different
# versions of {ec}
container <- new.env(hash = FALSE)
class(container) <- "container"

# for all functions required to generate new classes
contain <- function(expr) {
  expr <- substitute(expr)
  eval(expr, container)
}


r"(
container: All functions required to generate new capsules
template: A template capsule from which all capsules are derived.  Saved in the
  conatiner environment.
new_capsule(): Create a new capsule (object instance of class `ec_capsule`)
enclass(): Create a new class capsule (object instance of class `ec_capsule`)


Do we need to allow for the definition of a new capsule?
)"
