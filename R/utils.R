# some slightly more efficient functions

setdiff_ <- contain(function(x, y) {
  x[match(x, y, 0L) == 0L]
})

filter_ <- contain(function(x, f) {
  x[vapply(x, f, NA)]
})

list_as_env <- contain(function(x, parent = parent.frame()) {
  list2env(x, new.env(parent = parent, hash = TRUE))
})

as_list_env <- contain(function(x, sort = FALSE) {
  as.list.environment(x, all.names = TRUE, sorted = sort)
})

clone_env <- contain(function(env, parent = parent.frame()) {
  list_as_env(as_list_env(env), parent = parent)
})

clone_env2 <- contain(function(env, parent = parent.frame()) {
  ls <- as_list_env(env)
  funs <- filter_(ls, is.function)
  ls[names(funs)] <- lapply(funs, function(f) {
    environment(f) <- parent
    f
  })
  list_as_env(ls, parent = parent)
})

lock_binding <- contain(function(env, name) {
  (base::lockBinding)(name, env)
})

unlock_binding <- contain(function(env, name) {
  (base::unlockBinding)(name, env)
})
