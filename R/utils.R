# some slightly more efficient functions

clone_env_ <- contain(function(env) {
  as.environment(as.list(env, all = TRUE))
})

filter_ <- contain(function(x, f) {
  x[vapply(x, f, NA)]
})

env_to_list <- contain(function(env) {
  as.list.environment(env, all.names = TRUE, sorted = FALSE)
})

with_binding <- contain(function(name, env, expr) {
  (base::unlockBinding)(name, env)
  on.exit(lockBinding(name, env), add = TRUE)
  force(expr)
})
