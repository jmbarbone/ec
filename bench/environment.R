e <- asNamespace("mark")

# hash = FAlSE is much worse performance
# mget() is slightly worse than as.list()
bench::mark(
  list2env(as.list(e, all = TRUE), hash = TRUE), # good
  list2env(as.list(e, all = TRUE), hash = FALSE),
  as.environment(as.list(e, all = TRUE)), # good
  list2env(mget(names(e), e), hash = TRUE),
  list2env(mget(names(e), e), hash = FALSE),
  as.environment(mget(names(e), e)),
  rlang::env_clone(e), # okay
  check = FALSE
) |>
  (\(x) {
    print(x[c("expression", "min", "median", "itr/sec", "mem_alloc", "n_itr")])
    invisible(x)
  })() |>
  ggplot2::autoplot()


nm <- sample(names(e), 100)

bench::mark(
  mget(nm, e), # clear winner
  as.list(e)[nm],
  as.list(e, all = TRUE)[nm],
  as.list(e), # pretty okay
  mget(names(e), e), # terrible
  check = FALSE
) |>
  (\(x) {
    print(x[c("expression", "min", "median", "itr/sec", "mem_alloc", "n_itr")])
    invisible(x)
  })() |>
  ggplot2::autoplot()
