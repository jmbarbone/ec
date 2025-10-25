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

bench::mark(
  hashed_mget = mget(nms, e1), # third
  # unhashed_mget = mget(nms, e2), # this is just really bad
  hashed_as_list_ext = as.list(e1)[nms], # second
  unhashed_as_list_ext = as.list(e2)[nms], # first
  # hashed_as_list = as.list(e1, sorted = TRUE), # pretty bad
  # unhashed_as_list = as.list(e2, sorted = TRUE), # pretty bad
  check = TRUE,
  env = local({
    n <- runif(1e5)
    nms <- sort(as.character(n))
    ls <- structure(as.list(n), names = nms)
    e1 <- list2env(ls, hash = TRUE)
    e2 <- list2env(ls, hash = FALSE)
    environment()
  })
) |>
  (\(x) {
    print(x[c("expression", "min", "median", "itr/sec", "mem_alloc", "n_itr")])
    invisible(x)
  })() |>
  ggplot2::autoplot()


bench::mark(
  try(mget("fail", e1)),
  try(mget("fail", e2)), # obviously better
  check = FALSE,
  env = local({
    n <- runif(1e4)
    nms <- sort(as.character(n))
    ls <- structure(as.list(n), names = nms)
    e1 <- list2env(ls, hash = TRUE)
    e2 <- list2env(ls, hash = FALSE)
    environment()
  })
) |>
  (\(x) {
    print(x[c("expression", "min", "median", "itr/sec", "mem_alloc", "n_itr")])
    invisible(x)
  })() |>
  ggplot2::autoplot()
