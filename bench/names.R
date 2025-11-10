r"(
Use names(<env>) over ls(<env>, all.names = TRUE)
)"

mark <- asNamespace("mark")

stopifnot(
  setequal(
    names(mark),
    ls(mark, all.names = TRUE)
  ),
  setequal(
    grep("mark", names(mark), value = TRUE),
    ls(mark, pattern = "mark", all.names = TRUE)
  )
)

bench::mark(
  names(mark), # much faster
  ls(mark, all.names = TRUE),
  check = FALSE,
  iterations = 9999
) |>
  print() |>
  ggplot2::autoplot()

bench::mark(
  grep("mark", names(mark), value = TRUE), # much faster
  ls(mark, pattern = "mark", all.names = TRUE),
  check = FALSE,
  iterations = 9999
) |>
  print() |>
  ggplot2::autoplot()
