devtools::load_all(".")


# mock --------------------------------------------------------------------

# debugonce(new_capsule)
my_capsule <- new_capsule({
  # use 'active()' to provide setters and getters for properties.  These are
  # triggered when the property is accessed or modified, allowing for custom
  # validations and/or side effects.

  #' @field a A non-negative integer property
  a <- active(
    default = 0L,
    set = function(value) {
      self$.log()
      value <- as.integer(value) # coerce to integer
      stopifnot(value >= 0) # validate non-negative
      value
    },
    get = function() {
      self$.log()
      ..value..
    }
  )

  #' @field b b A non-negative integer property that resets 'a' when set
  b <- active(
    default = 0L,
    # 'b' will reset 'a' to the same value
    set = function(value) {
      self$.log()
      value <- as.integer(value) # coerce to integer
      stopifnot(value >= 0L) # validate non-negative
      self@a <- value # side effect
      self$.log()
      value
    },
    get = function() {
      self$.log()
      ..value..
    }
  )

  c <- lock(TRUE)

  .results <- integer()

  #' @description Add a non-negative integer to 'a' and return the sum of 'a',
  #' 'b', and the argument.
  #' @param x A non-negative integer to add to 'a'
  add <- lock(function(x = 0L) {
    self@a <- self@a + x
    self$.log()
    self@a + self@b + as.integer(x)
  })

  #' @description Show the current values of 'a' and 'b'
  logs <- lock(function() {
    invisible(lapply(
      self@.logs,
      function(x)
        cat(sprintf(
          "[%s] %s\n",
          format(x$time, "%Y-%m-%d %H:%M:%S"),
          deparse1(x$call)
        ))
    ))
  })

  .logs <- NULL

  .log <- function() {
    self@.logs <- c(
      list(
        list(
          time = Sys.time(),
          call = sys.calls(),
          frame = sys.frames()
        )
      ),
      self@.logs
    )
  }

  .__new__. <- function(x = 0L) {
    stopifnot(x >= 0)
    self@a <- x
  }
})

my_capsule@c
try(my_capsule@c <- 1)
my_capsule@a
my_capsule@a <- 1L
my_capsule@b
my_capsule@b <- 2L
my_capsule@a

cap <- new_capsule({
  x <- 1
  y <- active(
    default = 2L,
    set = function(value) {
      self@x <- value
    },
    get = function() {
      ..value.. <<- ..value.. + 1L
      ..value..
    }
  )

  reset <- function() {
    self@y <- 0L
    invisible(self)
  }
})

cap@x
cap@y
cap@x
cap@y

enclass(my_capsule)


# counter -----------------------------------------------------------------

# debugonce(enclass)
Counter := enclass({
  .actions <- list()
  .track <- function(action, value) {
    self@.actions <- c(
      self@.actions,
      list(
        list(
          time = Sys.time(),
          action = action,
          value = value
        )
      )
    )
  }

  .show_actions <- function() {
    Reduce(rbind, lapply(self@.actions, as.data.frame))
  }

  current <- active(
    default = 0L,
    set = function(value) {
      value <- as.integer(value)
      stopifnot(value >= 0L)
      value
    }
  )

  add <- function(x = 1L) {
    x <- as.integer(x)
    self$.track("add", x)
    self@current <- self@current + x
    invisible(self)
  }

  show <- function() {
    cat("Current value:", self@current, "\n")
    invisible(self)
  }

  reset <- function(x = 0L) {
    stopifnot(x >= 0L)
    x <- as.integer(x)
    self$.track("reset", x)
    self@current <- x
    invisible(self)
  }
})

counter <- Counter()
# debugonce(ec_at)
counter@current
counter$add()
counter$show()
counter@current
counter$add(2)
counter$add(3L)
counter$show()
counter$reset()
counter$show()
try(counter$reset(-1L))
counter$show()
counter$.show_actions()
counter[[".__name__."]]
counter[[".__package__."]]
