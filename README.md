
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ec

<!-- badges: start -->

<!-- badges: end -->

The goal of `{ec}` is to provide a lightweight encapsulation system for
**R** object-oriented programming.

## Installation

You can install the development version of `{ec}` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("jmbarbone/ec")
```

## Example

`{ec}` offers another avenue for quick, flexible class setups, primarily
for advanced users.

``` r
library(ec)
Person := enclass({
  # public property
  name <- NULL
  
  # active binding property
  birthday <- active(
    default = NULL,
    set = function(value) {
      if (!inherits(value, c("character", "Date", "POSIXt"))) {
        stop(
          "`birthday` must be a character, Date, or POSIXt", 
          call. = FALSE
        )
      }
      as.Date(value)
    }
  )
  
  # private property (use `.` prefix)
  .age <- active(
    # we'll use a getter that will calculate age from birthday
    get = function() {
       as.POSIXlt(Sys.Date())$year - as.POSIXlt(self@birthday)$year
    },
    # we'll prevent .age from being modified directly
    set = function(value) {
      stop("`.age` is read-only", call. = FALSE)
    }
  )
  
  greet <- function() {
    cat("Hello, my name is", self@name, "and I am", self@.age, "years old.\n")
    invisible(self)
  }
  
  .__new__. <- function(name, birthdate) {
    self@name <- name
    self@birthday <- birthdate
    self$greet()
  }
})

Person("Rowan", "1990-01-01")
#> Hello, my name is and I am 35 years old.
#> <ec_capsule>
#>   .__name__.  Person
#>   .__properties__.
#>       @ name
#>       @ birthday*
#>   .__methods__.
#>       $ greet()
#>       $ birthday(value)
try(Person("Riley", 100)) # fails validation
#> Error : `birthday` must be a character, Date, or POSIXt
riley <- Person("Riley", "1995-05-15")
#> Hello, my name is and I am 30 years old.
riley@.age
#> [1] 30
riley@birthday <- "2005-05-15"
riley@.age 
#> [1] 20
try(riley@.age <- 30) # fails validation
#> Error : `.age` is read-only
riley$greet()
#> Hello, my name is and I am 20 years old.
```

In some ways, an `active` **property** can function like a **method**.

## Why another OOP system?

Firstly, because I got too curious about playing around with **R**’s
environments and bindings. Secondly, out of some inspiration from
**python**’s `Class` implementation, and how properties and methods are
simply defined within the class body. In **R**’s case, we can pass an
expression to specific environment and use a functions instead of
decorators to define **properties** with *getters* and *setters*.

``` python
from datetime import date


class Person(object):
    name: str
    birthday: date
    _age: int
  
    def __init__(self, name: str, birthday: date):
        self.name = name
        self.birthday = birthday
        self.greet()

    @property
    def _age(self):
        return date.today().year - self.birthday.year
    
    def greet(self):
        print(f"Hello, my name is {self.name} and I am {self._age} years old.")


Person("Rowan", date(1990, 1, 1))
#> Hello, my name is Rowan and I am 35 years old.
#> <__main__.Person object at 0x7e094bb2c5d0>

try:
    Person("Riley", 100)  # fails validation
except Exception as e:
    print(e)
#> 'int' object has no attribute 'year'

riley = Person("Riley", date(1995, 5, 15))
#> Hello, my name is Riley and I am 30 years old.
print(riley._age)
#> 30

riley.birthday = date(2005, 5, 15)
print(riley._age)
#> 20

try:
    riley._age = 30  # fails validation
except Exception as e:
    print(e)
#> property '_age' of 'Person' object has no setter

riley.greet()
#> Hello, my name is Riley and I am 20 years old.
```

The `{ec}` classes also provide a similar experience when creating a new
class. Like `ReferenceClasses`, we can simply call the closer object
`Person()` to create a new instance of the class.
