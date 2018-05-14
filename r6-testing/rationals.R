Rational <- R6::R6Class(
  "Rational",
  public = list(
    initialize = function(numer, denom) {
      private$.numer = numer
      private$.denom = denom
    },
    print = function() {
      g <- private$gcd(self$numer, self$denom)
      cat(paste0(self$numer / g, "/", self$denom / g))
    },
    add = function(y) {
      stopifnot("Rational" %in% class(y))
      
      Rational$new(self$numer * y$denom + self$denom * y$numer, self$denom * y$denom)
    } 
  ),
  active = list(
    numer = function(value) {
      if (missing(value)) private$.numer else stop("Numerator cannot be override")
    },
    denom = function(value) {
      if (missing(value)) private$.denom else stop("Denominator cannot be override")
    }
  ),
  private = list(
    .numer = NULL,
    .denom = NULL,
    gcd = function(x, y) {
      if (y == 0) x else gcd(y, x%%y)
    }
  )
)


x <- Rational$new(1, 2)
y <- Rational$new(3, 4)

class(y)


x$add(y)

