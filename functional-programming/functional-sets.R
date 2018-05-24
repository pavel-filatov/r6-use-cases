# type Int => Boolean
singlton_set <- function(elem) {
  function(x) x == elem
}

# type (Int => Boolean, Int) => Boolean
contains <- function(set, x) {
  set(x)
}

# type (Int => Boolean, Int => Boolean) => (Int => Boolean)
union <- function(set_x, set_y) {
  function(x) { contains(set_x, x) | contains(set_y, x) }
}

intersect <- function(set_x, set_y) {
  function(x) { contains(set_x, x) & contains(set_y, x) }
}

diff <- function(set_x, set_y) {
  function(x) { contains(set_x, x) & !contains(set_y, x) }
}

filter <- function(set, f) {
  intersect(set, f)
}

print_set <- function(set) {
  xs <- sapply(-100:100, function(x) if (contains(set, x)) x else NA)
  xs <- xs[!is.na(xs)]
  paste(xs, collapse = ",")
}

A <- singlton_set(1)
B <- singlton_set(3)
C <- union(A, B)

print_set(C)
