parseInput <- function(s) {
  as.integer(unlist(strsplit(s, "")))
}

day01a <- function(s) {
  xs <- parseInput(s)
  sum(xs[diff(c(xs, xs[1])) == 0])
}

day01b <- function(s, lag = length(xs)/2) {
  xs <- parseInput(s)
  sum(xs[diff(c(xs, xs[1:lag]), lag = lag) == 0])
}
