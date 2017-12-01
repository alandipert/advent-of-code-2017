day1a <- function(s) {
  xs <- as.integer(unlist(strsplit(s,"")))
  sum(ifelse(diff(c(xs, xs[1])) == 0, xs, 0))
}

day1b <- function(s) {
  xs <- as.integer(unlist(strsplit(s,"")))
  len <- length(xs)/2
  sum(ifelse(diff(c(xs, xs[1:len]), lag = len) == 0, xs, 0))
}