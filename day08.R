library(rlang)

compile <- function(line) {
  tokens <- unlist(mapply(function(f, x) f(x),
                   list(identity, identity, as.numeric, sym, identity, identity, as.numeric),
                   strsplit(line, " ")[[1]]))
  quo({
    testVal <- if(is.null(registers[[!!tokens[[5]]]])) 0 else registers[[!!tokens[[5]]]]
    if (.Primitive(!!tokens[[6]])(testVal, !!tokens[[7]])) {
      reg <- !!tokens[[1]]
      prev <- if (is.null(registers[[reg]])) 0 else registers[[reg]]
      op <- !!if (tokens[[2]] == "inc") "+" else "-"
      registers[[reg]] <- .Primitive(op)(prev, !!tokens[[3]])
    }
  })
}

maxRegister <- function(registers) {
  if (length(registers) == 0) 0 else max(unlist(as.list(registers)))
}

run <- function(inputFile) {
  registers <- new_environment()
  evalEnv <- new_environment(data = list(registers = registers))
  maxReg <- 0
  for (expr in lapply(readLines(inputFile), compile)) {
    eval_tidy(expr, evalEnv)
    nextMax <- maxRegister(registers)
    if (nextMax > maxReg) maxReg <- nextMax
  }
  list(highestEnd = maxRegister(registers),
       highestEver = maxReg)
}