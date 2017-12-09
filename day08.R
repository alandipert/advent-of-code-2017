library(rlang)

get <- function(env, reg) {
  if (is.null(env[[reg]])) 0 else env[[reg]]
}

compile <- function(line) {
  toks <- strsplit(line, " ")[[1]]
  opReg <- toks[[1]]
  op <- list("inc" = "+", "dec" = "-")[[toks[[2]]]]
  opArg <- as.numeric(toks[[3]])
  cmpReg <- toks[[5]]
  cmp <- toks[[6]]
  cmpArg <- as.numeric(toks[[7]])
  expr(if (.Primitive(!!cmp)(get(REG, !!cmpReg), !!cmpArg)) {
    newVal <- .Primitive(!!op)(get(REG, !!opReg), !!opArg)
    if (newVal > MAX) MAX <- newVal
    REG[[!!opReg]] <- newVal
  })
}

run <- function(inputFile) {
  runEnv <- env(REG = new_environment(), MAX = 0)
  for (line in readLines(inputFile)) eval(compile(line), envir = runEnv)
  list(maxEnd = max(unlist(as.list(runEnv$REG))), maxAllTime = runEnv$MAX)
}