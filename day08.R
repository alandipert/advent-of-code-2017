library(rlang)

get <- function(env, reg) {
  if (is.null(env[[reg]])) 0 else env[[reg]]
}

compile <- function(line) {
  toks <- strsplit(line, " ")[[1]]
  op_reg <- toks[[1]]
  op <- list("inc" = "+", "dec" = "-")[[toks[[2]]]]
  op_arg <- as.numeric(toks[[3]])
  cmp_reg <- toks[[5]]
  cmp <- toks[[6]]
  cmp_arg <- as.numeric(toks[[7]])
  expr(if (.Primitive(!!cmp)(get(REG, !!cmp_reg), !!cmp_arg)) {
    newVal <- .Primitive(!!op)(get(REG, !!op_reg), !!op_arg)
    if (newVal > MAX) MAX <- newVal
    REG[[!!op_reg]] <- newVal
  })
}

run <- function(inputFile) {
  runEnv <- env(REG = new_environment(), MAX = 0)
  exprs <- lapply(readLines(inputFile), compile)
  for (e in exprs) eval(e, envir = runEnv)
  list(maxEnd = max(unlist(as.list(runEnv$REG))), maxAllTime = runEnv$MAX)
}