library(rlang)

get <- function(env, reg) {
  if (is.null(env[[reg]])) 0 else env[[reg]]
}

compile1 <- function(line) {
  toks <- strsplit(line, " ")[[1]]
  op_reg <- toks[[1]]
  op <- list("inc" = "+", "dec" = "-")[[toks[[2]]]]
  op_arg <- as.numeric(toks[[3]])
  cmp_reg <- toks[[5]]
  cmp <- toks[[6]]
  cmp_arg <- as.numeric(toks[[7]])
  expr(if (.Primitive(!!cmp)(get(REG, !!cmp_reg), !!cmp_arg)) {
    new_val <- .Primitive(!!op)(get(REG, !!op_reg), !!op_arg)
    if (new_val > MAX) MAX <- new_val
    REG[[!!op_reg]] <- new_val
  })
}

run <- function(input_file) {
  run_env <- env(REG = new_environment(), MAX = 0)
  for (line in readLines(input_file))
    eval(compile1(line), envir = run_env)
  list(maxEnd = max(unlist(as.list(run_env$REG))),
       maxAllTime = run_env$MAX)
}