library(rlang)

get <- function(env, reg) {
  if (is.null(env[[reg]])) 0 else env[[reg]]
}

compile1 <- function(line) {
  toks <- strsplit(line, " ")[[1]]
  op_reg <- toks[[1]]
  op <- sym(list("inc" = "+", "dec" = "-")[[toks[[2]]]])
  op_arg <- as.numeric(toks[[3]])
  cmp_reg <- toks[[5]]
  cmp <- sym(toks[[6]])
  cmp_arg <- as.numeric(toks[[7]])
  expr(if ((!!cmp)(get(REG, !!cmp_reg), !!cmp_arg)) {
    new_val <- (!!op)(get(REG, !!op_reg), !!op_arg)
    !!!(if (op == "+" && op_arg > 0)
      list(expr(if (new_val > MAX) MAX <- new_val)))
    REG[[!!op_reg]] <- new_val
  })
}

interpret1 <- function(line, REG, MAX) {
  toks <- strsplit(line, " ")[[1]]
  op_reg <- toks[[1]]
  op <- list("inc" = "+", "dec" = "-")[[toks[[2]]]]
  op_arg <- as.numeric(toks[[3]])
  cmp_reg <- toks[[5]]
  cmp <- toks[[6]]
  cmp_arg <- as.numeric(toks[[7]])
  if (.Primitive(cmp)(get(REG, cmp_reg), cmp_arg)) {
    new_val <- .Primitive(op)(get(REG, op_reg), op_arg)
    if (new_val > MAX$n) MAX$n <- new_val
    REG[[op_reg]] <- new_val
  }
}

run <- function(input_file, mode = "interpret") {
  if (mode == "interpret") {
    REG <- new_environment()
    MAX <- new_environment(data = list(n = 0))
    for (line in readLines(input_file))
      interpret1(line, REG, MAX)
    list(maxEnd = max(unlist(as.list(REG))),
         maxAllTime = MAX$n)
  } else if (mode == "compile") {
    run_env <- env(REG = new_environment(), MAX = 0)
    for (line in readLines(input_file))
      eval(compile1(line), envir = run_env)
    list(maxEnd = max(unlist(as.list(run_env$REG))),
         maxAllTime = run_env$MAX)
  } else {
    stop("Unknown mode")
  }
}

# library(microbenchmark)
#
# microbenchmark(run("/Users/alandipert/Desktop/input", mode = "compile"), times = 10)
# expr min    lq  mean median    uq   max neval
# 305 320.7 326.1  324.8 334.2 343.2    10
#
# microbenchmark(run("/Users/alandipert/Desktop/input", mode = "interpret"), times = 10)
# expr   min    lq  mean median    uq  max neval
# 16.54 17.48 19.46  19.56 22.01 22.6    10
