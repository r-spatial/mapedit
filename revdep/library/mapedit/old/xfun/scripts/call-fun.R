# This script is executed via the command line `Rscript call-fun.R arg1 arg2
# arg3`, where arg1 is a path to an .rds file, which contains the function and
# its arguments saved as a list; arg2 is a path to an .rds file to which the
# returned value of the function call is saved; arg3 saves the error message.

local({
  if (length(a <- commandArgs(TRUE)) != 3)
    stop('The number of arguments passed to Rscript should be 3.')
  # save the error message on exit if an error occurred
  on.exit(if (!file.exists(a[2])) saveRDS(geterrmessage(), a[3]))
  x = readRDS(a[1])  # list(fun, args)
  f = x[[1]]
  if (is.character(f)) f = eval(parse(text = f), envir = globalenv())
  r = do.call(f, x[[2]], envir = globalenv())
  saveRDS(r, a[2])
})
