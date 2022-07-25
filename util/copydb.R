if (Sys.info()["nodename"] != "SA-DG")
  stop("The script is customized to run on a different machine")

root <- here::here()
if (!(identical(root, getwd())))
  setwd(root)
if (!file.copy("../NFWP/data/nfwp.db", "app/data.db", overwrite = TRUE))
  cat("The database file could not be copiedd")