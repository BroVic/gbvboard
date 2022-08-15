source("util/sql/dbfuns.R")

nfwpOpts <- get_project_options("../NFWP")
nedcOpts <- get_project_options("../NEDC")

nfwpData <- combine_project_data(nfwpOpts$name, nfwpOpts$states, "../NFWP/data/nfwp.db")
nedcData <- combine_project_data(nedcOpts$name, nedcOpts$states, "../NEDC/data/nedc.db")

inspect_data(nfwpData, nedcData)

