source("util/sql/dbfuns.R")

nfwpOpts <- fetch_proj_options("../NFWP")
nedcOpts <- fetch_proj_options("../NEDC")

nfwpData <-
  combine_project_data(nfwpOpts$proj.name,
                       nfwpOpts$proj.states,
                       "../NFWP/data/nfwp.db")
nedcData <-
  combine_project_data(nedcOpts$proj.name,
                       nedcOpts$proj.states,
                       "../NEDC/data/nedc.db")

inspect_data(nfwpData, nedcData)

