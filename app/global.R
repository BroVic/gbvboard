controls <- list(
  project = list(id = "proj", label = "Project"),
  state = list(id = "state", label = "State"),
  tables = list(id = "dbtbl", label = "Data"),
  reset = list(id = "reset", label = "Clear"),
  invert = list(id = "invert", label = "Invert"),
  xvar = list(id = "x", label = "x"),
  yvar = list(id = "y", label = "y"),
  horiz = list(id = "rotate", label = "Horizontal layout"),
  order = list(id = "order", label = "Order by frequency")
)

opts <- list(allstates = "All")

dbTables <- c(Faciities = "FacilitiesBasic")
projectNames <- c("NFWP")
projects <- 
  structure(projectNames, names(paste0("proj", seq_along(projectNames))))
projectStates <- list(
  NFWP = c("Taraba", "Kebbi", "Niger")
)