# Source file: global.R
#
# License: MIT
#
# Copyright (c) 2022 Victor Ordu

controls <- list(
  project = list(id = "proj", label = "Project"),
  state = list(id = "state", label = "State"),
  tables = list(id = "dbtbl", label = "Data"),
  reset = list(id = "reset", label = "Clear"),
  invert = list(id = "invert", label = "Invert"),
  xvar = list(id = "x", label = "x"),
  yvar = list(id = "y", label = "y"),
  horiz = list(id = "rotate", label = "Horizontal layout"),
  order = list(id = "order", label = "Order by frequency"),
  stack = list(id = "stack", label = "Stack bars"),
  reverse = list(id = "reverse", label = "Reverse bar order")
)

opts <- list(allstates = "All")

dbTables <- c(Facilities = "FacilitiesBasic")
projectNames <- c("NFWP")
projects <- 
  structure(projectNames, names = paste0("proj", seq_along(projectNames)))
projectStates <- list(
  NFWP = c("Taraba", "Kebbi", "Niger")
)
