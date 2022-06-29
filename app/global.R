# Source file: global.R
#
# License: MIT
#
# Copyright (c) 2022 Victor Ordu

ctrl <- list(
  project = list(id = "proj", lab = "Project"),
  state = list(id = "state", lab = "State"),
  tables = list(id = "dbtbl", lab = "Data"),
  reset = list(id = "reset", lab = "Clear"),
  invert = list(id = "invert", lab = "Invert"),
  xvar = list(id = "x", lab = "x"),
  yvar = list(id = "y", lab = "y"),
  horiz = list(id = "rotate", lab = "Horizontal layout"),
  order = list(id = "order", lab = "Order by frequency"),
  stack = list(id = "stack", lab = "Stack bars"),
  reverse = list(id = "reverse", lab = "Reverse order"),
  bins = list(id = "bins", lab = "No. of bins"),
  saveplot = list(id = 'saveplot', lab = 'Save...'),
  log = list(id = "log", lab = "Log transform")
)

opts <- list(allstates = "All")

dbTables <- c(Facilities = "FacilitiesBasic")
projectNames <- c("NFWP")

projects <- 
  structure(projectNames, names = paste0("proj", seq_along(projectNames)))

projectStates <- structure(
  list(
    c("Taraba", "Kebbi", "Niger")
    # c("Adamawa", "Borno", "Yobe")
  ),
  names = projectNames
)
