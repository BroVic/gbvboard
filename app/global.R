# Source file: global.R
#
# License: MIT
#
# Copyright (c) 2022 Victor Ordu
ctrl <-
  c(
    Project = "proj",
    State = "state",
    Data = "dbtbl",
    Clear = "reset",
    Invert = "invert",
    x = "x",
    y = "y",
    `Horizontal layout` = "rotate",
    `Order by frequency` = "order",
    `Stack bars` = "stack",
    Fill = "fill",
    `Reverse order` = "reverse",
    `No. of bins` = "bins",
    `Save...` = "saveplot",
    `Log transform` = "log"
  ) |>
  purrr::imap(~ list(id = .x, lab = .y)) |>
  setNames(
    c(
      "project",
      "state",
      "tables",
      "reset",
      "invert",
      "xvar",
      "yvar",
      "horiz",
      "order",
      "stack",
      "fill",
      "reverse",
      "bins",
      "saveplot",
      "log"
    )
  )

allopts = "All"
dbTables <- c(Facilities = "FacilitiesBasic")

# projects <- 
#   structure(projectNames, names = paste0("proj", seq_along(projectNames)))

projectNames <- c("NFWP", "NEDC")

projectStates <- structure(
  list(
    c("Taraba", "Kebbi", "Niger"),
    c("Adamawa", "Borno", "Yobe")
  ),
  names = projectNames
)
