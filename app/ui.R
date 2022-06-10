# Source file: ui.R
#
# License: MIT
#
# Copyright (c) 2022 Victor Ordu

library(shiny)
library(shinythemes)

ctrl <- controls

fluidPage(
  title = "GBV Dashboard",
  theme = shinytheme("darkly"),
  lang = 'en',
  
  fluidRow(
    column(
      width = 4,
      img(
        src = "jhpiego-vector-logo.svg",
        height = 200,
        width = 400,
        alt = "Jhpiego logo"
      )
    ),
    column(
      width = 4,
      titlePanel("GBV Services Mapping Data")
    ),
    column(width = 4)
  ),
  
  fluidRow(
    column(
      width = 3,
      inputPanel(
        selectInput(ctrl$project$id, ctrl$project$label, choices = projectNames[1]),
        selectInput(ctrl$state$id, ctrl$state$label, choices = opts$allstates),
        selectInput(ctrl$tables$id, ctrl$tables$label, choices = names(dbTables), selected = "Facilities"),
        wellPanel(
          selectInput(ctrl$xvar$id, ctrl$xvar$label, choices = ""),
          selectInput(ctrl$yvar$id, ctrl$yvar$label, choices = ''),
          actionButton(ctrl$reset$id, ctrl$reset$label),
          actionButton(ctrl$invert$id, ctrl$invert$label)
        )
      )
    ),
    
    # Plot Area
    column(
      width = 6, 
      wellPanel(
        plotOutput("plot"),
        conditionalPanel(
          condition = 'output.xvar == "factor"',
          checkboxInput(ctrl$horiz$id, ctrl$horiz$label),
          checkboxInput(ctrl$order$id, ctrl$order$label),
          checkboxInput(ctrl$reverse$id, ctrl$reverse$label),
          conditionalPanel(
            "output.yvar == 'factor'",
            checkboxInput(ctrl$stack$id, ctrl$stack$label, value = TRUE)
          )
        ),
        conditionalPanel(
          "output.xvar == 'integer' || output.var == 'numeric'",
          checkboxGroupInput("log", "Log transform", c("None", "log2", "log10"), "None"),
          conditionalPanel(
            "output.yvar == null",
            sliderInput("bins", "No. of bins", 20, 80, 30)
          )
        )
      )
    ),
    
    # Summary table
    column(width = 3, wellPanel(tableOutput("sumtable")))
  ),
  
  # Data table
  fluidRow(
    column(width = 3),
    column(
      width = 9,
      wellPanel(
        checkboxInput("maindata", "Show data"),
        dataTableOutput("DT")
      )
    )
  )
)
