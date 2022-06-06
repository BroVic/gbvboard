# Source file: ui.R
#
# License: MIT
#
# Copyright (c) 2022 Victor Ordu

library(shiny)
library(shinythemes)

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
        selectInput(controls$project$id, controls$project$label, choices = projectNames[1]),
        selectInput(controls$state$id, controls$state$label, choices = opts$allstates),
        selectInput(controls$tables$id, controls$tables$label, choices = names(dbTables), selected = "Facilities"),
        wellPanel(
          selectInput(controls$xvar$id, controls$xvar$label, choices = ""),
          selectInput(controls$yvar$id, controls$yvar$label, choices = ''),
          actionButton(controls$reset$id, controls$reset$label),
          actionButton(controls$invert$id, controls$invert$label)
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
          checkboxInput(controls$horiz$id, controls$horiz$label),
          checkboxInput(controls$order$id, controls$order$label),
          conditionalPanel(
            "output.yvar == 'factor'",
            checkboxInput(controls$stack$id, controls$stack$label, value = TRUE)
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
