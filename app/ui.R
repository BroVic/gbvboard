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
        selectInput(ctrl$project$id, ctrl$project$lab, c(opts$allopts, projectNames)),
        selectInput(ctrl$state$id, ctrl$state$lab, opts$allopts),
        selectInput(ctrl$tables$id, ctrl$tables$lab, names(dbTables), "Facilities"),
        wellPanel(
          selectInput(ctrl$xvar$id, ctrl$xvar$lab, ""),
          selectInput(ctrl$yvar$id, ctrl$yvar$lab, ""),
          actionButton(ctrl$reset$id, ctrl$reset$lab),
          actionButton(ctrl$invert$id, ctrl$invert$lab)
        )
      )
    ),
    column(
      width = 6, 
      wellPanel(
        plotOutput("plot"),
        downloadButton(ctrl$saveplot$id, ctrl$saveplot$lab),
        conditionalPanel(
          condition = 'output.xvar == "factor"',
          checkboxInput(ctrl$horiz$id, ctrl$horiz$lab),
          checkboxInput(ctrl$order$id, ctrl$order$lab),
          checkboxInput(ctrl$reverse$id, ctrl$reverse$lab),
          conditionalPanel(
            "output.yvar == 'factor'",
            checkboxInput(ctrl$stack$id, ctrl$stack$lab, TRUE),
            conditionalPanel(
              "input.stack == true",
              checkboxInput(ctrl$fill$id, ctrl$fill$lab, FALSE)
            )
          )
        ),
        conditionalPanel(
          "output.xvar == 'integer' || output.var == 'numeric'",
          conditionalPanel(
            "output.yvar == null",
            sliderInput(ctrl$bins$id, ctrl$bins$lab, 20, 80, 30)
          ),
          radioButtons(ctrl$log$id, ctrl$log$lab, c("None", "log2", "log10"), "None")
        )
      )
    ),
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
