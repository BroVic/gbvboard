# Source file: ui.R
#
# License: MIT
#
# Copyright (c) 2022 Victor Ordu

library(shiny)
library(shinythemes)

fluidPage(
  title = "GBV Dassboard",
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
        selectInput("proj", "Project", choices = "NFWP"),
        selectInput("state", "State", choices = opts$allstates),
        selectInput("dbtbl", "Data", choices = names(dbTables), selected = "Facilities"),
        wellPanel(
          selectInput("x", "x", choices = ""),
          selectInput('y', 'y', choices = ''),
          actionButton("reset", "Clear"),
          actionButton("invert", "Invert")
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
          checkboxInput("rotate", "Horizontal layout"),
          checkboxInput("order", "Order by frequency")
        ),
        conditionalPanel(
          "output.xvar == 'integer' || output.var == 'numeric'",
          checkboxGroupInput("log", "Log transform", c("None", "log2", "log10"), "None"),
          conditionalPanel(
            "output.yvar == null",
            sliderInput("bins", "No. of bins", 20, 80, 30)
          ),
          conditionalPanel(
            "output.yvar == 'numeric' || output.yvar == 'integer'",
            checkboxInput("invert", "Invert axes")
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
